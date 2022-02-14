{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-}

{- 
Merge Sort
==========
-}

module MergeSort where

import SortedList (SortedList)
import qualified SortedList as SL
import qualified Data.List as List
import Data.Monoid
import Test.HUnit

{- 
A warm-up exercise: write the function `insert`, which takes an element and a
sorted list and inserts the element into the list at the first position where
it is less than or equal to the next element. For this definition, do not use
any functions from the `Data.List` library (which, indeed, contains such a
function).
-}

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = if a <= x then a:(x:xs) else x:(insert a xs)

{- 
Using this function, we can define the *insertion sort* algorithm over
lists. Insertion sort, like several other sorting algorithms, is *incremental*
-- it works by processing a single element of the input unsorted list at a
time, and when it finishes processing the input, its work is done. Using the
`insert` function above, write a function which takes a list and returns the
list sorted. You must express your answer in terms of a fold.
-}

insertionSort :: Ord a => [a] -> [a]
insertionSort lst = foldr insert [] lst 

{- 
Keep in mind that this sorting algorithm, although succinct, is not efficient
-- it has O(N ^ 2) asymptotic complexity.

You may have noticed that the `insert` function above has an invariant
attached to its description; namely, that its list argument is already
sorted. If that invariant holds, then `insert` guarantees that its output will
also be sorted; otherwise, there is no such guarantee.

A leading question: what if we could keep track of the "sorted-ness" of a list
using the type system? You already know that we can -- that's precisely what
we did in `SortedList.hs`.
-}

-------------------------------------------------------------

{- 
SortedLists to the Rescue
-------------------------

I previously promised that the interface we built for `SortedList`s would be
sufficient to do useful things with them.  One particularly obvious useful
thing to do with `SortedList`s is to construct them from arbitrary lists --
that is, to sort an arbitrary list and produce a `SortedList` with the result.
(UPDATE: this function is already defined in the `SortedList` module as `fromList`. You
can just use that definition here.)
-}

sortedFromList :: Ord a => [a] -> SortedList a
sortedFromList = SL.fromList 

{- 
By projecting out the underlying list, we get a sorting function, that we'll
call `sortedListSort`.
-}

sortedListSort :: Ord a => [a] -> [a]
sortedListSort = SL.toList . sortedFromList

testSortedFromList :: Test
testSortedFromList =
  let unsorted = [51,67,89,95,14,31,28,87,0,25]
      sorted   = [0,14,25,28,31,51,67,87,89,95] in
  sortedListSort unsorted ~?= sorted

{- 
One thing you may have noticed while writing the above function is that there
is only one place you could have made reference to the `SortedList` type
specifically: in the use of the `singleton` operation. Indeed, this operation
is the only `SortedList`-specific way to create new `SortedList`s -- any other
way comes through the `Monoid` instance. Perhaps there's some common pattern
here that we could abstract! (There is.) Let's express it by making the
`singleton` function into a parameter of a new function, that we will call
`foldMapList`, so that we can rewrite the algorithm above like so:
-}

sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList SL.singleton

{- 
Again, we can project out the underlying list to get a list sorting function.
-}

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = SL.toList . sortedFromList'

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted :: [Int]
      unsorted = [47,80,28,47,45,76,1,35,19,1] in
  sortedListSort' unsorted ~?= sortedListSort unsorted  -- old & new agree

{- 
In order to make this work, you need to define the `foldMapList` combinator.
-}

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f = foldMap f

{- 
The type of `foldMapList` is very general --- we can use this function to
combine arbitrary lists by providing a function that maps their contents to
some particular `Monoid` (such as `SortedList`). For instance, `foldMapList
Sum` gives us the sum of the numbers in the list; `foldMap Product` gives us
their product.

Like Merge Sort, the `sortedListSort` function is based on merging sorted
lists together.  This merge-sort-like algorithm has a flaw, though: it's
quadratic in runtime. Why?
-}

benchmark :: IO ()
benchmark = (print . last . sortedListSort') ([10000,9999..0] :: [Int])

{- 
(Try it yourself by setting  `:set +s` in ghci. On my machine, it take 26.61 secs
and allocates 17,604,539,672 bytes))

For any singleton `SortedList [a]` and any other `SortedList as`, computing
`SortedList [a] <> SortedList as` is identical not only in resultant value,
but also in algorithmic structure to computing the result of `insert a
as`. The definition of `foldMapList` linearly scans across its input list,
successively combining values using `(<>)` -- and so, like insertion sort, the
whole whole algorithm ends up executing a quadratic number of comparisons.

A real merge sort algorithm, as you likely know, divides its input more
intelligently than the one we've written above in terms of `foldMapList`. By
dividing its input roughly in half every iteration, it only has to do a
logarithmic amount of merging.

To make our merge sort do this, we need to use a different kind of `foldMap`!

The Foldable Typeclass
----------------------

At this point, I'd like to point something out: `foldMapList` can itself be
even further generalized. We already know that lists are not the only data
structures which support folding -- we've seen folds for trees of various kinds
and for other data structures as well. As a result, it makes sense to allow
some kind of `foldMap` operation for those structures also. In the standard
library, we therefore have:

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

That is to say, `foldMap` is a method of yet another type class, `Foldable`, of
which the type constructor `[]` is an instance. Implementing this interface
roughly corresponds to saying, "this data structure contains some elements, and
I know how to do a fold across them." To implement the `Foldable` class for some
type, we just need to implement `foldMap`.
-}

{- 

Back to Sorting
---------------

In order to express an efficient merge sort in terms of `foldMap`, we need to
design a data structure that represents a sequence of elements (just like a
list), but whose `Foldable` instance uses a divide-and-conquer strategy, rather
than the `[]` instance's linear fold pattern of recursion.
-}

newtype DivideList a = DivideList { getDivideList :: [a] } deriving (Eq, Show)

{- 
That means that we need `DivideList`s to be `Foldable`, but in a
different way. First, implement the `divide` function, which splits a
`DivideList` in its middle, returning the result of the split.

Look at the tests to see what "in the middle" means!

-}

divide :: DivideList a -> (DivideList a, DivideList a)
divide (DivideList lst) = 
  let (a, b) = splitAt (div (length lst) 2) lst in
    (DivideList a, DivideList b)


testDivide :: Test
testDivide = TestList [ divide (DivideList "abcd") ~?=
                          (DivideList "ab", DivideList "cd"),
                        divide (DivideList "abcde") ~?=
                          (DivideList "ab", DivideList "cde"),
                        divide (DivideList "") ~?=
                          (DivideList "", DivideList "") ]

{- 
Using this function, we can define the `Foldable` instance for `DivideList`s.
Note that this definition is trickier than it seems. If you encounter an
infinite loop, it means that you have not covered one of a particular set of
slightly non-trivial edge cases.
-}

instance Foldable DivideList where
  foldMap f xs =
    case divide xs of
      (DivideList [], DivideList []) -> mempty
      (as, DivideList []) -> foldMap f as
      (DivideList [], bs) -> foldMap f bs
      (DivideList [a], DivideList [b]) -> (f a) <> (f b) 
      (DivideList [a], bs) -> (f a) <> (foldMap f bs) 
      (as, DivideList [b]) -> (foldMap f as) <> (f b) 
      (as, bs) -> (foldMap f as) <> (foldMap f bs)

testDivideList :: Test
testDivideList =
  let xs = DivideList [1,2,3]
      ys = DivideList [] in
  TestList [ Product (6 :: Int) ~?= foldMap Product xs
           , Sum (0 :: Int) ~?= foldMap Sum ys
           ]

{- 
Now that we know how general the `foldMap` function is, have a look at the
implementation of `sortedListSort'` above -- does its input type need to only be
a list? Generalize its type signature so that it outputs a list of sorted
elements located inside an arbitrary `Foldable` structure.
-}

foldSort :: (Ord a, Foldable t) => t a -> [a]
foldSort = SL.toList . foldMap SL.singleton -- implementation should use foldMap

{- 
By parameterizing over any `Foldable` container, what we've done is to *factor
out the folding strategy* into the choice of original container! To pick a
different divide-and-conquer strategy, we need only specify a different
container type, and give it a `Foldable` instance that folds along different
creases.

So, while our `sortedListSort` was O(N ^ 2), we can produce a differently
structured algorithm by instead folding over a `DivideList` instead:
-}

realMergeSort :: Ord a => [a] -> [a]
realMergeSort = foldSort . DivideList

{- 
If you've done everything correctly, this main function should return rather
quickly. This is much faster than the example above. On my machine it takes
(0.07 secs, 41,595,632 bytes).
-}

main :: IO ()
main = (print . last . realMergeSort) ([10000,9999..0] :: [Int])


{- 
Concluding Thoughts About This Exercise
---------------------------------------

The important takeaway here is this: `foldMap` defines once and for all a
universal "divide-and-conquer" algorithm -- all we need to do to use it is to
provide a way to "divide" an input container (i.e. give a `Foldable instance`),
then give a way to compute on those elements (i.e. the mapped function `a -> m`)
and a way to combine ("conquer") the results of that computation (i.e. a
`Monoid` instance for the result type).

Almost any divide-and-conquer algorithm can be fit into this framework, and that
means we can avoid repeating ourselves when writing such programs. We can reuse
the division strategy of `DivideList` when writing some other algorithm, and
likewise for the sorted-merging combination strategy of `SortedList`.
-}
