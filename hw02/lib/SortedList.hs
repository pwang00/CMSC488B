{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-}

{-
Name: Philip Wang
Date: 02/15/2022
Assignment 2
-}

{- 
Sorted Lists
============

In this small excursion, we're going to define an abstract type of *sorted
lists*. A `SortedList a` is just an ordinary list of elements of type `a`, but
ordered according to the ordering on `a`. In order to prevent users from
constructing values of this type which violate this invariant, we're defining
the type and its operations in a separate module (this one) and only exposing
to other importing modules those functions which respect the invariants of the
type.

Abstract types in Haskell
-------------------------

The identifiers after the module name below define exactly what is exported by
this module.
-}

module SortedList ( SortedList,    -- the abstract type (and its instances)
                    singleton,     -- other functions
                    toList,
                    fromList,
                    minimum,
                    numDistinct,
                    count,
                    foldr,
                    length,
                    filter
                  ) where

import Test.HUnit ( (~?=), Test(TestList) )
import Prelude hiding ( foldr, length, filter, minimum )
import qualified Data.List as List
  -- you can use List library functions, prefixed with `List.`


{- 
In this module, we will define the `SortedList` type (see below), but
*not* export the data constructor (called `SL`) for this type (see export list above).

What that means is that within this module, we can use the data constructor to
make any `SortedList` we want, even a bad one. But outside this module, we
ensure that users can construct only sorted `SortedList`s by only providing
functions that are guaranteed to construct sorted lists.  

For example, because the one-element
list is always sorted, we can safely expose the `singleton` function (see below).

What other operations should this module provide for constructing `SortedList`s? 
We will need to provide more than just `singleton`.  Because clients of this module 
don't have access to the `SL` data constructor, they won't be able create new 
`SortedList`s out of regular lists. Therefore, they *only way* they will be able
to produce a `SortedList`s will be to use the operations provided here. 

Let's focus on two operations in particular. We should provide a way to construct a 
`SortedList` with zero elements and we should provide a way to construct a new `SortedList`
by combining two existing `SortedList`s together.

Many data structures have a general notion of "make an empty structure" 
and "combine two structures together". As a result,  the Haskell standard library
includes two type classes (`Semigroup` and `Monoid`) that capture this general idea.

Semigroups and Monoids
----------------------
-}

import Data.Semigroup  -- also re-exports much of Data.Monoid

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

{- 
Sorted lists
------------

As described the above, we need to define our abstract type as a wrapper around ordinary
lists. For this, we use Haskell's `newtype` keyword, which creates a new type
much like `data`, but with the guarantee that our access to the wrapped type
will be with zero runtime overhead.

-}

newtype SortedList a = SL {getSL :: [a]} deriving (Eq, Show)

{- 
We can use pattern matching to convert the sorted list into a regular list.
-}

-- | convert to a regular list. The elements should be produced in order.
toList :: SortedList a -> [a]
toList (SL as) = as

-- | convert from a regular list.
fromList :: Ord a => [a] -> SortedList a
fromList = foldList . map singleton

{- 
Some of the operations that we define for sorted lists just delegate 
to the version for regular lists.
-}

-- | construct a sorted list containing a single element
singleton :: a -> SortedList a
singleton a = SL [a]

-- | reduce a SortedList in order
foldr :: (a -> b -> b) -> b -> SortedList a -> b
foldr f b (SL xs) = List.foldr f b xs

-- | decide which elements of the sorted list to keep
filter :: (a -> Bool) -> SortedList a -> SortedList a
filter f (SL xs) = SL (List.filter f xs)

-- | count the number of elements in the sorted list
length :: SortedList a -> Int
length (SL xs) = List.length xs

{- 
However, the `Monoid` instance can take advantage of the 
sortedness of this data structure.

Now, fill in the `Monoid` instance for `SortedList`s. You should ensure that the
list is always sorted with smaller elements (according to `(<=)` coming 
before larger elements.)

Hint: keep in mind the properties of sorted lists when writing this
instance. This invariant lets you write faster code than you would otherwise
be able to do.
-}

instance Ord a => Semigroup (SortedList a) where
  a <> b = SL $ merge (getSL a) (getSL b) where 
    merge a [] = a
    merge [] b = b
    merge (x:xs) (y:ys)
      | x <= y = x:(merge xs (y:ys))
      | otherwise = y:(merge (x:xs) ys)

instance Ord a => Monoid (SortedList a) where
  mempty = SL []


{- 
Make sure that your implementation only produces sorted lists, and also
satisfies the properties of monoids!
-}

testSortedList :: Test
testSortedList =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [2,4] 
      t2 = SL [1,5] 
      t3 = SL [2,3] in
  TestList [ t1 <> t3 ~?= SL [2,2,3,4],    -- <> preserves sorting
             mempty <> t1 ~?= t1,                  -- left identity
             t1 <> mempty ~?= t1,                  -- right identity
             (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
           ]

---------------------------------------------------------

{- 
Invariant-sensitive Operations
------------------------------

Note that we didn't *have* to define `foldr`, `filter`, and `length` for `SortedList`s. 
The clients of the module could have also defined these operations themselves by using 
`toNormalList` and the `Monoid` operations. 

However, by defining operations in this module, we 
While merely the operations defined above are sufficient to define the analogues
of most list functions for `SortedList`s also, implementing a replica of the
list library only in terms of the above abstraction would necessarily come at a
performance cost; it would necessitate conversion to and from the `SortedList`
representation, which requires computational work.

On the other hand, if we were to implement these functions *here*, we could
take advantage of the internal sorted-ness invariant of the list in order to
make certain operations *faster*. Let's do that.

A first example: `minimum`. (Note: this definition does not have the same type 
as the `minimum` function in the standard library.)
-}

minimum :: SortedList a -> Maybe a
minimum (SL []) = Nothing
minimum (SL (x:xs)) = Just x

testMinimum :: Test
testMinimum =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [1,3,5] 
      t2 = SL [] 
      t3 = SL [1, error "kaboom!"] <> SL [2] in
  TestList [ minimum t1 ~?= Just 1   -- the minimum of a non-empty sorted list
           , minimum t2 ~?= Nothing  -- the minimum of an empty sorted list
           , minimum t3 ~?= Just 1   -- minimum need not examine whole list
           ]

{- 
In the above test cases, you will get an error if your implementation does not
take advantage of the sorted-ness invariant to avoid extra computation.

Another operation which can be made more efficient for `SortedList`s is
calculating the number of distinct values in the list.
-}

numDistinct :: Ord a => SortedList a -> Int
numDistinct lst = length $ fromList $ List.nub $ toList lst

testNumDistinct :: Test
testNumDistinct = TestList
 [numDistinct (SL [1::Int,1,3,3,5]) ~?= 3,
  numDistinct (SL ([]::[Int])) ~?= 0]

{- 
We can also count how many times every distinct value occurs in the list:
-}

count :: Eq a => SortedList a -> SortedList (a, Integer)
count lst = SL (map (\x -> (x, ((toInteger . length . filter (== x)) lst))) (List.nub $ toList lst))

{- 
Your implementation of `count` should result in another genuine, legal
`SortedList`. Convince yourself that it does before moving on, keeping in mind
the `Ord` instances for tuples are left-to-right lexicographic orderings,
dependent on the underlying `Ord` instances of the tuple's elements.
-}

testCount :: Test
testCount =
  let xs = SL "abbcccdddd" in
  count xs ~?= SL [('a', 1),('b',2),('c',3),('d',4)]

{- 
At this point, one important typeclass seems to have been left out in our
interface to the `SortedList` type: `Functor`. It seems natural that we should
be able to map a function over a `SortedList`, just like we can over an ordinary
list. This doesn't work, though. Why?

WRITE YOUR ANSWER HERE:

The result of the map may not be a sorted list.  Consider the piecewise function

  \x -> if x == 3 then 0 else x

When applied to [1, 2, 3], this produces [1, 2, 0], which no longer satisfies the sorted invariant of our monoid. 

-}


{- 
At this point, we have finished defining the internal implementation of
`SortedList`s. Because all the operations we expose to the user of this module
respect the sorted-ness property of `SortedList`s, we know that any value of
this type must be sorted. So, once we import it to other files, we will be
prevented from making "illegal" values of `SortedList`s.

-}




