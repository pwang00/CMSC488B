{-
Name: Philip Wang
Date: 02/05/2022
Assignment 1
-}

{-

This is the first homework assignment for CMSC 488B. It provides
practice with the basic built-in data structures of Haskell, including
lists, tuples and maybes, as well as recursion and pattern
matching. It also covers the basics of Haskell code style and
test-driven development. If you have not read the Basics module, and
completed the associated quiz, you should do that first.

This page is a "literate" Haskell program, meaning that explanation is
interspersed with actual Haskell code. To complete your assignment,
access your private hw01 repo, edit Main.hs and submit it through
Canvas/Gradescope.

This file starts by first declaring that we are creating a module
called Main and are using functions defined in the modules Prelude,
Test.HUnit, Data.List and Data.Char.

The Prelude line imports all except for the functions listed (which
you will write). The module Prelude is special in that it is always
imported by default, so the the point of this line is not to import
more functions, but rather to exclude a few functions. (Haskell does
not allow functions to be redefined in the same module.)

The Test.HUnit line imports all functions defined in that module. The
line Data.List imports all functions from that module, but makes them
available with qualified names, such as List.intersperse, etc.

-}

module Main where
import Prelude hiding (reverse, concat, zip, (++), takeWhile, all)
import Test.HUnit

{-
The main "entry point" for this assignment runs the tests for each
homework problem below. You should not edit this definition. Instead,
your goal is to modify the problems below so that all of the tests
pass. Note that the definitions in Haskell modules do not need to come
in any particular order; here, the main function uses the definitions
testStyle, testLists, etc, even though their definitions come much
later in the file.
-} 

main :: IO ()
main = do
  runTestTT testStyle
  runTestTT testLists
  runTestTT testHO
  runTestTT testFoldr
  runTestTT testTree
  return ()

{-

Now that we have the preliminaries out of the way, we can start the actual problems.

Recall that you can load this file into ghci with the command stack
ghci Main.hs. Or, you can build the executable first with stack build
and then run the test cases above with the command line stack exec --
hw01. (For each of these, make sure that you are in the hw01
subdirectory.)

-}

--------------------------------------------------------------------------------
-- Problem (Good Style)
-------------------------------------------------------------------------------- 

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzip ]

{-

All of the following Haskell code does what it is supposed to do
(i.e. the tests pass), but it is difficult to read. Rewrite the
following expressions so that they exactly follow the style guide. Be
careful: the style guide includes quite a few rules, and we've broken
most of them in what follows! (You don't need to rewrite the test
following each part, but you do need to make sure that you don't break
the code as you refactor it!)

NOTE: Do not change the name of any of the top level declarations
below, even if you think that they aren't very good (they aren't). We
will be using automatic testing to ensure that you do not break
anything when you rewrite these functions. On the other hand, local
variables (such as function parameters and those bound by let and
where) can and should be renamed.

-}

-- Part One

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)
tabc :: Test
tabc = "abc" ~: TestList [abc True False True  ~?= True,
                          abc True False False ~?= False,
                          abc False True True  ~?= False]

-- Part Two

arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
  (b * f - c * e, c * d - a * f, a * e - b * d)

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- Part Three
reverse :: [a] -> [a]
reverse l = reverseAux l [] where
  reverseAux [] acc = acc
  reverseAux (x:xs) acc = reverseAux xs (x:acc)

treverse :: Test
treverse = "reverse" ~: TestList
    [reverse [3,2,1] ~?= ([1,2,3] :: [Int]),
     reverse [1]     ~?= ([1]     :: [Int])]

-- Part Four
zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y):zip xs ys

tzip :: Test
tzip = "zip" ~:
  TestList [ zip "abc" [True,False,True] ~?= [('a', True),('b', False), ('c', True)],
             zip "abc" [True] ~?= [('a', True)],
             zip [] [] ~?= ([] :: [(Int, Int)]) ]

--------------------------------------------------------------------------------
-- Problem (List library chops)
-------------------------------------------------------------------------------- 

{-

Define, debug and test the following functions. Some of these
functions are part of the Haskell standard prelude or standard
libraries like Data.List. Their solutions are readily available
online. You should not google for this code: instead, implement them
yourself.

For each part of this problem, you should replace the testcase for
that part based on the description in the comments. Make sure to test
with multiple inputs using TestList. We will be grading your test
cases as well as the correctness and style of your solutions! HINT:
your testing code should include any tests that we give you in the the
comments!

Do not use any list library functions in this problem. This includes
any function from the Prelude or from Data.List thats take arguments
or returns a result with a list type. Note that (:) and [] are data
constructors for the list type, not functions, so you are free to use
them. Please also avoid list comprehension syntax, as it actually
de-sugars into library functions! This also includes foldr/map/etc.
You'll get a chance to use those further below! 

-}

testLists :: Test
testLists = "testLists" ~: TestList
  [tminimumMaybe, tstartsWith, tendsWith, ttranspose, tcountSub]

-- Part One

-- | The 'minimumMaybe` function computes the mininum value of a
-- nonempty list. If the list is empty, it returns Nothing.
--
-- >>> minumumMaybe []
-- Nothing
-- >>> minumumMaybe [2,1,3]
-- Just 1 
minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe [x] = Just x
minimumMaybe (x:xs) = 
  let y:ys = xs in
  if x < y then minimumMaybe (x:ys) else minimumMaybe xs


tminimumMaybe :: Test
tminimumMaybe =
  "minimumMaybe" ~: 
  TestList [ minimumMaybe [2, 1, 3] ~?= Just 1,
             minimumMaybe []        ~?= Nothing,
             minimumMaybe [1]       ~?= Just 1 ]

-- Part Two

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False
startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith _ "" = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys


tstartsWith :: Test
tstartsWith = "startsWith" ~: 
    TestList[ "Hello" `startsWith` "Hello World!" ~?= True,
              "Hello" `startsWith` "Wello Horld!" ~?= False,
              "aaaaa" `startsWith` "aaaaa bbbbb"  ~?= True]


-- Part Three

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

endsWith :: String -> String -> Bool
endsWith a b = startsWith (reverse a) (reverse b)

tendsWith :: Test
tendsWith = "endsWith" ~: 
    TestList[ "ld!" `endsWith` "Hello World!"   ~?= True,
              "World" `endsWith` "Hello Horld!" ~?= False,
              "aaaaa" `endsWith` "aaaaa"        ~?= True ]


-- Part Four

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose (i.e. the version of transpose from Data.List).
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- >>> transpose [] 
-- []
-- >>> transpose [[]] 
-- []
-- >>> transpose [[3,4,5]]
-- [[3],[4],[5]]
-- >>> transpose [[1,2],[3,4,5]]
-- [[1,3],[2,4]]
-- (WARNING: this one is tricky!)

listTails :: [[a]] -> [[a]]
listTails [] = []
listTails [[]] = []
listTails ([]:_) = []
listTails ((y:ys):xs) = ys : listTails xs

listHeads :: [[a]] -> [a]
listHeads [] = []
listHeads ([]:_) = []
listHeads ((y:ys):xs) = y:listHeads xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose lst = (listHeads lst) : transpose (listTails lst) 

ttranspose :: Test
ttranspose = "transpose" ~: 
    TestList[ transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
              transpose [[1,2],[3,4,5]]   ~?= [[1,3],[2,4]],
              transpose ([] :: [[Int]])   ~?= ([] :: [[Int]]),
              transpose [[3,4,5]]         ~?= [[3],[4],[5]],
              transpose ([[]] :: [[Int]])   ~?= ([] :: [[Int]]) ]

-- Part Five

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

countSub :: String -> String -> Int
countSub "" "" = 1
countSub a "" = 0
countSub a (x:xs) = 
    if startsWith a (x:xs) then 1 + (countSub a xs) else (countSub a xs)

tcountSub :: Test
tcountSub = "countSub" ~: 
  TestList [ countSub "aa" "aaa"         ~?= 2,
             countSub "" "aaac"          ~?= 5,
             countSub "pepe" "pepega"    ~?= 1 ]

--------------------------------------------------------------------------------
-- Problem (Higher-order list operations)
-------------------------------------------------------------------------------- 

{-

Complete these operations which take higher-order functions as
arguments. (For extra practice, you may try to define these operations
using foldr, but that is not required for this problem.) Otherwise,
you may not use any list library functions for this problem.

-}

testHO :: Test
testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x : xs) =
  if not (f x) then [] else x : (takeWhile f xs) 

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: 
  TestList[ takeWhile (< 3) [1,2,3,4,1,2,3,4]     ~?= [1, 2],
            takeWhile (< 9) [1,2,3]               ~?= [1, 2, 3],
            takeWhile (< 0) [1,2,3]               ~?= [],
            takeWhile (> 2) [1,2,3]               ~?= [] ]

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs) = if (f x) then Just x else find f xs
tfind :: Test
tfind = "find" ~: 
  TestList [ find odd [0,2,3,4]         ~?=  Just 3,
             find even [0, 2, 3, 4]     ~?=  Just 0,
             find odd []                ~?=  Nothing ]

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False

all  :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = (f x) && (all f xs)

tall :: Test
tall = "all" ~:
  TestList [ all odd [1,2,3]      ~?= False,
             all even [2,3,4]     ~?= True,
             all odd []           ~?= True ]

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] _ = []
map2 f _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : (map2 f xs ys)

tmap2 :: Test
tmap2 = "map2" ~: 
  TestList [ map2 (+) [1,2] [3,4]   ~?=   [4, 6],
             map2 (-) [1,2] [3,4]   ~?=   [-2,-2],
             map2 (+) [1,2] [3]     ~?=   [4],
             map2 (+) [1,2] [3,4,5] ~?=   [4, 6] ]

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = 
  let y = (f x) in 
    case y of Nothing -> mapMaybe f xs
              Just y -> y : (mapMaybe f xs)

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: 
  TestList [ mapMaybe root [0.0, -1.0, 4.0]       ~?=  [0.0,2.0], 
             mapMaybe root [0.0, -1.0, -4.0]      ~?=  [2.0],
             mapMaybe root []                     ~?=  [] ]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

--------------------------------------------------------------------------------
-- Problem (map and foldr practice for lists)
-------------------------------------------------------------------------------- 

{- 

Go back to the following functions that you defined above and redefine
them using one of the higher-order functions map, foldr or para (see
below). These are the only list library functions that you should use
on this problem. If you need any additional helper functions you must
define them yourself (and any helper functions should also use map,
foldr or para instead of explicit recursion).

-}

testFoldr :: Test
testFoldr = TestList [ tconcat',  tstartsWith', tendsWith', ttails, tcountSub' ]

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
--

{-

NOTE: remember you cannot use any list functions from the Prelude or
Data.List for this problem, even for use as a helper
function. Instead, define it yourself.

-}

concat' :: [[a]] -> [a]
concat' lst = foldr (\a b -> foldr (:) b a) [] lst

tconcat' :: Test
tconcat' = "concat'" ~: 
  TestList [ concat' [[1,2,3],[4,5,6],[7,8,9]]     ~?=   [1,2,3,4,5,6,7,8,9],
             concat' ["abc", "def", "ghi"]         ~?=   "abcdefghi",
             concat' [[1, 2, 3]]                   ~?=   [1, 2, 3],
             concat' ([] :: [[Int]])                            ~?=   ([] :: [Int]) ]

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

-- NOTE: use foldr for this one, but it is tricky! (Hint: the value returned by foldr can itself be a function.)

-- x is the head of the list, y is the accumulator, apply f(x, y)
-- What do we fold over?  I believe we fold over function composition.
startsWith' :: String -> String -> Bool
startsWith' s1 s2 = foldr (\x y -> x . \z x == y) (\x -> x) 
tstartsWith' = "tstartsWith'" ~: 
    TestList[ "Hello" `startsWith'` "Hello World!" ~?= True,
              "Hello" `startsWith'` "Wello Horld!" ~?= False,
              "aaaaa" `startsWith'` "aaaaa bbbbb"  ~?= True ]

-- INTERLUDE: para

{-

Now consider a variant of foldr called para. In the case of cons,
foldr provides access to the head of the list and the result of the
fold over the tail of the list. The para function should do the same,
but should also provide access to the tail of the list (before it has
been processed).

-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = foldr' f (f x acc) xs

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x:xs) = f x xs (para f b xs)

-- For example, consider the tails function.

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
--
-- >>> tails "abc"
-- ["abc", "bc", "c", ""],
--
tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

{- 

It is a natural fit to implement tails using para. See if you can
redefine the function above so that the test cases still pass.

-}

tails' :: [a] -> [[a]]
tails' lst = para (\x y z -> (x:y):z) [[]] lst  

ttails :: Test
ttails = "tails" ~: TestList [
    "tails0" ~: tails' "abc" ~?= ["abc", "bc", "c", ""],
    "tails1" ~: tails' ""    ~?= [""],
    "tails2" ~: tails' "a"   ~?= ["a",""] ]

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

-- NOTE: use para for this one!

endsWith' :: String -> String -> Bool
endsWith' s1 s2 = para (\x y z -> z || (y == s1)) False s2

tendsWith' :: Test
tendsWith' = "endsWith'" ~: 
    TestList[ "ld!" `endsWith'` "Hello World!"   ~?= True,
              "World" `endsWith'` "Hello Horld!" ~?= False,
              "aaaaa" `endsWith'` "aaaaa"        ~?= True ]

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

-- (You may use the para and startsWith' functions in countSub'.)

countSub' :: String -> String -> Int
countSub' "" "" = 1
countSub' a "" = 0
countSub' s1 s2 = para (\x y z -> if startsWith s1 y then 1 + z else z) 1 s2
tcountSub' :: Test
tcountSub' = "countSub'" ~: 
  TestList [ countSub' "aa" "aaa"         ~?= 2,
             countSub' "" "aaac"          ~?= 5,
             countSub' "pepe" "pepega"    ~?= 1 ]

--------------------------------------------------------------------------------
-- Problem (Tree Processing)
-------------------------------------------------------------------------------- 

testTree :: Test
testTree = TestList [
    tappendTree, tinvertTree, ttakeWhileTree, tallTree, tmap2Tree ]

{-

This next problem involves writing some library functions for tree
data structures. The following datatype defines a binary tree, storing
data at each internal node.

-}

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

{- This is the definition of a mapping operation for this data structure: -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

{- And here is a fold-like operations for trees: -}

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

{- Use one of these functions to define the following operations over trees. -}

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
--
-- >>>  
-- Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
--
-- and
-- 
-- >>> appendTree Empty (Branch 'a' Empty Empty)
-- Branch 'a' Empty Empty

appendTree :: Tree a -> Tree a -> Tree a
appendTree t1 t2 = foldTree (\x y z -> (Branch x y z)) t2 t1

tappendTree :: Test
tappendTree = "appendTree" ~: 
  TestList[ appendTree Empty (Branch 'a' Empty Empty) ~?= (Branch 'a' Empty Empty),
            appendTree (Branch 'a' (Branch 'a' Empty Empty) 
            (Branch 'a' Empty Empty)) (Branch 'a' Empty Empty)  ~?= 
              (Branch 'a' (Branch 'a' (Branch 'a' Empty Empty) 
              (Branch 'a' Empty Empty)) (Branch 'a' (Branch 'a' Empty Empty) 
              (Branch 'a' Empty Empty)))
            ]

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--
-- >>> invertTree (Branch ("a",True) Empty Empty)
-- Branch (True,"a") Empty Empty

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree (\(a, b) -> (b, a)) 
tinvertTree :: Test
tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)

-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

tree2 :: Tree Int
tree2 = (Branch 5 (Branch 2 (Branch 1 Empty Empty) 
              (Branch 4 Empty Empty)) (Branch 9 Empty (Branch 7 Empty Empty)))
-- >>> takeWhileTree (< 3) tree1
-- Branch 1 (Branch 2 Empty Empty) Empty
--
-- >>> takeWhileTree (< 0) tree1
-- Empty

takeWhileTree :: (a -> Bool) ->  Tree a -> Tree a
takeWhileTree f t = 
  foldTree (\x y z -> if (f x) then (Branch x y z) else Empty) Empty t

ttakeWhileTree :: Test 
ttakeWhileTree = "takeWhileTree" ~: 
  TestList [ takeWhileTree (< 3) tree1      ~?=  
                           (Branch 1 (Branch 2 Empty Empty) Empty),
             takeWhileTree odd tree2        ~?=  
                            (Branch 5 Empty (Branch 9 Empty 
                                        (Branch 7 Empty Empty))) ]
-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
--
-- >>> allTree odd tree1
-- False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f t = (foldTree (\x y z -> x && y && z) True (mapTree f t)) 
tallTree :: Test
tallTree = "allTree" ~: 
  TestList [ allTree (> 0) (Branch 1 Empty Empty)    ~?= True,
             allTree (> 0) Empty                     ~?= True,
             allTree (> 0) (Branch 1 (Branch 2 Empty Empty) 
                             (Branch 0 Empty Empty)) ~?= False ]

-- WARNING: This one is a bit tricky!  (Hint: use `foldTree` and remember
--  that the value returned by `foldTree` can itself be a function. If you are
-- stuck on this problem, go back to `startsWith` and make sure you understand
-- how that function can work with a single fold.)

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
-- 
-- >>> map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
-- Branch 4 Empty Empty

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = undefined

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)

