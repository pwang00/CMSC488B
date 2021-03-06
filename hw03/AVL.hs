{- 
Purely Functional Data structures
=================================
Name: Philip Wang
Date: 2/28/22
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AVL (AVL(..),
            avlEmpty,avlMember,avlInsert,avlDelete,
            isBST,heightInvariant, balanceInvariant,
            good1,good2,good3,bad1,bad2,bad3,main,rebalance,height,balanceFactor)
   where

import Prelude hiding (zipWith3)
import Test.QuickCheck hiding (elements)
import qualified Data.Foldable as Foldable
import qualified Data.List
import Control.Monad
import Data.Maybe (Maybe(Nothing))
import GHC.Base (undefined)
import Data.Bool (Bool)

{- 
The goal this homework is to implement a purely functional
version of AVL trees in Haskell. If you are unfamiliar with this data
structure, the [wikipedia page](http://en.wikipedia.org/wiki/AVL_tree)
is a good start.

AVL trees are an alternative implementation of balanced binary trees. Like
red-black trees, they can be used to implement a finite set interface.  (Note
that finite sets only retain one copy of each element inserted into the set.)

<    type AVL a  -- AVL tree containing elements of type a
-}

-- | An empty AVL tree
empty    :: AVL a
empty    = avlEmpty
{- 
>
-}

-- | Insert a new element into a tree, returning a new tree
insert   :: Ord a => a -> AVL a -> AVL a
insert   = avlInsert
{- 
>
-}

-- | Delete the provided element from the tree
delete   :: Ord a => a -> AVL a -> AVL a 
delete   = avlDelete
{- 
>
-}

-- | Determine whether an element is contained within the tree
member   :: Ord a => a -> AVL a -> Bool
member   = avlMember
{- 
>
-}

-- | List the elements in the tree, in ascending order
elements :: AVL a -> [a] 
elements = Foldable.toList


-- 1 -- Validity

{- 
AVL tree definition & validity
------------------------------

AVL trees can be implemented with the following datatype definition. This
definition is similar to that of standard binary trees, the only difference is
that nodes store the height of the tree at that point.
-}

data AVL e = E           -- empty tree
           | N           -- non-empty tree, with...
               Int       -- 1. cached height of the tree
               (AVL e)   -- 2. left subtree
               e         -- 3. value
               (AVL e)   -- 4. right subtree
    deriving (Show, Foldable)

{- 
The height of a tree is the maximum distance from a node any leaf below it. In a
wellformed `AVL` tree, we should be able to access this component straight off.
-}

-- | Access the height of the tree
height :: AVL e -> Int
height E = 0
height (N h _ _ _) = h

{- 
The balance factor corresponds to the difference in height between the left
subtree and the right subtree of the node.  An invariant of `AVL` trees is that
the balance factor must be between -1 and 1.
-}

-- | Calculate the balance factor of a node
balanceFactor :: AVL e -> Int
balanceFactor E = 0
balanceFactor (N _ l _ r) = height l - height r

{- 
As the definitions above imply, `AVL` trees must satisfy specific invariants
that ensure that the tree is balanced. In this problem, you'll need to define
quickcheck properties for those invariants.

Of course, `AVL` trees must be binary search trees.
-}

-- | The tree is a binary search tree
isBST :: Ord a => AVL a -> Bool
isBST E = True
isBST tr = all (== True) $ zipWith (<) lst xs where
    lst@(x:xs) = elements tr

{- 
And they must satisfy the AVL invariants about height and balance.
-}

-- | The height stored at each node is correctly calculated.
heightInvariant :: AVL a -> Bool
heightInvariant E = True
heightInvariant (N h l _ r) = 
    h == 1 + max (height l) (height r) && heightInvariant l && heightInvariant r
    

-- | The balance factor at each node is between -1 and +1.
balanceInvariant :: AVL a -> Bool
balanceInvariant E = True
balanceInvariant (N h l _ r) =
    abs (height l - height r) <= 1 && balanceInvariant l && balanceInvariant r

{- 
We can put these invariants together with a validity function:
-}

valid :: Ord a => AVL a -> Bool
valid t = isBST t && heightInvariant t && balanceInvariant t

{- 
And also create a QuickCheck property that informs us which of the invariants
was violated. 
-}

type A = Small Int

prop_Valid :: AVL A -> Property
prop_Valid tree = counterexample "Balance" (balanceInvariant tree) .&&.
                  counterexample "Height"  (heightInvariant tree) .&&.
                  counterexample "BST"     (isBST tree)

{- 
In order to use QuickCheck to test these properties for AVL trees, we need to
define a generator for arbitrary AVL trees. Feel free to use any of the
functions of AVL API in this implementation, even if you haven't defined
those operations yet.
-}

instance (Ord a, Arbitrary a) => Arbitrary (AVL a) where
    arbitrary :: Gen (AVL a)
    arbitrary = sized gen where 
        gen :: Int -> Gen (AVL a)
        gen 0 = return E
        gen n = do
            a <- arbitrary
            c <- gen (n `div` 2)
            return $ avlInsert a c

    shrink :: AVL a -> [AVL a]
    shrink tr = [ foldr insert E y | y <- shrink $ elements tr ] 

{- 
We can also define validity properties for the other operations.
-}

prop_DeleteValid :: AVL A -> A -> Property
prop_DeleteValid t x = prop_Valid (delete x t)

prop_ShrinkValid :: AVL A -> Property
prop_ShrinkValid t = conjoin (map prop_Valid (shrink t))

-- 2 

{- 
QuickCheck properties
---------------------

Add more QuickCheck properties to specify the correctness of your
AVL-tree based implementation of finite sets.

To assist your definitions, you may also use the following equality instance
for `AVL` trees.
-}

-- | Compare sets for equality
instance (Ord a) => Eq (AVL a) where
   t1 == t2 = elements t1 == elements t2

-- QuickCheck properties for `empty`, `insert`, `delete`, and
-- `member`.

-- Tree is empty -> no elements in its traversal
prop_Empty :: Bool
prop_Empty = null (elements E :: [a]) 

-- Insert in tree preserves sorted order
prop_Insert1 :: (Ord a) => a -> AVL a -> Property
prop_Insert1 n tr = valid tr ==> insert n tr == foldr insert E (n : elements tr)

-- Element shows up in tree after insertion
prop_Insert2 :: (Ord a) => a -> AVL a -> Property
prop_Insert2 n tr = valid tr ==> member n $ insert n tr

-- Inserting twice is same as inserting once
prop_Insert4 :: (Ord a) => a -> AVL a -> Property
prop_Insert4 n tr = valid tr ==> insert n (insert n tr) == insert n tr

-- Empty tree should not contain anything
prop_Member1 :: (Ord a) => a -> AVL a -> Property
prop_Member1 n tr = valid tr ==> member n $ insert n tr 

-- Element in tree -> element in in order traversal
prop_Member2 :: (Ord a) => a -> AVL a -> Property
prop_Member2 n tr = valid tr ==> member n (insert n tr) ==> n `elem` elements (insert n tr)

-- Deleting element from bst means it should no longer have element as member
prop_Delete1 :: (Ord a) => a -> AVL a -> Property
prop_Delete1 n tr = valid tr ==> not $ member n (delete n tr) 

-- Deleting element twice from bst is same as deleting it once
prop_Delete2 :: (Ord a) => a -> AVL a -> Property
prop_Delete2 n tr = valid tr ==> delete n (delete n tr) == delete n tr 
-- 3

{- 
AVL tree implementation
-----------------------

Define the first two operations, the empty tree and a function to lookup up
 elements in the tree.
-}

-- | An empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | Determine whether an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember e E = False
avlMember e (N h l v r) = e == v || if e < v then avlMember e l else avlMember e r 

-- 4

{- 
Sample trees
------------

Build a few particular trees that you can use as test cases
later---some that obey all of the AVL invariants...
-}

good1 :: AVL Int
good1 = N 1 E 0 E

good2 :: AVL Int
good2 = N 3 (N 1 E 3 E) 4 (N 2 (N 1 E 5 E) 6 E)

good3 :: AVL Int
good3 = N 3 (N 2 (N 1 E (-11) E) (-4) (N 1 E 5 E)) 12 (N 1 E 15 E)

{- 
... and some others that do not...
-}

bad1 :: AVL Int
bad1 = N 2 E 0 E

bad2 :: AVL Int
bad2 = N 3 (N 1 E 0 E) 0 (N 1 (N 1 E 0 E) 0 E)

bad3 :: AVL Int
bad3 = N 3 (N 2 (N 1 E (-11) E) (-4) (N 1 E 5 E)) 1 (N 1 E 15 E)

{- 
Make sure that you do NOT change the names or type annotations for these
trees. Your test cases should all have the same type, just some should
violate the correctness properties of the AVL type.
-}

trees = [("good1", good1), ("good2", good2), ("good3", good3), ("bad1", bad1), ("bad2", bad2), ("bad3", bad3)]

{- 
Now write a testing function that makes sure that the good trees are valid AVL trees
and the bad trees fail at least one property.
-}

-- v_i are good trees
-- b_i are bad trees
testProps :: IO ()
testProps = do
    putStrLn $ "good1 is valid: " ++ (show $ valid good1)
    putStrLn $ "good2 is valid: " ++ (show $ valid good2)
    putStrLn $ "good3 is valid: " ++ (show $ valid good3)
    putStrLn $ "bad1 is valid: " ++ (show $ valid bad1)
    putStrLn $ "bad2 is valid: " ++ (show $ valid bad2)
    putStrLn $ "bad3 is valid: " ++ (show $ valid bad3)


-- 5

{- 
Rebalance
---------

Write a function `rebalance` that takes a tree `e` whose root node has
balance factor -2 or +2 and rearranges it to an equivalent tree that
satifies the balance factor invariant.

For this step, you will probably find it helpful to have a good
diagram to refer to (such as the one on Wikipedia.)  Note, though,
that most explanations of AVL trees will talk about "rotating" the
nodes near the root, which implies some sort of pointer manipulation.
Here, we're simply rebuilding a completely new tree out of the pieces
of the old one, so the notion of rotating doesn't really apply.  In
particular, you may find it easier to implement the "double rotations"
that standard presentations of the algorithm talk about in a single
step.

Even so, a diagram that shows the effect such rotations are trying to
achieve is a useful guide to implementing your rearrangement.  I named
the variables in my patterns to match the labels in the diagram I was
looking at, and this made it very much easier to write the rearranged
trees correctly.
-}



-- | Rotate an AVL tree
-- Considers the 4 possible cases of an AVL tree where the balance factor is not in {-1, 0, 1}.
-- Namely LL, RR, LR, RL inbalances.  fixHeight recomputes the height for the nodes after the rebalance.

rebalance :: (Ord e) => AVL e -> AVL e
rebalance E = E
rebalance tr = 
    case tr of 
        N _ l@(N _ l2@(N _ l3 v3 r3) v2 r2) v r | map bfc [tr, l] == [2, 1] ->
            let (h, hl, hr) = fixHeight (l3, r3, r2, r) in
            N h (N hl l3 v3 r3) v2 (N hr r2 v r)
        N _ l v r@(N _ l2 v2 r2@(N _ l3 v3 r3)) | map bfc [tr, r] == [-2, -1] -> 
            let (h, hl, hr) = fixHeight (l, l2, l3, r3) in
            N h (N hl l v l2) v2 (N hr l3 v3 r3)
        N _ l v r@(N _ l2@(N _ l3 v3 r3) v2 r2) | map bfc [tr, r] == [-2, 1] ->
            let (h, hl, hr) = fixHeight (l, l3, r3, r2) in
            N h (N hl l v l3) v3 (N hr r3 v2 r2)
        N _ l@(N _ l2 v2 r2@(N _ l3 v3 r3)) v r | map bfc [tr, l] == [2, -1] ->
            let (h, hl, hr) = fixHeight (l2, l3, r3, r) in
            N h (N hl l2 v2 l3) v3 (N hr r3 v r) 
        _ -> let (N h l v r) = tr in N (1 + max (height l) (height r)) l v r 
        where 
            bfc = balanceFactor
            fixHeight (w, x, y, z) = 
                let hl = 1 + max (height w) (height x) in
                let hr = 1 + max (height y) (height z) in
                (1 + max hl hr, hl, hr)
 -- 6

{- 
Insert
------

Write an insertion function for adding new elements to AVL trees.

You should use QuickCheck to verify your implementation of
`avlInsert` -- both the fact that it correctly implements insertion and that
the resulting tree is an AVL tree.
-}

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert e E = N 1 E e E
avlInsert e (N h l v r) 
    | e < v = rebalance $ N h (avlInsert e l) v r
    | e > v = rebalance $ N h l v (avlInsert e r)
    | otherwise = N h l v r



-- 7

{- 
Delete
------

Write a function that removes an element from a tree and rebalances the
resulting tree as necessary. Again, use the properties defined above to test
your implementation, making sure that it implements the deletion operation
correctly and preserves the AVL tree properties.
-}

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete e E = E
avlDelete e (N h l v r) 
    | e < v = rebalance $ N h (avlDelete e l) v r
    | e > v = rebalance $ N h l v (avlDelete e r)
    | otherwise = 
        case (l, r) of 
            (E, E) -> E -- is leaf node
            (N h' l' v' r', E) -> rebalance $ N h' l' v' r'
            (E, N h' l' v' r') -> rebalance $ N h' l' v' r'
            (l, r) -> let (N h' l' v r') = inOrderSucc r in rebalance $ N h' l v r'
    where 
        inOrderSucc E = N h l v r
        inOrderSucc tr@(N h E v _) = tr
        inOrderSucc (N h l v r) = inOrderSucc l


        

 
            




{- 
Running QuickCheck
------------------

Using the `TemplateHaskell` extension, the following code below defines an
operation that will invoke QuickCheck with all definitions that start with
`prop_` above. This code must come *after* all of the definitions above (and
`runTests` is not in scope before this point).
-}

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  _ <- runTests
  return ()
