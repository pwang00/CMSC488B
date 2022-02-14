{-# LANGUAGE TypeApplications #-}
module Test where

import Test.HUnit

import qualified SortedList as SL
import qualified MergeSort as MS

import Data.Semigroup
import System.IO (stderr, stdout)
import Control.Monad

testSortedList :: Test
testSortedList =
  let t1, t2, t3 :: SL.SortedList Int
      t1 = SL.fromList [2,4] 
      t2 = SL.fromList [1,5] 
      t3 = SL.fromList [2,3] in
  TestList [ t1 <> t3 ~?= SL.fromList [2,2,3,4],   -- <> preserves sorting
             mempty <> t1 ~?= t1,                  -- left identity
             t1 <> mempty ~?= t1,                  -- right identity
             (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
           ]

testMinimum :: Test
testMinimum =
  let t1, t2, t3 :: SL.SortedList Int
      t1 = SL.fromList [1,3,5] 
      t2 = SL.fromList [] 
      t3 = SL.fromList [1, error "kaboom!"] <> SL.singleton 2 in
  TestList [ SL.minimum t1 ~?= Just 1   -- the minimum of a non-empty sorted list
           , SL.minimum t2 ~?= Nothing  -- the minimum of an empty sorted list
           , SL.minimum t3 ~?= Just 1   -- minimum need not examine whole list
           ]

testNumDistinct :: Test
testNumDistinct = TestList
 [ SL.numDistinct (SL.fromList [1::Int,1,3,3,5]) ~?= 3
 , SL.numDistinct (SL.fromList ([]::[Int])) ~?= 0
 , SL.numDistinct (SL.fromList ([1,3]::[Int])) ~?= 2]  

testCount :: Test
testCount =
  let xs = SL.fromList "abbcccdddd" in
  SL.count xs ~?= SL.fromList [('a', 1),('b',2),('c',3),('d',4)]

testSortedFromList :: Test
testSortedFromList =
  let unsorted1 = [51,25,67,89,95,14,31,28,87,0,25] :: [Int]
      unsorted2 = [1,2,3] :: [Int]
      unsorted3 = [1,3,5,2,4] :: [Int]
      unsorted4 = [] :: [Int]
      unsorted5 = [1,3,3,1] :: [Int]
      sorted1 = [0,14,25,25,28,31,51,67,87,89,95] :: [Int]
      sorted2 = [1,2,3] :: [Int]
      sorted3 = [1,2,3,4,5] :: [Int]
      sorted4 = [] :: [Int]
      sorted5 = [1,1,3,3]  :: [Int]
  in TestList [ MS.sortedListSort unsorted1 ~?= sorted1 
              , MS.sortedListSort unsorted2 ~?= sorted2
              , MS.sortedListSort unsorted3 ~?= sorted3
              , MS.sortedListSort unsorted4 ~?= sorted4
              , MS.sortedListSort unsorted5 ~?= sorted5             
              ]

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted1 = [51,25,67,89,95,14,31,28,87,0,25] :: [Int]
      unsorted2 = [1,2,3] :: [Int]
      unsorted3 = [1,3,5,2,4] :: [Int]
      unsorted4 = [] :: [Int]
      unsorted5 = [1,3,3,1] :: [Int]
      sorted1 = [0,14,25,25,28,31,51,67,87,89,95] :: [Int]
      sorted2 = [1,2,3] :: [Int]
      sorted3 = [1,2,3,4,5] :: [Int]
      sorted4 = [] :: [Int]
      sorted5 = [1,1,3,3]  :: [Int]
  in TestList [ MS.sortedListSort unsorted1 ~?= MS.sortedListSort' unsorted1 
              , MS.sortedListSort unsorted2 ~?= MS.sortedListSort' unsorted2
              , MS.sortedListSort unsorted3 ~?= MS.sortedListSort' unsorted3
              , MS.sortedListSort unsorted4 ~?= MS.sortedListSort' unsorted4
              , MS.sortedListSort unsorted5 ~?= MS.sortedListSort' unsorted5             
              ]

testDivide :: Test
testDivide = TestList [ MS.divide (MS.DivideList "abcd") ~?=
                          (MS.DivideList "ab", MS.DivideList "cd"),
                        MS.divide (MS.DivideList "abcde") ~?=
                          (MS.DivideList "ab", MS.DivideList "cde"),
                        MS.divide (MS.DivideList "") ~?=
                          (MS.DivideList "", MS.DivideList "") ]

testDivideList :: Test
testDivideList =
  let xs = MS.DivideList [1,2,3]
      ys = MS.DivideList [] in
  TestList [ Product (6 :: Int) ~?= foldMap Product xs
           , Sum (0 :: Int) ~?= foldMap Sum ys
           ]

testMergeSort :: Test
testMergeSort =
  let unsorted1 = [51,25,67,89,95,14,31,28,87,0,25] :: [Int]
      unsorted2 = [1,2,3] :: [Int]
      unsorted3 = [1,3,5,2,4] :: [Int]
      unsorted4 = [] :: [Int]
      unsorted5 = [1,3,3,1] :: [Int]
      sorted1 = [0,14,25,25,28,31,51,67,87,89,95] :: [Int]
      sorted2 = [1,2,3] :: [Int]
      sorted3 = [1,2,3,4,5] :: [Int]
      sorted4 = [] :: [Int]
      sorted5 = [1,1,3,3]  :: [Int]
  in TestList [ MS.realMergeSort unsorted1 ~?= sorted1 
              , MS.realMergeSort unsorted2 ~?= sorted2
              , MS.realMergeSort unsorted3 ~?= sorted3
              , MS.realMergeSort unsorted4 ~?= sorted4
              , MS.realMergeSort unsorted5 ~?= sorted5             
              ]

main :: IO ()
main =
  forM_ [ testSortedList, testMinimum, testNumDistinct, testCount
        , testSortedFromList, testSortedFromList', testDivide, testDivideList
        , testMergeSort
        ] $ \t -> do
    _ <- runTestText (putTextToHandle stderr False) t
    return ()
