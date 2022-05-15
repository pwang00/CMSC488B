{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Control.Monad
import qualified Control.Monad as Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import PietTypes
import PietTypes (cmdTable)
import Interpreter
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)
import Data.Vector ((!), (!?))


instance Arbitrary Stack where
    arbitrary :: Gen Stack
    arbitrary = sized gen where
        gen :: Int -> Gen Stack
        gen 0 = frequency
            [(1, Stack . return <$> choose (1, 1000000)) -- Represent integers that might appear after ops
            ,(2, Stack . return <$> choose (1, 256)) -- All ASCII
            ,(2, Stack . return <$> choose (32, 127))] -- Printable character range

        gen n = do
            (Stack x) <- gen (n `div` 2)
            (Stack y) <- gen (n `div` 3)
            (Stack z) <- gen (n `div` 4)
            return $ Stack (x ++ y ++ z)


instance Arbitrary PietInstr where
    arbitrary :: Gen PietInstr
    arbitrary = do
        n <- arbitrary :: Gen Int
        return $ cmdTable ! (n `mod` (Vec.length cmdTable))

    shrink :: PietInstr -> [PietInstr]
    shrink cmd = []

instance Arbitrary DirectionPtr where
    arbitrary :: Gen DirectionPtr
    arbitrary = oneof
            [return (DP DPRight)
            ,return (DP DPLeft)
            ,return (DP DPDown)
            ,return (DP DPUp)]

    shrink :: DirectionPtr -> [DirectionPtr]
    shrink dp = []

instance Arbitrary CodelChooser where
    arbitrary :: Gen CodelChooser
    arbitrary = oneof
        [return (CC CCLeft)
        ,return (CC CCRight)]
    
    shrink :: CodelChooser -> [CodelChooser]
    shrink dir = []

-- Tests to make sure we do arithmetic correctly
prop_RotateDP :: DirectionPtr -> Int -> Bool
prop_RotateDP dp@(DP dir) n = rotate dp n == rotate dp (n `mod` 4)

prop_SwitchCC :: CodelChooser -> Int -> Bool 
prop_SwitchCC cc@(CC dir) n = switch cc n == switch cc (n `mod` 2)

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  _ <- runTests
  return ()