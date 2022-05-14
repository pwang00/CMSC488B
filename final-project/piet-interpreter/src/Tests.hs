{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests where

import Control.Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import PietTypes
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)
import Data.Vector ((!), (!?))


{-instance Arbitrary PietInstr where
    arbitrary :: Gen PietInstr
    arbitrary = sized gen where
        gen :: Int -> [PietInstr]-}


{-instance Arbitrary DirectionPtr where
    arbitrary :: Gen DirectionPtr
    arbitrary = gen where
        gen -}