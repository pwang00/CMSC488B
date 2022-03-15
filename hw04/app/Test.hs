{-# LANGUAGE TypeApplications #-}
module Test where

import Test.HUnit

import qualified RegExp 

import Data.Semigroup
import System.IO (stderr, stdout)
import Control.Monad

main :: IO ()
main =
  forM_ [ ] $ \t -> do
    _ <- runTestText (putTextToHandle stderr False) t
    return ()
