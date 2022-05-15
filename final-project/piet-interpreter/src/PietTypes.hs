-- Simulates Piet's stack and direction pointer
{-# LANGUAGE TemplateHaskell #-}

module PietTypes where

import Control.Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import Control.Lens
import Data.Vector ((!), (!?))

type Vector = Vec.Vector

type CodelSize = Int
type Position = (Int, Int)

data DPDir = DPRight | DPDown | DPLeft | DPUp deriving (Enum, Eq, Show)
data CCDir = CCLeft | CCRight deriving (Enum, Eq, Show)

newtype Stack = Stack [Int] deriving (Show)
newtype DirectionPtr = DP { _dpdir :: DPDir } deriving (Eq, Show)
newtype CodelChooser = CC { _ccdir :: CCDir } deriving (Eq, Show)

type ImageGrid = Vector (Vector PixelRGB8)

data ProgramState = State {
  _stack :: Stack, 
  _dp :: DirectionPtr, 
  _cc :: CodelChooser,
  _pos :: Position,
  _cb :: Int, -- Number of codels in the current color block
  _rctr :: Int -- Retries counter: program terminates after 8 unsuccessful attempts
} deriving (Show)

data PietProgram = Prog {
  _grid :: ImageGrid,
  _width :: Int,
  _height :: Int,
  _cs :: Int, -- Codel size
  _pstate :: ProgramState
} 

makeLenses ''PietProgram
makeLenses ''ProgramState

data ProgramResult = AwaitingIO | Continue | Error String

initialState = State {
  _stack = Stack [], 
  _dp = DP DPRight, 
  _cc = CC CCLeft,
  _pos = (0, 0),
  _cb = 0, -- Number of codels in the current color block
  _rctr = 0 -- Terminates the program if 8 attempts are made
}

--newtype PietMT m a = PietMT { runPiet :: m (PietMT)}

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta deriving (Enum, Eq, Show)
data Lightness = Light Hue | Regular Hue | Dark Hue | Black | White deriving (Eq, Show)

instance Enum Lightness where
  fromEnum (Light _) = 0
  fromEnum (Regular _) = 1
  fromEnum (Dark _) = 2

  toEnum 0 = Light Red
  toEnum 1 = Regular Red
  toEnum 2 = Dark Red

data PietInstr = Nop | Push | Pop | Add | Sub | Mul 
                | Div | Mod | Not | Grt | Ptr | Swi
                | Dup | Roll | IntIn | IntOut | CharIn | CharOut deriving (Eq, Show)

white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0

cmdTable :: Vector PietInstr
cmdTable = Vec.fromList $ [Nop, Add, Div, Grt, Dup, CharIn, 
                        Push, Sub, Mod, Ptr, Roll, IntOut, 
                        Pop, Mul, Not, Swi, IntIn, CharOut]


