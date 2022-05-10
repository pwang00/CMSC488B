-- Simulates Piet's stack and direction pointer
module PietTypes where

import Control.Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import Control.Lens
import Data.Vector ((!), (!?))

type Vector = Vec.Vector

type CodelSize = Int

data DPDir = DPRight | DPDown | DPLeft | DPUp deriving (Enum, Eq, Show)
data CCDir = CCLeft | CCRight deriving (Enum, Eq, Show)

newtype Stack = Stack [Int] deriving (Show)
newtype DirectionPtr = DP { _dpdir :: DPDir } deriving (Show)
newtype CodelChooser = CC { _ccdir :: CCDir } deriving (Show)

type ImageGrid = Vector (Vector PixelRGB8)

data PietProgram = Prog {
    _grid :: ImageGrid,
    _width :: Int,
    _height :: Int,
    _cs :: Int
}

type Position = (Int, Int)

data ProgramState = State {
    _stack :: Stack, 
    _dp :: DirectionPtr, 
    _cc :: CodelChooser,
    _pos :: Position,
    _cb :: Int, -- Number of codels in the current color block
    _rctr :: Int -- Retries counter: program terminates after 8 unsuccessful attempts
} deriving (Show)

data Result = AwaitingIO | Continue | Error String

initialState = State {
    _stack = Stack [], 
    _dp = DP DPRight, 
    _cc = CC CCLeft,
    _pos = (0, 0),
    _cb = 0, -- Number of codels in the current color block
    _rctr = 0 -- Terminates the program if 8 attempts are made
}


data PietInstr = Nop | Push | Pop | Add | Sub | Mul 
                | Div | Mod | Not | Grt | Ptr | Switch 
                | Dup | Roll | IntIn | IntOut | CharIn | CharOut deriving (Show)

white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0

cmdTable :: Vector PietInstr
cmdTable = Vec.fromList $ [Nop, Add, Div, Grt, Dup, CharIn, 
                        Push, Sub, Mod, Ptr, Roll, IntOut, 
                        Pop, Mul, Not, Switch, IntIn, CharOut]

unaryStackOps = [Pop, Push, Not, Dup]
binaryStackOps = [Add, Sub, Mul, Div, Mod, Grt, Roll]
pointerOps = [Ptr, Switch]
inOutOps = [IntIn, IntOut, CharIn, CharOut]


