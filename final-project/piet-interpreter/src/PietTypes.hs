-- Simulates Piet's stack and direction pointer
module PietTypes where

import Control.Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import Control.Lens
import Data.Vector ((!), (!?))

type Vector = Vec.Vector

-- The Piet stack can hold either integers or characters
data Value = PInt Int | PChr Char deriving (Show)

type CodelSize = Int

newtype Stack = Stack [Value] deriving (Show)
newtype DirectionPtr = DP { _dpdir :: Int }
newtype CodelChooser = CC { _ccdir :: Int }

data ProgramState = State {
    _stack :: Stack, 
    _dp :: DirectionPtr, 
    _cc :: CodelChooser,
    _row :: Int, -- Row in image
    _col :: Int, -- Col in image
    _cb :: Int, -- Number of codels in the current color block
    _ctr :: Int -- Terminates the program if 8 attempts are made
}

data Error = TypeError String | StackError String

type Program = Vector (Vector PixelRGB8)

instance Show CodelChooser where
    show (CC 0) = "CC Left"
    show (CC 1) = "CC Right"

(dpRight, dpDown, dpLeft, dpUp) = (0, 1, 2, 3)
(ccLeft, ccRight) = (0, 1)

instance Show DirectionPtr where
    show (DP 0) = "DP Right"
    show (DP 1) = "DP Down"
    show (DP 2) = "DP Left"
    show (DP 3) = "DP Up"
    show (DP x) = show (DP $ x `mod` 4)

data PietInstr = Nop | Push | Pop | Add | Sub | Mul | Div | Mod | Not | Grt | Ptr | Switch | Dup | Roll | IntIn | IntOut | CharIn | CharOut deriving (Show)
data InstrType = UStack | BStack | Pointer | InOut

cmdTable :: Vector PietInstr
cmdTable = Vec.fromList $ [Nop, Add, Div, Grt, Dup, CharIn, 
                        Push, Sub, Mod, Ptr, Roll, IntOut, 
                        Pop, Mul, Not, Switch, IntIn, CharOut]

unaryStackOps = [Pop, Push, Not, Dup]
binaryStackOps = [Add, Sub, Mul, Div, Mod, Grt, Roll]
pointerOps = [Ptr, Switch]
inOutOps = [IntIn, IntOut, CharIn, CharOut]


