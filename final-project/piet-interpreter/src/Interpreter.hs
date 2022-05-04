module Interpreter where

import Control.Lens
import PietTypes
import ImageLoader
import Codec.Picture
import Data.Vector ((!), (!?))
import Data.Char (ord, chr)
import Data.Function (on)
import Data.List
import Data.Monoid
import Data.Foldable

-- Rotates direction pointer 
rotate :: DirectionPtr -> Int -> DirectionPtr
rotate (DP x) y = DP $ (x + y) `mod` 4

switch :: CodelChooser -> CodelChooser
switch (CC x) = CC $ (x + 1) `mod` 2

{-determineInstrType :: PietInstr -> InstrType
determineInstrType instr | elem instr unaryStackOps = UStack
determineInstrType instr | elem instr binaryStackOps = BStack
determineInstrType instr | elem instr pointerOps = Pointer
determineInstrType instr | elem instr inOutOps = InOut
determineInstrType _ = Noop-}

initialState = State {
    _stack = Stack [], 
    _dp = DP 0, 
    _cc = CC 0,
    _pos = (0, 0),
    _cb = 0, -- Number of codels in the current color block
    _ctr = 0 -- Terminates the program if 8 attempts are made
}

checkBoundaries :: PietProgram -> Position -> Bool
checkBoundaries prog (row, col) = 
    (row >= 0 && row <= _height prog && col >= 0 && col <= _width prog)

-- Finds all codels in a color block
bfsCodel :: PietProgram -> Position -> PixelRGB8 -> [Position]
bfsCodel prog@(Prog {_grid = grid, _cs = cs}) pos@(r, c) pixel = 
    nub $ (aux grid [] pos) where
        aux grid seen pos = 
            if (checkBoundaries prog pos) && (grid ! r ! c) == pixel 
                && not (elem pos seen) then 
                    pos : (concatMap (aux grid (pos : seen)) adjacencies) else [] where 
                        adjacencies = [(r - cs, c), (r + cs, c), (r, c + cs), (r, c - cs)]

codelFromPositions :: [Position] -> ProgramState -> Position
codelFromPositions coords state@(State{ _pos = (r, c), _dp = dp, _cc = cc}) =
    let cond = case dp of 
            (DP 0) -> case cc of 
                (CC 0) -> maxCol <> minRow
                (CC 1) -> maxCol <> maxRow
            (DP 1) -> case cc of 
                (CC 0) -> maxRow <> minCol
                (CC 1) -> maxRow <> maxCol
            (DP 2) -> case cc of
                (CC 0) -> minCol <> maxRow
                (CC 1) -> minCol <> minRow
            (DP 3) -> case cc of
                (CC 0) -> minRow <> minCol
                (CC 1) -> minRow <> maxCol
            where 
                minRow = (flip compare `on` fst)
                minCol = (flip compare `on` snd)
                maxRow = (compare `on` fst)
                maxCol = (compare `on` snd) in
    maximumBy cond coords

nextColorBlockCoords :: PietProgram -> ProgramState -> Position
nextColorBlockCoords prog@(Prog {_cs = cs}) state@(State {_pos = (r, c), _dp = dp}) = 
    case dp of 
        (DP 0) -> (r, c + cs)
        (DP 1) -> (r + cs, c)
        (DP 2) -> (r, c - cs)
        (DP 3) -> (r - cs, c)

-- Unary arithmetic ops that 
evalUnaryStackInstr :: ProgramState -> PietInstr -> ProgramState
evalUnaryStackInstr state@(State {_cb = cb, _stack = Stack stk@(x:xs)}) instr = 
    case instr of
        Push -> state {_stack = (Stack (cb : stk))}
        Pop -> state {_stack = (Stack xs)}
        Not -> case x of 
            0 -> state {_stack = Stack (1 : xs)} 
            _ -> state {_stack = Stack (0 : xs)}
        _ -> error "Invalid instruction supplied"
evalUnaryStackInstr _ _ = error "Not enough arguments on stack"

evalPointerInstr :: ProgramState -> PietInstr -> ProgramState
evalPointerInstr state@(State {_cb = cb, _dp = dp, _cc = cc, _stack = Stack stk@(x:xs)}) instr = 
    case instr of 
        Ptr -> state {_stack = Stack xs, _dp = rotate dp x}
        Switch -> state {_stack = Stack xs, _cc = if (x `mod` 2 == 0) then cc else switch cc}
        _ -> error "Invalid instruction supplied"

evalBinaryStackInstr :: ProgramState -> PietInstr -> ProgramState
evalBinaryStackInstr state@(State {_stack = Stack (x:y:xs)}) instr = 
    case instr of 
        Add -> state {_stack = Stack $ (x + y) : xs}
        Sub -> state {_stack = Stack $ (y - x) : xs}
        Mul -> state {_stack = Stack $ (x * y) : xs}
        Div -> state {_stack = Stack $ (fst (divMod y x)) : xs}
        Mod -> state {_stack = Stack $ (snd (divMod y x)) : xs}
        Grt -> state {_stack = Stack $ (if y > x then 1 else 0) : xs}
        Roll -> state {_stack = Stack $ (cycle y topY) ++ rest} where
            cycle = drop <> take
            topY = take y xs
            rest = drop y xs
        _ -> error "Invalid binary operation"
evalBinaryStackInstr _ _ = error "Not enough arguments on stack"

evalIOInstr :: ProgramState -> PietInstr -> IO (ProgramState)
evalIOInstr state@(State {_stack = Stack stk@(x:xs)}) instr = 
    case instr of 
        CharIn -> do
                    putStr "Input Char: "
                    c <- getChar
                    return state {_stack = Stack (ord c : stk)}
        IntIn -> do
                    putStr "Input Int: "
                    n <- getLine
                    return state {_stack = Stack (((read n) :: Int) : stk)}
        CharOut -> do
                    putChar $ chr x
                    return state
        IntOut -> do
                    putStrLn $ show x
                    return state


-- cs: codel size
interp :: PietProgram -> ProgramState -> IO (Either String PietProgram)
interp prog state@(State {_ctr = 8}) = return $ Right prog
interp prog state = do
    let pos@(r, c) = _pos state
    let grid = _grid prog
    let currCodel = grid ! r ! c
    let block = bfsCodel prog pos currCodel
    let updatedPos = codelFromPositions block 
    let cb = length block

    let coords@(r2, c2) = nextColorBlockCoords prog state
    let updatedState = state {_cb = cb, _pos = coords}
    let nextCodel = grid ! r2 ! c2

    let instr = decodeInstr currCodel nextCodel

    putStrLn $ show instr

    interp prog updatedState
 


