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
rotate (DP x) y = DP $ toEnum (fromEnum x + y `mod` 4)

switch :: CodelChooser -> CodelChooser
switch (CC x) = CC $ toEnum (fromEnum x + 1 `mod` 2)

checkBoundaries :: PietProgram -> Position -> Bool
checkBoundaries prog (row, col) = 
    (row >= 0 && row < _height prog && col >= 0 && col < _width prog)

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
            (DP DPRight) -> case cc of 
                (CC CCLeft) -> maxCol <> minRow
                (CC CCRight) -> maxCol <> maxRow
            (DP DPDown) -> case cc of 
                (CC CCLeft) -> maxRow <> minCol
                (CC CCRight) -> maxRow <> maxCol
            (DP DPLeft) -> case cc of
                (CC CCLeft) -> minCol <> maxRow
                (CC CCRight) -> minCol <> minRow
            (DP DPUp) -> case cc of
                (CC CCLeft) -> minRow <> minCol
                (CC CCRight) -> minRow <> maxCol
            where 
                minRow = (flip compare `on` fst)
                minCol = (flip compare `on` snd)
                maxRow = (compare `on` fst)
                maxCol = (compare `on` snd) in
    maximumBy cond coords

nextColorBlockCoords :: PietProgram -> ProgramState -> Position
nextColorBlockCoords prog@(Prog {_cs = cs}) state@(State {_pos = (r, c), _dp = dp}) = 
    case dp of 
        (DP DPRight) -> (r, c + cs)
        (DP DPDown) -> (r + cs, c)
        (DP DPLeft) -> (r, c - cs)
        (DP DPUp) -> (r - cs, c)

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
evalPointerInstr state@(State {_dp = dp, _cc = cc, _stack = Stack stk@(x:xs)}) instr = 
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

-- For white pixels only
slide :: PietProgram -> Position -> DirectionPtr -> Position
slide prog@(Prog {_grid = grid, _width = width, _height = height}) pos@(x, y) dp =
    case dp of 
        (DP DPRight) -> maximumBy (compare `on` fst) rows
        (DP DPDown) -> maximumBy (compare `on` snd) cols
        (DP DPLeft) -> minimumBy (compare `on` fst) rows
        (DP DPUp) -> minimumBy (compare `on` snd) cols 
    where
        rows = [(x', y) | x' <- [0 .. width] , grid ! x' ! y == white]
        cols = [(x, y') | y' <- [0 .. height] , grid ! x ! y' == white]


step :: PietProgram -> ProgramResult
step = undefined

-- cs: codel size
interp :: PietProgram -> ProgramState -> IO (Either String PietProgram)
interp prog state@(State {_rctr = 8}) = return $ Right prog
interp prog@(Prog {_grid = grid}) state@(State {_rctr = rctr, _pos = pos@(r, c)}) = do
    let currCodel = grid ! r ! c
    let block = bfsCodel prog pos currCodel
    let updatedPos = codelFromPositions block 
    let cb = length block
    let coords@(r2, c2) = nextColorBlockCoords prog state

    let dp = _dp state
    let cc = _cc state
    -- putStrLn $ "Curr coords: " ++ show pos ++ " Next coords: " ++ show coords
    -- Updates the _cb value with the number of codels in the color block
    case (checkBoundaries prog coords, rctr `mod` 2) of
        (False, 0) -> interp prog state {_cb = cb, _cc = switch cc, _rctr = rctr + 1, _pos = pos}
        (False, 1) -> interp prog state {_cb = cb, _dp = rotate (DP DPDown) 1, _rctr = rctr + 1, _pos = pos}
        (True, _)  -> do
                        let nextCodel = grid ! r2 ! c2
                        putStrLn $ show ((decodeInstr currCodel nextCodel) :: PietInstr)
                        interp prog state {_cb = cb, _pos = coords}



