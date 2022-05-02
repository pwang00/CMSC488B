module Interpreter where

import Control.Lens
import PietTypes
import ImageLoader
import Data.Vector ((!), (!?))

-- Rotates direction pointer 
rotate :: DirectionPtr -> Int -> DirectionPtr
rotate (DP x) y = DP $ (x + y) `mod` 4

switch :: CodelChooser -> CodelChooser
switch (CC x) = CC $ (x + 1) `mod` 2

determineInstrType :: PietInstr -> InstrType
determineInstrType instr | elem instr unaryStackOps = UStack
determineInstrType instr | elem instr binaryStackOps = BStack
determineInstrType instr | elem instr pointerOps = Pointer
determineInstrType instr | elem instr inOutOps = InOut

nextCodel :: ProgramState -> CodelSize -> (Int, Int)
nextCodel (State {_dp = dp, _cc = cc, _row = r, _col = c}) cs = 
    case (dp, cc) of
        ((DP dpRight), (CC ccLeft)) -> (r - cs, c)
        ((DP dpRight), (CC ccRight)) -> (r + cs, c)
        ((DP dpDown), (CC ccLeft)) -> (r, c + cs)
        ((DP dpDown), (CC ccRight)) -> (r, c - cs)
        ((DP dpLeft), (CC ccLeft)) -> (r + cs, c)
        ((DP dpLeft), (CC ccRight)) -> (r + cs, c)
        ((DP dpUp), (CC ccLeft)) -> (r, c - cs)
        ((DP dpUp), (CC ccRight)) -> (r, c + cs)


-- Unary arithmetic ops that 
evalUnaryStackInstr :: ProgramState -> PietInstr -> ProgramState
evalUnaryStackInstr state@(State {_cb = cb, _stack = Stack stk@(x:xs)}) instr = 
    case instr of
        Push -> state {_stack = (Stack $ (PInt cb) : stk)}
        Pop -> state {_stack = (Stack xs)}
        Not -> case x of 
            (PInt 0) -> state {_stack = (Stack $ (PInt 1) : xs)} 
            _ -> state {_stack = Stack ((PInt 0) : xs)}
        _ -> error "Invalid instruction supplied"
evalUnaryStackInstr _ _ = error "Not enough arguments on stack"

evalPointerInstr :: ProgramState -> PietInstr -> ProgramState
evalPointerInstr state@(State {_cb = cb, _stack = Stack stk@(x:xs)}) instr = 
    case instr of 
        Ptr -> case x of 
            (PInt v) -> state {_stack = Stack xs, _dp = rotate _dp v}
            _ -> error "Top stack argument is not an integer"
        Switch -> case x of 
            (PInt v) -> state {_stack = Stack xs, _cc = switch _cc v}
            _ -> error "Top stack argument is not an integer"
        _ -> error "Invalid instruction supplied"

evalBinaryStackInstr :: ProgramState -> PietInstr -> ProgramState
evalBinaryStackInstr state@(State {_stack = Stack (x:y:xs)}) instr = case (x, y) of
    (PInt a, PInt b) -> case instr of 
        Add -> state {_stack = Stack $ (PInt $ a + b) : xs}
        Sub -> state {_stack = Stack $ (PInt $ a - b) : xs}
        Mul -> state {_stack = Stack $ (PInt $ a * b) : xs}
        Div -> state {_stack = Stack $ (PInt (fst (divMod a b))) : xs}
        Mod -> state {_stack = Stack $ (PInt (snd (divMod a b))) : xs}
        Grt -> state {_stack = Stack $ (PInt (if b > a then 1 else 0)) : xs}
        _ -> error "Invalid binary operation"
    _ -> error "Type error"
evalBinaryStackInstr _ _ = error "Not enough arguments on stack"

evalIOInstr :: ProgramState -> PietInstr -> IO (ProgramState)
evalIOInstr state@(State {_stack = Stack stk@(x:xs)}) instr = case instr of 
        CharIn -> do
                    putStr "Input Char: "
                    c <- getChar
                    return state {_stack = Stack $ (PChr c) : stk}
        IntIn -> do
                    putStr "Input Int: "
                    n <- getLine
                    return state {_stack = Stack $ (PInt ((read n) :: Int)) : stk}
        instr | instr == CharOut || instr == IntOut -> do
                    case x of 
                        (PInt n) -> putStrLn (show n)
                        (PChr c) -> putStrLn (show c)
                    return state


-- cs: codel size
interp :: Program -> ProgramState -> CodelSize -> IO (ProgramState)
interp prog state@(State {_ctr = 8}) sz = return state
interp prog state cs = do
    let (r1, c1) = (_row state, _col state)
    let (r2, c2) = nextCodel state cs
    
    -- Safe indexing of pixel matrix
    let row1 = prog !? r1
    case row1 of 
        (Just vec) -> 
    let row2 = prog !? r2
    p2 <- row2 !? c2

    let instr = decodeInstr p1 p2
    let instrtype = determineInstrType instr

    updatedState <- case instrtype of 
                        UStack -> return $ evalUnaryStackInstr state instr
                        BStack -> return $ evalBinaryStackInstr state instr
                        Pointer -> return $ evalPointerInstr state instr
                        InOut -> evalIOInstr state instr

    return updatedState



