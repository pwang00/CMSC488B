module Interpreter where

import Control.Lens
import PietTypes
import ImageLoader
import Data.Vector ((!), (!?))
import Data.Char (ord, chr)
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
        Push -> state {_stack = (Stack (cb : stk))}
        Pop -> state {_stack = (Stack xs)}
        Not -> case x of 
            0 -> state {_stack = Stack (1 : xs)} 
            _ -> state {_stack = Stack (0 : xs)}
        _ -> error "Invalid instruction supplied"
evalUnaryStackInstr _ _ = error "Not enough arguments on stack"

evalPointerInstr :: ProgramState -> PietInstr -> ProgramState
evalPointerInstr state@(State {_cb = cb, _stack = Stack stk@(x:xs)}) instr = 
    case instr of 
        Ptr -> state {_stack = Stack xs, _dp = rotate _dp x}
        Switch -> state {_stack = Stack xs, _cc = switch _cc v}
        _ -> error "Invalid instruction supplied"

evalBinaryStackInstr :: ProgramState -> PietInstr -> ProgramState
evalBinaryStackInstr state@(State {_stack = Stack (x:y:xs)}) instr = case instr of 
        Add -> state {_stack = Stack $ (x + y) : xs}
        Sub -> state {_stack = Stack $ (x - y) : xs}
        Mul -> state {_stack = Stack $ (x * y) : xs}
        Div -> state {_stack = Stack $ (fst (divMod x y)) : xs}
        Mod -> state {_stack = Stack $ (snd (divMod x y)) : xs}
        Grt -> state {_stack = Stack $ (if y > x then 1 else 0) : xs}
        _ -> error "Invalid binary operation"
evalBinaryStackInstr _ _ = error "Not enough arguments on stack"

evalIOInstr :: ProgramState -> PietInstr -> IO (ProgramState)
evalIOInstr state@(State {_stack = Stack stk@(x:xs)}) instr = case instr of 
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
interp :: Program -> ProgramState -> CodelSize -> IO (ProgramState)
interp prog state@(State {_ctr = 8}) sz = return state
interp prog state cs = do
    let (r1, c1) = (_row state, _col state)
    let (r2, c2) = nextCodel state cs
    
    -- Safe indexing of pixel matrix
    let row1 = prog !? r1
    let p1 = case row1 of 
        (Just vec) -> case (vec !? c1) of 

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



