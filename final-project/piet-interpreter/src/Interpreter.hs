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
import Debug.Trace

-- Rotates direction pointer 
rotate :: DirectionPtr -> Int -> DirectionPtr
rotate (DP x) y = DP $ toEnum ((fromEnum x + y) `mod` 4)

switch :: CodelChooser -> Int -> CodelChooser
switch (CC x) y = CC $ toEnum ((fromEnum x + y) `mod` 2)

checkBoundaries :: PietProgram -> Position -> Bool
checkBoundaries prog (row, col) = 
  (row >= 0 && row < _height prog && col >= 0 && col < _width prog)

-- Finds all codels in a color block
-- Maybe I'll use an O(1) queue or something
computeBlock :: PietProgram -> Position -> Lightness -> [Position]
computeBlock prog@(Prog {_grid = grid, _cs = cs}) pos color = 
  nub $ floodfill [pos] [pos] where
    floodfill :: [Position] -> [Position] -> [Position]
    floodfill [] block = block 
    floodfill (top@(r, c) : stack') block = floodfill (adj ++ stack') (adj ++ block) 
        where 
          valid = \lc@(y, x) -> (checkBoundaries prog lc && grid ! y ! x == color 
                                  && notElem lc block)
          up = (r - cs, c)
          down = (r + cs, c)
          left = (r, c - cs)
          right = (r, c + cs)
          adj = filter valid [up, down, left, right]

codelFromPositions :: [Position] -> DirectionPtr -> CodelChooser -> Position
codelFromPositions nextBlockEntry dp cc =
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
  maximumBy cond nextBlockEntry

moveInDir :: PietProgram -> Position -> DirectionPtr -> Position
moveInDir prog@(Prog {_cs = cs}) pos@(r, c) dp = 
  case dp of 
    (DP DPRight) -> (r, c + cs)
    (DP DPDown) -> (r + cs, c)
    (DP DPLeft) -> (r, c - cs)
    (DP DPUp) -> (r - cs, c)

execInstr :: ProgramState -> PietInstr -> Result
execInstr state@(State {_stack = stk@(Stack s), _inbuf = ib, _outbuf = ob}) instr = 
  let dp = _dp state
      cc = _cc state
      cb = _cb state
      state' = case instr of 
        Add -> op2 stk (+)
        Sub -> op2 stk (-)
        Mul -> op2 stk (*)
        Div -> op2 stk (div)
        Mod -> op2 stk (mod)
        Not -> op1 stk $ fromEnum . (== 0)
        Grt -> op2 stk $ (fromEnum .) . (>) 
        Dup -> dup stk
        Roll -> roll stk
        Swi -> chptr stk 0 dp cc
        Ptr -> chptr stk 1 dp cc
        Push -> state {_stack = Stack (cb : s)}
        Pop -> pop stk
        CharIn -> pushBufToStack
        IntIn -> pushBufToStack
        IntOut -> pushToOutBuf
        CharOut -> pushToOutBuf
        Nop -> state
    
      ib' = _inbuf state'
      ob' = _outbuf state'
    
      action = case instr of 
        CharIn -> if (length ib') == 0 then CharInRequest else Continue
        CharOut -> if (length ob') == 1 then CharOutRequest else Continue
        IntIn -> if (length ib') == 0 then IntInRequest else Continue
        IntOut -> if (length ob') == 1 then IntOutRequest else Continue
        _ -> Continue in
      
      Res state' action

    where
      -- Unary arithmetic stack ops

      pushBufToStack :: ProgramState
      pushBufToStack = if (length ib) == 1 then state{_stack = Stack (ib ++ s), _inbuf = []} else state

      pushToOutBuf :: ProgramState
      pushToOutBuf = if (length s) >= 1 then 
                        let (x:xs) = s in
                            state{_stack = Stack xs, _outbuf = [x]} else state
      
      op1 :: Stack -> (Int -> Int) -> ProgramState
      op1 = \s op -> case s of 
                          Stack (x:xs) -> state {_stack = Stack $ (op x) : xs}
                          _ -> state
      -- Binary arithmetic stack ops
      op2 :: Stack -> (Int -> Int -> Int) -> ProgramState
      op2 = \s op -> case s of 
                          Stack (x:y:xs) -> state {_stack = Stack $ (y `op` x):xs}
                          _ -> state

      pop :: Stack -> ProgramState
      pop = \s -> case s of 
                       Stack (x:xs) -> state {_stack = Stack xs}
                       _ -> state

      chptr :: Stack -> Int -> DirectionPtr -> CodelChooser -> ProgramState
      chptr = \s t dp cc -> case (s, t) of 
                          (Stack (x:xs), 0) -> state {_stack = Stack xs, _cc = switch cc x}
                          (Stack (x:xs), 1) -> state {_stack = Stack xs, _dp = rotate dp x}
                          _ -> state

      dup :: Stack -> ProgramState
      dup = \s -> case s of 
                        Stack (x:xs) -> state {_stack = Stack (x:x:xs)}
                        _ -> state
 
      roll :: Stack -> ProgramState
      roll = \s -> case s of
                      (Stack (x:y:xs)) -> 
                        let (rolls, depth) = (x, y)
                            (topY, rest) = (take depth xs, drop depth xs) in
                            if (depth < 0) then state 
                            else state {_stack = Stack $ (rot x topY) ++ rest}
                      _ -> state

      rot :: Int -> [a] -> [a]
      rot n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs

-- Case for when out of bounds or encountering a black block
recalculateEntry :: ProgramState -> [Position] -> ProgramState
recalculateEntry state@(State {_dp = dp, _cc = cc, _rctr = rctr}) block =
    let dp' = rotate dp 1
        cc' = switch cc 1
        rctr' = _rctr state + 1 in

        case odd rctr of 
            True ->
              let fixedPos = codelFromPositions block dp' cc in
              state{_pos = fixedPos, _rctr = rctr', _dp = dp'}
            False ->
              let fixedPos = codelFromPositions block dp cc' in
              state{_pos = fixedPos, _rctr = rctr', _cc = cc'}

step :: PietProgram -> ProgramState -> Result
step prog state@(State {_rctr = 7}) = Res state EndProg
step prog@(Prog {_grid = grid, _cs = cs}) state@(State {_rctr = rctr, 
                                                  _pos = pos@(r, c), _dp = dp, _cc = cc}) =
    let currCodel = grid ! r ! c
        block = computeBlock prog pos currCodel
        furthestCodelInBlock = codelFromPositions block dp cc
        nextBlockEntry@(r2, c2) = moveInDir prog furthestCodelInBlock dp
    -- Updates the _cb value with the number of codels in the color block
        colorAtPos = if checkBoundaries prog nextBlockEntry then Just (grid ! r2 ! c2) else Nothing in

    case colorAtPos of
        Nothing -> Res (recalculateEntry state block) Continue
        Just Black -> Res (recalculateEntry state block) Continue
        (Just nextCodel) -> let instr = (decodeInstr currCodel nextCodel)
                                (Res newState res) = execInstr state {_cb = length block} instr in 
                                Res newState {_pos = nextBlockEntry, _rctr = 0} res

interp :: PietProgram -> Result -> IO (ProgramState)
interp prog (Res finalState EndProg) = return finalState
interp prog res@(Res state@(State {_inbuf = ib, _outbuf = ob}) _) = 
    case res of
      (Res state Continue) -> interp prog (step prog state)
      (Res state CharInRequest) -> do
        x <- getChar
        interp prog (step prog state{_inbuf = [(ord x)]})

      (Res state IntInRequest) -> do
        x <- getLine
        interp prog (step prog state{_inbuf = [(read x) :: Int]})

      (Res state CharOutRequest) -> case ob of 
        [x] -> do
          putChar $ chr x
          interp prog (step prog state{_outbuf = []})
        _ -> do
          interp prog (Res state{_outbuf = []} Continue)

      (Res state IntOutRequest) -> case ob of 
        [x] -> do
          putStr $ show x
          interp prog (step prog state{_outbuf = []})
        _ -> do
          interp prog (Res state{_outbuf = []} Continue)
            
      