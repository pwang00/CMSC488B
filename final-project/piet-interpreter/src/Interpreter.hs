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

execInstr :: ProgramState -> PietInstr -> IO (ProgramState)
execInstr state@(State {_stack = stk@(Stack s), _cb = cb, _dp = dp, _cc = cc}) instr = 
  case instr of 
    Add -> return $ op2 stk (+)
    Sub -> return $ op2 stk (-)
    Mul -> return $ op2 stk (*)
    Div -> return $ op2 stk (div)
    Mod -> return $ op2 stk (mod)
    Not -> return $ op1 stk $ fromEnum . (== 0)
    Grt -> return $ op2 stk $ (fromEnum .) . (>) 
    Dup -> return $ dup stk
    Roll -> return $ roll stk
    Swi -> return $ chptr stk 0
    Ptr -> return $ chptr stk 1
    Push -> return $ state {_stack = Stack (cb : s)}
    Pop -> return $ pop stk
    Nop -> return $ state
    CharOut -> 
      if (length s) > 0 then
        do
          putChar $ chr (head s)
          return $ (pop stk) 
      else return $ state
    IntOut -> 
      if (length s) > 0 then 
        do
          putStr $ show (head s)
          return $ pop stk 
      else return $ state
    IntIn -> 
      do 
        putStr "Input Int: "
        n <- getLine
        return $ state {_stack = Stack (((read n) :: Int) : s)}
    CharIn ->
      do
        putStr "Input Char: "
        c <- getChar
        return $ state {_stack = Stack (ord c : s)}
      

    where 
      -- Unary arithmetic stack ops
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

      chptr :: Stack -> Int -> ProgramState
      chptr = \s t -> case (s, t) of 
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

step :: PietProgram -> ProgramState -> IO (PietProgram)
step prog state@(State {_rctr = 7}) = return prog
step prog@(Prog {_grid = grid, _cs = cs}) state@(State {_rctr = rctr, _pos = pos@(r, c)}) = do
    let currCodel = grid ! r ! c
    let block = computeBlock prog pos currCodel
    let dp = _dp state
    let cc = _cc state
    let furthestCodelInBlock = codelFromPositions block dp cc
    let nextBlockEntry@(r2, c2) = moveInDir prog furthestCodelInBlock dp
    -- Updates the _cb value with the number of codels in the color block
    let colorAtPos = if checkBoundaries prog nextBlockEntry then Just (grid ! r2 ! c2) else Nothing

    case colorAtPos of
        Nothing -> step prog $ recalculateEntry state block
        Just Black -> step prog $ recalculateEntry state block
        pix -> do
            let (Just nextCodel) = colorAtPos
            let instr = (decodeInstr currCodel nextCodel)
            let s' = state {_cb = length block}
            newState <- execInstr s' instr
            let stack@(Stack stk) = _stack newState
            step prog newState{_pos = nextBlockEntry, _rctr = 0}

