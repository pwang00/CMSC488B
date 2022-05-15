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
-- Not sure why 
computeBlock :: PietProgram -> Position -> PixelRGB8 -> [Position]
computeBlock prog@(Prog {_grid = grid, _cs = cs}) pos pixel = 
  nub $ [pos] ++ floodfill [pos] [] where
    floodfill [] block = block 
    floodfill (top@(r, c) : stack') block = floodfill (adj ++ stack') (adj ++ block) 
        where 
          valid = \lc@(y, x) -> (checkBoundaries prog lc && grid ! y ! x == pixel 
                                  && notElem lc block)
          up = (r - cs, c)
          down = (r + cs, c)
          left = (r, c - cs)
          right = (r, c + cs)
          adj = filter valid [up, down, left, right]
  -- Push initial position to stack
  

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

exec :: ProgramState -> PietInstr -> IO (ProgramState)
exec state@(State {_stack = stk@(Stack s), _cb = cb, _dp = dp, _cc = cc}) instr = 
  return $ case instr of 
    Add -> op2 stk (+)
    Sub -> op2 stk (-)
    Mul -> op2 stk (*)
    Div -> op2 stk (div)
    Mod -> op2 stk (mod)
    Not -> op1 stk $ fromEnum . (== 0)
    Grt -> op2 stk $ (fromEnum .) . (>) -- Should have just quickchecked this shit from the beginning tbh
    Dup -> dup stk
    Roll -> roll stk
    Swi -> chptr stk 0
    Ptr -> chptr stk 1
    Push -> state {_stack = Stack (cb : s)}
    Pop -> pop stk
    Nop -> state
    CharOut -> 
      do
        if (length stk) > 0 then 
          do
            putChar $ chr (head s)
            pop stk else state
    IntOut -> pop stk
      do
        if (length stk) > 0 then 
          do
            putChar $ show (head s)
            pop stk else state
    IntIn -> 
      do 
        putStr "Input Int: "
        n <- getLine
        state {_stack = Stack (((read n) :: Int) : s)}
    CharIn ->
      do
        putStr "Input Char: "
        c <- getChar
        state {_stack = Stack (ord c : s)}
      

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
                      (Stack (x:y:xs)) -> let (rolls, depth) = (x, y)
                                              (topY, rest) = (take depth xs, drop depth xs) in
                                              if (depth < 0) then state else
                                              state {_stack = Stack $ (rot x topY) ++ rest}
                      _ -> state

      rot :: Int -> [a] -> [a]
      rot n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs


interp :: PietProgram -> ProgramState -> IO (Either String PietProgram)
interp prog state@(State {_rctr = 7}) = return $ Right prog
interp prog@(Prog {_grid = grid, _cs = cs}) state@(State {_rctr = rctr, _pos = pos@(r, c)}) = do

    let currCodel = grid ! r ! c
    let block = computeBlock prog pos currCodel
    let dp = _dp state
    let cc = _cc state
    let furthestCodelInBlock = codelFromPositions block dp cc
    let nextBlockEntry@(r2, c2) = moveInDir prog furthestCodelInBlock dp
    -- Updates the _cb value with the number of codels in the color block
    let pixelAtPos = if checkBoundaries prog nextBlockEntry then Just (grid ! r2 ! c2) else Nothing
    --putStrLn $ (show pixelAtPos) ++ " " ++ (show dp) ++ " " ++ (show cc) ++ " " ++ (show block) ++ show pos
    case pixelAtPos of
        pix | pix == Nothing || pix == Just (PixelRGB8 0 0 0) -> do
            case (odd rctr) of 
                True -> do
                    let dp' = rotate dp 1
                    let fixedPos = codelFromPositions block dp' cc
                    interp prog state{_pos = fixedPos, _rctr = rctr + 1, _dp = dp'}
                False -> do
                    let cc' = switch cc 1
                    let fixedPos = codelFromPositions block dp cc'
                    interp prog state{_pos = fixedPos, _rctr = rctr + 1, _cc = cc'}
        pix -> do
            let nextCodel = grid ! r2 ! c2
            let instr = (decodeInstr currCodel nextCodel)
            let s' = state {_cb = length block} -- intermediate state with color block number updated
            case instr of 
                CharIn -> do
                            let stack@(Stack stk) = _stack s'
                            putStr "Input Char: "
                            c <- getChar
                            interp prog s' {_stack = Stack (ord c : stk), _pos = nextBlockEntry, _rctr = 0}
                IntIn -> do
                            let stack@(Stack stk) = _stack s'
                            putStr "Input Int: "
                            n <- getLine
                            interp prog s' {_stack = Stack (((read n) :: Int) : stk), _pos = nextBlockEntry, _rctr = 0}
                CharOut -> do
                            let stack@(Stack stk) = _stack s'
                            if (length stk) > 0 then do
                                putChar $ chr (head stk)
                                interp prog s' {_stack = Stack (tail stk), _pos = nextBlockEntry, _rctr = 0} else interp prog s' 
                IntOut -> do
                            let stack@(Stack stk) = _stack s'
                            if (length stk) > 0 then do
                                putStr $ show (head stk)
                                interp prog s' {_stack = Stack (tail stk), _pos = nextBlockEntry, _rctr = 0} else interp prog s' 

                _ -> do
                      
                      let newState = exec s' instr -- Have to divide by square of codel size
                      let stack@(Stack stk) = _stack newState
                      --putStrLn $ show instr ++ " " ++ show (reverse stk::[Int]) ++ " coords: " ++ show nextBlockEntry ++ show cc ++ show dp
                      interp prog newState{_pos = nextBlockEntry, _rctr = 0}

