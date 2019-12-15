import Data.List.Extra
import Data.Bits
import Data.Char
import Data.Bool
import Data.Complex
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

import Debug.Trace

-- Parsing the input (comma separated ints)
parser :: String -> Memory
parser = either (error "Bad parse") (M.fromList . zip [0..]) . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Intcode computer
type Memory = M.Map Int Int
type Hull = M.Map Pos Int
type Facing = Complex Float
type Pos = (Int, Int)

index :: Int -> Memory -> Int
index = M.findWithDefault 0

args :: Int -> Int -> Memory -> (Int, Int, Int)
args i rel mem = 
  let mode x = div (M.findWithDefault 0 i mem) x `rem` 10
      f x = case mode (10^(x+1)) of
                0 -> index (index (i + x) mem) mem
                1 -> index (i + x) mem 
                2 -> index (rel + (index (i + x) mem)) mem 
      g = case div (index i mem) 10000 `rem` 10 of 
            2 -> rel + (index (i + 3) mem) 
            _ -> index (i + 3) mem
   in (f 1, f 2, g)
      
compute :: Int -> Int -> [Int] -> Memory -> [Int]
compute i rel input memory =
  let (a, b, c) = args i rel memory
   in case rem (M.findWithDefault 0 i memory) 100 of 
        1 -> compute (i + 4) rel input (M.insert c (a + b) memory)
        2 -> compute (i + 4) rel input (M.insert c (a * b) memory)
        3 -> let ix = case div (index i memory) 100 `rem` 10 of
                        2 -> rel + (index (i + 1) memory)
                        _ -> index (i + 1) memory 
              in compute (i + 2) rel (tail input) (M.insert ix (head input) memory)
        4 -> a : compute (i + 2) rel input memory
        5 -> compute (bool (i + 3) b (0 /= a)) rel input memory
        6 -> compute (bool (i + 3) b (0 == a)) rel input memory
        7 -> compute (i + 4) rel input (M.insert c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) rel input (M.insert c (bool 0 1 (a == b)) memory) 
        9 -> compute (i + 2) (rel + a) input memory
        99 -> []
        i -> error ("Invalid opcode: " ++ show i)

-- Arcade
data Objects = Wall
             | Block
             | Paddle
             | Ball
             | Score Int
             deriving (Eq, Show)

type Board = M.Map (Int, Int) Objects
type Ball = (Int, Int)
type Paddle = (Int, Int)
type Output = [Int]
type Score = Int
data Game = Game Board Output Paddle Ball Score

startState :: Game
startState = Game M.empty [] (0,0) (0,0) 0

arcade :: Game -> [Int] -> Game
arcade (Game state out pad ball score) [(-1), 0, newScore] = Game state [] pad ball newScore
arcade game@(Game state out pad@(px, py) ball@(bx, by) score) [x, y, inst] = 
  case inst of
    0 -> Game (M.delete (x,y) state) [] pad ball score    
    1 -> Game (M.insert (x, y) Wall state) [] pad ball score 
    2 -> Game (M.insert (x, y) Block state) [] pad ball score 
    3 -> Game state [] (x, y) ball score 
    4 -> let (newx, newy) = if abs (px - bx) == 2 then (0,0) else newOut pad (x, y)
          in Game state [newx] pad (x, y) score

newOut :: Ball -> Ball -> (Int, Int)
newOut (0,0) _ = (0,0)
newOut _ (0,0) = (0,0)
newOut (bx, by) (newX, newY) = (norm $ newX - bx, norm $ newY - by)
  where norm a 
          | a > 0 = 1
          | a < 0 = (-1)
          | a == 0 = 0

solveA :: Memory -> Int
solveA mem = length 
           . filter (==Block) 
           . M.elems 
           . board
           . foldl arcade startState
           . chunksOf 3 
           $ compute 0 0 [] mem
             where board (Game state out pad ball score) = state

solveB :: Memory -> Int
solveB mem = 
  let tape = M.insert 0 2 mem
      intcode = compute 0 0 (concatMap getOutput input) tape
      input = scanl arcade startState . chunksOf 3 $ intcode
      getOutput (Game _ out _ _ _) = out
      Game _ _ _ _ res = last input
   in res

main = do
  contents <- parser <$> readFile "data/input-2019-13.txt"
  print $ solveA contents
  print $ solveB contents

-- 228
-- 10776
