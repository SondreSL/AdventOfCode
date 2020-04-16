import Data.List.Extra
import Data.Bits
import Data.Char
import Data.Bool
import Data.Complex
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

data ProgramState = Wait Int Int [Int] [Int] Memory 
                  | Halt [Int]
                  deriving Show

data RobotState = Hold Hull Pos Facing
                | Fin Hull

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
      
compute :: Int -> Int -> [Int] -> [Int] -> Memory -> ProgramState
compute i rel input out memory =
  let (a, b, c) = args i rel memory
   in case rem (M.findWithDefault 0 i memory) 100 of 
        1 -> compute (i + 4) rel input out (M.insert c (a + b) memory)
        2 -> compute (i + 4) rel input out (M.insert c (a * b) memory)
        3 -> let ix = case div (index i memory) 100 `rem` 10 of
                        2 -> rel + (index (i + 1) memory)
                        _ -> index (i + 1) memory 
              in compute (i + 2) rel (tail input) out (M.insert ix (head input) memory)
        4 -> Wait (i + 2) rel input (a:out) memory
        5 -> compute (bool (i + 3) b (0 /= a)) rel input out memory
        6 -> compute (bool (i + 3) b (0 == a)) rel input out memory
        7 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a == b)) memory) 
        9 -> compute (i + 2) (rel + a) input out memory
        99 -> Halt out
        i -> error ("Invalid opcode: " ++ show i)

-- Ship
data Objects = Scaffold
             | Empty
             | Rob Robot
             deriving (Eq, Show)

data Robot = Robot Pos Facing
  deriving (Eq, Show)

type View = Map Pos Objects
data Ship = Ship Pos View

newDir :: Facing -> Int -> Facing
newDir c 1 = c * (0:+1)
newDir c 0 = c * (0:+(-1))

move :: Pos -> Facing -> Pos
move (x, y) (1:+1) = (x, y + 1) -- Up
move (x, y) (1:+(-1)) = (x - 1, y) -- Left
move (x, y) ((-1):+1) = (x + 1, y) -- Right
move (x, y) ((-1):+(-1)) = (x, y - 1) -- Down

ascii :: Ship -> ProgramState -> Ship
ascii ship@(Ship pos@(x,y) view) (Halt out) = ship
ascii ship@(Ship pos@(x,y) view) state@(Wait ip rel inp (out:outs) mem) = 
  let next = compute ip rel inp outs mem
   in case out of 
        35 -> ascii (Ship (x+1,y) $ M.insert pos Scaffold view) $ next
        46 -> ascii (Ship (x+1,y) view) $ next
        10 -> ascii (Ship (0,y+1) view) $ next
        94 -> ascii (Ship (x+1,y) $ M.insert pos (Rob (Robot pos (1:+1))) view) $ next
        60 -> ascii (Ship (x+1,y) $ M.insert pos (Rob (Robot pos (1:+(-1)))) view) $ next
        118 -> ascii (Ship (x+1,y) $ M.insert pos (Rob (Robot pos ((-1):+(-1)))) view) $ next
        62 -> ascii (Ship (x+1,y) $ M.insert pos (Rob (Robot pos ((-1):+1))) view) $ next
        _ -> error $ "Unknown ascii code: " ++ show inp

findCrosses :: Ship -> Int
findCrosses (Ship _ view) =
  let allSides (x,y) = [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
      hasNeigbours pos = all (flip M.member view) $ allSides pos
   in sum . map (uncurry (*)) . filter hasNeigbours $ M.keys view

view :: Ship -> View
view (Ship _ v) = v

solveA :: Memory -> Int
solveA mem = findCrosses . ascii (Ship (0,0) M.empty) $ compute 0 0 [] [] mem

-- solveB :: Memory -> Int
solveB mem = undefined

main = do
  input <- parser <$> readFile "data/input-2019-17.txt"
  -- print input
  print $ solveA input
  -- print $ solveB input

-- 3608
