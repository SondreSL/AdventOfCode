module Day15 where

import Data.List.Extra
import Data.Bits
import Data.Char
import Data.Bool
import Data.Complex
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

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

-- Robot
data Objects = Wall
             | Empty
             | Tank
             deriving (Eq, Show)

type Maze = M.Map (Int, Int) Objects
type Output = [Int]
type Robot = Pos

data Dir = North | South | East | West | Back
data Ship = Ship Robot Maze Pos [Pos] Int

dir :: Pos -> Pos -> Int
dir (x,y) (a,b)
  | y < b = 1
  | x < a = 4
  | x > a = 3
  | y > b = 2
  | otherwise = error $ show (x, y, a, b)

nextAttempt :: Robot -> Maze -> [Pos] -> (Int, Pos, [Pos])
nextAttempt (x,y) maze hist
  | Nothing == M.lookup (x, y + 1) maze = (1, (x, y + 1), hist)
  | Nothing == M.lookup (x + 1, y) maze = (4, (x + 1, y), hist)
  | Nothing == M.lookup (x, y - 1) maze = (2, (x, y - 1), hist)
  | Nothing == M.lookup (x - 1, y) maze = (3, (x - 1, y), hist)
  | otherwise = let inst = dir (x, y) (head hist)
                 in (inst, head hist, tail hist)

robot :: Ship -> Int -> Ship
robot (Ship rob@(x,y) maze att@(a,b) hist old) inst =
   case inst of
      0 -> let newMaze = M.insert att Wall maze
               (newInst, newAttempt, newHist) = nextAttempt rob newMaze hist
            in Ship rob newMaze newAttempt hist newInst
      1 -> let newMaze = M.insert att Empty maze
               newHist = if M.member att maze then (tail hist) else (rob : hist)
               (newInst, newAttempt, _) = nextAttempt att newMaze newHist
            in Ship att newMaze newAttempt newHist newInst
      2 -> let newMaze = M.insert att Tank maze
               newHist = if M.member att maze then (tail hist) else (rob : hist)
               (newInst, newAttempt, _) = nextAttempt att newMaze newHist
            in Ship att newMaze newAttempt newHist newInst

getOutput :: Ship -> Int
getOutput (Ship _ _ _ _ i) = i

emptyHist :: Ship -> Bool
emptyHist (Ship _ _ _ [] _) = True
emptyHist _ = False

bfs :: Maze -> Pos -> M.Map Pos Int
bfs maze start = inner [(0, start)] (S.singleton start) M.empty
  where
    addCoords (x, y) l seen = filter (f seen maze) [(l,(x,y+1)), (l,(x+1,y)), (l,(x,y-1)), (l,(x-1,y))]
    f seen maze (i, pos) = (not (S.member pos seen)) && (M.lookup pos maze == (Just Tank) || M.lookup pos maze == (Just Empty))
    inner queue seen m
      | queue == [] = m
      | otherwise =
        let (len, curr) = head queue
            new = addCoords curr (len + 1) seen
            newQueue = queue ++ new
            newSet = foldl (flip S.insert) seen (map snd new)
            newMap = M.insert curr len m
         in inner (tail newQueue) newSet newMap

findTank :: Maze -> Pos
findTank = M.foldrWithKey (\k v acc -> if v == Tank then k else acc) (0,0)

mkMaze :: Memory -> Maze
mkMaze mem =
  let instructions = scanl robot (Ship (0,0) (M.singleton (0,0) Empty) (0,1) [] 1) $ input
      input = compute 0 0 (map getOutput instructions) mem
      Just (Ship _ maze _ _ _) = find emptyHist . drop 20 $ instructions
   in maze

solveA :: Memory -> Int
solveA mem =
  let maze = mkMaze mem
      dists = bfs maze (0,0)
      tank = findTank maze
   in M.findWithDefault 0 tank dists

solveB :: Memory -> Int
solveB mem =
  let maze = mkMaze mem
      tank = findTank maze
      dists = bfs maze tank
   in maximum dists

main = do
  input <- parser <$> readFile "data/input-2019-15.txt"
  print $ solveA input
  print $ solveB input

-- 280
-- 400
