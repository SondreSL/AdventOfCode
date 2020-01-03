import Data.List.Extra
import Data.Bits
import Data.Char
import Data.Bool
import Data.Complex
import Data.Function
import Data.Bifunctor
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

data ProgramState = Wait Int Int [Int] (Maybe Int) Memory 
                  | Halt (Maybe Int)
                  | Input Int Int [Int] (Maybe Int) Memory

instance Show ProgramState where
  show (Wait i r inp out mem) = "Wait " ++ show inp ++ " " ++ show out
  show (Input i r inp out mem) = "Input " ++ show inp ++ " " ++ show out  
  show (Halt out) = "Halt " ++ show out

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
      
compute :: Int -> Int -> [Int] -> Maybe Int -> Memory -> ProgramState
compute i rel input out memory =
  let (a, b, c) = args i rel memory
   in case rem (M.findWithDefault 0 i memory) 100 of 
        1 -> compute (i + 4) rel input out (M.insert c (a + b) memory)
        2 -> compute (i + 4) rel input out (M.insert c (a * b) memory)
        3 -> let ix = case div (index i memory) 100 `rem` 10 of
                        2 -> rel + (index (i + 1) memory)
                        _ -> index (i + 1) memory 
              in case input of
                   [] -> Input i rel input Nothing memory
                   (x:xs) -> compute (i + 2) rel xs out (M.insert ix x memory)
        4 -> Wait (i + 2) rel input (Just a) memory
        5 -> compute (bool (i + 3) b (0 /= a)) rel input out memory
        6 -> compute (bool (i + 3) b (0 == a)) rel input out memory
        7 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a == b)) memory) 
        9 -> compute (i + 2) (rel + a) input out memory
        99 -> Halt out
        i -> error ("Invalid opcode: " ++ show i)

halted :: ProgramState -> Bool
halted (Halt _) = True
halted _ = False

run :: ProgramState -> ProgramState
run (Halt out) = Halt out
run (Input ip rel inp out mem) = Input ip rel inp out mem
run (Wait ip rel inp out mem) = compute ip rel inp Nothing mem

untilHalt :: [ProgramState] -> [ProgramState]
untilHalt (Halt o:_) = [Halt o]
untilHalt (x:xs) = x : untilHalt xs

untilInput :: [ProgramState] -> [ProgramState]
untilInput (inp@(Input _ _ _ _ _):_) = [inp]
untilInput (x:xs) = x : untilInput xs

-- Ship
type View = Map Pos Objects

data Objects = Scaffold
             | Empty
             | Rob Robot
             deriving (Eq, Show)

data Robot = Robot Pos Facing
  deriving (Eq, Show)

data Ship = Ship Pos View
  deriving Show

isRobot :: Objects -> Bool
isRobot (Rob _) = True
isRobot _ = False

newDir :: Facing -> Int -> Facing
newDir c 1 = c * (0:+1)
newDir c 0 = c * (0:+(-1))

move :: Pos -> Facing -> Pos
move (x, y) (1:+1) = (x, y + 1) -- Up
move (x, y) (1:+(-1)) = (x - 1, y) -- Left
move (x, y) ((-1):+1) = (x + 1, y) -- Right
move (x, y) ((-1):+(-1)) = (x, y - 1) -- Down

drawMap :: View -> IO ()
drawMap positions = 
  let ks = M.keys positions
      xs = map fst ks
      ys = map snd ks
      (topX, botX) = (maximum xs, minimum xs)
      (topY, botY) = (maximum ys, minimum ys)
      coords = map (zip [botX .. topX] . repeat) [botY .. topY]
      drawObj (Just (Rob _)) = '^'
      drawObj (Just Scaffold) = '#'
      drawObj Nothing = ' '
   in mapM_ putStrLn $ map (map (drawObj . flip M.lookup positions)) coords

ascii :: Ship -> ProgramState -> Ship
ascii ship@(Ship pos@(x,y) view) (Halt out) = ship
ascii ship@(Ship pos@(x,y) view) state@(Wait ip rel inp out mem) = 
  case out of
    (Just 35) -> Ship (x+1,y) $ M.insert pos Scaffold view
    (Just 46) -> Ship (x+1,y) view
    (Just 10) -> Ship (0,y+1) view
    (Just 94) -> Ship (x+1,y) $ M.insert pos (Rob (Robot pos (1:+1))) view
    (Just 60) -> Ship (x+1,y) $ M.insert pos (Rob (Robot pos (1:+(-1)))) view
    (Just 118) -> Ship (x+1,y) $ M.insert pos (Rob (Robot pos ((-1):+(-1)))) view
    (Just 62) -> Ship (x+1,y) $ M.insert pos (Rob (Robot pos ((-1):+1))) view
    _ -> ship

findCorners :: Ship -> [Pos]
findCorners (Ship _ view) = 
  let allSides (x,y) = [[(x+1,y), (x,y+1)], [(x-1,y), (x,y-1)], [(x-1,y),(x,y+1)], [(x+1,y),(x,y-1)]]
      hasNeigbours pos = (==1) . length . filter (all (flip M.member view)) $ allSides pos
   in filter hasNeigbours $ M.keys view

findCrosses :: Ship -> [Pos]
findCrosses (Ship _ view) = 
  let allSides (x,y) = [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
      hasNeigbours pos = all (flip M.member view) $ allSides pos
   in filter hasNeigbours $ M.keys view

getOutput :: ProgramState -> Int
getOutput (Halt (Just a)) = a

solveA :: Memory -> Int
solveA mem = sum
             . map (uncurry (*))
             . findCrosses 
             . foldl ascii (Ship (0,0) M.empty) 
             . takeWhile (not . halted) 
             . tail 
             $ iterate run (Wait 0 0 [] Nothing mem)

solveB :: Memory -> Int
solveB mem = 
  let input = untilInput $ iterate run (Wait 0 0 [] Nothing $ M.insert 0 2 mem)
      (Input i r inp out memory) = last input
      -- By hand
      cmd = "A,B,A,C,A,B,C,A,B,C\n"
      a = "R,8,R,10,R,10\n"
      b = "R,4,R,8,R,10,R,12\n"
      c = "R,12,R,4,L,12,L,12\n"
      n = "n\n"
      (Wait _ _ _ (Just ans) _) = last 
                                  . init 
                                  . untilHalt 
                                  $ iterate run (Wait i r (map ord (cmd++a++b++c++n)) Nothing memory)
   in ans

main = do
  input <- parser <$> readFile "data/input-2019-17.txt"
  print $ solveA input
  print $ solveB input

-- 3608
-- 897426
