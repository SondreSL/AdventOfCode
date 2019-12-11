import Data.List
import Data.Bits
import Data.Char
import Data.Bool
import Data.Complex
import Data.Map.Strict (Map)
import Debug.Trace
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> Memory
parser = either (error "Bad parse") (M.fromList . zip [0..]) . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Logic
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

-- Paint robot
runRobot :: ProgramState -> Hull -> Pos -> Facing -> (Int, M.Map Pos Int)
runRobot (Halt []) hull pos fac = (length $ M.keys hull, hull)
runRobot (Wait i r inp [c] m) hull pos fac =
  let (Wait ii rr input [new] mem) = compute i r inp [] m
      newFace = newDir fac new
      newPos = move pos newFace
      newHull = M.insert pos c hull
      col = M.findWithDefault 0 newPos newHull
   in runRobot (compute ii rr (input ++ [col]) [] mem) newHull newPos newFace
runRobot a hull pos fac = error $ show a

newDir :: Facing -> Int -> Facing
newDir c 1 = c * (0:+1)
newDir c 0 = c * (0:+(-1))

move :: Pos -> Facing -> Pos
move (x, y) (1:+1) = (x, y + 1) -- Up
move (x, y) (1:+(-1)) = (x - 1, y) -- Left
move (x, y) ((-1):+1) = (x + 1, y) -- Right
move (x, y) ((-1):+(-1)) = (x, y - 1) -- Down

printHull :: M.Map Pos Int -> IO ()
printHull positions = 
  let ks = M.keys positions
      xs = map fst ks
      ys = map snd ks
      (topX, botX) = (maximum xs, minimum xs)
      (topY, botY) = (maximum ys, minimum ys)
      lines = map (mkLine positions botX topX) [botY..topY]
   in mapM_ putStrLn $ reverse lines

mkLine :: M.Map Pos Int -> Int -> Int -> Int -> String
mkLine hull from to row
  | from > to = ""
  | otherwise = toChar (M.findWithDefault 0 (from, row) hull) : mkLine hull (from + 1) to row
    where toChar x = bool '🖤' '🔴' (x == 1)

solveA :: Memory -> Int
solveA mem = 
  let (a, xs) = runRobot (compute 0 0 [0] [] mem) M.empty (0,0) (1:+1)
   in a

solveB :: Memory -> IO ()
solveB mem = 
  let (a, xs) = runRobot (compute 0 0 [1] [] mem) M.empty (0,0) (1:+1)
   in printHull xs

main = do
  -- testIN <- parser <$> readFile "data/input-2019-9.txt"
  -- print $ test testIN
  contents <- parser <$> readFile "data/input-2019-11.txt"
  print $ solveA contents
  solveB contents

-- 1747
-- ZCGRHKLB
