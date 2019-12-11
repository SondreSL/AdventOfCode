import Data.List
import Data.Bits
import Data.Char
import Data.Bool
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> Memory
parser = either (error "Bad parse") (M.fromList . zip [0..]) . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Logic
type Memory = M.Map Int Int

data ProgramState = Wait Int Int [Int] [Int] Memory 
                  | Halt [Int]
                  deriving Show

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

untilHalt :: ProgramState -> [Int]
untilHalt (Wait i r inp out mem) = untilHalt $ compute i r inp out mem
untilHalt (Halt a) = a

solveA :: Memory -> Int
solveA memory = head . untilHalt $ compute 0 0 [1] [] memory

solveB :: Memory -> Int
solveB memory = head . untilHalt $ compute 0 0 [2] [] memory

main = do
  contents <- parser <$> readFile "data/input-2019-9.txt"
  print $ solveA contents
  print $ solveB contents

-- 3241900951
-- 83089
