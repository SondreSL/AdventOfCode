import Prelude hiding ((!!))
import Data.List hiding ((!!))
import Data.Bits
import Data.Char
import Data.Vector (Vector, (//), (!), fromList) -- Sequence might be a better option
import qualified Data.Sequence as S
import Data.Sequence (Seq, index)
import Data.Bool
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> Seq Int
parser = either (error "Bad parse") S.fromList . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Logic
type Memory = Seq Int

(!!!) :: Memory -> Int -> Int
(!!!) mem i = index mem (index mem i)

args :: Int -> Memory -> (Int, Int, Int)
args i vec = 
  let mode x = (== 0) . (`rem` 10) . div (index vec i) $ x
      f x v = bool index (!!!) (mode (100*(10^(x-1)))) vec (i + x)
   in (f 1 vec, f 2 vec, index vec (i + 3))
      
compute :: Int -> [Int] -> Memory -> [Int]
compute i input memory =
  let (a, b, c) = args i memory
   in case rem (index memory i) 100 of 
        1 -> compute (i + 4) input (S.update c (a + b) memory)
        2 -> compute (i + 4) input (S.update c (a * b) memory)
        3 -> let ix = index memory (i + 1)
              in compute (i + 2) (tail input) (S.update ix (head input) memory)
        4 -> a : compute (i + 2) input memory
        5 -> compute (bool (i + 3) b (0 /= a)) input memory
        6 -> compute (bool (i + 3) b (0 == a)) input memory
        7 -> compute (i + 4) input (S.update c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) input (S.update c (bool 0 1 (a == b)) memory) 
        99 -> []
        i -> error ("Invalid opcode: " ++ show i)

solveA :: Memory -> Int
solveA = last . compute 0 [1]

solveB :: Memory -> Int
solveB = last . compute 0 [5]

main = do
  contents <- parser <$> readFile "data/input-2019-5.txt"
  print $ solveA contents
  print $ solveB contents

-- 14155342
-- 8684145
