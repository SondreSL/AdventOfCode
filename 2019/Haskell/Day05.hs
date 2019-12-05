import Prelude hiding ((!!))
import Data.List hiding ((!!))
import Data.Bits
import Data.Char
import Data.Vector (Vector, (//), (!), fromList) -- Sequence might be a better option
import Data.Bool
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> Vector Int
parser = either (error "Bad parse") fromList . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Logic
type Memory = Vector Int

(!!) :: Memory -> Int -> Int
(!!) vec i = vec ! (vec ! i)

args :: Int -> Memory -> (Int, Int, Int)
args i vec = 
  let mode x = (== 0) . (`rem` 10) . div (vec ! i) $ x
      f x v = bool (!) (!!) (mode (100*(10^(x-1)))) vec (i + x)
   in (f 1 vec, f 2 vec, vec ! (i + 3))
      
compute :: Int -> [Int] -> [Int] -> Memory -> (Int, [Int])
compute i input out vec =
  let (a, b, c) = args i vec
   in case rem (vec ! i) 100 of 
        1 -> compute (i + 4) input out (vec // [(c, a + b)])
        2 -> compute (i + 4) input out (vec // [(c, a * b)])
        3 -> let ix = vec ! (i + 1)
              in compute (i + 2) (tail input) out (vec // [(ix, head input)])
        4 -> compute (i + 2) input (a : out) vec
        5 -> compute (bool (i + 3) b (0 /= a)) input out vec
        6 -> compute (bool (i + 3) b (0 == a)) input out vec
        7 -> compute (i + 4) input out (vec // [(c, bool 0 1 (a < b))])
        8 -> compute (i + 4) input out (vec // [(c, bool 0 1 (a == b))])
        99 -> (vec ! 0, out)
        i -> error ("Invalid opcode: " ++ show i)

solveA :: Memory -> Int
solveA = head . snd . compute 0 [1] []

solveB :: Memory -> Int
solveB = head . snd . compute 0 [5] []

main = do
  contents <- parser <$> readFile "data/input-2019-5.txt"
  print $ solveA contents
  print $ solveB contents

-- 14155342
-- 8684145
