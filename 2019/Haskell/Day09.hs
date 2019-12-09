import Data.List
import Data.Bits
import Data.Char
import Data.Bool
import Data.Sequence (Seq, update, index)
import qualified Data.Sequence as S
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> [Int]
parser = either (error "Bad parse") id . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

-- Logic
type Memory = Seq Int

(!!!) :: Memory -> Int -> Int
(!!!) mem i = index mem (index mem i)

args :: Int -> Int -> Memory -> (Int, Int, Int)
args i rel vec = 
  let mode x = div (index vec i) x `rem` 10
      f x = case mode (100*(10^(x-1))) of
                0 -> vec !!! (i + x)
                1 -> index vec (i + x)
                2 -> index vec (rel + (index vec (i + x)))
      g x = case div (index vec i) 10000 `rem` 10 of
                2 -> rel + (index vec (i + 3))
                _ -> index vec (i + 3)
   in (f 1, f 2, g 3)
      
compute :: Int -> Int -> [Int] -> Memory -> [Int]
compute i rel input memory =
  let (a, b, c) = args i rel memory
   in case rem (index memory i) 100 of 
        1 -> compute (i + 4) rel input (update c (a + b) memory)
        2 -> compute (i + 4) rel input (update c (a * b) memory)
        3 -> let ix = case div (index memory i) 100 `rem` 10 of
                        2 -> rel + (index memory (i + 1))
                        _ -> index memory (i + 1)
              in compute (i + 2) rel (tail input) (update ix (head input) memory)
        4 -> a : compute (i + 2) rel input memory
        5 -> compute (bool (i + 3) b (0 /= a)) rel input memory
        6 -> compute (bool (i + 3) b (0 == a)) rel input memory
        7 -> compute (i + 4) rel input (update c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) rel input (update c (bool 0 1 (a == b)) memory) 
        9 -> compute (i + 2) (rel + a) input memory
        99 -> []
        i -> error ("Invalid opcode: " ++ show i)

solveA :: [Int] -> Int
solveA memory = head $ compute 0 0 [1] (S.fromList $ memory ++ replicate 1000 0)

solveB :: [Int] -> Int
solveB memory = head $ compute 0 0 [2] (S.fromList $ memory ++ replicate 1000 0)

main = do
  contents <- parser <$> readFile "data/input-2019-9.txt"
  print $ solveA contents
  print $ solveB contents

-- 3241900951
-- 83089
