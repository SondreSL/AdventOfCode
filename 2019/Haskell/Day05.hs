import Data.List
import Data.Bits
import Data.Char
import Data.Vector (Vector, (//), (!), fromList)
import Data.Bool
import Text.ParserCombinators.Parsec

parser :: String -> Vector Int
parser = either (error "Bad parse") fromList . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',') 
        num = many1 digit 
        minus = (:) <$> char '-' <*> num

compute :: Int -> [Int] -> [Int] -> Vector Int -> (Int, [Int])
compute i input out vec =
    case rem (vec ! i) 100 of 
      1 -> arit (+) i input out vec
      2 -> arit (*) i input out vec
      3 -> getVal i input out vec
      4 -> output i input out vec
      5 -> compute (jump i vec) input out vec
      6 -> compute (jump i vec) input out vec
      7 -> compute (i + 4) input out (comp (<) i vec)
      8 -> compute (i + 4) input out (comp (==) i vec)
      99 -> (vec ! 0, out)
      i -> error ("Invalid opcode: " ++ show i)

args :: Int -> Vector Int -> (Int, Int, Int)
args i vec = 
  let [x, y, z] = zipWith (.|.) (replicate 3 0) . pad . (map digitToInt . show) $ vec ! i
      pad xs = replicate (5 - (length xs)) 0 ++ xs
      imm i = (vec ! i)
      pos i = (vec ! (imm i))
      a = bool (imm (i + 1)) (pos (i + 1)) (z == 0)
      b = bool (imm (i + 2)) (pos (i + 2)) (y == 0)
      c = vec ! (i + 3)
   in (a, b, c)
      
arit :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]-> Vector Int -> (Int, [Int])
arit f i input out vec = compute (i + 4) input out (vec // [(c, new)])
  where (a, b, c) = args i vec
        new = f a b

jump :: Int -> Vector Int -> Int
jump i vec = bool (i + 3) y (comp x)
  where (x, y, _) = args i vec
        comp = if rem (vec ! i) 100 == 5
                  then (/= 0)
                  else (== 0)

comp :: (Int -> Int -> Bool) -> Int -> Vector Int -> Vector Int
comp op i vec = vec // [(z, new)]
  where (x, y, z) = args i vec
        new = bool 0 1 (op x y)

getVal :: Int -> [Int] -> [Int] -> Vector Int -> (Int, [Int])
getVal i input out vec = compute (i + 2) (tail input) out (vec // [(ix, (head input))])
  where ix = vec ! (i + 1)

output :: Int -> [Int] -> [Int] -> Vector Int -> (Int, [Int])
output i input out vec = compute (i + 2) input (x : out) vec 
  where (x, _, _) = args i vec

solveA :: Vector Int -> Int
solveA = head . snd . compute 0 [1] []

solveB :: Vector Int -> Int
solveB = head . snd . compute 0 [5] []

main = do
  contents <- parser <$> readFile "data/input-2019-5.txt"
  print $ solveA contents
  print $ solveB contents

-- 14155342
-- 8684145
