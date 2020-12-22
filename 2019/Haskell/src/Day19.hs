module Day19 where

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
      code = rem (M.findWithDefault 0 i memory) 100
   in case code of
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
data Beam = B { _x   :: Int
              , _y   :: Int
              , _len :: Int
              }
              deriving Show

inBeam :: Memory -> (Int, Int) -> Bool
inBeam mem (a, b) = [1] == compute 0 0 [a, b] mem

findBeam :: Memory -> Int -> Int -> Beam
findBeam mem a b =
  let xs = [(x,y) | x <- [a..], y <- [b..]]
      beam = takeWhile (inBeam mem) $ dropWhile (not . (inBeam mem)) xs
      (x,y) = head beam
   in B x y (length beam)

findSpace :: Memory -> M.Map (Int, Int) Beam -> Int -> Int -> Int -> Int -> Int
findSpace mem seen size spread cand starty =
  let (B x y len) = M.findWithDefault (findBeam mem cand starty) (cand, starty) seen
      y' = (y + (len - 1)) - (size - 1)
      x' = x + (size - 1)
      next = inBeam mem (x', y')
   in case next of
        True  -> case spread of
                  1 -> x * 10000 + y' -- Found answer
                  _ -> findSpace mem (M.insert (cand, starty) (B x y len) seen) size (spread `div` 2) (cand - (spread `div` 2)) 0 -- Go back
        False -> case spread of
                   1 -> findSpace mem (M.insert (cand, starty) (B x y len) seen) size 1 (cand + 1) y -- Check next
                   _ -> findSpace mem (M.insert (cand, starty) (B x y len) seen) size spread (cand + spread) y

solveA :: Memory -> Int
solveA mem =
  let xs = [(x,y) | x <- [0..49], y <- [0..49]]
      in count id $ map (inBeam mem) xs
     where count f = length . filter f

solveB :: Memory -> Int
solveB mem = findSpace mem M.empty 100 1024 1024 0

main = do
  input <- parser <$> readFile "data/input-2019-19.txt"
  print $ solveA input
  print $ solveB input


-- 150
-- 12201460

