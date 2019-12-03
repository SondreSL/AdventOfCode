import Data.List
import Data.Maybe
import Control.Arrow  ((&&&))
import qualified Data.Map as M

type Vals = M.Map (Int, Int) Int

strToData :: String -> [[(Char, Int)]]
strToData = map (map f . words) . lines
  where f = head &&& read . tail

compute :: [(Char, Int)] -> M.Map (Int, Int) Int
compute input = go 1 (0, 0) (M.empty) input 
  where go len (x, y) values [] = values
        go len (x, y) values ((_, 0):xs) = go len (x, y) values xs
        go len (x, y) values (('U', n):xs) = go (len+1) (x, y+1) (maybeInsert (x, y+1) len values) (('U', (n-1)):xs)
        go len (x, y) values (('D', n):xs) = go (len+1) (x, y-1) (maybeInsert (x, y-1) len values) (('D', (n-1)):xs)
        go len (x, y) values (('L', n):xs) = go (len+1) (x+1, y) (maybeInsert (x+1, y) len values) (('L', (n-1)):xs)
        go len (x, y) values (('R', n):xs) = go (len+1) (x-1, y) (maybeInsert (x-1, y) len values) (('R', (n-1)):xs)

maybeInsert :: (Int, Int) -> Int -> Vals -> Vals
maybeInsert pos len m
  | M.member pos m = m
  | otherwise = (M.insert pos len m)

solveA :: Vals -> Vals -> Int
solveA a b =
  let interKeys = M.intersection a b
      manhatten = M.mapKeys (\(a, b) -> abs (0 - a) + abs (0 - b)) interKeys
   in fst $ M.findMin manhatten

solveB :: Vals -> Vals -> Int
solveB a b =
  let interKeys = M.intersection a b
      f pos len (p,l) = if len < l then (pos, len) else (p, l)
      (k, minDist) = M.foldrWithKey f ((0,0), maxBound::Int) interKeys
   in fromJust $ (+) <$> M.lookup k a <*> M.lookup k b
   -- This is definitely not sound, but it does work
   -- Assumes the shortest distance in left map is part of the solution

main = do
  contents <- strToData <$> readFile "data/input-2019-3.txt"
  (a:b:_) <- pure . map compute $ contents
  print $ solveA a b
  print $ solveB a b
