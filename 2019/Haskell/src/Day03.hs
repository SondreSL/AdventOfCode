import Prelude hiding (foldr)
import Data.List hiding (insert, foldr)
import Control.Arrow  ((&&&))
import Data.Map (insertWith, insert, member, empty, intersection, intersectionWith, foldr, foldrWithKey, Map)

type Vals = Map (Int, Int) Int

strToData :: String -> [[(Char, Int)]]
strToData = map (map f . words) . lines
  where f = head &&& read . tail

compute :: [(Char, Int)] -> Vals
compute input = go 1 (0, 0) (empty) input 
  where go len (x, y) values [] = values
        go len (x, y) values ((_, 0):xs) = go len (x, y) values xs
        go len (x, y) values (('U', n):xs) = go (len+1) (x, y+1) (maybeInsert (x, y+1) len values) (('U', (n-1)):xs)
        go len (x, y) values (('D', n):xs) = go (len+1) (x, y-1) (maybeInsert (x, y-1) len values) (('D', (n-1)):xs)
        go len (x, y) values (('L', n):xs) = go (len+1) (x+1, y) (maybeInsert (x+1, y) len values) (('L', (n-1)):xs)
        go len (x, y) values (('R', n):xs) = go (len+1) (x-1, y) (maybeInsert (x-1, y) len values) (('R', (n-1)):xs)
        maybeInsert = insertWith const

solveA :: Vals -> Vals -> Int
solveA = (foldrWithKey (const . min . manDist) (maxBound::Int) .) . intersection
  where manDist (a, b) = abs a + abs b

solveB :: Vals -> Vals -> Int
solveB = (minimum .) . intersectionWith (+)

main = do
  (a:b:_) <- map compute . strToData <$> readFile "data/input-2019-3.txt"
  print $ solveA a b
  print $ solveB a b

-- Part A: 209
-- Part B: 43258
