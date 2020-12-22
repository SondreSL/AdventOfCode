module Day10 where

import Prelude hiding (lookup)
import Data.List.Extra hiding (lookup)
import Data.Maybe
import Data.Map.Strict (Map, lookup)
import Data.Tuple.Extra
import qualified Data.Map.Strict as M

type Data = Map Asteroid Distance
type Asteroid = (Int, Int)
type Distance = Int

coords :: [String] -> Data
coords input = M.map (\_ -> 0) . M.fromList $ go 0 input
  where go _ [] = []
        go a (x:xs) = filter ((/='.') . snd) $ (map (\(col, ch) -> ((col, a), ch)) (zip [0..] x)) ++ go (a + 1) xs

angle :: Asteroid -> Asteroid -> (Int, Int)
angle (x, y) (a, b) =
  let (dx, dy) = (a - x, b - y)
      gd = gcd dx dy
   in (div dx gd, div dy gd)

inSight :: Data -> Asteroid -> Asteroid -> Bool
inSight as (x, y) (a, b) =
  let addPair (a, b) (x, y) = (a + x, b + y)
      an = angle (x, y) (a, b)
   in all (flip M.notMember as) . tail . takeWhile (/= (a, b)) . iterate (addPair an) $ (x, y)

findInSight :: [(Asteroid, Asteroid)] -> Data -> Data
findInSight [] asts = asts
findInSight ((a,b):xs) asts
  | inSight asts a b = findInSight xs $ M.update add1 a $ M.update add1 b asts
  | otherwise        = findInSight xs asts
       where add1 a = Just (a + 1)

solveA :: Data -> (Asteroid, Distance)
solveA xs = M.foldrWithKey (\k v (a, b) -> if v > b then (k, v) else (a, b)) ((0, 0),0) res
  where res = findInSight =<< (pairs . M.keys) $ xs
        pairs [] = []
        pairs (x:xs) = map ((,) x) xs ++ pairs xs

solveB :: Data -> Asteroid -> Int
solveB xs base =
  let angles = sortOn (uncurry (flip atan2) . both fromIntegral . angle base) . filter (/=base) $ M.keys xs
   in uncurry (+) . first (*100) $ (reverse angles) !! 199

main = do
  contents <- coords . lines <$> readFile "data/input-2019-10.txt"
  (base, res) <- pure $ solveA contents
  print res
  print $ solveB contents base

-- 263
-- 1110
