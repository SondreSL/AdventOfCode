module Day23 where

import Data.Digits
import qualified Data.IntMap.Strict as Map
import           Data.IntMap.Strict   ( IntMap )
import Data.Function ((&))

-- A port of Sarek's solution
-- https://github.com/sarsko/aoc-2020/blob/main/src/day23.rs
--
play :: Int -> Int -> Int -> IntMap Int -> IntMap Int
play _ 0 _ mp = mp
play m n current mp =
  let first = mp Map.! current
      second = mp Map.! first
      third = mp Map.! second
      dest = findDest (current - 1)
      findDest val
        | val == 0 = findDest m
        | val == first || val == second || val == third = findDest (val - 1)
        | otherwise = val
      mp' = Map.insert current (mp Map.! third) mp
          & Map.insert third (mp Map.! dest)
          & Map.insert dest first
   in play m (n - 1) (mp' Map.! current) mp'

part1 :: [Int] -> Int
part1 xs = unDigits 10 $ take 8 (go 1)
  where mp = play 9 100 (head xs) $ Map.fromList $ (zip <*> tail) xs
        go n = let v = mp Map.! n
                in v : go v

part2 :: [Int] -> Int
part2 xs = let one = mp Map.! 1
               two = mp Map.! one
            in one * two
  where
    mp = play 1000000 10000000 (head xs) $ Map.fromList $ (zip <*> tail) xs

main :: IO ()
main = do
    let input = digits 10 327465189
    print $ part1 $ input <> [3]
    print $ part2 $ input <> [10 .. 1000000] <> [3]

-- 82934675
-- 474600314018
