module Day23 where

import Data.Digits ( digits, unDigits )
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as V

-- A port of Sarek's solution
-- https://github.com/sarsko/aoc-2020/blob/main/src/day23.rs
play :: Int -> Int -> Int -> [(Int, Int)] -> V.Vector Int
play m n current mp = runST $ do
  vec <- MV.replicate (m + 1) 0

  forM_ mp $ \(i, v) -> do
    MV.write vec i v

  let run current turn
        | turn == n = V.freeze vec
        | otherwise = do
            first <- MV.read vec current
            second <- MV.read vec first
            third <- MV.read vec second
            let findDest val
                  | val == 0 = findDest m
                  | val == first || val == second || val == third = findDest (val - 1)
                  | otherwise = val
                dest = findDest (current - 1)
            MV.read vec third >>= MV.write vec current
            MV.read vec dest >>= MV.write vec third
            MV.write vec dest first
            current' <- MV.read vec current
            run current' (succ turn)

  run current 0

part1 :: [Int] -> Int
part1 xs = unDigits 10 $ take 8 (go 1)
  where mp = play 9 100 (head xs) $ (zip <*> tail) xs
        go n = let v = mp V.! n
                in v : go v

part2 :: [Int] -> Int
part2 xs = let one = mp V.! 1
               two = mp V.! one
            in one * two
  where
    mp = play 1000000 10000000 (head xs) $ (zip <*> tail) xs

main :: IO ()
main = do
    let input = digits 10 327465189
    print $ part1 $ input <> [3]
    print $ part2 $ input <> [10 .. 1000000] <> [3]

-- 82934675
-- 474600314018
