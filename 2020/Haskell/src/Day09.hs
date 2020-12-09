module Day09 where

import Lib ( linedNums )
import Data.Maybe ( listToMaybe )
import Data.List.Extra ( inits, tails )
import Control.Monad (replicateM, guard)

part1 :: [Int] -> Int -> Maybe Int
part1 input n = listToMaybe $ do
  ts <- tails input
  let xs = take n ts
      pairs = map sum $ replicateM 2 xs
      target = ts !! n
  guard $ target `notElem` pairs
  pure target

part2 :: [Int] -> Int -> Maybe Int
part2 input n = listToMaybe $ do
  xs <- tails input
  let ts = last . takeWhile ((<= n) . sum) $ inits xs
  guard $ sum ts == n
  pure $ minimum ts + maximum ts

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day09.in"
  let i = part1 input 25
  print i
  print $ i >>= part2 input

-- 2089807806
-- 245848639
