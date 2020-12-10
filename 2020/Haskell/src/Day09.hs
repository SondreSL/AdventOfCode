module Day09 where

import Lib ( linedNums )
import Data.Maybe ( listToMaybe )
import Data.List.Extra ( inits, tails )
import Control.Monad (guard)

part1 :: [Int] -> Int -> Maybe Int
part1 input n = listToMaybe $ do
  ts <- tails input
  let xs = take n ts
      target = ts !! n
      pairs = do
        y : ys <- tails xs
        z <- ys
        pure $ y + z
  guard $ target `notElem` pairs
  pure target

part2 :: [Int] -> Int -> Maybe Int
part2 input n = listToMaybe $ do
  xs <- dropWhile ((< n) . sum) $ inits input
  candidate <- take 1 . dropWhile ((n <) . sum) $ tails xs
  guard $ sum candidate == n
  pure $ minimum candidate + maximum candidate

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day09.in"
  let i = part1 input 25
  print i
  print $ i >>= part2 input

-- 2089807806
-- 245848639
