module Day09 where

import Lib ( linedNums )
import Data.Maybe ( listToMaybe )
import Data.List.Extra ( inits, tails )
import Control.Monad (replicateM, guard)

parseInput :: String -> [Int]
parseInput = linedNums

part1 :: (Eq a, Num a) => Int -> [a] -> Maybe a
part1 n input = listToMaybe $ do
  t <- tails input
  let xs = take n t
      pairs = map sum $ replicateM 2 xs
      y = t !! n
  guard $ y `notElem` pairs
  -- guard $ any (\x -> (y - x) `elem` xs) xs
  pure y

part2 :: (Num a, Ord a) => a -> [a] -> Maybe a
part2 n input = listToMaybe $ do
  xs <- tails input
  ys <- inits xs
  guard $ sum ys == n
  let mini = minimum ys
      maxa = maximum ys
  pure $ mini + maxa


main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  let Just i = part1 25 input
  print i
  print $ part2 i input

-- 2089807806
-- 245848639
