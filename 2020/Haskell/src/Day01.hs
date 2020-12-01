module Day01 where

import Lib (linedNums)
import Data.Maybe (listToMaybe)

part1 :: [Int] -> Maybe Int
part1 input =
  listToMaybe
    [ x * y
    | x <- input
    , y <- input
    , x + y == 2020
    ]

part2 :: [Int] -> Maybe Int
part2 input =
  listToMaybe
    [ x * y * z
    | x <- input
    , y <- input
    , z <- input
    , x + y + z == 2020
    ]

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day01.in"
  print $ part1 input
  print $ part2 input

-- 793524
-- 61515678
