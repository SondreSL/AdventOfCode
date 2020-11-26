module Day09 where

import Relude

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  print input
