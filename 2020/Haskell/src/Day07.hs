module Day07 where

import Relude

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day07.in"
  print input
