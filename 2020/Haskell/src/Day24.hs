module Day24 where

import Relude

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day24.in"
  print input
