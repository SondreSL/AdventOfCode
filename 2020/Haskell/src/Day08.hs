module Day08 where

import Relude

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  print input
