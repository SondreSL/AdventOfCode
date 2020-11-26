module Day02 where

import Relude

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  print input
