module Day11 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day11.in"
  print input
