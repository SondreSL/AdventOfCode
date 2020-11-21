module Day10 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  print input
