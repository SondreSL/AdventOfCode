module Day03 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  print input
