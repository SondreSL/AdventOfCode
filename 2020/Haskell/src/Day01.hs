module Day01 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day01.in"
  print input
