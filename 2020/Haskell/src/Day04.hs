module Day04 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  print input