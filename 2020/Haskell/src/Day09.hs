module Day09 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  print input
