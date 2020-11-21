module Day14 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day14.in"
  print input
