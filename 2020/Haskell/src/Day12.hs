module Day12 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day12.in"
  print input
