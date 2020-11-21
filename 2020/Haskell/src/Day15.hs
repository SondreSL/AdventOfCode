module Day15 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day15.in"
  print input
