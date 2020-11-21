module Day17 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day17.in"
  print input
