module Day18 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  print input
