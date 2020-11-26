module Day06 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  print input
