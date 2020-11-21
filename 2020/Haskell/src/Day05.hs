module Day05 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  print input
