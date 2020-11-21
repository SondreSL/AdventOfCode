module Day19 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day19.in"
  print input
