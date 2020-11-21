module Day25 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  print input
