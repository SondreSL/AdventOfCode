module Day21 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day21.in"
  print input
