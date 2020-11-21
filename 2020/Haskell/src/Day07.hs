module Day07 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day07.in"
  print input
