module Day23 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  print input
