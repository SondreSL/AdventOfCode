module Day20 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day20.in"
  print input
