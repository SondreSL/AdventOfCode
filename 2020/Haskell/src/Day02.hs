module Day02 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  print input
