module Day13 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day13.in"
  print input