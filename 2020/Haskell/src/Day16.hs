module Day16 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day16.in"
  print input
