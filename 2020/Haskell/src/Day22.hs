module Day22 where

parseInput = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day22.in"
  print input
