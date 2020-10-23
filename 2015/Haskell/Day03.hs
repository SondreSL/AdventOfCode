module Day03 where

import           Linear.V2
import           Data.List                      ( nub )
import           Control.Lens

type Point = V2 Int

move :: String -> [Point]
move = nub . scanl go (V2 0 0)
 where
  go p '^' = p + V2 1 0
  go p 'v' = p + V2 (-1) 0
  go p '<' = p + V2 0 (-1)
  go p '>' = p + V2 0 1

part1 :: String -> Int
part1 = length . move

part2 :: String -> Int
part2 input = length . nub $ santa ++ robot
 where
  santa = move $ input ^.. (traversed . indices even)
  robot = move $ input ^.. (traversed . indices odd)

main :: IO ()
main = do
  input <- init <$> readFile "../data/03.in"
  print $ part1 input
  print $ part2 input
