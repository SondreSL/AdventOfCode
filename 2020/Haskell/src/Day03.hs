module Day03 where

import Control.Lens ( folded, maximumOf, view )
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (Point, count, parseAsciiMap)
import Linear (R1 (_x), R2 (_y), V2 (V2))

parseInput :: String -> Set Point
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '.' = Nothing
    f '#' = Just True

toboggan :: Set Point -> Point -> Int
toboggan input = count (`Set.member` input) . slope
  where
    Just maxY = maximumOf (folded . _y) input
    Just maxX = maximumOf (folded . _x) input
    slope p = takeWhile ((<= maxY) . view _y) $ iterate (addMod p) (V2 0 0)
    addMod (V2 x y) (V2 x' y') = V2 ((x + x') `mod` succ maxX) (y + y')

part1 :: Set Point -> Point -> Int
part1 = toboggan

part2 :: Set Point -> Int
part2 input = product $ map (toboggan input) [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  print $ part1 input (V2 3 1)
  print $ part2 input

-- 299
-- 3621285278
