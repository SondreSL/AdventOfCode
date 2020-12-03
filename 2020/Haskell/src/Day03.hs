module Day03 where

import Control.Lens (bimap, view, _2)
import qualified Data.Map as Map
import Data.Semigroup ( Max(Max) )
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (count, parseAsciiMap, pointToTuple)

type Tuple = (Int, Int)

parseInput :: String -> Set Tuple
parseInput = Set.map pointToTuple . Map.keysSet . parseAsciiMap f
  where
    f '#' = Just True
    f _ = Nothing

toboggan :: Set Tuple -> Tuple -> Int
toboggan input = count (`Set.member` input) . slope
  where
    (Max maxX, Max maxY) = foldMap (bimap Max Max) input
    slope p = takeWhile ((<= maxY) . view _2) $ iterate (addMod p) (0, 0)
    addMod (x, y) (x', y') = ((x + x') `mod` succ maxX, y + y')

part1 :: Set Tuple -> Tuple -> Int
part1 = toboggan

part2 :: Set Tuple -> Int
part2 = product . (<$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]) . toboggan

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  print $ part1 input (3, 1)
  print $ part2 input

-- 299
-- 3621285278
