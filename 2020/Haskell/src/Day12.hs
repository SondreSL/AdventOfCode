module Day12 where

import Advent.Coord
  ( down, left, origin, right, turnLeft, turnRight, up,)
import Control.Lens (Lens', view, (%~), (&), (+~), _1, _2)
import Data.List (foldl')
import Lib (Point, mannDist)
import Linear (V2 (..), (*^))

data Command = N | S | E | W | L | R | F
    deriving (Show, Read, Eq, Ord)

parseInput :: String -> [(Command, Int)]
parseInput = map (\x -> (read [head x], read $ tail x)) . lines

move :: Lens' (Point, Point) Point -> (Point, Point) -> (Command, Int) -> (Point, Point)
move l ship c = ship & case c of
  (N, n  ) -> l  +~ n *^ up
  (E, n  ) -> l  +~ n *^ right
  (S, n  ) -> l  +~ n *^ down
  (W, n  ) -> l  +~ n *^ left
  (_, 180) -> _1 %~ negate
  (L, 90 ) -> _1 %~ turnLeft
  (R, 90 ) -> _1 %~ turnRight
  (L, 270) -> _1 %~ turnRight
  (R, 270) -> _1 %~ turnLeft
  (F, n  ) -> _2 +~ n *^ view _1 ship

part1 :: [(Command, Int)] -> Int
part1 = mannDist (V2 0 0) . snd . foldl' (move _2) (right, origin)

part2 :: [(Command, Int)] -> Int
part2 = mannDist (V2 0 0) . snd . foldl' (move _1) (V2 10 1, origin)

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day12.in"
    print $ part1 input
    print $ part2 input

-- 1424
-- 63447
