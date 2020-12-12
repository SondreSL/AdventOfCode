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
  (F, n  ) -> _2 +~ n *^ view _1 ship
  (L, n  ) -> _1 %~ (!! mod (div n 90) 4) . iterate turnLeft
  (R, n  ) -> _1 %~ (!! mod (div n 90) 4) . iterate turnRight

run :: Lens' (Point, Point) Point -> Point -> [(Command, Int)] -> Int
run l start = mannDist origin . snd . foldl' (move l) (start, origin)

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day12.in"
    print $ run _2 right input
    print $ run _1 (V2 10 1) input

-- 1424
-- 63447
