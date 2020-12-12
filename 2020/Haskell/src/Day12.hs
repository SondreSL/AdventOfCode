module Day12 where

import Advent.Coord
    ( origin, up, down, left, right, turnLeft, turnRight )
import Control.Lens ( Lens', (&), view, (%~), (+~), makeLenses )
import Lib ( mannDist, Point )
import Linear (V2 (..), (*^) )
import Data.List (foldl')

data Command = N | S | E | W | L | R | F
    deriving (Show, Read, Eq, Ord)

data Ship = Ship
    { _dir :: !Point
    , _pos :: !Point
    }
    deriving (Show, Eq, Ord)
makeLenses ''Ship

parseInput :: String -> [(Command, Int)]
parseInput = map (\x -> (read [head x], read $ tail x)) . lines

move :: Lens' Ship (V2 Int) -> Ship -> (Command, Int) -> Ship
move l ship c = ship & case c of
  (N, n  ) -> l +~ n *^ up
  (E, n  ) -> l +~ n *^ right
  (S, n  ) -> l +~ n *^ down
  (W, n  ) -> l +~ n *^ left
  (_, 180) -> dir %~ negate
  (L, 90 ) -> dir %~ turnLeft
  (R, 90 ) -> dir %~ turnRight
  (L, 270) -> dir %~ turnRight
  (R, 270) -> dir %~ turnLeft
  (F, n)   -> pos +~ (n *^ view dir ship)

part1 :: [(Command, Int)] -> Int
part1 = mannDist (V2 0 0) . view pos . foldl' (move pos) (Ship right origin)

part2 :: [(Command, Int)] -> Int
part2 = mannDist (V2 0 0) . view pos . foldl' (move dir) (Ship (V2 10 1) origin)

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day12.in"
    print $ part1 input
    print $ part2 input

-- 1424
-- 63447
