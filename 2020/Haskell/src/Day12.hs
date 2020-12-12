module Day12 where

import Control.Lens ( (&), view, (%~), (+~), (<>~), makeLenses )
import Lib ( mannDist, Dir(..), Point )
import Linear (V2 (..), perp)

data Move = N | S | E | W | L | R | F
    deriving (Show, Read, Eq, Ord)

data Ship = Ship
    { _dir :: !Dir
    , _pos :: !Point
    }
    deriving (Show, Eq, Ord)
makeLenses ''Ship

parseInput :: String -> [(Move, Int)]
parseInput = map (\x -> (read [head x], read $ tail x)) . lines

dirToMove North = N
dirToMove East = E
dirToMove West = W
dirToMove South = S

move :: Ship -> (Move, Int) -> Ship
move ship m = case m of
  (N, n  ) -> ship & pos +~ V2 0 n
  (E, n  ) -> ship & pos +~ V2 n 0
  (S, n  ) -> ship & pos +~ V2 0 (-n)
  (W, n  ) -> ship & pos +~ V2 (-n) 0
  (_, 180) -> ship & dir <>~ South
  (L, 90 ) -> ship & dir <>~ West
  (R, 90 ) -> ship & dir <>~ East
  (L, 270) -> ship & dir <>~ East
  (R, 270) -> ship & dir <>~ West
  (F, n)   -> move ship (dirToMove (view dir ship), n)

part1 :: [(Move, Int)] -> Int
part1 = mannDist (V2 0 0) . view pos . foldl move (Ship East (V2 0 0))

data Waypoint = Waypoint
  { _wp :: Point
  , _ship :: Point
  }
  deriving (Show, Eq, Ord)
makeLenses ''Waypoint

waypoint :: Waypoint -> (Move, Int) -> Waypoint
waypoint current m = case m of
  (N, n  ) -> current & wp +~ V2 0 n
  (E, n  ) -> current & wp +~ V2 n 0
  (S, n  ) -> current & wp +~ V2 0 (-n)
  (W, n  ) -> current & wp +~ V2 (-n) 0
  (_, 180) -> current & wp %~ negate
  (L, 90 ) -> current & wp %~ perp
  (R, 90 ) -> current & wp %~ perp . perp . perp
  (L, 270) -> waypoint current (R, 90)
  (R, 270) -> waypoint current (L, 90)
  (F, n  ) -> current & ship +~ V2 n n * view wp current

part2 :: [(Move, Int)] -> Int
part2 = mannDist (V2 0 0) . view ship . foldl waypoint (Waypoint (V2 10 1) (V2 0 0))

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day12.in"
    print $ part1 input
    print $ part2 input

-- 1424
-- 63447
