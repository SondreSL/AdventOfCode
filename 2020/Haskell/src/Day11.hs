module Day11 where

import Lib ( count, Point, parseAsciiMap, neighbours, fixedPoint )
import Data.Maybe ( mapMaybe )
import qualified Data.Map as Map
import           Data.Map   ( Map )
import Linear ( V2(..) )
import Data.Foldable (find)

parseInput :: String -> Map Point Bool
parseInput = parseAsciiMap f
  where
    f 'L' = Just True
    f _ = Nothing

step :: (Point -> [Point]) -> Int -> Map Point Bool -> Map Point Bool
step seats cmp input = Map.mapWithKey check input
  where check k v =
          let nbs = count id $ mapMaybe (`Map.lookup` input) $ seats k
           in case (v, nbs) of
                (False, 0) -> True
                (True, n) | n >= cmp -> False
                (n, _) -> n

-- Generate the seats seen by each seat when looking in all eight directions.
diagonalSeats :: Map Point Bool -> Map Point [Point]
diagonalSeats input = Map.mapWithKey (\k _ -> mapMaybe (seats k) angles) input
  where
    seats seat f = find (`Map.member` input) . takeWhile inBounds . tail $ iterate f seat
    angles = fmap (+) . tail $ V2 <$> [0, 1, -1] <*> [0,1,-1]
    inBounds (V2 x y) = x < 96 && y < 90 && x >= 0 && y >= 0

run :: (Map Point Bool -> Map Point Bool) -> Map Point Bool -> Maybe Int
run f = fmap (count id) . fixedPoint f

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day11.in"
  print $ run (step neighbours 4) input
  let p2seats = diagonalSeats input
  print $ run (step (p2seats Map.!) 5) input

-- 2243
-- 2027
