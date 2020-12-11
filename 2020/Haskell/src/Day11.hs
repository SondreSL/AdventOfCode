module Day11 where

import Lib
import Data.Maybe
import qualified Data.Map as Map
import           Data.Map   ( Map )
import Linear ( V2(..) )

data Seat = Free | Taken
  deriving (Show, Eq, Ord)

parseInput :: String -> Map Point Seat
parseInput = parseAsciiMap f
  where
    f 'L' = Just Free
    f _ = Nothing

seatsSeen :: Map Point Seat -> Point -> [Seat]
seatsSeen input seat = mapMaybe seats angles
  where
    seats f = listToMaybe . mapMaybe (`Map.lookup` input) . takeWhile inBounds . tail $ iterate f seat
    angles = fmap (+) . tail $ V2 <$> [0, 1, -1] <*> [0,1,-1]
    inBounds (V2 x y) = x < 96 && y < 90 && x >= 0 && y >= 0

step :: Map Point Seat -> Map Point Seat
step input = Map.mapWithKey check input
  where check k v =
          let nbs = count (== Taken) . mapMaybe (`Map.lookup` input) $ neighbours k
            in case (v, nbs) of
                (Free, 0) -> Taken
                (Taken, n) | n >= 4 -> Free
                (n, _) -> n

step' :: Map Point Seat -> Map Point Seat
step' input = Map.mapWithKey check input
  where check k v =
          let nbs = count (== Taken) $ seatsSeen input k
           in case (v, nbs) of
                (Free, 0) -> Taken
                (Taken, n) | n >= 5 -> Free
                (n, _) -> n

run :: (Map Point Seat -> Map Point Seat) -> Map Point Seat -> Maybe Int
run f = fmap (count (==Taken)) . firstRepeat . iterate f

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day11.in"
  print $ run step input
  print $ run step' input


-- 2243
-- 2027
