module Day17 where

import Lib ( count, parseAsciiMap )
import Data.Maybe ( mapMaybe )
import Linear ( V2(V2), V3(..), V4(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set   ( Set )

parseInput :: String -> Set (V2 Int)
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '#' = Just ()
    f _ = Nothing

step :: Ord a => (a -> [a]) -> Set a -> Set a
step genNearby input = Set.fromList . mapMaybe f . concatMap genNearby $ Set.toList input
  where
    f p = case (p `Set.member` input, Lib.count (`Set.member` input) (tail $ genNearby p)) of
            (True, 2) -> Just p
            (True, 3) -> Just p
            (False, 3) -> Just p
            _ -> Nothing

part1 :: Set (V3 Int) -> Int
part1 = Set.size . (!! 6) . iterate (step allNei)
  where
    allNei p = (p +) <$> (V3 <$> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1])

part2 :: Set (V4 Int) -> Int
part2 = Set.size . (!! 6) . iterate (step allNei4)
  where
    allNei4 p = (p +) <$> (V4 <$> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1])

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day17.in"
  let v2tov3 (V2 x y) = V3 x y 0
      v2tov4 (V2 x y) = V4 x y 0 0
  print $ part1 (Set.map v2tov3 input)
  print $ part2 (Set.map v2tov4 input)

-- 240
-- 1180
