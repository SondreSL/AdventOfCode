module Day17 where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (count, parseAsciiMap)
import Linear (V2 (..), V3 (..), V4 (..))

parseInput :: String -> Set (V2 Int)
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '#' = Just ()
    f _ = Nothing

step :: Ord a => (a -> [a]) -> Set a -> Set a
step genNearby input = Set.fromList . mapMaybe f . Set.toList $ candidates
  where
    candidates = Set.fromList (concatMap genNearby input) `Set.union` input
    f p = case (p `Set.member` input, Lib.count (`Set.member` input) (genNearby p)) of
        (True, 2) -> Just p
        (True, 3) -> Just p
        (False, 3) -> Just p
        _ -> Nothing

-- Will generate neighbours for V2, V3, V4 +++
neighs ::
    (Traversable t, Applicative t, Num a, Eq (t a)) =>
    t a ->
    [t a]
neighs p = do
    n <- tail $ sequenceA (pure [0, 1, -1])
    pure $ (+) <$> p <*> n

part1 :: Set (V3 Int) -> Int
part1 = Set.size . (!! 6) . iterate (step neighs)

part2 :: Set (V4 Int) -> Int
part2 = Set.size . (!! 6) . iterate (step neighs)

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day17.in"
    let v2tov3 (V2 x y) = V3 x y 0
        v2tov4 (V2 x y) = V4 x y 0 0
    print $ part1 (Set.map v2tov3 input)
    print $ part2 (Set.map v2tov4 input)

-- 240
-- 1180
