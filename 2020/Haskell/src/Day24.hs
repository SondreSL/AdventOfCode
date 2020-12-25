module Day24 where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lib
import Linear (V3 (..))
import Text.ParserCombinators.Parsec

type Hex = V3 Integer

data Dir
    = NE
    | E
    | SE
    | SW
    | W
    | NW
    deriving (Show, Eq, Ord, Enum, Bounded)

-- https://stackoverflow.com/questions/1838656/how-do-i-represent-a-hextile-hex-grid-in-memory
move :: Dir -> Hex -> Hex
move NE = (+ V3 1 0 (-1))
move E = (+ V3 1 (-1) 0)
move SE = (+ V3 0 (-1) 1)
move SW = (+ V3 (-1) 0 1)
move W = (+ V3 (-1) 1 0)
move NW = (+ V3 0 1 (-1))

flipHexes :: [[Dir]] -> Set Hex
flipHexes = foldl f Set.empty
  where
    locatePoint = foldl (flip move) (V3 0 0 0)
    f seen (locatePoint -> p) =
        if Set.member p seen
            then Set.delete p seen
            else Set.insert p seen

step :: Set Hex -> Set Hex
step input = stayBlack <> toBlack
  where
    cellCount = Map.unionsWith (+) . map (freqs . explode) $ Set.toList input
    explode p = map (`move` p) [NE ..]
    stayBlack =
        Map.keysSet
            . Map.filter (`elem` [1, 2])
            $ Map.restrictKeys cellCount input
    toBlack =
        Map.keysSet
            . Map.filter (== 2)
            $ Map.withoutKeys cellCount input

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day24.in"
    print . Set.size . flipHexes $ input
    print . Set.size . (!! 100) . iterate step . flipHexes $ input

-- Parsing
parseInput :: String -> [[Dir]]
parseInput = either (error . show) id . traverse (parse dirs "") . lines
  where
    dirs =
        many1
            ( choice
                [ NE <$ try (string "ne")
                , NW <$ try (string "nw")
                , SE <$ try (string "se")
                , SW <$ try (string "sw")
                , E <$ string "e"
                , W <$ string "w"
                ]
            )
