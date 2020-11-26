{-# LANGUAGE BangPatterns #-}
module Day18 where

import Data.Array (range)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Set as Set
import Lib (
  Point,
  neighbours,
  parseAsciiMap,
 )
import Linear
import Data.Bool (bool)

data Acre
  = Lumberyard
  | Trees
  deriving (Show, Eq, Ord)

type World = Map Point Acre

parseInput :: String -> Map Point Acre
parseInput = parseAsciiMap f
 where
  f '#' = Just Lumberyard
  f '|' = Just Trees
  f _ = Nothing

gatherNeighbours :: World -> Point -> [Acre]
gatherNeighbours w = mapMaybe (`Map.lookup` w) . neighbours

step :: World -> Point -> Maybe Acre
step w p = let ns = gatherNeighbours w p
               trees = count (== Trees) ns
               ls = count (== Lumberyard) ns
               count f = length . filter f
            in case Map.lookup p w of
                 Nothing -> bool Nothing (Just Trees) $ trees > 2
                 Just Trees -> bool (Just Trees) (Just Lumberyard) $ ls > 2
                 Just Lumberyard -> bool Nothing (Just Lumberyard) $ ls > 0 && trees > 0

-- Bounds are known to be (50, 50)
stepWorld :: World -> World
stepWorld w =
  Map.map fromJust
    . Map.filter isJust
    . Map.fromSet (step w)
    . Set.fromList
    $ range (V2 0 0, V2 49 49)

score :: World -> Int
score = uncurry (*) . count . Map.elems
 where
  count xs =
    ( length $ filter (== Lumberyard) xs
    , length $ filter (== Trees) xs
    )

findLoop :: World -> (Int, Int) -- first step of loop, size of loop
findLoop w0 = go (Map.singleton w0 0) 1 w0
  where
    go !seen !i !w = case Map.lookup w' seen of
                       Nothing -> go (Map.insert w' i seen) (i + 1) w'
                       Just seenI -> (seenI, i - seenI)
      where
        w' = stepWorld w

part1 :: World -> Int
part1 = score . (!! 10) . iterate stepWorld

part2 :: Int -> World -> Int
part2 n w = score . (!! (beforeLoop + extra)) . iterate stepWorld $ w
  where
    (beforeLoop, loopSize) = findLoop w
    (_, extra) = (n - beforeLoop) `divMod` loopSize

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  print $ part1 input
  print $ part2 1000000000 input

-- 638400
-- 195952
