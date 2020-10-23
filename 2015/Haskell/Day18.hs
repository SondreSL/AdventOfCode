module Main where

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import Data.Maybe (mapMaybe)
import Data.List

type Point = (Int, Int)
type Grid = Map Point Bool

coords :: [Point]
coords = [ (x, y) | y <- [1 .. 100], x <- [1 .. 100] ]

parse :: String -> Grid
parse = Map.fromList . zip coords . map (== '#') . concat . lines

nbs :: Point -> [Point]
nbs (x, y) =
  [ (a, b)
  | a <- [x - 1 .. x + 1]
  , a > 0
  , a < 101
  , b <- [y - 1 .. y + 1]
  , b > 0
  , b < 101
  , (a, b) /= (x, y)
  ]

next :: (Grid -> Grid) -> Grid -> Grid
next f m = f $ Map.mapWithKey go m
  where
    go k v = let living = length . filter id . mapMaybe (`Map.lookup` m) $ nbs k
              in if v then living == 2 || living == 3 else living == 3

corners :: Grid -> Grid
corners m = foldl (\acc n -> Map.insert n True acc) m [(100,1), (100,100), (1,1), (1,100)]

run :: Grid -> [Grid]
run = iterate (next id)

run2 :: Grid -> [Grid]
run2 = iterate (next corners) . corners

main :: IO ()
main = do
  input <- parse <$> readFile "../data/18.in"
  print . Map.size . Map.filter id . (!! 100) . run $ input
  print . Map.size . Map.filter id . (!! 100) . run2 $ input
