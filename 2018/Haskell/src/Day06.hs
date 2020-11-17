module Day06 where

import Lib ( freqs )

import qualified Data.Map as Map
import           Data.Map   ( Map )
import Text.ParserCombinators.Parsec
import Data.Semigroup
import Data.List.Extra ( minimumOn )
import Data.Maybe (mapMaybe)

type Pair = (Int, Int)
type Coords = [Pair]

data Bounds =
  Bounds
  { left :: Int
  , top :: Int
  , right :: Int
  , bottom :: Int
  }
  deriving (Show, Eq, Ord)

parseInput :: String -> Coords
parseInput = either (error "Bad parse") id . traverse (parse parseTup "Tuple") . lines

parseTup :: Parser (Int, Int)
parseTup = do
  x <- read <$> many digit
  string ", "
  y <- read <$> many digit
  pure (x, y)

bounds :: Coords -> Bounds
bounds cs = Bounds (getMin left) (getMin top) (getMax right) (getMax bottom)
  where
    (left, top, right, bottom) = foldMap (\(x, y) -> (Min x, Min y, Max x, Max y)) cs

manhatten :: (Int, Int) -> (Int, Int) -> Int
manhatten (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

closestPoint :: Pair -> Coords -> Maybe Pair
closestPoint (x,y) cs =
  let candidates = [ (p, manhatten (x,y) p) | p <- cs ]
      shortest = minimumOn snd candidates
      count = length $ filter ((== snd shortest) . snd) candidates
   in case count of
        1 -> Just $ fst shortest
        _ -> Nothing

findNearest :: Bounds -> Coords -> Map Pair (Maybe Pair)
findNearest (Bounds l t r b) cs =
  Map.fromList [ ((x, y), pair)
               | x <- [l .. r]
               , y <- [t .. b]
               , let pair = closestPoint (x,y) cs
               ]

removeInfinite :: Bounds -> Map Pair (Maybe Pair) -> Map Pair (Maybe Pair)
removeInfinite (Bounds l t r b) cands =
  let left = zip (repeat l) [t .. b]
      top = zip (repeat t) [l .. r]
      right = zip (repeat r) [t .. b]
      bottom = zip (repeat b) [l .. r]
      edge = mapMaybe (`Map.lookup` cands) $ left ++ right ++ top ++ bottom
   in Map.filter (not . (`elem` edge)) cands

part1 :: Map Pair (Maybe Pair) -> Int
part1 = maximum . freqs . Map.elems

allDists :: (Int, Int) -> [(Int, Int)] -> Int
allDists p cs = sum [ manhatten p c | c <- cs ]

findAllLocations :: Int -> Bounds -> Coords -> Int
findAllLocations maxDist (Bounds l t r b) cs =
  length [ (x,y)
         | x <- [l .. r]
         , y <- [t .. b]
         , allDists (x,y) cs < maxDist
         ]

part2 :: Bounds -> Coords -> Int
part2 = findAllLocations 10000

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  let bs = bounds input
      coords = removeInfinite bs . findNearest bs  $ input
  print $ part1 coords
  print $ part2 bs input

-- 4233
-- 45290
