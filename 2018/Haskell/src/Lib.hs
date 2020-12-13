{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Lens
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable
import Data.List.Extra (splitOn, chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Linear ( V2(..) )
import qualified Data.Set as Set
import           Data.Set ( Set )

firstReapeat :: Ord a => [a] -> a
firstReapeat = go Set.empty
  where
    go !seen (x:xs) = if Set.member x seen
                        then go (Set.insert x seen) xs
                        else x

skipLoop :: (Ord a) => (a -> (Int, a)) -> (Int -> Int -> a -> a) -> Int -> [a] -> a
skipLoop normalize shift n xs = (!! extra) . map (shift loopShift looped) . drop loopN $ xs
  where
    (loopN, loopSize, loopShift) = findLoop normalize xs
    (looped, extra) = (n - loopN) `divMod` loopSize

findLoop :: Ord a => (a -> (Int, a)) -> [a] -> (Int, Int, Int) -- first loop, size of loop, shift
findLoop normalize (x:xs) = go (Map.singleton x (0, 0)) 1 xs
  where
    go !seen !i (w:ws) = case Map.lookup w'Norm seen of
                       Nothing -> go (Map.insert w'Norm (mn, i) seen) (i + 1) ws
                       Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        (mn, w'Norm) = normalize w

-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch
    :: (Int -> Bool)
    -> Int                  -- ^ min
    -> Int                  -- ^ max
    -> Maybe Int
binaryMinSearch p = go
  where
    go !x !y
        | x == mid || y == mid = Just (x + 1)
        | p mid                = go x mid
        | otherwise            = go mid y
      where
        mid = ((y - x) `div` 2) + x

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . map (,1) . toList

-- | Useful functions for displaying some collection of points in 2D
findBounds ::
  -- | The points in 2D space
  [(Int, Int)] ->
  -- | V2 (V2 minX minY) (V2 maxX maxY)
  (Int, Int, Int, Int)
findBounds cs = (getMin minX, getMin minY, getMax maxX, getMax maxY)
 where
  (minX, minY, maxX, maxY) = foldMap f cs
  f (x, y) = (Min x, Min y, Max x, Max y)

type Point = V2 Int

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold Point String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

display ::
  (Foldable t) =>
  -- | Projection function
  (a -> ((Int, Int), Bool)) ->
  -- | the bounds of the points
  (Int, Int, Int, Int) ->
  -- | foldable of coordinates
  t a ->
  String
display project (minX, minY, maxX, maxY) =
  unlines
    . chunksOf (maxX - minX + 1)
    . map (bool '░' '▓')
    . elems
    . accumArray (||) False ((minY, minX), (maxY, maxX))
    . map project
    . toList

-- | All eight surrounding neighbours
neighbours :: Point -> [Point]
neighbours p0 =
  [ p
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , p <- [p0 + V2 x y]
  , p /= p0
  ]

-- | Neighbours left, right, above and below
neighbours4 :: Point -> [Point]
neighbours4 p = (p +) <$> [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Various simple parser
linedNums :: String -> [Int]
linedNums = map read . lines . filter (/= '+')

commaNums :: String -> [Int]
commaNums = map read . splitOn ","

-- Fold
unionFind :: Ord a => (a -> Set a -> Bool) -> [a] -> Set (Set a)
unionFind f = foldl' go Set.empty
  where
    go set x = let (same, diff) = Set.partition (f x) set
                in Set.insert (Set.unions same <> Set.singleton x) diff

