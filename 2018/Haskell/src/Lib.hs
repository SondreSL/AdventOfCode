{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( freqs
    , display
    , findBounds
    , parseAsciiMap
    , asciiGrid
    ) where

import qualified Data.Map as Map
import           Data.Map   ( Map )
import Data.Foldable
import Data.List.Extra (chunksOf)
import Data.Bool (bool)
import Data.Array (accumArray, elems)
import Data.Semigroup (Max(getMax, Max), Min(getMin, Min))
import Control.Lens
import Linear

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . map (,1) . toList


-- | Useful functions for displaying some collection of points in 2D
findBounds
  :: [(Int, Int)]           -- ^ The points in 2D space
  -> (Int, Int, Int, Int)   -- ^ V2 (V2 minX minY) (V2 maxX maxY)
findBounds cs = (getMin minX, getMin minY, getMax maxX, getMax maxY)
 where
  (minX, minY, maxX, maxY) = foldMap f cs
  f (x, y) = (Min x, Min y, Max x, Max y)

type Point = V2 Int

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold Point String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

display
  :: (Foldable t)
  => (a -> ((Int, Int), Bool))
  -> (Int, Int, Int, Int)    -- ^ the bounds of the points
  -> t a        -- ^ foldable of coordinates
  -> String
display project (minX, minY, maxX, maxY) =
  unlines
    . chunksOf (maxX - minX + 1)
    . map (bool '░' '▓')
    . elems
    . accumArray (||) False ((minY, minX), (maxY, maxX))
    . map project
    . toList

