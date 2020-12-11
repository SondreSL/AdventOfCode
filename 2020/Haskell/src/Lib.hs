{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Lens
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable ( Foldable(foldl', toList) )
import Data.List.Extra (splitOn, chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Linear ( V2(..) )
import qualified Data.Set as Set
import           Data.Set ( Set )
import Control.Monad (guard, foldM)

isPrime :: Integral a => a -> Bool
isPrime n = null $ do
              x <- 2 : [ 3, 5 .. round (sqrt $ fromIntegral n) ]
              guard $ n `mod` x == 0
              pure x

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

rotate :: Int -> [a] -> [a]
rotate = drop <> take

zipWithTail :: [a] -> [(a, a)]
zipWithTail = zip <*> tail

zipWithTail' :: [a] -> [(a, a)]
zipWithTail' = zip <*> rotate 1

firstRepeat :: (Foldable t, Ord a) => t a -> Maybe a
firstRepeat = firstRepeatOn id

firstRepeatOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
firstRepeatOn project = either Just (const Nothing) . foldM f Set.empty
 where
   f seen x = let var = project x
               in if Set.member var seen
                     then Left x
                     else Right $ Set.insert var seen

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : case f x of
                         Nothing -> []
                         Just v -> iterateMaybe f v

toMapIndexed :: [a] -> Map Int a
toMapIndexed = Map.fromList . zip [0..]

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

pointToTuple :: Point -> (Int, Int)
pointToTuple (V2 x y) = (x, y)

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

-- Some directional stuff, for moving in a certain facing
data Dir = North | South | East | West
  deriving (Ord, Eq, Show, Bounded, Enum)

-- North is like moving forward, so does not change direction
instance Semigroup Dir where
  (<>) North = id
  (<>) East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
  (<>) South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
  (<>) West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

instance Monoid Dir where
  mempty = North

dirPoint :: Dir -> Point
dirPoint North = V2 0 1
dirPoint South = V2 0 (-1)
dirPoint West = V2 (-1) 0
dirPoint East = V2 1 0
