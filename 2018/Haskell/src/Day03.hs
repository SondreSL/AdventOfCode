{-# LANGUAGE RecordWildCards #-}
module Day03 where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict   ( Map )
import qualified Data.Set as Set
import Data.List.Extra ( splitOn )

type Coords = Map (Int, Int) [Int]

data Claim = Claim
  { _id :: Int
  , _left :: Int
  , _top :: Int
  , _width :: Int
  , _height :: Int
  }
  deriving (Show, Eq, Ord)

parse :: String -> [Claim]
parse = map toClaim . lines
  where
    toClaim str =
      let [ids, _, dists, sizes] = splitOn " " $ tail str
          id = read ids
          left = read $ takeWhile (/= ',') dists
          top = read . tail . init $ dropWhile (/= ',') dists
          width = read $ takeWhile (/= 'x') sizes
          height = read . tail $ dropWhile (/= 'x') sizes
       in Claim id left top width height

area :: Claim -> Coords
area Claim{..} =
  let coords = [ (x, y) | x <- [_left .. (_left + _width - 1)], y <- [_top .. (_top + _height - 1)] ]
   in foldr (`Map.insert` [_id]) Map.empty coords

areas :: [Claim] -> Coords
areas = Map.unionsWith (++) . map area

countOverlap :: Coords -> Int
countOverlap = length . Map.filter ((>1) . length)

findUnique :: [Claim] -> Coords -> Int
findUnique claims = minimum . Set.difference (ids claims) . contested
  where
    ids = Set.fromList . map _id
    contested = Set.unions . map Set.fromList . filter ((>1) . length) . Map.elems

main :: IO ()
main = do
  input <- parse <$> readFile "../data/day03.in"
  let as = areas input
  print $ countOverlap as
  print $ findUnique input as

-- 98005
-- 331
