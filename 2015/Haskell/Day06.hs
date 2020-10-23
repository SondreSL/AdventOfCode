module Main where

import           Data.List.Extra                ( splitOn )
import qualified Data.Map.Lazy                 as Map
import           Data.Map.Lazy                  ( Map )
import           Data.Foldable

import Data.Array

type Point = (Int, Int)
type Grid = Map (Int, Int) Status
type DimGrid = Map (Int, Int) Int

data Status = Light
            | Dark
            deriving (Eq, Ord, Show)

data Cmd = On Point Point
         | Off Point Point
         | Toggle Point Point
         deriving Show

swap :: Status -> Status
swap Dark  = Light
swap Light = Dark

parseNum :: String -> Point
parseNum = go . splitOn ","
  where go [a, b] = (read a, read b)

parse :: String -> Cmd
parse = go . words
 where
  go ["toggle", from, _, to] = Toggle (parseNum from) (parseNum to)
  go [_, "off", from, _, to] = Off (parseNum from) (parseNum to)
  go [_, "on" , from, _, to] = On (parseNum from) (parseNum to)

exec :: Grid -> Cmd -> Grid
exec grid (On  from to) = foldl (\g co -> Map.insert co Light g) grid (coords from to)
exec grid (Off from to) = foldl (\g co -> Map.insert co Dark g) grid (coords from to)
exec grid (Toggle from to) =
  foldl (\g co -> Map.insertWith (const swap) co Light g) grid (coords from to)

dim :: DimGrid -> Cmd -> DimGrid
dim grid (On     from to) = foldl (\g co -> Map.insertWith increase co 1 g) grid (coords from to)
dim grid (Off    from to) = foldl (\g co -> Map.insertWith (flip (+)) co (-1) g) grid (coords from to)
dim grid (Toggle from to) = foldl (\g co -> Map.insertWith increase co 2 g) grid (coords from to)

increase :: Int -> Int -> Int
increase a b | b < 0     = a
             | otherwise = a + b

coords :: Point -> Point -> [Point]
coords (fromX, fromY) (toX, toY) = [ (x, y) | y <- [fromY .. toY], x <- [fromX .. toX] ]

size :: Int
size = 999

start :: a -> Array (Int, Int) a
start = array ((0, 0), (size, size)) . zip [(x, y) | x <- [0 .. size], y <- [0 .. size]] . repeat

tick f acc ins = acc // [(pt, f ins (acc ! pt)) | pt <- ins ]

part1 :: [Cmd] -> Int
part1 = length . filter (== 1) . elems . foldl' f (start 0)
  where
    f acc (On a b)     = acc // [(p, 1) | p <- coords a b]
    f acc (Off a b)    = acc // [(p, 0) | p <- coords a b]
    f acc (Toggle a b) = acc // [(p, if (acc ! p) == 0 then 1 else 0) | p <- coords a b]

part2 :: [Cmd] -> Int
part2 = sum . filter (> 0) . elems . foldl' f (start 0)
  where
    f acc (Off a b)    = acc // [(p, max 0 ((acc ! p) - 1)) | p <- coords a b]
    f acc (On a b)     = acc // [(p, (acc ! p) + 1) | p <- coords a b]
    f acc (Toggle a b) = acc // [(p, (acc ! p) + 2) | p <- coords a b]

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "../data/06.in"
  print $ part1 input
  print $ part2 input

-- Pretty slow
--
-- 377891
-- 14110788
