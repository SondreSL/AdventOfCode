-- https://adventofcode.com/2015/day/17
{-# LANGUAGE DeriveFunctor #-}
module Main where

import           Data.Functor.Foldable
import           Data.List                      ( nub
                                                , delete
                                                , sort
                                                , group
                                                , sortOn
                                                )

parse :: String -> [Int]
parse = map read . lines

data TreeF a r = NodeF a [r]
  deriving (Functor, Show)

type Tree a = Fix (TreeF a)

solve :: (Int, [Int]) -> [[Int]]
solve = map nub . hylo cat an
 where
  an (n, []) = NodeF n []
  an (n, xs) = let m = minimum xs in NodeF n [(n - m, delete m xs), (n, delete m xs)]
  cat (NodeF 0 []) = [[0]]
  cat (NodeF n []) = []
  cat (NodeF n xs) = map ((n:) . nub) . concat $ xs



part1 :: (Int, [Int]) -> Int
part1 = length . solve

part2 :: (Int, [Int]) -> Int
part2 x = length . filter ((== m) . length) $ xs
  where xs = solve x
        m = minimum . map length $ xs

main :: IO ()
main = do
  input <- parse <$> readFile "../data/17.in"
  print $ part1 (150, input)
  print $ part2 (150, input)
