module Main where

import           Control.Arrow
import           Text.Regex.TDFA
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.List.Extra                ( splitOn
                                                , transpose
                                                , permutations
                                                )

parse :: String -> [[Int]]
parse = map go . lines
 where
  go :: String -> [Int]
  go s = map read $ getAllTextMatches (s =~ "-*[0-9]+")

score :: [Int] -> [[Int]] -> (Int, Int)
score v = (product . init &&& last) . map (max 0 . sum . zipWith (*) v) . transpose

genNums :: Int -> [[Int]]
genNums n = concatMap permutations $
  [ [a, b, c, d]
  | a <- [0 .. n]
  , b <- [a .. n]
  , c <- [b .. n]
  , d <- [c .. n]
  , a + b + c + d == n
  ]

main :: IO ()
main = do
  input <- parse <$> readFile "../data/15.in"
  let nums = map (`score` input) $ genNums 100
  print . fst $ maximum nums
  print . fst . maximum . filter ((== 500) . snd) $ nums
