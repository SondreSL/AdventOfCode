module Main where

import qualified Data.Map as Map
import           Data.Map   ( Map )
import           Data.List ( permutations
                           , nub
                           )
import Data.Maybe (mapMaybe)

parse :: [String] -> Map (String, String) Int
parse  = Map.fromList . concatMap (go . words)
  where
    go [from, "to", to, "=", dist] = [((from, to), read dist), ((to, from), read dist)]

paths :: Map (String, String) Int -> [Int]
paths m = let perms = map (zip <*> tail) . permutations . nub . map fst $ Map.keys m
           in map (sum . map (m Map.!)) perms

main :: IO ()
main = do
  input <- parse . lines <$> readFile "../data/09.in"
  print . minimum $ paths input
  print . maximum $ paths input
