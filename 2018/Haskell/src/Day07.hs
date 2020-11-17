module Day07 where

import Data.Char (
  isUpper,
  ord,
 )
import Data.List (
  sort,
 )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph = Map Char (Set Char)

parseInput :: String -> Graph
parseInput = Map.fromListWith Set.union . concatMap (toTuple . tail . filter isUpper) . lines
 where
  toTuple [x, y] = [(x, Set.empty), (y, Set.singleton x)]
  toTuple _ = error "Not two chars"

run :: Int -> Int -> Graph -> [(Char, Int)]
run fixedCost workers = loop <*> map (cost 0) . take workers . Map.keys . Map.filter Set.null
 where
  cost costSoFar ch = (costSoFar + fixedCost + (ord ch - ord 'A') + 1, ch)
  loop g ((c, k) : ready)
    | Map.null g' && null ready' = [(k, c)]
    | otherwise = (k, c) : loop g' ready'
   where
    g' = Map.map (Set.delete k) $ Map.delete k g
    next = map (cost c)
         . Map.keys
         $ Map.filterWithKey (\k v -> k `notElem` map snd ready && Set.null v) g'
    ready' = sort $ ready ++ take (workers - length ready) next

part1 :: Graph -> String
part1 = map fst . run 0 1

part2 :: Graph -> Int
part2 = snd . last . run 60 5

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day07.in"
  print $ part1 input
  print $ part2 input

-- BGJCNLQUYIFMOEZTADKSPVXRHW
-- 1017
