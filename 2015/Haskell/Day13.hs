{-# LANGUAGE TupleSections #-}
module Main where

import           Data.List.Extra                ( splitOn
                                                , permutations
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map
                                                , (!)
                                                )
import           Debug.Trace

type Table = Map String (Map String Int)

parse :: [String] -> Table
parse lines = Map.unionsWith Map.union (map (go . splitOn " ") lines)
 where
  go [name, _, "gain", val, _, _, _, _, _, _, to] = Map.singleton name (Map.singleton (init to) (read val))
  go [name, _, "lose", val, _, _, _, _, _, _, to] = Map.singleton name (Map.singleton (init to) (negate (read val)))

seating :: Table -> [Int]
seating t = map (sum . map score) xs
 where
  xs = map (\x -> zip <*> tail $ last x : x) . permutations $ Map.keys t
  score (x, y) = ((t ! x) ! y) + ((t ! y) ! x)

addPerson :: Table -> Table
addPerson table = Map.union me add
  where
    me = Map.singleton "Me" . Map.fromList . map (,0) $ Map.keys table
    add = Map.unionWith Map.union table . Map.fromList . map (,Map.singleton "Me" 0) $ Map.keys table

main :: IO ()
main = do
  raw <- lines <$> readFile "../data/13.in"
  let input = parse raw
  print . maximum $ seating input
  print . maximum . seating $ addPerson input
