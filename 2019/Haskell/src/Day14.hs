module Day14 where

import Data.Maybe
import Data.Bifunctor
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M

import Debug.Trace

type Name = String
type Minimum = Int
type Excess = M.Map Name Int
type Produced = Int
type Data = M.Map Name Chemical
data Chemical = Chemical Name Minimum [(Name, Int)]
  deriving Show

parseLine :: String -> (Name, Chemical)
parseLine line =
  let [instr, chem] = splitOn " => " line
      [chemNum, chemName] = splitOn " " chem
      parts = map toTup . map (splitOn " ") $ splitOn ", " instr
      toTup [a,b] = (b, read a)
   in (chemName, Chemical chemName (read chemNum) parts)

parser :: String -> Data
parser input = M.fromList . map parseLine . lines $ input

produce :: Data -> Excess -> (Name, Int) -> Excess
produce input exc ("ORE", newAmount) = M.insertWith (+) "ORE" newAmount exc
produce input exc (name, newAmount) =
  let (Just (Chemical _ minAmount edges)) = M.lookup name input
      currExc = M.findWithDefault 0 name exc
      diff = currExc - newAmount
      mult = ceiling $ (((fromIntegral newAmount - fromIntegral currExc)) / fromIntegral minAmount)
   in if diff >= 0
         then M.insert name diff exc
         else foldl (produce input) (M.insertWith (+) name ((mult * minAmount) - newAmount) exc) (map (second (* mult)) edges)

edges :: Chemical -> [(Name, Int)]
edges (Chemical _ _ e) = e

solveA :: Data -> Int
solveA input = M.findWithDefault 0 "ORE" . produce input M.empty $ ("FUEL", 1)

solveB :: Data -> Int -> Int -> Int
solveB input start target =
  let exc = produce input M.empty $ ("FUEL", start)
      new = M.findWithDefault 0 "ORE" exc
      inner exc inp = produce input exc inp
   in M.findWithDefault 0 "ORE" . last . takeWhile ((< target) . M.findWithDefault 0 "ORE") . scanl inner exc $ repeat ("FUEL", 1)

-- solveB :: Data -> Int -> Int -> Int
-- solveB input n target =
--   let x1 = 1000
--       x2 = 10000
--       y1 = 466335870
--       y2 = 4662685411
--       slope = (y2 - y1) `div` (x2 - x1)
--       b = y1 - slope * x1
--    in (target - b) `div` slope

main = do
  input <- parser <$> readFile "data/input-2019-14.txt"
  target <- pure 1000000000000

  print $ M.findWithDefault 0 "ORE" $ produce input M.empty ("FUEL", 2144702)

  print $ solveB input 2144000 target

-- 857266
-- 999999752281
