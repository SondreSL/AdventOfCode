module Main where

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Text.Regex.TDFA
import           Data.List.Extra                ( splitOn
                                                , maximumOn
                                                , group
                                                , sort
                                                , transpose
                                                )

data Reindeer = R { name :: String
                  , speed :: Int
                  , time :: Int
                  , rest :: Int
                  } deriving (Show, Eq, Ord)

parse :: String -> [Reindeer]
parse = map go . lines
 where
  go l =
    let name                = head (splitOn " " l)
        [speed, time, rest] = map read $ getAllTextMatches (l =~ "[0-9]+")
    in  R name speed time rest

distance :: Int -> Reindeer -> [Int]
distance time (R _ s t r) = take time $ move s
 where
  move score = [score, score + s .. score + (s * (t - 1))] ++ wait (score + (s * (t - 1)))
  wait score = take r [score, score ..] ++ move (score + s)

prSecond :: [[Int]] -> Int
prSecond xs = maximum . map length . group . sort  . concatMap go $ transpose xs
 where
  go l = map fst . filter ((== maxVal) . snd) $ zip [0 ..] l
    where maxVal = maximum l

main :: IO ()
main = do
  input <- parse <$> readFile "../data/14.in"
  print $ maximum (map (last . distance 2503) input)
  print $ prSecond (map (distance 2503) input)
