module Main where

import Text.Regex.TDFA
import Data.List.Extra (splitOn)

constraints = [("children", 3),
               ("cats", 7),
               ("samoyeds", 2),
               ("pomeranians", 3),
               ("akitas", 0),
               ("vizslas", 0),
               ("goldfish", 5),
               ("trees", 3),
               ("cars", 2),
               ("perfumes", 1)]

type Sue = (Int, [(String, Int)])

parse :: String -> [Sue]
parse = map go . lines
  where
    go str = (read (str =~ "[0-9]+"), rest str)
    rest :: String -> [(String, Int)]
    rest str = let ms = getAllTextMatches (str =~ "[a-z]+: [0-9]+")
                in map ((\[a,b] -> (a, read b)) . splitOn ": ") ms

findSue :: Sue -> Bool
findSue (i, cs) = all go constraints
  where go (x, y) = comp x (== y)
        comp x f = maybe True f (lookup x cs)

findSue2 :: Sue -> Bool
findSue2 (i, cs) = all go constraints
  where go ("cats", y) = comp "cats" (> y)
        go ("trees", y) = comp "trees" (> y)
        go ("pomeranians", y) = comp "pomeranians" (< y)
        go ("goldfish", y) = comp "goldfish" (< y)
        go (x, y) = comp x (== y)
        comp x f = maybe True f (lookup x cs)

main = do
  input <- parse <$> readFile "../data/16.in"
  print $ filter findSue input
  print $ filter findSue2 input
