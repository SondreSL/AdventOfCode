-- Works, but the second part is very slow.
module Main where

import qualified Data.Set as Set
import           Data.Set   ( Set )

parseFile :: String -> [Int]
parseFile = map read . lines . filter (/= '+')

solveA :: String -> Int
solveA = sum . parseFile

solveB :: String -> Int
solveB = findFirst Set.empty . scanl (+) 0 . cycle . parseFile
 where
  findFirst seen (new : xs) = if Set.member new seen
                                 then new
                                 else findFirst (Set.insert new seen) xs

main :: IO ()
main = do
    contents <- readFile "data/day01.in"
    print $ solveA contents
    print $ solveB contents

