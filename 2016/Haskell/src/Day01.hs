module Day01 where

import Lib
    ( Point, firstReapeat, Dir(North, East, West), dirPoint )
import Linear


data Move = Turn Dir
          | Walk
  deriving (Show)

data Sleigh = (:@) Dir Point
  deriving (Eq, Show)

instance Ord Sleigh where
  (_ :@ p) `compare` (_ :@ p') = p `compare` p'

parseInput :: String -> [Move]
parseInput = concatMap parse . words . filter (/= ',')
 where
  parse ('R' : xs) = let i = read xs in Turn East : replicate (i - 1) Walk
  parse ('L' : xs) = let i = read xs in Turn West : replicate (i - 1) Walk

move :: Sleigh -> Move -> Sleigh
move (d :@ p) Walk = d :@ (p + dirPoint d)
move (d :@ p) (Turn dir) =
  let d' = d <> dir
   in d' :@ (p + dirPoint d')

part1 :: Foldable t => t Move -> Int
part1 input =
  let _ :@ p = foldl move (North :@ V2 0 0) input
   in sum p

part2 :: [Move] -> Int
part2 input = case firstReapeat $ scanl move (North :@ V2 0 0) input of
                Nothing -> error "No repeating point"
                Just (_ :@ p) -> sum p

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day01.in"
  print $ part1 input
  print $ part2 input
