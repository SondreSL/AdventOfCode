module Day06 where

import Data.List.Extra ( splitOn )
import qualified Data.Set as Set
import           Data.Set   ( Set )

parseInput :: String -> [[Set Char]]
parseInput = map (map Set.fromList . lines) . splitOn "\n\n"

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  print . sum . map (length . Set.unions) $ input
  print . sum . map (length . foldl Set.intersection (Set.fromList ['a'..'z'])) $ input

-- 6799
-- 3354
