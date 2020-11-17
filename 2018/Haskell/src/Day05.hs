module Day05 where

import Data.Char ( isLower, toLower )
import qualified Data.Group.Free as F
import Data.Algebra.Free ( FreeAlgebra(returnFree, foldMapFree) )
import Data.Group ( invert )

inject :: Char -> F.FreeGroupL Char
inject c
  | isLower c = returnFree c
  | otherwise = invert $ returnFree (toLower c)

part1 :: [Char] -> Int
part1 = length . F.toList . foldMap inject

clean :: Char -> (F.FreeGroupL Char -> F.FreeGroupL Char)
clean c = foldMapFree $ \d ->
            if d == c
              then mempty
              else returnFree d

part2 :: [Char] -> Int
part2 string = minimum [ length $ F.toList $ clean c poly
                       | c <- ['a' .. 'z']
                       ]
  where
    poly = foldMap inject string

main :: IO ()
main = do
  input <- init <$> readFile "../data/day05.in"
  print $ part1 input
  print $ part2 input

-- 9686
-- 5524
