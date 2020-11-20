{-# LANGUAGE BangPatterns #-}
module Day14 where

import qualified Data.Digits as D
import qualified Data.Sequence as S
import Data.List.Utils

input :: Int
input = 409551

generate :: [Int]
generate = 3 : 7 : go 0 1 (S.fromList [3,7])
  where
    go !a !b !s = new ++ go a' b' s'
      where x = S.index s a
            y = S.index s b
            new = digits $ x + y
            s' = s <> S.fromList new
            a' = (a + 1 + x) `mod` length s'
            b' = (b + 1 + y) `mod` length s'

-- Data.Digits.digits fails for 0 -> [] rather than [0]
digits :: Int -> [Int]
digits n = case n `divMod` 10 of
             (0, n) -> [n]
             (n, m) -> [n, m]

part1 :: Int -> Int
part1 n = D.unDigits 10 . take 10 $ drop n generate

part2 :: Int -> Maybe Int
part2 n = subIndex (D.digits 10 n) generate

main :: IO ()
main = do
  print $ part1 input
  print $ part2 input

-- 1631191756
-- 20219475
