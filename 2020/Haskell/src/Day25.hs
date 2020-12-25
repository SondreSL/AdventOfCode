module Day25 where

import Data.Finite (Finite, finite, getFinite)
import Data.List.Extra (elemIndex)
import Lib (tuple)

part1 :: Finite 20201227 -> Finite 20201227 -> Maybe Integer
part1 card door = getFinite . (card ^) <$> doorIx
  where
    doorIx = elemIndex door $ iterate (*7) 1

main :: IO ()
main = do
    (card, door) <- tuple . map read . lines <$> readFile "../data/day25.in"
    print $ part1 (finite card) (finite door)

-- 19924389
