module Day02 where

import           Data.List.Extra                ( splitOn
                                                , sort
                                                )

type Gift = [Int]

paper :: Gift -> Int
paper [l, w, h] = sum xs + (minimum xs `div` 2)
  where xs = [2 * l * w, 2 * w * h, 2 * h * l]

ribbon :: Gift -> Int
ribbon g = dimension + bow
  where
    dimension = sum (take 2 $ sort g) * 2
    bow = product g

parse :: String -> Gift
parse = map read . splitOn "x"

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "../data/02.in"
  print . sum $ map paper input
  print . sum $ map ribbon input
