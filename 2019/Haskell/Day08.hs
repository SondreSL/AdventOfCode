import Data.List
import Data.Ord
import Data.Function
import Debug.Trace

layer :: Int -> Int -> String -> [String]
layer _ _ [] = []
layer w h str = take (w * h) str : layer w h (drop (w * h) str)

color :: String -> String -> String
color "" "" = ""
color ('2':xs) (y:ys) = y : color xs ys
color (x:xs) (_:ys) = x : color xs ys

image :: Int -> String -> String
image w "" = "\n"
image w str = (take w str) ++ "\n" ++ image w (drop w str)

solveA :: Int -> Int -> String -> Int
solveA w h str = ones * twos
  where new = layer w h str
        fewestZeros = minimumBy (comparing (length . filter (== '0'))) new
        ones = length $ filter (== '1') fewestZeros
        twos = length $ filter (== '2') fewestZeros

solveB :: Int -> Int -> String -> String
solveB w h str = 
  let ls = layer w h str
      res = foldr1 color ls
      f '1' = 'O'
      f '0' = ' '
      f a = a
   in map f $ image w res

main = do
  contents <- head . lines <$> readFile "data/input-2019-8.txt"
  print $ solveA 25 6 contents
  putStrLn $ solveB 25 6 contents

-- 1620
-- BCYEF
