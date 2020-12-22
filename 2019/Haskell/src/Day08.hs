module Day08 where

import Data.Ord
import Data.List
import Data.List.Extra (chunksOf)

color :: String -> String -> String
color "" "" = ""
color ('2':xs) (y:ys) = y : color xs ys
color (x:xs) (_:ys) = x : color xs ys

solveA :: Int -> Int -> String -> Int
solveA w h str = (occ '1' xs) * (occ '2' xs)
  where occ n ls = length $ filter (== n) ls
        xs = minimumBy (comparing (occ '0')) $ chunksOf (w * h) str

solveB :: Int -> Int -> String -> IO ()
solveB w h str =
  let res = foldr1 color $ chunksOf (w * h) str
      f '1' = '🔴'
      f '0' = '🖤'
   in mapM_ putStrLn $ chunksOf w  $ map f res

main = do
  str <- init <$> readFile "data/input-2019-8.txt"
  print $ solveA 25 6 str
  solveB 25 6 str

-- 1620
-- BCYEF
