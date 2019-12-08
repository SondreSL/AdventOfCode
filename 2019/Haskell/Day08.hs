import Data.Ord
import Data.List
import Data.List.Split (chunksOf)

color :: String -> String -> String
color "" "" = ""
color ('2':xs) (y:ys) = y : color xs ys
color (x:xs) (_:ys) = x : color xs ys

solveA :: Int -> Int -> String -> Int
solveA w h str = (occ '1') * (occ '2')
  where fewestZeros = minimumBy (comparing (length . filter (== '0'))) $ chunksOf (w * h) str
        occ n = length $ filter (== n) fewestZeros

solveB :: Int -> Int -> String -> String
solveB w h str = 
  let res = foldr1 color $ chunksOf (w * h) str
      f '1' = 'O'
      f '0' = ' '
      f a = a
   in map f $ intercalate "\n" $ chunksOf w res

main = do
  contents <- head . lines <$> readFile "data/input-2019-8.txt"
  print $ solveA 25 6 contents
  putStrLn $ solveB 25 6 contents

-- 1620
-- BCYEF
