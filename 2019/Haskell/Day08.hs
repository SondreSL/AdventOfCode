import Data.Ord
import Data.List
import Data.List.Split (chunksOf)

color :: String -> String -> String
color "" "" = ""
color ('2':xs) (y:ys) = y : color xs ys
color (x:xs) (_:ys) = x : color xs ys

solveA :: Int -> Int -> String -> Int
solveA w h str = (occ '1' xs) * (occ '2' xs)
  where occ n ls = length $ filter (== n) ls
        xs = minimumBy (comparing (occ '0')) $ chunksOf (w * h) str

solveB :: Int -> Int -> String -> String
solveB w h str = 
  let res = foldr1 color $ chunksOf (w * h) str
      f '1' = '#'
      f '0' = ' '
   in intercalate "\n" . chunksOf w  $ map f res

main = do
  str <- head . lines <$> readFile "data/input-2019-8.txt"
  print $ solveA 25 6 str
  putStrLn $ solveB 25 6 str

-- 1620
-- BCYEF
