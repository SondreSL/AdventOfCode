import Data.List (group)
import Data.Char

genNums :: Int -> [Int]
genNums digs = go 1 digs
  where go start digs 
          | digs == 1 = [start .. 9] 
          | otherwise = concatMap (\x -> map (+ (x * (10^(digs - 1)))) (go x (digs - 1))) [start .. 9] 

validNum :: (Int -> Bool) -> Int -> Bool
validNum eq = any eq . map length . group . show

solveA :: Int -> Int -> Int
solveA start stop = length . filter (validNum (>= 2)) . filter (<= stop) . filter (>= start) $ genNums 6

solveB :: Int -> Int -> Int
solveB start stop = length . filter (validNum (== 2)) . filter (<= stop) . filter (>= start) $ genNums 6

main = do
  print $ solveA 172930 683082
  print $ solveB 172930 683082

-- 1675
-- 1142
