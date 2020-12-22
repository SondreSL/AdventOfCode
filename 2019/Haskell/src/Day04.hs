module Day04 where

import Data.List (group)
import Data.Char
import Data.SBV

genNums :: Int -> [Int]
genNums digs = go 1 digs
  where go start digs
          | digs == 1 = [start .. 9]
          | otherwise = concatMap (\x -> map (+ (x * (10^(digs - 1)))) (go x (digs - 1))) [start .. 9]

validNum :: (Int -> Bool) -> Int -> Bool
validNum eq = any eq . map length . group . show

solveA :: Int -> Int -> Int
solveA start stop = length . filter (<= stop) . filter (validNum (>= 2)) . filter (>= start) $ genNums 6

solveB :: Int -> Int -> Int
solveB start stop = length . filter (<= stop) . filter (validNum (== 2)) . filter (>= start) $ genNums 6

type Solution = [SWord32]

-- isValid :: Solution -> SBool
-- isValid n = sAll (\(a, b) -> a .<= b) ((flip zip =<< tail) n)
--         .&& sAny (\(a, b) -> a .== b) ((flip zip =<< tail) n)
--         .&& sAll range n
--         .&& numVal n .> 172930
--         .&& numVal n .< 683082
--             where range x = x .>= 1 .&& x .<= 9
--                   numVal x = foldl (\acc n -> acc * 10 + n) 0 x

-- solveAZ3 :: Int -> IO ()
-- solveAZ3 n = do putStrLn $ "Finding solutions for " ++ show n ++ " digits"
--                 res <- allSat $ isValid <$> mkExistVars n
--                 cnt <- displayModels disp res
--                 putStrLn $ "Part A: " ++ show cnt
--                   where disp i p = dispSolution (snd p)
--                         dispSolution :: [Word32] -> IO ()
--                         dispSolution model = putStr ""


main = do
  -- solveAZ3 6
  print $ solveA 172930 683082
  print $ solveB 172930 683082

-- 1675
-- 1142
