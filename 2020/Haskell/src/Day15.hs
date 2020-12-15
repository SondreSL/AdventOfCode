module Day15 where

import qualified Data.Vector.Mutable as MV
import Control.Monad.ST ( ST, runST )

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map
import Control.Monad (foldM)

run :: Int -> [Int] -> ST s Int
run target xs = do
  vec <- MV.new (target + 1)
  MV.set vec 0
  vec <- foldM initial vec (zip [1..] (init xs))
  runMut vec (last xs) 6
    where
      initial vec (i, x) = do
        MV.write vec x i
        pure vec
      runMut vec current turn = do
        prev <- MV.read vec current
        let next = if prev == 0
                     then 0
                     else turn - prev
        MV.write vec current turn
        if turn == target
           then pure current
           else runMut vec next (turn + 1)

play :: Int -> IntMap Int -> Int -> Int -> Int
play target seen turn current = go seen turn current
  where
    go seen !turn !current
      | turn == target = current
      | otherwise =
          go (Map.insert current turn seen)
             (succ turn)
             (case Map.lookup current seen of
                Nothing -> 0
                Just prev -> turn - prev
             )

initialize :: Int -> [Int] -> Int
initialize n xs = play n (Map.fromList (zip (init xs) [1 ..])) (length xs) (last xs)

main :: IO ()
main = do
    let input = [1, 20, 11, 6, 12, 0]
    -- print $ initialize 2020 input
    -- print $ initialize 30000000 input

    print $ runST (run 2020 input)
    print $ runST (run 30000000 input)

-- 1085
-- 10652
