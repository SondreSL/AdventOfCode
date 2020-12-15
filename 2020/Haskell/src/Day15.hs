module Day15 where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (forM_)

run :: Int -> [Int] -> Int
run target xs = runST $ do
    vec <- MV.replicate (target + 1) 0

    forM_ (zip [1 ..] (init xs)) $ \(i, x) -> do
        MV.write vec x i

    let runMut current turn
          | turn == target = pure current
          | otherwise = do
              next <- MV.read vec current >>= \case
                        0    -> pure 0
                        prev -> pure $ turn - prev
              MV.write vec current turn
              if turn == target
                  then pure current
                  else runMut next (turn + 1)

    runMut (last xs) (length xs)

main :: IO ()
main = do
    let input = [1, 20, 11, 6, 12, 0]
    print $ run 2020 input
    print $ run 30000000 input

-- 1085
-- 10652
