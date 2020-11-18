{-# LANGUAGE BangPatterns #-}
module Day09 where

import Data.List.PointedList.Circular
import Data.Maybe (fromJust)
import Data.Foldable ( foldl' )
import qualified Data.IntMap as Map
import           Data.IntMap   ( IntMap )

step
  :: PointedList Int        -- ^ New marble should go at the end
  -> Int                    -- ^
  -> (Int, PointedList Int) -- ^ (Score for current round, list)
step xs n
  | n `mod` 23 == 0 = let xs' = moveN (-7) xs
                          m = _focus xs'
                          xs'' = fromJust $ deleteRight xs'
                       in (n + m, xs'')
  | otherwise = let xs' = moveN 1 xs
                 in (0, insertRight n xs')

type Scores = IntMap Int

run
  :: Int      -- ^ # players
  -> Int      -- ^ Max marble value
  -> Scores
run numPlayers maxVal = fst . foldl' go (Map.empty, singleton 0) $ zip players toInsert
  where
    go (!score, !lst) (!player, !val) = (Map.insertWith (+) player newScore score, newLst)
      where
        (newScore, newLst) = step lst val
    players = (`mod` numPlayers) <$> [0 .. ]
    toInsert = [1 .. maxVal]

part1 :: Int
part1 = maximum $ run 455 71223

part2 :: Int
part2 =  maximum $ run 455 (71223 * 100)

-- 455 players; last marble is worth 71223 points
main :: IO ()
main = do
  print part1
  print part2

-- 384288
-- 3189426841
