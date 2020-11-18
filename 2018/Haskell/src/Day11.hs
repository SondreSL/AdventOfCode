{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Control.DeepSeq (force)
import Data.Array
import Data.Foldable (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as S
import Linear (V2 (..))

-- https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-11

type Point = V2 Int

input :: Int
input = 3628

powerLevel :: Int -> Point -> Int
powerLevel i (V2 x y) = dm ((rackID * y + i) * rackID) - 5
 where
  rackID = x + 10
  dm = (`mod` 10) . (`div` 100)

mkMap :: Int -> Map Point Int
mkMap i =
  M.fromSet (powerLevel i)
    . S.fromList
    $ range (V2 1 1, V2 300 300)

summedAreaTable :: Map Point Int -> Map Point Int
summedAreaTable mp = force sat
 where
  sat = M.mapWithKey go mp
  go p0 v =
    (+ v) . sum . catMaybes $
      [ negate <$> M.lookup (p0 - V2 1 1) sat
      , M.lookup (p0 - V2 1 0) sat
      , M.lookup (p0 - V2 0 1) sat
      ]

fromSAT :: Map Point Int -> Point -> Int -> Int
fromSAT sat (subtract (V2 1 1) -> p) n =
  sum . catMaybes $
    [ M.lookup p sat
    , M.lookup (p + V2 n n) sat
    , negate <$> M.lookup (p + V2 0 n) sat
    , negate <$> M.lookup (p + V2 n 0) sat
    ]

findMaxAny :: Map Point Int -> [Int] -> (Point, Int)
findMaxAny sat squareSizes =
  fst . maximumBy (comparing snd) $
    [ ((p, n), fromSAT sat p n)
    | n <- squareSizes
    , p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
    ]

main :: IO ()
main = do
  let sm = summedAreaTable $ mkMap input
  print $ findMaxAny sm [3]
  print $ findMaxAny sm [1 .. 300]
