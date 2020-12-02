module Day16 where

import           Data.List                      ( delete )
import           Control.Lens                   ( toListOf
                                                , itraversed
                                                , indices
                                                )
import           Debug.Trace
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , (><)
                                                , Seq((:<|), Empty)
                                                , (|>)
                                                , (<|)
                                                )

part1 :: Int -> Maybe Int
part1 = Seq.lookup 0 . until ((== 1) . Seq.length) (rotate . remove) . Seq.fromList . enumFromTo 1
 where
  remove = Seq.deleteAt 1
  rotate (x :<| rest) = rest |> x

-- Sequences are crazy
part2 :: Int -> Maybe Int
part2 = Seq.lookup 0 . until ((== 1) . Seq.length) (rotate . remove) . Seq.fromList . enumFromTo 1
 where
  rotate (x :<| rest) = rest |> x
  remove seq = Seq.deleteAt (Seq.length seq `div` 2) seq

puzzleInput :: Int
puzzleInput = 3012210

main :: IO ()
main = do
  print $ part1 puzzleInput -- 1830117
  print $ part2 puzzleInput -- 1417887

-- import Data.Sequence
-- import Prelude hiding (length, splitAt)
--
-- a1 = solve $ const 1
-- a2 = solve $ (`div` 2) . length
-- solve f = findRemaining . fromList . enumFromTo 1
--   where findRemaining = (`index` 0) . until ((==1) . length) (rotate . remove)
--         remove = deleteAt =<< f
--         rotate = uncurry (flip mappend) . splitAt 1
