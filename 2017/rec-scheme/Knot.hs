module Knot where

import qualified Data.Map as M
import           Data.Functor

nums :: M.Map Int Int
nums = M.fromList [(i, i `mod` 5) | i <- [10..200]]

-- How to count the number of parents (keys) for each value?
-- i.e. how many keys point to 3, or 4?


countNums :: M.Map Int Int
countNums = nums <&> \par -> case M.lookup par countNums of
                               Nothing -> 0
                               Just n  -> n + 1


main :: IO ()
main = do
  -- print nums
  print countNums
