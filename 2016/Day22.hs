module Main where

import           Data.Monoid
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Char
import           Text.Regex.TDFA

-- size  Used  Avail  Use%
data Node = N { x :: Int
              , y :: Int
              , size :: Int
              , used :: Int
              , avail :: Int
              , percent :: Int
              } deriving (Show, Eq, Ord)

parse :: String -> Node
parse str = N x y size used avail percent
 where
  [x, y, size, used, avail, percent] = map read $ getAllTextMatches (str =~ "[0-9]+") :: [Int]

part1 :: Set Node -> Int
part1 = Set.size . Set.filter f . (Set.cartesianProduct <*> id)
 where
  f :: (Node, Node) -> Bool
  f (a, b) = used a > 0 && (x a, y a) /= (x b, y b) && used a <= avail b

-- printSet :: Set Node -> IO ()
-- printSet set = 
--   let
--   maxX = maximum $ map x set
--   maxY = maximum $ map y set
--   printNode (N x y s u a p) 
--     | u == 0 = " _/" ++ show avail
--     | x == 0 && y == 
--   loop xx yy = x <- [0 .. x]
--                y <- [0 .. y]

cap :: Int -> String
cap c | c > 400   = "|"
      | otherwise = "."


printNode :: Node -> String
printNode (N x y s u a p) | u == 0           = " _ "
                          | x == 0 && y == 0 = "(" ++ cap s ++ ")"
                          | y == 31          = " " ++ cap s ++ "\n"
                          | otherwise        = " " ++ cap s ++ " "

main :: IO ()
main = do
  diskUsage <- Set.fromList . map parse . lines <$> readFile "input/22.in"
  print $ Set.size diskUsage
  print $ part1 diskUsage
  putStr $ concatMap printNode $ Set.toList diskUsage -- look at out

-- 937
-- 188
