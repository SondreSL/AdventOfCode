{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.List.Extra (splitOn
                       , intercalate
                       , nub
                       )
import Data.Functor.Foldable

data TreeF a r = NodeF a [r]
  deriving (Show, Eq, Ord, Functor)

type Tree a = Fix (TreeF a)

parse :: String -> (String, String)
parse str = let [a, b] = splitOn " => " str
         in (a, b)

new :: String -> (String, String) -> [String]
new str (a, b) = go x xs
  where (x:xs) = splitOn a str
        go :: String -> [String] -> [String]
        go xs [] = []
        go pre (x:xs) = (pre ++ b ++ intercalate a (x:xs)) : go (pre ++ a ++ x) xs

replaceMolecule :: [(String, String)] -> String -> [String]
replaceMolecule trans input = nub $ concatMap (new input) trans

replaceMolecule2 :: [(String, String)] -> String -> [String]
replaceMolecule2 trans input = take 10 $ concatMap (new input) trans

part1 :: String -> [(String, String)] -> Int
part1 str trans = length $ replaceMolecule trans str

part2 :: [(String, String)] -> String -> Int
part2 table str = hylo count build (str, 0)
  where build :: (String, Int) -> TreeF (String, Int) (String, Int)
        build (s, n) = NodeF n [ (x, n + 1) | y <- table, x <- new s y ]
        count :: TreeF (String, Int) Int -> Int
        count = undefined


swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)

main :: IO ()
main = do
  (input : intrans) <- lines <$> readFile "../data/19.in"
  let trans = map parse intrans
  mapM_ (print . swap) trans
  print $ part1 input trans
  print $ input
