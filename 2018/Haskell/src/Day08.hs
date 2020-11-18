module Day08 where

import qualified Text.Parsec    as P
import Data.List.Extra
import Control.Monad.State
import Data.Either (fromRight)
import Control.Lens
import Data.Maybe (mapMaybe)

type Parser = P.Parsec [Int] ()

parseInput :: String -> [Int]
parseInput = map read . splitOn " " . init

sumMeta :: Parser Int
sumMeta = do
  nChildren <- P.anyToken
  nMeta <- P.anyToken
  cs <- sum <$> replicateM nChildren sumMeta
  ms <- sum <$> replicateM nMeta P.anyToken
  pure $ cs + ms

sumRoot :: Parser Int
sumRoot = do
  nChildren <- P.anyToken
  nMeta <- P.anyToken
  cs <- replicateM nChildren sumRoot
  ms <- replicateM nMeta P.anyToken
  pure $ case cs of
    [] -> sum ms
    _ -> sum $ mapMaybe (\x -> cs ^? ix (x - 1)) ms

part1 :: [Int] -> Int
part1 = fromRight 0 . P.parse sumMeta ""

part2 :: [Int] -> Int
part2 = fromRight 0 . P.parse sumRoot ""

-- Do it with state instead of Parser, much worse ...
sumState :: State [Int] Int
sumState = do
  xs <- get
  let nChild = head xs
  let nMeta = xs !! 1
  put $ drop 2 xs
  children <- sum <$> replicateM nChild sumState
  new <- get
  let meta = sum (take nMeta new)
  put $ drop nMeta new
  pure $ children + meta

sumStateMeta :: State [Int] Int
sumStateMeta = do
  xs <- get
  let nChild = head xs
  let nMeta = xs !! 1
  put $ drop 2 xs
  children <- replicateM nChild sumStateMeta
  new <- get
  let meta = take nMeta new
  put $ drop nMeta new
  pure $ if null children
            then sum meta
            else sum $ mapMaybe (\x -> children ^? ix (x - 1)) meta
----

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  print $ part1 input
  print $ part2 input
  print $ runState sumState input
  print $ runState sumStateMeta input

-- 41028
-- 20849
