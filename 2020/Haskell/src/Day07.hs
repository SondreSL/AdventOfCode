module Day07 where

import Control.Monad (replicateM_)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple.Extra (first)
import Text.ParserCombinators.Parsec

data Bag = B String String
    deriving (Show, Eq, Ord)

parseInput :: String -> Map Bag [(Int, Bag)]
parseInput = Map.fromList . either (error . show) id . traverse (parse p "") . lines
  where
    word = many1 letter
    content =
        (,) <$> (read <$> many1 digit) <* space
            <*> ( B <$> many1 letter <* space
                    <*> many1 letter <* space <* word
                )
    p = do
        fabric <- word <* space
        col <- word <* space
        replicateM_ 2 (word <* space)
        rules <- sepBy content (string ", ")
        pure (B fabric col, rules)

flippedMap :: Map Bag [(Int, Bag)] -> Map Bag [(Int, Bag)]
flippedMap = Map.fromListWith (++) . concatMap (unfoldr f) . Map.toList
  where
    f (_, []) = Nothing
    f (b, (i, x) : xs) = Just ((x, [(i, b)]), (b, xs))

findAll :: Map Bag [(Int, Bag)] -> [(Int, Bag)] -> [(Int, Bag)]
findAll input xs = xs ++ concatMap (findAll input) ys
  where
    ys = mapMaybe ((`Map.lookup` input) . snd) xs

findSum :: Map Bag [(Int, Bag)] -> [(Int, Bag)] -> [(Int, Bag)]
findSum input xs = xs ++ concatMap (findSum input) ys
  where
    ys = mapMaybe (\(i, v) -> map (first (* i)) <$> Map.lookup v input) xs

part1 :: Map Bag [(Int, Bag)] -> Int
part1 (flippedMap -> input) = length . nub . map snd $ findAll input (input Map.! B "shiny" "gold")

part2 :: Map Bag [(Int, Bag)] -> Int
part2 input = sum . map fst $ findSum input (input Map.! B "shiny" "gold")

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day07.in"
    print $ part1 input
    print $ part2 input

-- 226
-- 9569
