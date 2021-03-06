module Day19 where

import Control.Monad (foldM, guard)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List.Extra ( splitOn )
import Lib (count, tuple)
import Text.ParserCombinators.Parsec
  (char, digit, letter, many1, parse, sepBy, sepEndBy, space, string, (<|>),)
import Data.Bifunctor (bimap)

type Rule = Either Char [[Int]]

parseInput :: String -> (IntMap Rule, [String])
parseInput = bimap rules lines . tuple . splitOn "\n\n"
  where
    rules = Map.unions . map (either (error . show) id . parse parseRules "") . lines
    direct = Left <$> (char '"' *> letter <* char '"')
    parseRules =
        Map.singleton
          <$> (read <$> many1 digit <* string ": ")
          <*> (direct <|> Right <$> sepBy (sepEndBy (read <$> many1 digit) space) (string "| "))

expand :: IntMap Rule -> Rule -> String -> [String]
expand _ _ [] = []
expand rules rule (s : str) = case rule of
    Left c -> guard (c == s) >> pure str
    Right xs -> xs >>= foldM (\acc x -> expand rules (rules Map.! x) acc) (s : str)

part1 :: IntMap Rule -> [String] -> Int
part1 rules = count (any null) . map (expand rules (Right [[0]]))

part2 :: IntMap Rule -> [String] -> Int
part2 rules = count (any null) . map (expand (newRules <> rules) (Right [[0]]))
  where
    newRules =
        Map.fromList
            [ (8,  Right [[42    ], [42,  8    ]])
            , (11, Right [[42, 31], [42, 11, 31]])
            ]

main :: IO ()
main = do
    (rules, input) <- parseInput <$> readFile "../data/day19.in"
    print $ part1 rules input
    print $ part2 rules input
