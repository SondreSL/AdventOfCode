module Day02 where

import Lib (count)
import Control.Lens (Ixed (ix), (^?))
import Text.ParserCombinators.Parsec (alphaNum, char, digit, letter, many1, parse, space, string)

data Password = P Int Int Char String

parseInput :: String -> [Password]
parseInput = either (error . show) id . traverse (parse parsePassword "") . lines
  where
    num = read <$> many1 digit
    parsePassword =
      P <$> num <* char '-'
        <*> num <* space
        <*> letter <* string ": "
        <*> many1 alphaNum

part1 :: [Password] -> Int
part1 = count valid
  where
    valid (P a b l str) = a <= le && le <= b
      where
        le = count (== l) str

part2 :: [Password] -> Int
part2 = count valid
  where
    valid (P a b l str) = aa /= bb
      where
        aa = str ^? ix (a - 1) == Just l
        bb = str ^? ix (b - 1) == Just l

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  print $ part1 input
  print $ part2 input

-- 517
-- 284
