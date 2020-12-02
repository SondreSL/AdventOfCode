module Day02 where

import Lib ( count )
import Control.Lens
    ( (^?), Ixed(ix) )
import Text.ParserCombinators.Parsec
    ( alphaNum, char, digit, letter, space, string, many1, parse )

data Pass = P { _a :: Int
              , _b :: Int
              , _ch :: Char
              , _password :: String
              }
  deriving (Show, Eq, Ord)

parseInput :: String -> [Pass]
parseInput = either (error . show) id . traverse (parse pass "") . lines
 where
  num = read <$> many1 digit
  pass =
    P <$> num <* char '-'
      <*> num <* space
      <*> letter <* string ": "
      <*> many1 alphaNum

part1 :: [Pass] -> Int
part1 = count valid
  where
   valid (P a b l str) = a <= le && le <= b
    where le = count (== l) str

part2 :: [Pass] -> Int
part2 = count pos
  where
    pos (P a b l str) = aa && not bb || not aa && bb
      where aa = str ^? ix (a - 1) == Just l
            bb = str ^? ix (b - 1) == Just l

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  print $ part1 input
  print $ part2 input

-- 517
-- 284
