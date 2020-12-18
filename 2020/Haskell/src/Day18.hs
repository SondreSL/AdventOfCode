module Day18 where

import Text.ParserCombinators.Parsec
    ( char, digit, oneOf, between, many1, (<|>), parse )

part1 :: String -> Int
part1 = sum . either (error . show) id . traverse (parse parseExp "") . lines . filter (/= ' ')
  where
    parseExp = parseFactor >>= loop
      where termSuffix t1 = do
              op <- oneOf "+*"
              t2 <- parseFactor
              case op of
                '+' -> loop (t1 + t2)
                '*' -> loop (t1 * t2)
            loop t = termSuffix t <|> pure t
    parseFactor = parseConst <|> parseParen
    parseParen = between (char '(') (char ')') parseExp
    parseConst = read <$> many1 digit

part2 :: String -> Int
part2 = sum . either (error . show) id . traverse (parse parseExp "") . lines . filter (/= ' ')
  where
    parseExp = parseTerm >>= loop
      where termSuffix t1 = do
              t2 <- oneOf "*" *> parseTerm
              loop (t1 * t2)
            loop t = termSuffix t <|> pure t
    parseTerm = parseFactor >>= loop
      where factorSuffix x = do
              y <- oneOf "+" *> parseFactor
              loop (x + y)
            loop t = factorSuffix t <|> pure t
    parseFactor = parseConst <|> parseParen
    parseParen = between (char '(') (char ')') parseExp
    parseConst = read <$> many1 digit

main :: IO ()
main = do
  input <- readFile "../data/day18.in"
  print $ part1 input
  print $ part2 input

-- 8929569623593
-- 231235959382961
