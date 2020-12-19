{-# LANGUAGE OverloadedStrings #-}
module Day18 where

import Text.Parsec
    ( char, digit, string, between, many1, (<?>), (<|>), parse, Parsec )
import Text.Parsec.Expr
    ( buildExpressionParser, Assoc(AssocLeft), Operator(Infix) )
import Control.Lens (Identity)

type Op = Operator String () Identity Int
type Parser = Parsec String ()

expr1 :: Parser Int
expr1 = buildExpressionParser table1 term1 <?> "expression"
  where
    term1 = between (char '(') (char ')') expr1 <|> (read <$> many1 digit) <?> "term"
    table1 = [ [ binary  "*" (*) AssocLeft, binary  "+" (+) AssocLeft ] ]

expr2 :: Parser Int
expr2 = buildExpressionParser table2 term2 <?> "expression"
  where
    term2 = between (char '(') (char ')') expr2 <|> (read <$> many1 digit) <?> "term"
    table2 = [ [ binary  "+" (+) AssocLeft ]
             , [ binary  "*" (*) AssocLeft ]
             ]

binary :: String -> (Int -> Int -> Int) -> Assoc -> Op
binary name f = Infix  (f <$ string name)

run :: Parsec String () Int -> [String] -> Int
run e = sum . either (error . show) id . traverse (parse e "")

main :: IO ()
main = do
  input <- lines . filter (/= ' ') <$> readFile "../data/day18.in"
  print $ run expr1 input
  print $ run expr2 input

-- 8929569623593
-- 231235959382961
