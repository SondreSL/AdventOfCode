module Day04 where

import Lib ( count )
import Data.Char ( isAlphaNum, isDigit )
import Text.ParserCombinators.Parsec
  ( alphaNum,
    char,
    many1,
    digit,
    parse,
    sepBy,
    sepEndBy,
    space,
    string,
    try,
    (<|>),
  )

data Measure = Imperial | Metric
  deriving (Show)

data Passport
  = BirthYear Int
  | IssueYear Int
  | ExpirationYear Int
  | Height String
  | HairColor String
  | EyeColor String
  | PassportID String
  | CountryID String
  deriving (Show)

parseInput :: String -> [[Passport]]
parseInput = either (error . show) id . parse (sepBy passport space) ""
  where
    chars = alphaNum <|> char '#'
    passport = sepEndBy passports space
    passports =
      BirthYear . read <$> (string "byr:" *> many1 digit)
        <|> try (IssueYear . read <$> (string "iyr:" *> many1 digit))
        <|> try (ExpirationYear . read <$> (string "eyr:" *> many1 digit))
        <|> try (Height <$> (string "hgt:" *> many1 chars))      -- Here there is bad input
        <|> try (HairColor <$> (string "hcl:" *> many1 chars))   -- Here there is bad input
        <|> try (EyeColor <$> (string "ecl:" *> many1 chars))    -- Here there is bad input
        <|> try (PassportID <$> (string "pid:" *> many1 chars))  -- Here there is bad input
        <|> try (CountryID <$> (string "cid:" *> many1 chars))   -- Here there is bad input

part1 :: [[Passport]] -> Int
part1 = count valid
  where
    valid = (>= 7) . count validate
    validate (CountryID _) = False
    validate _ = True

part2 :: [[Passport]] -> Int
part2 = count valid
  where
    valid = (>= 7) . count validate
    validate (BirthYear i) = 1920 <= i && i <= 2002
    validate (IssueYear i) = 2010 <= i && i <= 2020
    validate (ExpirationYear i) = 2020 <= i && i <= 2030
    validate (Height (a:b:c:"cm")) = let i = read [a,b,c] in 150 <= i && i <= 193
    validate (Height (a:b:"in")) = let i = read [a,b] in 59 <= i && i <= 76
    validate (Height _) = False
    validate (HairColor ('#':xs)) = all isAlphaNum xs && (length xs == 6)
    validate (HairColor _) = False
    validate (EyeColor str) = str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    validate (PassportID str) = (length str == 9) && all isDigit str
    validate (CountryID _) = False

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  print $ part1 input
  print $ part2 input

-- 219
-- 127
