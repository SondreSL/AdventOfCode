{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Day04HKD where

import Relude (mapMaybe,  readMaybe )
import Control.Monad (guard, (>=>))
import Control.Monad.Identity (Identity)
import GHC.Generics (Generic)
import Lib (count)
import Text.ParserCombinators.Parsec (
    Parser,
    alphaNum,
    char,
    choice,
    digit,
    many1,
    parse,
    sepBy,
    sepEndBy,
    space,
    string,
    try,
    (<|>),
 )

-- https://reasonablypolymorphic.com/blog/higher-kinded-data/
-- "Higher-Kinded Data"
type family HKD f a where
    HKD Identity a = a
    HKD f a = f a

data Passport' f
    = BirthYear (HKD f Int)
    | IssueYear (HKD f Int)
    | ExpirationYear (HKD f Int)
    | Height (HKD f Measure)
    | HairColor (HKD f String)
    | EyeColor (HKD f String)
    | PassportID (HKD f Int)
    | CountryID
    deriving (Generic)

type Passport = Passport' Identity

data Measure = Imperial Int | Metric Int
    deriving (Show)

validate :: Passport' Maybe -> Maybe Passport
validate (BirthYear i) = BirthYear <$> i
validate (IssueYear i) = IssueYear <$> i
validate (ExpirationYear i) = ExpirationYear <$> i
validate (Height i) = Height <$> i
validate (HairColor i) = HairColor <$> i
validate (EyeColor i) = EyeColor <$> i
validate (PassportID i) = PassportID <$> i
validate CountryID = Nothing

-- Parsing
parseInput :: String -> [[Passport' Maybe]]
parseInput = either (error . show) id . parse (sepBy passport (many1 space)) ""
  where
    chars = char '#' <|> alphaNum
    hex = legalHex <$> many1 chars
    measure = (fmap . fmap) Metric (try (num 150 193 <* string "cm"))
          <|> (fmap . fmap) Imperial (try (num 59 76 <* string "in"))
          <|> (Nothing <$ many1 alphaNum)
    num :: Int -> Int -> Parser (Maybe Int)
    num i j = between i j . read <$> many1 digit
    pid = validPid <$> many1 chars
    validPid = len 9 >=> readMaybe
    eyeColor = legalEyeColor <$> many1 chars
    legalEyeColor str = guard (str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) >> Just str
    legalHex ('#' : str) =
        let hexed = filter (`elem` "abcdef0123456789") str
         in guard (length str == 6 && str == hexed) >> Just ('#' : str)
    legalHex _ = Nothing
    between i j n = guard (i <= n && n <= j) >> pure n
    len n str = guard (length str == n) >> Just str
    passport = sepEndBy passports space
    passports =
        choice
            [ BirthYear <$> (string "byr:" *> num 1920 2002)
            , try (IssueYear <$> (string "iyr:" *> num 2010 2020))
            , try (ExpirationYear <$> (string "eyr:" *> num 2020 2030))
            , try (Height <$> (string "hgt:" *> measure)) -- Here there is bad input
            , try (HairColor <$> (string "hcl:" *> hex)) -- Here there is bad input
            , try (EyeColor <$> (string "ecl:" *> eyeColor))
            , try (PassportID <$> (string "pid:" *> pid)) -- Here there is bad input
            , try (CountryID <$ (string "cid:" *> many1 alphaNum)) -- Here there is bad input
            ]

--
part1 :: [[Passport' Maybe]] -> Int
part1 = count valid
  where
    valid = (>= 7) . length . filter validate
    validate CountryID = False
    validate _ = True

part2 :: [[Passport' Maybe]] -> Int
part2 = count valid
  where
    valid = (>= 7) . length . mapMaybe validate

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"

  print $ part1 input
  print $ part2 input

-- 219
-- 127
