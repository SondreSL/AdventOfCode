{-# LANGUAGE RecordWildCards #-}

module Day04 where

import Data.Foldable (maximumBy)
import Data.List (sortOn)
import Data.List.Extra (
  group,
  maximumOn,
  sort,
 )
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

type Id = Int

data Data
  = ShiftStart Id Time
  | FallAsleep Time
  | WakeUp Time
  deriving (Show, Eq, Ord)

shiftStart (ShiftStart _ _) = True
shiftStart _ = False

data Time = Time
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  }
  deriving (Show, Eq, Ord)

-- Parsing
parseType :: Parser (Time -> Data)
parseType = start <|> snooze <|> wake
 where
  start = do
    i <- string "Guard #" *> many digit <* string " begins shift"
    pure $ ShiftStart (read i)
  snooze = do
    string "falls asleep"
    pure FallAsleep
  wake = do
    string "wakes up"
    pure WakeUp

parseData :: Parser Data
parseData = do
  char '['
  year <- read <$> many digit
  char '-'
  month <- read <$> many digit
  char '-'
  day <- read <$> many digit
  space
  hour <- read <$> many digit
  char ':'
  minute <- read <$> many digit
  char ']'
  space
  dat <- parseType
  pure $ dat $ Time year month day hour minute

parseInput :: String -> [Data]
parseInput = map (either (error "Bad parse") id . parse parseData "data") . lines

-- Logic

-- Gather all the data into a map from ID to minutes asleep
run :: Map Int [Int] -> [Data] -> Map Id [Int]
run sleep [] = sleep
run sleep (ShiftStart id Time{..} : xs) =
  let (curr, rest) = break shiftStart xs
   in run (buildMap sleep id curr) rest
run _ _ = error "Should not happend | run"

buildMap :: Map Int [Int] -> Id -> [Data] -> Map Id [Int]
buildMap sleep _ [] = sleep
buildMap sleep id (FallAsleep fallAsleep : WakeUp wakeUp : xs) =
  let time = [hour fallAsleep * 10 + minute fallAsleep .. hour wakeUp * 10 + minute wakeUp]
   in buildMap (Map.insertWith (++) id time sleep) id xs
buildMap _ _ _ = error "Bad buildMap"

mostCommonMinute :: [Int] -> [(Int, Int)]
mostCommonMinute = map (\l -> (head l, length l)) . group . sort

part1 :: Map Int [Int] -> Int
part1 =
  uncurry (*)
    . fmap (head . maximumOn length . group . sort)
    . maximumOn (length . snd)
    . Map.toList

part2 :: Map Int [Int] -> Int
part2 =
  uncurry (*)
    . fmap fst
    . maximumOn (snd . snd)
    . Map.toList
    . fmap (maximumOn snd . mostCommonMinute)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  print $ part1 $ run Map.empty input
  print $ part2 $ run Map.empty input

-- 39422
-- 65474
