module Day14 where

import Control.Lens
import Data.Digits
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

data Instruction
    = SetMask String
    | SetMem Int Int
    deriving (Show, Eq, Ord)
makePrisms ''Instruction

data Memory = Memory
    { _mem :: Map Int Int
    , _mask :: String
    }
    deriving (Show)
makeLenses ''Memory

parseInput :: String -> [Instruction]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    p = try mask <|> mem
    mask = SetMask <$> (string "mask = " *> many1 (oneOf "X10"))
    mem = SetMem <$> (string "mem[" *> (read <$> many1 digit) <* string "] = ") <*> (read <$> many1 digit)

zipMask :: String -> [Int] -> [Int]
zipMask = zipWith f
  where
    f 'X' a = a
    f '0' _ = 0
    f '1' _ = 1

modifyMask :: String -> Int -> [Int]
modifyMask str num = map (unDigits 2 . reverse) . traverse f $ zip (take 36 . (++ repeat 0) . reverse $ digits 2 num) str
  where
    f (n, '0') = [n]
    f (_, '1') = [1]
    f (_, 'X') = [0, 1]

applyMask :: String -> Int -> Int
applyMask m = unDigits 2 . reverse . zipMask m . take 36 . (++ repeat 0) . reverse . digits 2

run :: Memory -> Instruction -> Memory
run memory (SetMask str) = memory & mask .~ reverse str
run memory (SetMem loc val) =
    let val' = applyMask (view mask memory) val
     in memory & mem %~ Map.insert loc val'

run2 :: Memory -> Instruction -> Memory
run2 memory (SetMask str) = memory & mask .~ reverse str
run2 memory (SetMem loc val) =
    let locs = modifyMask (view mask memory) loc
     in memory & mem .~ foldl (\acc x -> Map.insert x val acc) (view mem memory) locs

part1 :: [Instruction] -> Int
part1 = sum . view mem . foldl run (Memory Map.empty "")

part2 :: [Instruction] -> Int
part2 = sum . view mem . foldl run2 (Memory Map.empty "")

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day14.in"
    print $ part1 input
    print $ part2 input

-- 13727901897109
-- 5579916171823
