module Day14 where

import Control.Lens ( (&), view, makePrisms, (.~), makeLenses )
import Data.Digits ( digits, unDigits )
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
    ( digit, oneOf, string, many1, (<|>), parse, try )

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

applyMask :: ((Int, Char) -> [Int]) -> String -> Int -> [Int]
applyMask f m n =
    map (unDigits 2 . reverse)
        . traverse f
        $ zip (take 36 . (++ repeat 0) . reverse $ digits 2 n) m

run :: (Memory -> Int -> Int -> Map Int Int) -> Memory -> Instruction -> Memory
run _ memory (SetMask str) = memory & mask .~ reverse str
run f memory (SetMem loc val) = memory & mem .~ f memory loc val

part1 :: [Instruction] -> Int
part1 = sum . view mem . foldl (run f) (Memory Map.empty "")
  where
    f (Memory me ma) loc val =
      let val' = head $ applyMask maskOp ma val
       in Map.insert loc val' me
    maskOp (a, 'X') = [a]
    maskOp (_, '0') = [0]
    maskOp (_, '1') = [1]

part2 :: [Instruction] -> Int
part2 = sum . view mem . foldl (run f) (Memory Map.empty "")
  where
    f (Memory me ma) loc val =
      let locs = applyMask maskOp ma loc
       in foldl (\acc l -> Map.insert l val acc) me locs
    maskOp (n, '0') = [n]
    maskOp (_, '1') = [1]
    maskOp (_, 'X') = [0, 1]

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day14.in"
    print $ part1 input
    print $ part2 input

-- 13727901897109
-- 5579916171823
