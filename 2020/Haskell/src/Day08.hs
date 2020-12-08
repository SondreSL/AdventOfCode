module Day08 where

import Lib
import Control.Lens
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)

data Instruction = Nop Int | Jmp Int | Acc Int
    deriving (Show, Eq, Ord, Read)

data Program = Program
    { _instructions :: [Instruction]
    , _ip :: Int
    , _acc :: Int
    }
    deriving (Show)
makeLenses ''Program

parseInput :: String -> [Instruction]
parseInput = map (read . over _head toUpper) . lines . filter (/= '+')

run :: Program -> Program
run p@Program{..} = case _instructions ^? ix _ip of
    Just (Nop _) -> p & ip +~ 1
    Just (Jmp i) -> p & ip +~ i
    Just (Acc i) -> p & ip +~ 1 & acc +~ i
    Nothing -> p

part1 :: Program -> Maybe Int
part1 = fmap _acc . firstRepeatOn _ip . iterate run . run

changeInstructions :: [Instruction] -> [Instruction] -> [[Instruction]]
changeInstructions pre post =
    let (bef, i : aft) = break candidate post
        pre' = pre ++ bef
     in (pre' ++ (changeInst i : aft)) : changeInstructions (pre' ++ [i]) aft
  where
    candidate (Acc _) = False
    candidate _ = True
    changeInst (Jmp i) = Nop i
    changeInst (Nop i) = Jmp i
    changeInst n = n

part2 :: Program -> Maybe Int
part2 = fmap _acc . find ((== 647) . _ip) . take 2000 . iterate run

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day08.in"
    let mkProgram is = Program is 0 0
    print $ part1 $ mkProgram input
    print $ listToMaybe $ mapMaybe (part2 . mkProgram) . changeInstructions [] $ input

-- 1949
-- 2092
