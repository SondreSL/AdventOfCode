module Day08 where

import Control.Lens
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Lib (firstRepeatOn)
import Control.Comonad.Store ( peeks )

data Instruction = Nop Int | Jmp Int | Acc Int
    deriving (Show, Eq, Ord, Read)
makePrisms ''Instruction

data Program = Program
    { _instructions :: Vector Instruction
    , _ip :: Int
    , _acc :: Int
    }
    deriving (Show)
makeLenses ''Program

mkProgram :: [Instruction] -> Program
mkProgram is = Program (Vec.fromList is) 0 0

parseInput :: String -> [Instruction]
parseInput = map (read . over _head toUpper) . lines . filter (/= '+')

run :: Program -> Program
run p@Program{..} = case _instructions ^? ix _ip of
    Just (Nop _) -> p & ip +~ 1
    Just (Jmp i) -> p & ip +~ i
    Just (Acc i) -> p & ip +~ 1 & acc +~ i
    Nothing -> p

changeInstructions :: [Instruction] -> [[Instruction]]
changeInstructions input = map (peeks changeInst) (holesOf (traverse . filtered (hasn't _Acc))  input)
  where
    changeInst (Jmp i) = Nop i
    changeInst (Nop i) = Jmp i
    changeInst n = n

part1 :: [Instruction] -> Maybe Int
part1 = fmap _acc . firstRepeatOn _ip . iterate run . mkProgram

part2 :: [Instruction] -> Maybe Int
part2 = fmap _acc . find ((== 647) . _ip) . mapMaybe check . changeInstructions
  where
    check = firstRepeatOn _ip . iterate run . mkProgram

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day08.in"
    print $ part1 input
    print $ part2 input

-- 1949
-- 2092
