{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Day21 where

import Control.Lens
import Data.Functor.Foldable (hylo)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Day19 (
  Instruction (..),
  Opcode (..),
  Program (..),
  mem,
  parseInput,
  runMemory,
 )

mkProgram ::
  -- | New value of register 0
  Int ->
  Seq (Instruction Opcode) ->
  Program
mkProgram n = P (Map.fromList (zip [0 ..] (n : replicate 5 0)))

eval :: Int -> Program -> Log Program
eval ipReg p@P{..} =
  if ip >= length _ins
    then Done $ _mem Map.! 0
    else case op of
      I Eqrr a _ _ ->
        Check (Map.lookup a _mem) $
          p{_mem = runMemory _mem (S.index _ins ip)} & over (mem . ix ipReg) (+ 1)
      _ -> Running $ p{_mem = runMemory _mem (S.index _ins ip)} & over (mem . ix ipReg) (+ 1)
 where
  ip = _mem Map.! ipReg
  op = S.index _ins ip

data Log a
  = Running a
  | Check (Maybe Int) a
  | Done Int
  deriving (Show, Eq, Ord, Functor)

runToHalt :: Log [Maybe Int] -> [Maybe Int]
runToHalt (Running a) = a
runToHalt (Check i a) = i : a
runToHalt (Done _) = []

run :: Int -> Seq (Instruction Opcode) -> Int -> [Maybe Int]
run ip seq n = (runToHalt `hylo` eval ip) (mkProgram n seq)

part1 :: Int -> Seq (Instruction Opcode) -> Maybe Int
part1 i s = head $ run i s 1

main :: IO ()
main = do
  (ip, S.fromList -> instrs) <- parseInput <$> readFile "../data/day21.in"
  print $ part1 ip instrs

-- 1024276
-- 5876609 (not solved, solved in python in 2018 or through manual analysis)
