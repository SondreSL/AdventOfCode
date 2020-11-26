{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Lens
import Data.Bits
import Data.Bool (bool)
import Data.Char
import Data.Functor.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.ParserCombinators.Parsec

type Memory = Map Int Int
data Opcode
  = Addr | Addi | Mulr | Muli
  | Banr | Bani | Borr | Bori
  | Setr | Seti | Gtir | Gtri
  | Gtrr | Eqir | Eqri | Eqrr
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Instruction a = I
  { _opcode :: a
  , _a :: Int
  , _b :: Int
  , _c :: Int
  }
  deriving (Show, Eq, Ord, Functor)

makeLenses ''Instruction

data Program = P
  { _mem :: Memory
  , _ins :: Seq (Instruction Opcode)
  }
  deriving (Show, Eq, Ord)

makeLenses ''Program

runMemory :: Memory -> Instruction Opcode -> Memory
runMemory m (I op a b c) =
  case op of
    Addr -> Map.insert c (m ! a + m ! b) m
    Addi -> Map.insert c (m ! a + b) m
    Mulr -> Map.insert c (m ! a * m ! b) m
    Muli -> Map.insert c (m ! a * b) m
    Banr -> Map.insert c (m ! a .&. m ! b) m
    Bani -> Map.insert c (m ! a .&. b) m
    Borr -> Map.insert c (m ! a .|. m ! b) m
    Bori -> Map.insert c (m ! a .|. b) m
    Setr -> Map.insert c (m ! a) m
    Seti -> Map.insert c a m
    Gtir -> Map.insert c (bool 0 1 $ a > (m ! b)) m
    Gtri -> Map.insert c (bool 0 1 $ (m ! a) > b) m
    Gtrr -> Map.insert c (bool 0 1 $ (m ! a) > (m ! b)) m
    Eqir -> Map.insert c (bool 0 1 $ a == (m ! b)) m
    Eqri -> Map.insert c (bool 0 1 $ (m ! a) == b) m
    Eqrr -> Map.insert c (bool 0 1 $ (m ! a) == (m ! b)) m

eval :: Int -> Program -> Log Program
eval ipReg p@P{..} =
  if ip >= length _ins
    then Done $ _mem ! 0
    else Running $ p{_mem = runMemory _mem (S.index _ins ip)} & over (mem . ix ipReg) (+1)
 where
  ip = _mem ! ipReg

data Log a
  = Running a
  | Done Int
  deriving (Show, Eq, Ord, Functor)

runToHalt :: Log (Maybe Int) -> Maybe Int
runToHalt (Running a) = a
runToHalt (Done i) = Just i

part1 :: Int -> Program -> Maybe Int
part1 = (runToHalt `hylo`) . eval

main :: IO ()
main = do
  (ip, ops) <- parseInput <$> readFile "../data/day19.in"
  print $ part1 ip $ P (Map.fromList (zip [0 ..] (replicate 6 0))) (S.fromList ops)

-- | Parsing
parseInput :: String -> (Int, [Instruction Opcode])
parseInput =
  either (error . show) id
    . parse
      ( (,)
          <$> (read <$> (string "#ip " *> many1 digit <* newline))
          <*> sepEndBy1 parseInstr newline
      )
      ""
 where
  parseOpcode :: Parser Opcode
  parseOpcode = many1 alphaNum <&> read . over _head toUpper
  parseInstr :: Parser (Instruction Opcode)
  parseInstr =
    I <$> parseOpcode <* space
      <*> pNum <* space
      <*> pNum <* space
      <*> pNum
  pNum = read <$> many digit
