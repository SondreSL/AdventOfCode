{-# LANGUAGE DeriveFunctor #-}
module Day16 where

import Data.Bits
import Data.Bool
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Data.List (foldl')
import Data.Maybe (listToMaybe)

type Memory = Map Int Int
data Opcode
  = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
  | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
  deriving (Show, Eq, Ord, Enum, Bounded)

data Instruction a = I
  { opcode :: a
  , a :: Int
  , b :: Int
  , c :: Int
  }
  deriving (Show, Functor)

data Test = T
  { before :: Memory
  , instr :: Instruction Int
  , after :: Memory
  }
  deriving (Show)

parseInput :: String -> ([Test], [Instruction Int])
parseInput =
  either (error . show) id
    . parse
      ( (,) <$> sepEndBy1 parseOpcodes newline <* many1 newline
          <*> sepEndBy1 parseInstr newline
      )
      ""
 where
  parseOpcodes :: Parser Test
  parseOpcodes =
    T <$> between (string "Before: ") newline parseList
      <*> parseInstr <* newline
      <*> between (string "After:  ") newline parseList
  parseList :: Parser Memory
  parseList =
    Map.fromList . zip [0 ..] . map read
      <$> between (char '[') (char ']') (sepBy (many digit) (string ", "))
  parseInstr :: Parser (Instruction Int)
  parseInstr =
    I <$> pNum <* space
      <*> pNum <* space
      <*> pNum <* space
      <*> pNum
  pNum = read <$> many digit

matchOpcode :: Test -> Set Opcode
matchOpcode (T bef ins aft) = Set.fromList $ filter runTest [Addr ..]
 where
  runTest o = eval bef (ins{opcode = o}) == aft

eval :: Memory -> Instruction Opcode -> Memory
eval m (I op a b c) =
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

determineOpcode :: [Test] -> Maybe (V.Vector Opcode)
determineOpcode tests = listToMaybe
  . flip evalStateT Set.empty
  . V.generateM 16
  $ \i -> do
    Just poss <- pure $ Map.lookup i cands
    unseen <- gets (poss `Set.difference`)
    pick <- lift $ Set.toList unseen
    modify $ Set.insert pick
    pure pick
 where
  cands = candidates tests

candidates :: [Test] -> Map Int (Set Opcode)
candidates tests = Map.fromListWith Set.intersection $ zip (map (opcode . instr) tests) (map matchOpcode tests)

test :: [Test] -> [(Int, Set Opcode)]
test tests = zip (map (opcode . instr) tests) (map matchOpcode tests)

part1 :: [Test] -> Int
part1 = length . filter ((>= 3) . Set.size) . map matchOpcode

part2 :: [Test] -> [Instruction Int] -> Int
part2 tests instrs = (Map.! 0) $ foldl' eval (Map.fromList (zip [0..] [0, 0, 0, 0])) ops
 where
   trans = case determineOpcode tests of
             Nothing -> error "Cannot determine opcodes"
             Just v -> Map.fromList . zip [0..] $ V.toList v
   ops = map (\(I op a b c) -> I (trans Map.! op) a b c) instrs

main :: IO ()
main = do
  (opcodes, program) <- parseInput <$> readFile "../data/day16.in"
  print $ part1 opcodes
  print $ part2 opcodes program
