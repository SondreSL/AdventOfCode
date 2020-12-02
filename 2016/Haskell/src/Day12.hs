module Day12 where

import           Data.Char                      ( isDigit )
import           Data.Function                  ( fix )
import           Data.List.Extra                ( splitOn )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import qualified Data.IntMap                   as IntMap
import           Data.IntMap                    ( IntMap )
import           Debug.Trace

type Val = Either Int String

data Instruction = Cpy Val String
                 | Inc String
                 | Dec String
                 | Jnz Val Int
                 deriving (Show, Eq, Ord)

data Computer = C { pc :: Int
                  , instructions :: IntMap Instruction
                  , registers :: Registers
                  } deriving (Show, Eq, Ord)

type Registers = Map String Int

initRegisters :: Registers
initRegisters = Map.fromList $ zip ["a", "b", "c", "d"] [0, 0, 0, 0]

parse :: String -> Instruction
parse val = case splitOn " " val of
  ["cpy", a, b] -> Cpy (parseVal a) b
  ["inc", a]    -> Inc a
  ["dec", a]    -> Dec a
  ["jnz", a, b] -> Jnz (parseVal a) (either id (error "Wut") $ parseVal b)

parseVal :: String -> Val
parseVal ('-' : xs) = Left . negate $ read xs
parseVal val        = if all isDigit val then Left $ read val else Right val

compute :: Computer -> Computer
compute computer@(C pc insts regs) =
  let fetch = either id (regs !)
  in  case IntMap.lookup pc insts of
        Nothing        -> computer
        Just (Inc x  ) -> C (pc + 1) insts (Map.insertWith (+) x 1 regs)
        Just (Dec x  ) -> C (pc + 1) insts (Map.insertWith subtract x 1 regs)
        Just (Jnz x y) -> C (if fetch x /= 0 then pc + y else pc + 1) insts regs
        Just (Cpy x y) -> case x of
          Left  x -> C (pc + 1) insts (Map.insert y x regs)
          Right x -> C (pc + 1) insts (Map.insert y (regs ! x) regs)

run :: Computer -> Int
run c = let comp@(C pc insts regs) = compute c in if comp == c then regs ! "a" else run comp

part1 :: Computer -> Int
part1 = run

part2 :: Computer -> Int
part2 (C pc insts regs) = run . C pc insts $ Map.insert "c" 1 regs

main :: IO ()
main = do
  insts <- IntMap.fromList . zip [0 ..] . map parse . lines <$> readFile "input/12.in"
  print $ part1 (C 0 insts initRegisters)
  print $ part2 (C 0 insts initRegisters)
