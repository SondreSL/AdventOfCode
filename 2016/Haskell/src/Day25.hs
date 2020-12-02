module Day25 where

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

data Instruction = Cpy Val Val
                 | Inc Val
                 | Dec Val
                 | Jnz Val Val
                 | Tgl Val
                 | Out Val
                 deriving (Show, Eq, Ord)

data Computer = C { pc :: Int
                  , instructions :: IntMap Instruction
                  , registers :: Registers
                  , out :: Maybe Int
                  } deriving (Show, Eq, Ord)

type Registers = Map String Int

initRegisters :: Registers
initRegisters = Map.fromList $ zip ["a", "b", "c", "d"] [0, 0, 0, 0]

parse :: String -> Instruction
parse val = case splitOn " " val of
  ["cpy", a, b] -> Cpy (parseVal a) (parseVal b)
  ["inc", a]    -> Inc (parseVal a)
  ["dec", a]    -> Dec (parseVal a)
  ["jnz", a, b] -> Jnz (parseVal a) (parseVal b)
  ["tgl", a]    -> Tgl $ parseVal a
  ["out", a]    -> Out $ parseVal a

parseVal :: String -> Val
parseVal ('-' : xs) = Left . negate $ read xs
parseVal val        = if all isDigit val then Left $ read val else Right val

compute :: Computer -> Computer
compute computer@(C pc insts regs out) =
  let fetch = either id (regs !)
      toS   = either (error "Bad toS") id
  -- in  case trace (show computer ++ "\n") IntMap.lookup pc insts of
  in  case IntMap.lookup pc insts of
        Nothing        -> computer
        Just (Out x  ) -> C (pc + 1) insts (Map.insertWith (+) (toS x) 1 regs) (Just $ fetch x)
        Just (Inc x  ) -> C (pc + 1) insts (Map.insertWith (+) (toS x) 1 regs) out
        Just (Dec x  ) -> C (pc + 1) insts (Map.insertWith subtract (toS x) 1 regs) out
        Just (Jnz x y) -> C (if fetch x /= 0 then pc + fetch y else pc + 1) insts regs out
        Just (Cpy x y) -> case x of
          Left  x -> C (pc + 1) insts (Map.insert (toS y) x regs) out
          Right x -> C (pc + 1) insts (Map.insert (toS y) (regs ! x) regs) out
        Just (Tgl x) ->
          let v = fetch x
              f new = IntMap.insert (pc + v) new insts
          in  case IntMap.lookup (pc + v) insts of
                Nothing        -> compute $ C (pc + 1) insts regs out
                Just (Inc x  ) -> C (pc + 1) (f $ Dec x) regs out
                Just (Dec x  ) -> C (pc + 1) (f $ Inc x) regs out
                Just (Tgl x  ) -> C (pc + 1) (f $ Inc x) regs out
                Just (Jnz x y) -> C (pc + 1) (f $ Cpy x y) regs out
                Just (Cpy x y) -> C (pc + 1) (f $ Jnz x y) regs out

run :: Computer -> [Int]
run c = let comp@(C pc insts regs out) = compute c
         in case out of
              Nothing -> run comp
              Just n -> n : run comp

part1 :: Computer -> [[Int]]
part1 (C pc insts regs Nothing) = map (\x -> take 10 . run $ C pc insts (Map.insert "a" x regs) Nothing) [1..10]

main :: IO ()
main = do
  insts <- IntMap.fromList . zip [0 ..] . map parse . lines <$> readFile "input/23.in"
  mapM_ print $ part1 (C 0 insts initRegisters Nothing)
  -- print $ part2 (C 0 insts initRegisters) -- Takes forever
