module Day23 where

import Data.List.Extra
import Data.Bits
import Data.Char
import Data.Bool
import Data.Maybe
import Data.Complex
import Data.Function
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

-- Parsing the input (comma separated ints)
parser :: String -> Memory
parser = either (error "Bad parse") (M.fromList . zip [0..]) . parse numbers "Opcodes"
  where numbers = map read <$> sepBy (minus <|> num) (char ',')
        num = many1 digit
        minus = (:) <$> char '-' <*> num

-- Intcode computer
type Memory = M.Map Int Int
type Hull = M.Map Pos Int
type Facing = Complex Float
type Pos = (Int, Int)

data ProgramState = Wait Int Int [Int] [Int] Memory
                  | Halt [Int]
                  | Input Int Int [Int] [Int] Memory

instance Show ProgramState where
  show (Wait i r inp out mem) = "Wait " ++ show inp ++ " " ++ show out
  show (Input i r inp out mem) = "Input " ++ show inp ++ " " ++ show out
  show (Halt out) = "Halt " ++ show out

data RobotState = Hold Hull Pos Facing
                | Fin Hull

index :: Int -> Memory -> Int
index = M.findWithDefault 0

args :: Int -> Int -> Memory -> (Int, Int, Int)
args i rel mem =
  let mode x = div (M.findWithDefault 0 i mem) x `rem` 10
      f x = case mode (10^(x+1)) of
                0 -> index (index (i + x) mem) mem
                1 -> index (i + x) mem
                2 -> index (rel + (index (i + x) mem)) mem
      g = case div (index i mem) 10000 `rem` 10 of
            2 -> rel + (index (i + 3) mem)
            _ -> index (i + 3) mem
   in (f 1, f 2, g)

compute :: Int -> Int -> [Int] -> [Int] -> Memory -> ProgramState
compute i rel input out memory =
  let (a, b, c) = args i rel memory
   in case rem (M.findWithDefault 0 i memory) 100 of
        1 -> compute (i + 4) rel input out (M.insert c (a + b) memory)
        2 -> compute (i + 4) rel input out (M.insert c (a * b) memory)
        3 -> let ix = case div (index i memory) 100 `rem` 10 of
                        2 -> rel + (index (i + 1) memory)
                        _ -> index (i + 1) memory
              in case input of
                   [] -> Input i rel input out memory
                   (x:xs) -> compute (i + 2) rel xs out (M.insert ix x memory)
        4 -> Wait (i + 2) rel input (out ++ [a]) memory
        5 -> compute (bool (i + 3) b (0 /= a)) rel input out memory
        6 -> compute (bool (i + 3) b (0 == a)) rel input out memory
        7 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a < b)) memory)
        8 -> compute (i + 4) rel input out (M.insert c (bool 0 1 (a == b)) memory)
        9 -> compute (i + 2) (rel + a) input out memory
        99 -> Halt out
        i -> error ("Invalid opcode: " ++ show i)

halted :: ProgramState -> Bool
halted (Halt _) = True
halted _ = False

run :: ProgramState -> ProgramState
run v@(Wait ip rel inp out mem) = run (compute ip rel inp out mem)
run v = v

restart :: ProgramState -> [Int] -> ProgramState
restart v@(Input ip rel inp out mem) i = run (Wait ip rel (inp ++ i) out mem)

start :: Memory -> [Int] -> ProgramState
start mem n = run (Wait 0 0 n [] mem)

emptyOutput :: ProgramState -> Bool
emptyOutput (Input ip rel inp out mem) = null out
emptyOutput _ = False

-- Logic
type Machines = M.Map Int ProgramState

multi :: Int -> Memory -> [Int]
multi num mem = go machines 0 0 (0,0) -- Assume the NAT value is overwritten before it is read
  where
    machines = M.fromList . zip [0..num] . map (start mem) $ chunksOf 1 [0..num]
    go ms count idleCount nat =
      let (cand, new) = process count ms
          idle = emptyOutput $ M.findWithDefault (error ".") count ms
          idleCount' = if idle then idleCount + 1 else 0
          newnat@(a,b) = if isJust cand then fromJust cand else nat
       in case idleCount' of
            101  -> b : go (sendNat [a,b] new) 0 0 newnat
            _   -> go new ((count + 1) `mod` (num + 1)) idleCount' newnat

sendNat :: [Int] -> Machines -> Machines
sendNat i m = M.insert 0 (restart (M.findWithDefault (error "Lookup on zero") 0 m) i) m

process :: Int -> Machines -> (Maybe (Int, Int), Machines)
process c m =
  let (Just (Input ip rel inp out mem)) = M.lookup c m
   in sendInput c out Nothing $ M.insert c (run (compute ip rel [-1] [] mem)) m

sendInput :: Int -> [Int] -> Maybe (Int, Int) -> Machines -> (Maybe (Int, Int), Machines)
sendInput c [] nat m = (nat, m)
sendInput c (n:x:y:xs) nat m =
  let next = M.findWithDefault (Halt []) n m
   in case next of
        Halt val -> sendInput c xs (Just (x, y)) m
        Input ip rel inp out mem -> sendInput c xs nat $ M.insert n (run (compute ip rel [x,y] out mem)) m
        a -> error $ show a

solveA :: Memory -> Int
solveA = head . multi 49

solveB :: Memory -> Int
solveB = fst . head . dropWhile (uncurry (/=)) . (zip <*> tail) . multi 49

main = do
  input <- parser <$> readFile "data/input-2019-23.txt"
  print $ solveA input
  print $ solveB input

-- 17849
-- 12235
