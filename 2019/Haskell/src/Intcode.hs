module Intcode where

import Lib ( commaNums )
import Data.Char ( chr, ord )
import Control.Lens
import Data.Bool ( bool )
import Data.Map (Map)
import qualified Data.Map as Map

-- Logic
type Memory = Map Int Int

data Intcode = Intcode
  { _ip :: Int
  , _rel :: Int
  , _input :: [Char]
  , _output :: [Int]
  , _memory :: Memory
  }
  deriving (Show, Eq, Ord)
makeLenses ''Intcode

getIndex :: Int -> Memory -> Int
getIndex = Map.findWithDefault 0

args :: Intcode -> (Int, Int, Int)
args Intcode{..} =
  let mode x = div (Map.findWithDefault 0 _ip _memory) x `rem` 10
      f x = case mode (10^(x+1)) of
                0 -> getIndex (getIndex (_ip + x) _memory) _memory
                1 -> getIndex (_ip + x) _memory
                2 -> getIndex (_rel + getIndex (_ip + x) _memory) _memory
      g = case div (getIndex _ip _memory) 10000 `rem` 10 of
            2 -> _rel + getIndex (_ip + 3) _memory
            _ -> getIndex (_ip + 3) _memory
   in (f 1, f 2, g)

compute :: Intcode -> ProgramState
compute ic@Intcode{..} =
  let (a, b, c) = args ic
   in case rem (Map.findWithDefault 0 _ip _memory) 100 of
        1 -> compute $ ic & ip +~ 4 & memory %~ Map.insert c (a + b)
        2 -> compute $ ic & ip +~ 4 & memory %~ Map.insert c (a * b)
        3 -> let currIx = case div (getIndex _ip _memory) 100 `rem` 10 of
                        2 -> _rel + getIndex (_ip + 1) (view memory ic)
                        _ -> getIndex (_ip + 1) (view memory ic)
              in case _input of
                   [] -> Input ic
                   (x:xs) -> compute $ ic & ip +~ 2 & input .~ xs & memory %~ Map.insert currIx (ord x)
        4 -> Wait $ ic & ip +~ 2 & output %~ (++ [a])
        5 -> compute (Intcode (bool (_ip + 3) b (0 /= a)) _rel _input _output _memory)
        6 -> compute (Intcode (bool (_ip + 3) b (0 == a)) _rel _input _output _memory)
        7 -> compute (Intcode (_ip + 4) _rel _input _output (Map.insert c (bool 0 1 (a < b)) _memory))
        8 -> compute (Intcode (_ip + 4) _rel _input _output (Map.insert c (bool 0 1 (a == b)) _memory))
        9 -> compute (Intcode (_ip + 2) (_rel + a) _input _output _memory)
        99 -> Halt ic
        i -> error ("Invalid opcode: " ++ show i)

data ProgramState = Wait Intcode
                  | Halt Intcode
                  | Input Intcode
                  deriving (Eq, Ord)

instance Show ProgramState where
  show (Wait ic) = "Wait: " ++ map chr (_output ic)
  show (Input ic) = "Input: " ++ map chr (_output ic)
  show (Halt ic) = "Input: " ++ map chr (_output ic)

untilHalt :: ProgramState -> [Int]
untilHalt (Wait ic) = untilHalt $ compute ic
untilHalt (Halt ic) = view output ic

run :: ProgramState -> ProgramState
run (Halt ic) = Halt ic
run (Input ic) = Input ic
run (Wait ic) = compute ic

untilInput :: [ProgramState] -> ProgramState
untilInput (Halt ic:_) = Halt ic
untilInput (Input ic:_) = Input ic
untilInput (_:xs) = untilInput xs

parseIntcode :: String -> Map Int Int
parseIntcode = Map.fromList . zip [0..] . commaNums
