import Data.List.Extra
import qualified Data.IntMap.Strict as IM

data State = A | B | C | D | E | F
  deriving (Eq, Show)

data TM = TM { _tape   :: IM.IntMap Int
             , _cursor :: Int
             , _state  :: State
             }
             deriving Show

run :: TM -> TM
run m@(TM tape cursor state) = 
  let val = IM.findWithDefault 0 cursor tape
   in case state of
        A -> case val of
               0 -> TM one right B
               1 -> TM zero left C
        B -> case val of
               0 -> TM one left A
               1 -> TM one left D
        C -> case val of
               0 -> TM one right D
               1 -> TM zero right C
        D -> case val of
               0 -> TM zero left B
               1 -> TM zero right E
        E -> case val of
               0 -> TM one right C
               1 -> TM one left F
        F -> case val of
               0 -> TM one left E
               1 -> TM one right A
    where 
      one = IM.insert cursor 1 tape
      zero = IM.delete cursor tape
      right = cursor + 1
      left = cursor - 1

start :: TM
start = TM IM.empty 0 A

part1 :: Int
part1 = IM.size . _tape . (!! 12656374) . iterate run $ start

main = do
  input <- readFile "data/25.in"
  print $ part1
