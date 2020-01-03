import Data.Maybe
import Data.Char
import Data.List.Extra
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

data Program = P { _registers :: M.Map String Int
                 , _ip        :: Int
                 , _input     :: [Network]
                 , _commands  :: Commands
                 }

type Command = [String]
type Commands = V.Vector Command

data Network = Send Int | Receive
  deriving Show

isSend :: Network -> Bool
isSend (Send i) = True
isSend _        = False

drop1rec :: [Network] -> [Network]
drop1rec xs = takeWhile isSend xs ++ tail (dropWhile isSend xs)

unwrap :: Network -> Int
unwrap (Send i) = i

parse :: String -> [Command]
parse = map words . lines

run :: Program -> [Network]
run p@(P regs ip input ops) = 
  if ip >= length ops || ip < 0
     then [] 
     else let valid = all (\x -> isDigit x || x == '-')
              value x = if valid x then read x else curr x
              curr x  = M.findWithDefault 0 x regs
              new r x = p{ _registers = M.insert r x regs, _ip = ip + 1 }
              op      = ops V.! ip
           in case op of
                ["set", reg, val] -> run $ new reg (value val)
                ["add", reg, val] -> run $ new reg (curr reg + value val)
                ["mul", reg, val] -> run $ new reg (curr reg * value val)
                ["mod", reg, val] -> run $ new reg (curr reg `mod` value val)
                ["jgz", reg, val] -> run $ P regs (if value reg > 0 then ip + value val else ip + 1) input ops
                ["snd", val]      -> Send (value val) : (run $ P regs (ip + 1) (drop1rec input) ops)
                ["rcv", val]      -> Receive : case input of
                                                 (Send i):rest -> run $ P (M.insert val i regs) (ip + 1) rest ops
                                                 _             -> []

part1 :: Commands -> Int
part1 = unwrap . last . takeWhile isSend . run . P M.empty 0 []

part2 :: Commands -> Int
part2 cmds = 
  let outa = run $ P (M.singleton "p" 0) 0 outb cmds
      outb = run $ P (M.singleton "p" 1) 0 outa cmds
   in length $ filter isSend outb

main = do
  input <- V.fromList . parse <$> readFile "data/18.in"
  print $ part1 input
  print $ part2 input

-- 3423
-- 7493
