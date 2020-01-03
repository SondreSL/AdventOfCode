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

instance Show Program where
  show (P reg ip inp com) = "P { " ++ show ip ++ ", " ++ show (M.toList reg) ++ " }"

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

run :: Program -> Program
run p@(P regs ip input ops) = 
  if (ip >= length ops) || ip < 0
     then p
     else let valid = all (\x -> isDigit x || x == '-')
              value x = if valid x then read x else curr x
              curr x  = M.findWithDefault 0 x regs
              new r x = p{ _registers = M.insert r x regs, _ip = ip + 1 }
              op      = ops V.! ip
           in case op of
                ["set", reg, val] -> new reg (value val)
                ["mul", reg, val] -> new reg (curr reg * value val)
                ["jnz", reg, val] -> P regs (if value reg /= 0 then ip + value val else ip + 1) input ops
                ["sub", reg, val] -> new reg (curr reg - value val)
                a                 -> error $ show a

start :: [Command] -> Program
start = P M.empty 0 [] . V.fromList

part1 :: [Command] -> Int
part1 = count isMul . findFix . iterate run . start
  where
    findFix (p:q:xs) = if _ip p == _ip q then [] else p : findFix (q:xs)
    count f = length . filter f
    isMul (P _ ip _ cmds) = ("mul" ==) . head . (V.! ip) $ cmds

part2 :: [Command] -> [Program]
part2 = take 100000 . iterate run . P (M.singleton "a" 1) 0 [] . V.fromList

main = do
  input <- parse <$> readFile "data/23.in"
  print $ part1 input
  mapM_ print $ part2 input

-- 3969
-- 917 (number of non-primes in [106500,(106500 + 17)..123500]
--

-- Translation of assembly
-- b = 106500
-- c = 123500
-- 
-- do:
--     f = 1
--     d = 2
-- 
--     outer:
--     e = 2
--     
--     inner:
-- 
--     if d * e == b:
--         f = 0
-- 
--     e += 1
-- 
--     if e /= b:
--         GOTO inner;
--     
--     d += 1
-- 
--     if d /= b:
--         GOTO outer;
-- 
--     if f == 0:
--         rv += 1 -- Want final value of rv
--     
--     if b == c:
--         END
--     
--     b += 17
-- 
-- while True;
-- 
-- -- 917 (number of non-primes in [106500,(106500 + 17)..123500]
