import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Command = Command (Int -> Int -> Int) String Int (Int -> Int -> Bool) String Int

instance Show Command where
  show (Command arit reg i op t j) = "Command " ++ show reg

type Registers = Map String Int

toCommand :: [String] -> Command
toCommand [ch, ar, i, _, t, op, j] = Command (arit ar) ch (read i) (func op) t (read j)

func :: String -> (Int -> Int -> Bool)
func a = case a of
            ">" -> (>) 
            "<" -> (<) 
            ">=" -> (>=) 
            "<=" -> (<=) 
            "==" -> (==)
            "!=" -> (/=)

arit :: String -> (Int -> Int -> Int)
arit a = case a of
            "dec" -> (-)
            "inc" -> (+)

eval :: Registers -> Command -> Registers
eval regs (Command arit reg i op t j) = 
  if op (M.findWithDefault 0 t regs) j 
     then let old = M.findWithDefault 0 reg regs 
           in M.insert reg (arit old i) regs 
     else regs

solveA :: [Command] -> Int
solveA input = maximum . foldl eval M.empty $ input

solveB :: [Command] -> Int
solveB input = maximum . map maximum . tail . scanl eval M.empty $ input

main = do
  contents <- map (toCommand . splitOn " ") . lines <$> readFile "data/08.in"
  print $ solveA contents
  print $ solveB contents
