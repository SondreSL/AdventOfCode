import Data.Char
import Data.Maybe
import Data.List.Extra
import qualified Data.Map.Strict as M

type Pos = (Int, Int)
type Tubes = M.Map Pos Object

data Dir = North | South | East | West
  deriving Show

data Object = Path
            | Crossing
            | Letter Char
            | Empty
            deriving (Show, Eq)

isLet :: Object -> Bool
isLet (Letter _) = True
isLet _          = False

parse :: String -> Tubes
parse input = M.fromList . filter ((/=Empty) . snd) . map mkObjects . zip cs . concat $ path
  where 
    path = lines input
    cs = [(x,y) | y <- [0..(length path) - 1], x <- [0..(length (head path)) - 1]]

mkObjects :: (Pos, Char) -> (Pos, Object)
mkObjects (pos, ch)
  | isLetter ch = (pos, Letter ch)
  | ch == '+' = (pos, Crossing)
  | ch == '-' = (pos, Path)
  | ch == '|' = (pos, Path)
  | ch == ' ' = (pos, Empty)

next :: Pos -> Dir -> Pos
next (x,y) North = (x,y-1)
next (x,y) South = (x,y+1)
next (x,y) East  = (x+1,y)
next (x,y) West  = (x-1,y)

newDir :: Tubes -> Pos -> Dir -> Dir
newDir path (x,y) dir =
  case M.findWithDefault Empty (x,y) path of
    Crossing -> case dir of
                  North -> if M.member (x-1,y) path then West else East
                  South -> if M.member (x-1,y) path then West else East
                  East  -> if M.member (x,y+1) path then South else North
                  West  -> if M.member (x,y+1) path then South else North
    _        -> dir

move :: Tubes -> Pos -> Dir -> [Object]
move path pos dir = takeWhile (/=Empty) $ go pos dir
  where go pos dir = let curr = M.findWithDefault Empty pos path 
                         dir' = newDir path pos dir 
                         pos' = next pos dir' 
                       in curr : go pos' dir'

startPos :: Tubes -> Pos
startPos = fromJust . find ((==0) . snd) . M.keys

part1 :: Tubes -> String
part1 path = map (\(Letter c) -> c) . filter isLet $ move path (startPos path) South

part2 :: Tubes -> Int
part2 path = length $ move path (startPos path) South

main = do
  input <- parse <$> readFile "data/19.in"
  print $ part1 input
  print $ part2 input

