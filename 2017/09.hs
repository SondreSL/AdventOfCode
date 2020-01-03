import Text.ParserCombinators.Parsec
import Data.Maybe (catMaybes)

data Tree = Group [Tree]
          | Garbage String
          deriving Show

parseTree :: String -> Tree
parseTree = either (error $ "Bad parse") id . parse parse' "Group"
  where parse' = parseGroup <|> parseGarbage
        parseGroup = Group <$> between (char '{') (char '}') (sepBy parse' (char ','))
        parseGarbage = Garbage <$> between (char '<') (char '>') (catMaybes <$> many garbageChar)
        garbageChar = 
          Nothing <$ (char '!' *> anyChar) <|> Just <$> noneOf ">"

score :: Tree -> Int
score = go 1
  where go n (Garbage _) = 0
        go n (Group xs) = n + (sum $ map (go (n+1)) xs)

countGarbage (Group xs) = sum $ map part2 xs
countGarbage (Garbage x) = length x

part1 :: Tree -> Int
part1 = score

part2 :: Tree -> Int
part2 = countGarbage

main = do
  input <- parseTree <$> readFile "data/09.in"
  print $ part1 input
  print $ part2 input

-- 11089
-- 5288
