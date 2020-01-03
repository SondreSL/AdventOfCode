import Data.Function
import Data.List.Extra
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec

type VecString = V.Vector Char

readOps :: String -> [(VecString -> VecString)]
readOps input = either (error "Bad parse") id . parse (sepBy ops (char ',')) "Ops" $ filter (/='\n') input
  where
    ops = parseSpin <|> parsePartner <|> parseExchange
    parseExchange = ex <$> (char 'x' *> sepBy (many1 digit) (char '/'))
    parsePartner = pa <$> (char 'p' *> sepBy anyChar (char '/'))
    parseSpin = spin . read <$> (char 's' *> many1 digit)
    ex [a,b] = exchange (read a) (read b)
    pa [a,b] = partner a b

spin :: Int -> VecString -> VecString
spin n xs = V.fromList . take len . drop inv . cycle $ V.toList xs
  where
    len = length xs
    inv = len - n

exchange :: Int -> Int -> VecString -> VecString
exchange a b vec = vec V.// [(a, vec V.! b), (b, vec V.! a)]

partner :: Char -> Char -> VecString -> VecString
partner a b vec = exchange ia ib vec
  where 
    (Just ia) = V.findIndex (==a) vec
    (Just ib) = V.findIndex (==b) vec

findRep :: [(VecString -> VecString)] -> Int
findRep = (+1) 
        . length 
        . takeWhile ((/="abcdefghijklmnop") . V.toList) 
        . tail 
        . scanl (&) (V.fromList "abcdefghijklmnop") 
        . cycle

part1 :: [(VecString -> VecString)] -> VecString
part1 = foldl (&) (V.fromList "abcdefghijklmnop")

part2 :: [(VecString -> VecString)] -> VecString
part2 input = part1 $ take rep (cycle input)
  where rep = 1000000000 `mod` findRep input

main = do
  input <- readOps <$> readFile "data/16.in"
  print $ part1 input
  print $ part2 input

-- "bijankplfgmeodhc"
-- "bpjahknliomefdgc"
