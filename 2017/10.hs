import Numeric
import Text.Printf
import Data.Char
import Data.Bits
import Data.Word
import Control.Lens
import Data.List.Extra
import qualified Data.Vector as V 

data Knot = K { _vec :: V.Vector Word8
              , _pos :: Word8
              , _skip :: Word8
              }

tie :: Knot -> Word8 -> Knot
tie (K v p s) n = K v' p' s'
  where 
    ixs = fromIntegral . (+ p) <$> init [0..n]
    vals = map (v V.!) ixs
    v' = v V.// zip ixs (reverse vals)
    p' = p + s + n
    s' = s + 1

process :: [Word8] -> V.Vector Word8
process = _vec . foldl tie (K (V.fromList [0..255]) 0 0)

knot :: String -> [Word8]
knot = map (foldr1 xor) 
      . chunksOf 16
      . V.toList
      . process
      . concat . replicate 64
      . (++ [17, 31, 73, 47, 23])
      . map (fromIntegral . ord) . init

part1 :: String -> Int
part1 = product 
      . V.map fromIntegral . V.take 2 
      . process 
      . map read . splitOn "," . init

part2 :: String -> String
part2 = concatMap (printf "%02x") . knot

main = do
  input <- readFile "data/10.in"

  print $ part1 input
  print $ part2 input
