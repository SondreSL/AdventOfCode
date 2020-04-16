{-# LANGUAGE DeriveFunctor #-}
import           Data.Maybe (fromJust)
import           Control.Lens
import           Data.List.Extra (delete, splitOn)
import qualified Data.IntMap.Strict as Map

type Piece = (Int, Int)
type Chain = [Piece]
type Pool = Map.IntMap [Int]

parse :: String -> Pool
parse = presort . over each ((\[x, y] ->  (read x, read y)) . splitOn "/") . lines

addPiece :: Piece -> Pool -> Pool
addPiece (m, n) = if m /= n 
                     then add m n . add n m
                     else add m n
    where 
      add m n pool = 
        case Map.lookup m pool of
          Nothing  -> Map.insert m [n] pool
          Just lst -> Map.insert m (n : lst) pool

removePiece :: Piece -> Pool -> Pool
removePiece (m, n) = if m /= n
                     then rem m n . rem n m
                     else rem m n
  where
    rem :: Int -> Int -> Pool -> Pool
    rem m n pool = 
      case fromJust $ Map.lookup m pool of
        []  -> Map.delete m pool
        lst -> Map.insert m (delete n lst) pool

presort :: [Piece] -> Pool
presort = foldr addPiece Map.empty

data TreeF a = NodeF Int [a] 
  deriving (Show, Functor)

newtype Fix f = Fix { unfix :: f (Fix f) }

type Tree = Fix TreeF
type Coalgebra f a = a -> f a
type Algebra   f a = f a -> a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana f = Fix . fmap (ana f) . f

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo cat an = cat . fmap (hylo cat an) . an 

grow :: Coalgebra TreeF (Int, Pool)
grow (n, pool) =
  case Map.lookup n pool of
    Nothing -> NodeF n []
    Just xs -> NodeF n [(x, removePiece (n, x) pool) | x <- xs ]

tree = ana grow

chainAlg :: Algebra TreeF (Int, [Chain])
chainAlg (NodeF n []) = (n, [])
chainAlg (NodeF n lst) = (n, concat [push (n, m) bs | (m, bs) <- lst])
  where
    push :: Piece -> [Chain] -> [Chain]
    push (n, m) [] = [[(n, m)]]
    push (n, m) bs = [(n, m) : br | br <- bs]

score :: Chain -> Int
score = sumOf (each . each)

part1 :: Pool -> Int
part1 pool = maximum . fmap score . snd $ hylo chainAlg grow (0, pool)

part2 :: Pool -> Int
part2 pool = maximum . fmap score . filter ((maxLen ==) . length) $ chains
  where
    (_, chains) = hylo chainAlg grow (0, pool)
    maxLen = maximum $ fmap length chains

solve :: Pool -> IO ()
solve pool = do
  xs <- pure . snd $ hylo chainAlg grow (0, pool)
  let maxLen = maximum $ fmap length xs
  print $ maximum . fmap score $ xs
  print $ maximum . fmap score . filter ((maxLen ==) . length) $ xs
  

main :: IO ()
main = do
  input <- parse <$> readFile "data/24.in"
  solve input
