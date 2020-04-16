{-# LANGUAGE DataKinds #-}
import Data.Finite
import Data.Group
import Data.Monoid
import Data.Modular
import Data.Semigroup
import Data.List.Extra
import GHC.TypeNats (KnownNat)
import Text.ParserCombinators.Parsec

data Transform n = T (Finite n -> Finite n)

instance Monoid (Transform n) where
  mempty = T id

instance Semigroup (Transform n) where
  T n <> T m = T (n . m)

parser :: KnownNat n => String -> [Transform n]
parser = map parse . lines
  where
    parse line = case words line of 
           ("cut":n:_)           -> T $ \x -> x - (modulo $ read n)
           ("deal":"into":_)     -> T $ \x -> maxBound - x
           ("deal":"with":_:n:_) -> T $ \x -> x * (modulo $ read n)

getIndex :: KnownNat n => Transform n -> Finite n -> Finite n
getIndex (T f) x = f x

solveA :: [Transform 10007] -> Finite 10007
solveA input = getIndex totalTrans 2019
  where
    totalTrans = mconcat $ reverse input

solveB :: [Transform 119315717514047] -> Finite 119315717514047
solveB input = getIndex bigTrans 2020
  where
    totalTrans = mconcat $ reverse input
    bigTrans = stimes 101741582076661 totalTrans

main = do
  contents <- readFile "input-2019-22.txt"

  print . getFinite . solveA . parser $ contents
  print . getFinite . solveB . parser $ contents
