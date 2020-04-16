{-# LANGUAGE DataKinds #-}
import Data.Monoid
import Data.Modular
import Data.Semigroup
import Data.List.Extra
import GHC.TypeNats (KnownNat)

type Transform n = (Mod Integer n -> Mod Integer n)

parser :: KnownNat n => String -> [Transform n]
parser = map parse . lines
  where
    parse line = case words line of 
           ("cut":n:_)           -> \x -> x - (read n)
           ("deal":"into":_)     -> \x -> negate x
           ("deal":"with":_:n:_) -> \x -> x * (read n)

solveA :: [Transform 10007] -> Mod Integer 10007
solveA input = appEndo totalTrans 2019
  where
    totalTrans = foldMap Endo $ reverse input

solveB :: [Transform 119315717514047] -> Mod Integer 119315717514047
solveB input = appEndo bigTrans 2019
  where
    totalTrans = foldMap Endo $ reverse input
    bigTrans = stimes 101741582076661 totalTrans

main = do
  contents <- readFile "input-2019-22.txt"

  print . solveA . parser $ contents
  print . solveB . parser $ contents
