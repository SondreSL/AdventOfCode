{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
import Data.Finite
import Data.Group
import Data.Monoid
import Numeric.Modular
import Data.Semigroup
import Data.List.Extra
import GHC.TypeNats (KnownNat)
import Text.ParserCombinators.Parsec

-- data Transform n = T (Finite n -> Finite n)

-- instance Monoid (Transform n) where
--   mempty = T id

-- instance Semigroup (Transform n) where
--   T n <> T m = T (n . m)

data Linear n = L { mul :: Finite n
                  , add :: Finite n }

instance KnownNat n => Monoid (Linear n) where
  mempty = L 1 0

instance KnownNat n => Semigroup (Linear n) where
  L a b <> L x y = L (a * x) (a * y + b)

instance KnownNat n => Group (Linear n) where
  invert (L a b) = L a' b'
    where
      a' = recipNat a
      b' = negate (a' * b)

recipNat :: forall n. KnownNat n => Finite n -> Finite n
recipNat a = a ^ (maxBound @(Finite 119315717514047) - 1)

parser :: KnownNat n => String -> [Linear n]
parser = map parse . lines
  where
    parse line = case words line of
           ("cut":n:_)           -> L 1                   (negate (modulo $ read n))
           ("deal":"into":_)     -> L (negate 1)          (negate 1)
           ("deal":"with":_:n:_) -> L (modulo $ read n)   0

getIndex :: KnownNat n => Linear n -> Finite n -> Finite n
getIndex (L a b) n = a * n + b

solveA :: [Linear 10007] -> Finite 10007
solveA input = getIndex totalTrans 2019
  where
    totalTrans = mconcat $ reverse input

solveB :: [Linear 119315717514047] -> Finite 119315717514047
solveB input = getIndex (invert bigTrans) 2020
  where
    totalTrans = mconcat $ reverse input
    bigTrans = stimes 101741582076661 totalTrans

main :: IO ()
main = do
  contents <- readFile "data/input-2019-22.txt"

  print . getFinite . solveA . parser $ contents
  print . getFinite . solveB . parser $ contents

-- 2480
-- 102842916433010 too high
-- 62416301438548
