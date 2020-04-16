{-# LANGUAGE DeriveFunctor #-}
import Data.Functor.Foldable

data NatF a = Z | S a
  deriving Functor

-- newtype Fix f = In { unfix :: f (Fix f) }

type Nat = Fix NatF

type Coalgebra f a = a   -> f a
type Algebra   f a = f a -> a

toPeano :: Int -> NatF Int
toPeano 0 = Z
toPeano n = S (n - 1)

fromPeano :: NatF Int -> Int
fromPeano Z     = 0
fromPeano (S n) = n + 1

one :: Nat
one = Fix (S (Fix Z))

main :: IO ()
main = do
  print $ hylo fromPeano toPeano 100
  print $ cata fromPeano one
