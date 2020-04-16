import Data.Group
import Data.Monoid
import Data.Semigroup

f = Endo (+1)


data Add = A Int
  deriving Show

instance Monoid Add where
  mempty = A 0

instance Semigroup Add where
  A a <> A b = A (a + b)

instance Group Add where
  invert (A i) = A (negate i)

applyAdd :: Add -> Int -> Int
applyAdd (A n) i = n + i

main = do
  print $ applyAdd (invert (stimes 10000000000 (A 2))) 5
