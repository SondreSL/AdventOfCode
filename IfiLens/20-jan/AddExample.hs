import Data.Group
import Data.Semigroup

data Add = A Int
  deriving Show

-- Monoid :: a -> a
-- mempty = id
instance Monoid Add where
  mempty = A 0

instance Semigroup Add where
  A i <> A j = A (i + j)

instance Group Add where
  invert (A i) = A (negate i)

appAdd :: Add -> Int -> Int
appAdd (A i) j = (i + j)

main = do
  print $ appAdd (stimes 1000000000000 (A 5)) 10
  -- print $ appEndo (stimes 1000000000 (Endo (+5))) 10
