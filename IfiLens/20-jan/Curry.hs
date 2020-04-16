{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}
import Prelude hiding (Bool(..))

aa :: a -> a
aa a = a

aba :: a -> b -> a
aba = const

data Or a b = OrL a | OrR b

data And a b = And a b

andL :: And a b -> a
andL (And a b) = a

andR :: And a b -> b
andR (And a b) = b

orA :: Or a a -> a
orA (OrL a) = a
orA (OrR a) = a

-- p -> p or p
pp :: a -> Or a a
pp a = OrL a

-- !(p & !p) 
andAnotA :: Not (And a (Not a))
andAnotA = undefined

data True = True

data False

type Not a = a -> False


-- F -> p
absurd :: False -> a
absurd x = case x of {}

-- a -> !!a
a2notNotA :: a -> (a -> False) -> False
a2notNotA a a2False = a2False a

-- !!a -> a
notNotA :: ((a -> False) -> False) -> a
notNotA = undefined

noe :: Or a False -> a
noe = undefined

-- ?
implOr :: (a -> b) -> Or (Not a) b
implOr = undefined

orImpl :: Or (Not a) b -> (a -> b)
orImpl = undefined

law1 :: Not (Or a b) -> And (Not a) (Not b)
--law1 :: ((Or a b) -> False) -> And (a -> False) (b -> False)
law1 orAB2Void = undefined

law2 :: And (Not a) (Not b) -> Not (Or a b)
law2 = undefined

law3 :: Not (And a b) -> Or (Not a) (Not b)
law3 = undefined
law4 :: Or (Not a) (Not b) -> Not (And a b)
law4 = undefined

main = print 0
