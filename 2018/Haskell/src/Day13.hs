{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Day13 where
-- https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-13

import Lib ( parseAsciiMap )
import Control.Lens (makeLenses, over, view)
import Data.Bifunctor ( second )
import Data.Functor.Foldable (hylo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Linear ( V2(..), _x, _y )

type Point = V2 Int

data Turn
  = Slash
  | Backslash
  | Cross
  deriving (Show, Eq)

data Dir = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

data Cart = Cart
  { _dir :: Dir
  , _turns :: Int
  }
  deriving (Show)

makeLenses ''Cart

newtype ScanPoint = SP {_getSP :: Point}
  deriving (Eq, Show, Num)

instance Ord ScanPoint where
  compare =
    comparing (view _y . _getSP)
      <> comparing (view _x . _getSP)

type World = Map Point Turn
type Carts = Map ScanPoint Cart

parseWorld :: String -> (World, Carts)
parseWorld = second (Map.mapKeys SP)
           . Map.mapEither (second (`Cart` 0))
           . parseAsciiMap go
  where
    go = \case
      '/'  -> Just . Left  $ Slash
      '\\' -> Just . Left  $ Backslash
      '+'  -> Just . Left  $ Cross
      'v'  -> Just . Right $ DDown
      '^'  -> Just . Right $ DUp
      '>'  -> Just . Right $ DRight
      '<'  -> Just . Right $ DLeft
      _    -> Nothing

stepCart :: World -> ScanPoint -> Cart -> (ScanPoint, Cart)
stepCart w0 (SP p) c@(Cart di tu) = (SP p', maybe id turn (Map.lookup p' w0) c)
 where
  p' =
    p + case di of
      DUp -> V2 0 (-1)
      DDown -> V2 0 1
      DLeft -> V2 (-1) 0
      DRight -> V2 1 0
  turn = \case
    Slash -> over dir $ \case DLeft -> DDown; DDown -> DLeft; DUp -> DRight; DRight -> DUp
    Backslash -> over dir $ \case DLeft -> DUp; DDown -> DRight; DUp -> DLeft; DRight -> DDown
    Cross -> over turns (+ 1) . over dir (turnWith tu)
  turnWith i = case i `mod` 3 of
    0 -> turnLeft
    1 -> id
    _ -> turnLeft . turnLeft . turnLeft
  turnLeft DLeft = DDown
  turnLeft DDown = DRight
  turnLeft DUp = DLeft
  turnLeft DRight = DUp

data CartLog a
  = -- | Crash at Point
    Crash Point a
  | -- | Nothing happened
    Tick a
  | -- | Last cart at Point
    Done Point
  deriving (Show, Functor)

step :: World -> (Carts, Carts) -> CartLog (Carts, Carts)
step w (waiting, done) =
  case Map.minViewWithKey waiting of
    Nothing -> case Map.minViewWithKey done of
      Just ((SP lastPos, _), Map.null -> True) -> Done lastPos
      _ -> Tick (done, Map.empty)
    Just (uncurry (stepCart w) -> (sp, c), waiting') ->
      case Map.lookup sp (waiting' <> done) of
        Nothing -> Tick (waiting', Map.insert sp c done)
        Just _ -> Crash (_getSP sp) (Map.delete sp waiting', Map.delete sp done)

firstCrash :: CartLog (Maybe Point) -> Maybe Point
firstCrash (Crash p _) = Just p
firstCrash (Tick cs) = cs
firstCrash (Done _) = Nothing

lastCart :: CartLog Point -> Point
lastCart (Crash _ cs) = cs
lastCart (Tick cs) = cs
lastCart (Done p) = p

part1 :: World -> Carts -> Maybe Point
part1 w c = (firstCrash `hylo` step w) (c, Map.empty)

part2 :: World -> Carts -> Point
part2 w c = (lastCart `hylo` step w) (c, Map.empty)

main :: IO ()
main = do
  (world, carts) <- parseWorld <$> readFile "../data/day13.in"
  print $ part1 world carts
  print $ part2 world carts

-- 26,99
-- 62,48
