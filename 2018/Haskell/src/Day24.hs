module Day24 where

import Data.List.Extra ((\\), maximumOn)
data Team = T
  { _units :: Int
  , _hp :: Int
  , _weakness :: [Trait]
  , _immune :: [Trait]
  , _ad :: Int
  , _attack :: Trait
  , _initiative :: Int
  }
  deriving (Show, Eq, Ord)

data Trait
  = Fire
  | Cold
  | Bludgeoning
  | Radiation
  | Slashing
  deriving (Show, Eq, Ord, Enum, Bounded)

immune :: [Team]
immune =
  [ T 3135 10795 [Fire] [] 27 Bludgeoning 19
  , T 2318 5166 [] [] 21 Fire 9
  , T 902 2807 [] [Cold, Bludgeoning] 31 Radiation 6
  , T 370 8764 [] [Bludgeoning] 218 Bludgeoning 10
  , T 1610 2916 [] [Fire] 14 Slashing 17
  , T 34 9413 [] [Cold] 2653 Fire 1
  , T 4321 9448 [Radiation] [] 20 Slashing 2
  , T 718 7439 [Slashing] [] 95 Bludgeoning 5
  , T 6317 5015 [Fire] [] 7 Slashing 15
  , T 1238 1805 [Slashing, Cold] [Bludgeoning] 13 Bludgeoning 14
  ]

infection :: [Team]
infection =
  [ T 1846 11138 [] [Slashing, Radiation] 11 Radiation 13
  , T 4982 12496 [Cold] [Radiation, Bludgeoning] 4 Cold 7
  , T 502 37572 [Radiation, Bludgeoning] [] 144 Bludgeoning 4
  , T 3460 22756 [Fire] [Slashing] 12 Fire 3
  , T 1534 34356 [Fire, Cold] [] 40 Bludgeoning 16
  , T 3476 37466 [Radiation] [Cold] 20 Slashing 12
  , T 288 51427 [Cold] [Radiation, Slashing] 321 Radiation 18
  , T 70 16923 [Slashing] [] 358 Cold 11
  , T 639 31233 [] [Radiation] 72 Slashing 8
  , T 1087 31406 [Fire] [Slashing] 53 Slashing 20
  ]

targetSelection ::
  -- | Immunes
  [Team] ->
  -- | Infection
  [Team] ->
  -- | (Attacker, defender)
  [(Team, Team)]
targetSelection [] [] = []
targetSelection att def =
  let (at, restAtt) = firstAttacker att
      (de, restDef) = mostDamage at def
   in (at, de) : targetSelection restAtt restDef

mostDamage :: Team -> [Team] -> (Team, [Team])
mostDamage t xs = (target, xs \\ [target])
  where target = maximumOn (damage t) xs

firstAttacker :: [Team] -> (Team, [Team])
firstAttacker xs = (att, xs \\ [att])
  where att = maximumOn (\(T u _ _ _ ad _ init) -> (u * ad, init)) xs

attackPower :: Team -> Int
attackPower t = _ad t * _units t

damage :: Team -> Team -> Int
damage at def
  | _attack at `elem` _immune def = 0
  | _attack at `elem` _weakness def = attackPower at * 2
  | otherwise = attackPower at

test1, test2 :: Team
test1 = T 17 5390 [Radiation, Bludgeoning] [] 4507 Fire 2
test2 = T 801 4706 [Radiation] [] 116 Bludgeoning 1

attackPhase ::
  -- | (Attacker, defender)
  [(Team, Team)] ->
  -- | Remnants of (immune, infection)
  ([Team], [Team])
attackPhase = undefined

main :: IO ()
main = do
  -- mapM_ print immune
  mapM_ print $ targetSelection immune infection
