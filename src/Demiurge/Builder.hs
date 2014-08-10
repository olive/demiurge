{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Builder where

import Demiurge.Resource

data Builder c g o t = Builder c (g c) (o t) t [Resource]

getPos :: Builder c g o t -> c
getPos (Builder c _ _ _ _) = c

getGoal :: Builder c g o t -> g c
getGoal (Builder _ gol _ _ _) = gol

getOrd :: Builder c g o t -> o t
getOrd (Builder _ _ ord _ _) = ord

getTsk :: Builder c g o t -> t
getTsk (Builder _ _ _ tsk _) = tsk

move :: c -> Builder c g o t -> Builder c g o t
move c (Builder _ gol ord tsk rs) = Builder c gol ord tsk rs

goal :: g c -> Builder c g o t -> Builder c g o t
goal g (Builder c _ ord tsk rs) = Builder c g ord tsk rs

order :: o t -> Builder c g o t -> Builder c g o t
order o (Builder c gol _ tsk rs) = Builder c gol o tsk rs

task :: t -> Builder c g o t -> Builder c g o t
task tsk (Builder c gol ord _ rs) = Builder c gol ord tsk rs

collectStone :: Builder c g o t -> Builder c g o t
collectStone (Builder c gol ord tsk rs) = Builder c gol ord tsk (Stone:rs)

hasStone :: Builder c g o t -> Bool
hasStone (Builder _ _ _ _ (Stone:_)) = True
hasStone _ = False

-- Because allow and perform are separate, we cannot avoid
-- checking the case where
spendStone :: Builder c g o t -> Builder c g o t
spendStone (Builder c gol ord tsk (Stone:xs)) = Builder c gol ord tsk xs
spendStone _ = undefined -- at least catch that case and die
