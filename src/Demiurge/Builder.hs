{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Builder where

import Demiurge.Resource

data Builder c o t = Builder c (o t) t [Resource]

getPos :: Builder c o t -> c
getPos (Builder c _ _ _) = c

getOrd :: Builder c o t -> o t
getOrd (Builder _ ord _ _) = ord

getTsk :: Builder c o t -> t
getTsk (Builder _ _ tsk _) = tsk

move :: c -> Builder c o t -> Builder c o t
move c (Builder _ ord tsk rs) = Builder c ord tsk rs

order :: o t -> Builder c o t -> Builder c o t
order o (Builder c _ tsk rs) = Builder c o tsk rs

task :: t -> Builder c o t -> Builder c o t
task tsk (Builder c ord _ rs) = Builder c ord tsk rs

hasStone :: Builder c o t -> Bool
hasStone (Builder _ _ _ (Stone:_)) = True
hasStone _ = False

-- Because allow and perform are separate, we cannot avoid
-- checking the case where
spendStone :: Builder c o t -> Builder c o t
spendStone (Builder c ord tsk (Stone:xs)) = Builder c ord tsk xs
spendStone _ = undefined -- at least catch that case and die
