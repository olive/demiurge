{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Builder where

import Demiurge.Data.Coordinate
import Demiurge.Order



data Builder c o t = Builder c (o t) t

finishO :: (Coordinate c, Order o) => Builder c o t -> Builder c o t
finishO = order noneO

getOrd :: Builder c o t -> o t
getOrd (Builder _ ord _) = ord

getTsk :: Builder c o t -> t
getTsk (Builder _ _ tsk) = tsk

order :: Order o => o t -> Builder c o t -> Builder c o t
order o (Builder c _ tsk) = Builder c o tsk

move :: c -> Builder c o t -> Builder c o t
move c (Builder _ ord tsk) = Builder c ord tsk

task :: t -> Builder c o t -> Builder c o t
task tsk (Builder c ord _) = Builder c ord tsk

