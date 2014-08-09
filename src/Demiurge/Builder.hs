{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Builder where



data Builder c o t = Builder c (o t) t

getPos :: Builder c o t -> c
getPos (Builder c _ _) = c

getOrd :: Builder c o t -> o t
getOrd (Builder _ ord _) = ord

getTsk :: Builder c o t -> t
getTsk (Builder _ _ tsk) = tsk

move :: c -> Builder c o t -> Builder c o t
move c (Builder _ ord tsk) = Builder c ord tsk

order :: o t -> Builder c o t -> Builder c o t
order o (Builder c _ tsk) = Builder c o tsk

task :: t -> Builder c o t -> Builder c o t
task tsk (Builder c ord _) = Builder c ord tsk

