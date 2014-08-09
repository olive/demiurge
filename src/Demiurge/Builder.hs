{-# LANGUAGE RankNTypes #-}
module Demiurge.Builder where

import Demiurge.Data.Coordinate
import Demiurge.World
import Demiurge.Utils

type OrderPool o = [o]

manageTask :: (World w c, Order o, Task t)
           => Builder c o t
           -> w
           -> OrderPool o
           -> (Builder c o t, w, OrderPool o)
manageTask b@(Builder xy ord t) w pool =
    if allowed b w t
    then perform b w |@| pool
    else (Builder xy noneO noneT, w, ord:pool)

class Task t where
    perform :: World w c
            => Builder c o t
            -> w
            -> (Builder c o t, w)
    allowed :: World w c => Builder c o t -> w -> t -> Bool
    noneT :: t

class Order o where
    next :: Task t => o -> (o, t)
    rewind :: o
    noneO :: o

data Builder c o t = Builder c o t

finishO :: (Coordinate c, Order o) => Builder c o t -> Builder c o t
finishO = order noneO

order :: Order o => o -> Builder c o t -> Builder c o t
order o (Builder c _ tsk) = Builder c o tsk

move :: Coordinate c => c -> Builder c o t -> Builder c o t
move c (Builder _ ord tsk) = Builder c ord tsk

task :: Task t => t -> Builder c o t -> Builder c o t
task tsk (Builder c ord _) = Builder c ord tsk

