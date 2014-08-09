{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Builder where

import Demiurge.Data.Coordinate
import Demiurge.World

type OrderPool o = [o]

-- take a builder and do their current task
manageTask :: (c ~ GetC t, World w c, Order o, Task t)
           => Builder c o t
           -> w
           -> OrderPool o
           -> (Builder c o t, w, OrderPool o)
manageTask b@(Builder xy _ t) w pool =
    if allowed t b w
    then perform t b w pool
    else (Builder xy noneO noneT, w, pool)

-- check to see that a builder has orders. if not, give them one if one is available
manageOrder :: (Order o, Task t)
            => Builder c o t
            -> OrderPool o
            -> (Builder c o t, OrderPool o)
manageOrder b pool =
    if ((isNoneO $ getOrd b) && (isNoneT $ getTsk b))
    then giveOrd b pool
    else (b, pool)

giveOrd :: Order o => Builder c o t -> OrderPool o -> (Builder c o t, OrderPool o)
giveOrd b [] = (b, [])
giveOrd b (x:xs) = (order x b, xs)


data Thing a = Move a
             | Path [a]
             | None


class Task t where
    type GetC t
    perform :: (c ~ GetC t, World w c)
            => t
            -> Builder c o t
            -> w
            -> OrderPool o
            -> (Builder c o t, w, OrderPool o)
    allowed :: (c ~ GetC t, World w c) => t -> Builder c o t -> w -> Bool
    noneT :: t
    isNoneT :: t -> Bool

thingNone :: Thing a -> Bool
thingNone None = True
thingNone _ = False

instance Task (Thing a) where

    type GetC (Thing a) = a
    noneT = None
    isNoneT = thingNone
    allowed (Move pos) _ w = free w pos
    perform (Move pos) b w pool = (move pos b, w, pool)

class Order o where
    next :: Task t => o -> (o, t)
    rewind :: o
    noneO :: o
    isNoneO :: o -> Bool

data Builder c o t = Builder c o t

finishO :: (Coordinate c, Order o) => Builder c o t -> Builder c o t
finishO = order noneO

getOrd :: Builder c o t -> o
getOrd (Builder _ ord _) = ord

getTsk :: Builder c o t -> t
getTsk (Builder _ _ tsk) = tsk


order :: Order o => o -> Builder c o t -> Builder c o t
order o (Builder c _ tsk) = Builder c o tsk

move :: c -> Builder c o t -> Builder c o t
move c (Builder _ ord tsk) = Builder c ord tsk

task :: Task t => t -> Builder c o t -> Builder c o t
task tsk (Builder c ord _) = Builder c ord tsk

