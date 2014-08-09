{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Task where
import Demiurge.World
import Demiurge.Builder
import Demiurge.Order
import Demiurge.Common
data Thing a = Move a
             | Build a
             | None


class Task t where
    type GetC t
    perform :: (c ~ GetC t, World w c)
            => t
            -> Builder c o t
            -> w
            -> OrderPool o t
            -> (Builder c o t, w, OrderPool o t)
    allowed :: (c ~ GetC t, World w c)
            => t
            -> Builder c o t
            -> w
            -> Bool
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
    allowed (Build pos) _ w = free w pos
    allowed None _ _ = True
    perform (Move pos) b w pool = (move pos b, w, pool)
    perform (Build pos) b w pool = (b, put w pos Solid, pool)
    perform None b w pool = (b, w, pool)

