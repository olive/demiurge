{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Order where

--import Demiurge.Task

class Order o where
    next :: o t -> (o t, {-Maybe-}t)
    rewind :: o a -> o a
    noneO :: o a
    isNoneO :: o a -> Bool

data OrderList t = TaskList [t] [t] | Empty

olNone :: OrderList t -> Bool
olNone Empty = True
olNone _ = False

instance Order OrderList where
    next (TaskList i (x:xs)) = (TaskList i xs, x)
    next (TaskList _ []) = (Empty, undefined)
    next Empty = (Empty, undefined)

    rewind (TaskList i _) = TaskList i i

    noneO = Empty

    isNoneO = olNone
