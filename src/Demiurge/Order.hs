{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Demiurge.Order where

import Demiurge.Common
import Demiurge.Task
class Order o t where
    next :: o -> (o, Maybe (Cell -> t))
    rewind :: o -> o
    noneO :: o
    isNoneO :: o -> Bool

data OrderList t = TaskList [Cell->t] [Cell->t] | Empty



olNone :: OrderList t -> Bool
olNone Empty = True
olNone _ = False

instance Task t => Order (OrderList t) t where
    next (TaskList i (x:xs)) = (TaskList i xs, Just x)
    next (TaskList _ []) = (Empty, Nothing)
    next Empty = (Empty, Nothing)

    rewind (TaskList i _) = TaskList i i

    noneO = Empty

    isNoneO = olNone
