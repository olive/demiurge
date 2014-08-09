{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Demiurge.Order where

import Demiurge.Common

class Order o t | o -> t where
    next :: o t -> (o t, Maybe (Cell -> t))
    rewind :: o t -> o t
    noneO :: o t
    isNoneO :: o t -> Bool

data OrderList t = TaskList [Cell->t] [Cell->t] | Empty



olNone :: OrderList t -> Bool
olNone Empty = True
olNone _ = False

instance Order OrderList t where
    next (TaskList i (x:xs)) = (TaskList i xs, Just x)
    next (TaskList _ []) = (Empty, Nothing)
    next Empty = (Empty, Nothing)

    rewind (TaskList i _) = TaskList i i

    noneO = Empty

    isNoneO = olNone
