{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Demiurge.Order where

import Demiurge.Common

class Order o t | o -> t where
    next :: o t -> (o t, Maybe (Cell -> t))
    noneO :: o t
    isNoneO :: o t -> Bool

data OrderList t = TaskList [Cell->t] | Empty



olNone :: OrderList t -> Bool
olNone Empty = True
olNone _ = False

instance Order OrderList t where
    next (TaskList (x:xs)) = (TaskList xs, Just x)
    next (TaskList []) = (Empty, Nothing)
    next Empty = (Empty, Nothing)

    noneO = Empty

    isNoneO = olNone
