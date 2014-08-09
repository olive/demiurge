{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Blueprint where

import Demiurge.Order

class Blueprint a where
    generate :: a -> OrderList o

