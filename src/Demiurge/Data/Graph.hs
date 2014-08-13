{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}

module Demiurge.Data.Graph where

import Prelude hiding (any)

import Demiurge.Data.Coordinate
-- a -- collection type
-- b -- the node type
-- c -- coordinate type
class Coordinate c => Graph a b c | a -> c where
    neighbors :: a b -> c -> [(c, Float)]

