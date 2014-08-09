{-# LANGUAGE MultiParamTypeClasses #-}

module Demiurge.Data.Graph where

import Demiurge.Data.Coordinate

-- a -- collection type
-- b -- coordinate type
class Coordinate b => Graph a b where
    neighbors :: a -> b -> [(b, Float)]
