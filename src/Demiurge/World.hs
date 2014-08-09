{-# LANGUAGE MultiParamTypeClasses #-}
module Demiurge.World where

import Prelude hiding (all)
import Data.Foldable

import Demiurge.Data.Coordinate
import Demiurge.Utils

data Tile = Solid | Free deriving Eq

class (Coordinate b) => World a b where
    get :: a -> b -> Maybe Tile
    put :: a -> b -> Tile -> a

    oob :: a -> b -> Bool

    free :: a -> b -> Bool
    free w c = all (==Free) (get w c)

    adjWhere :: a -> (b -> Bool) -> b -> [b]
    adjWhere w p c =
        filter ((not . oob w) &&& p) $ adj c

