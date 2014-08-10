{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Demiurge.World where

import Prelude hiding (all)
import Control.Applicative((<$>))
import Data.Foldable
import Data.Maybe

import Demiurge.Data.Coordinate
import Demiurge.Data.Array2d
import Demiurge.Utils
import Demiurge.Common
import Demiurge.Resource

data TileType = Solid | Free deriving Eq

data Tile = Tile TileType [LatentResource]

getType :: Tile -> TileType
getType (Tile t _) = t

setType :: TileType -> Tile -> Tile
setType t' (Tile _ rs) = Tile t' rs

isFree :: Tile -> Bool
isFree = (==Free).getType

class (Coordinate b) => World a b where
    get :: a -> b -> Maybe Tile
    put :: a -> b -> Tile -> a
    mod :: a -> b -> (Tile -> Tile) -> a
    oob :: a -> b -> Bool

    free :: a -> b -> Bool
    free w c = all isFree (get w c)

    adjWhere :: a -> (b -> Bool) -> b -> [b]
    adjWhere w p c =
        filter ((not . oob w) &&& p) $ adj c

instance World (Array2d Tile) Cell where
    get arr cell = geti arr cell
    put arr cell t = puti arr cell t
    oob arr cell = (not . inRange arr) cell
    mod arr cell f = let tile = get arr cell in
                     fromMaybe arr $ (\t -> puti arr cell (f t)) <$> tile

