module Demiurge.World where

import Demiurge.Common

data Tile = Solid | Free

class World a where
    get :: World a => Cell -> Maybe Tile
    put :: World a => Cell -> Tile -> a
