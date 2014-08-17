module Demiurge.Drawing.Renderable where

import Antiqua.Graphics.TileRenderer
import Antiqua.Data.CP437
import Antiqua.Graphics.Tile
import Antiqua.Common

class Renderable a where
    render :: a -> TR XY (Tile CP437) -> TR XY (Tile CP437)
