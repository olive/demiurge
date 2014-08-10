{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}

module Demiurge.Data.Graph where

import Prelude hiding (any)
import Control.Applicative((<$>))
import Data.Maybe
import Data.Foldable

import Demiurge.Common
import Demiurge.Data.Array2d
import Demiurge.Data.Coordinate
import Demiurge.World
-- a -- collection type
-- b -- the node type
-- c -- coordinate type
class Coordinate c => Graph a b c | a -> c where
    neighbors :: a b -> c -> [(c, Float)]


instance Graph Array2d Tile Cell where
    neighbors arr c = (\x -> (x, 1)) <$> (catMaybes $ checkSolid <$> adj c)
        where checkSolid x = if ((not . inRange arr) x) && any isFree (geti arr x) then Just x else Nothing
