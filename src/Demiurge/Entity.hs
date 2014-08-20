module Demiurge.Entity where

import Demiurge.Common
import qualified Demiurge.Tile as T

data Job = Builder | Gatherer | Miner
    deriving (Ord, Eq)

data Entity = Entity Int XYZ Job (Maybe T.Resource)

getJob :: Entity -> Job
getJob (Entity _ _ j _) = j



getResource :: Entity -> Maybe T.Resource
getResource (Entity _ _ _ rs) = rs

mapResource :: Entity -> (T.Resource -> Maybe T.Resource) -> Entity
mapResource (Entity i p j rs) f = Entity i p j (rs >>= f)

getPos :: Entity -> XYZ
getPos (Entity _ p _ _) = p

setPos :: XYZ -> Entity -> Entity
setPos xyz (Entity i _ j rs) = Entity i xyz j rs
