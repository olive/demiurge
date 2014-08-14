module Demiurge.Tile(
    Resource(..),
    Tile,
    isStandable,
    isFree,
    isWalkable,
    isWholeSolid,
    getResources,
    addResource,
    takeResource,
    emptyMs
) where
data Resource = Stone deriving Eq

data TileState = WholeSolid | FloorSolid | Stair | Free deriving Eq
type MultiSet a = [a]
data Tile = Tile TileState (MultiSet Resource)

getState :: Tile -> TileState
getState (Tile ts _) = ts

isStandable :: Tile -> Maybe Tile -> Bool
isStandable t Nothing = inState t [FloorSolid, Stair]
isStandable t (Just r) = isState r Stair && inState t [Stair, Free]

inState :: Tile -> [TileState] -> Bool
inState t = elem (getState t)

isState :: Tile -> TileState -> Bool
isState t = (==) (getState t)

isFree :: Tile -> Bool
isFree t = isState t Free

isWholeSolid :: Tile -> Bool
isWholeSolid t = isState t WholeSolid

isWalkable :: Tile -> Bool
isWalkable t = isState t FloorSolid

getResources :: Tile -> MultiSet Resource
getResources (Tile _ rs) = rs

addResource :: Resource -> (Tile -> Tile)
addResource r (Tile ts rs) = Tile ts (r:rs)

takeResource :: Resource -> (Tile -> Tile)
takeResource _ (Tile ts (_:rs)) = Tile ts rs
takeResource _ t = t

emptyMs :: MultiSet a
emptyMs = []
