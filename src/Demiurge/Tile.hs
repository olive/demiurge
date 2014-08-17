module Demiurge.Tile(
    Resource(..),
    TileState(..),
    Tile(..),
    render,
    isStandable,
    isFree,
    isWalkable,
    isWholeSolid,
    getResources,
    addResource,
    takeResource,
    emptyMs
) where
import qualified Antiqua.Graphics.Tile as T
import Antiqua.Graphics.Color
import Antiqua.Data.CP437
data Resource = Stone deriving Eq

data TileState = WholeSolid | FloorSolid | Stair | Free deriving Eq
type MultiSet a = [a]
data Tile = Tile TileState (MultiSet Resource)

render :: Tile -> Maybe Tile -> T.Tile CP437
render (Tile tt _) below =
    case (tt, below) of
        (WholeSolid,_) -> T.Tile (:≡)  darkGrey grey
        (FloorSolid,_) -> T.Tile C'DoubleQuote darkGreen green
        (Stair,_) -> T.Tile C'X black brown
        (Free, Just (Tile Stair _)) -> T.Tile C'v black brown
        (Free, Just (t@(Tile FloorSolid _))) -> T.Tile (:.) black $ T.getFg $ render t Nothing
        (Free, _) -> T.Tile C'Space black black


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
