{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demiurge.World where

import Prelude hiding (any)
import Control.Applicative((<$>))
import Data.Foldable(any)
import Data.Maybe
import Control.Monad (join)

import Antiqua.Game
import Antiqua.Graphics.Renderer
import Antiqua.Graphics.TileRenderer
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Graphics.Window
import Antiqua.Graphics.Assets
import qualified Antiqua.Input.Controls as C
import Antiqua.Common
import Antiqua.Data.Graph
import qualified Antiqua.Pathing.Dijkstra as D
import qualified Antiqua.Graphics.Tile as Antiqua
import Antiqua.Data.CP437
import Antiqua.Graphics.Color

import Demiurge.Data.Coordinate
import qualified Demiurge.Tile as T
import Demiurge.Common
import qualified Demiurge.Data.Array3d as A3D
import qualified Demiurge.Drawing.Renderable as Demiurge
import Demiurge.Utils
data Job = Builder | Gatherer | Miner

data Task = Drop | Gather
data Goal = Build XYZ
type Reason = String


data Working
data Idle
data Worker k where
    Employed :: Int              -- | id
             -> XYZ              -- | position
             -> Job              -- | job
             -> Maybe T.Resource -- | inventory
             -> Goal             -- | current goal
             -> NonEmpty Task    -- | current tasks
             -> Worker Working
    Unemployed :: Int              -- | id
               -> XYZ              -- | position
               -> Job              -- | job
               -> Maybe T.Resource -- | inventory
               -> Reason           -- | why unemployed
               -> Worker Idle

data EWorker = WorkingWorker (Worker Working)
             | IdleWorker (Worker Idle)

getPos :: EWorker -> XYZ
getPos (WorkingWorker (Employed _ pos _ _ _ _)) = pos
getPos (IdleWorker (Unemployed _ pos _ _ _)) = pos

jobToTile :: Job -> Antiqua.Tile CP437
jobToTile j =
    case j of
        Builder -> Antiqua.Tile C'B black white
        Gatherer -> Antiqua.Tile C'G black white
        Miner -> Antiqua.Tile C'M black white

instance Demiurge.Renderable EWorker where
    render (WorkingWorker (Employed _ pos job _ _ _)) tr =
        let t1 = (drop3 pos, jobToTile job) in
        tr <+ t1
    render (IdleWorker (Unemployed _ pos job _ _)) tr =
        let t1 = (drop3 pos, jobToTile job) in
        let t2 = (drop3 $ pos |+| (0,-1,0),  Antiqua.Tile (:?) black white) in
        tr <++ [t1, t2]



takeItem :: T.Resource -> Worker k -> Worker k
takeItem r (Employed i pos job _ gol tsks) = Employed i pos job (Just r) gol tsks
takeItem r (Unemployed i pos job _ rsn) = Unemployed i pos job (Just r) rsn

data Schema = Schema

data GameState = GameState Viewer Schema (A3D.Array3d T.Tile) [EWorker]
data Viewer = Viewer Int Int Int (Int,Int,Int)

clampView :: Viewer -> Viewer
clampView (Viewer x y z m@(xm, ym, zm)) =
    Viewer (clamp 0 xm x)
           (clamp 0 ym y)
           (clamp 0 zm z)
           m

updateViewer :: Viewer -> C.Controls ControlKey C.TriggerAggregate -> Viewer
updateViewer (Viewer x y z clam) ctrls =
    let chkPress k = C.isPressed $ C.getControl k ctrls in
    let up = select 0 (-1) $ chkPress CK'ZUp in
    let down = select up 1 $ chkPress CK'ZDown in
    clampView $ Viewer x y (z + down) clam
class Coordinate c => World w c | w -> c where
    getTile :: w -> c -> Maybe T.Tile
    putTile :: w -> c -> T.Tile -> w

    resources :: w -> c -> [T.Resource]
    resources arr xy = fromMaybe T.emptyMs $ T.getResources <$> getTile arr xy

    addResource :: w -> c -> T.Resource -> w
    addResource arr xy r = modTile arr xy (T.addResource r)

    takeResource :: w -> c -> T.Resource -> Worker k -> (w, Worker k)
    takeResource arr xy r wk = (modTile arr xy (T.takeResource r), takeItem r wk)

    hasResource :: w -> c -> T.Resource -> Bool
    hasResource w xy r = elem r $ resources w xy

    modTile :: w -> c -> (T.Tile -> T.Tile) -> w
    modTile w xy f = let p = f <$> getTile w xy in
                     fromMaybe w $ (putTile w xy) <$> p

    isStandable :: w -> c -> Bool

    isWalkable :: w -> c -> Bool
    isWalkable arr xy = any T.isWalkable (getTile arr xy)

    isFree :: w -> c -> Bool
    isFree arr xy = any T.isFree (getTile arr xy)

    isWholeSolid :: w -> c -> Bool
    isWholeSolid arr xy = any T.isWholeSolid (getTile arr xy)

    pfind :: w -> c -> c -> Either Reason [c]

instance World (A3D.Array3d T.Tile) XYZ where
    getTile = A3D.get
    putTile = undefined--must modify tile above if WholeSolid

    isStandable arr xy = any id $ do
        t <- getTile arr xy
        return $ T.isStandable t $ getTile arr (xy |+| (0,0,-1))

    pfind arr src dst =
        case D.pfind arr src dst of
            Just path -> Right path
            Nothing -> Left "No Path"


instance Graph (A3D.Array3d T.Tile) XYZ  where
    neighbors arr (x, y, z) =
        let ns = [(x-1, y, z),
                  (x+1, y, z),
                  (x, y+1, z),
                  (x, y-1, z)]
        in
        (, 1) <$> filter (isStandable arr) ns

filterOffscreen :: Viewer -> [EWorker] -> [EWorker]
filterOffscreen (Viewer _ _ z _) wks =
    filter onScreen wks
    where onScreen w =
              let (_, _, wz) = getPos w in
              wz == z
instance Drawable GameState where
    draw (GameState v _ world wks) tex = do
        let (Viewer _ _ level _) = v
        let (Just layer) = A3D.getLayer world level
        let below = A3D.getLayer world (level-1)
        let ts = Tileset 16 16 16 16
        let ren = Renderer tex ts
        let tr = empty
        let fl r (p, t) =
                let b = join $ (\x -> A2D.get x p) <$> below in
                r <+ (p, T.render t b)
        let tr' = (A2D.foldl fl tr layer)
        let rwks = Demiurge.render <$> (filterOffscreen v wks)
        render ren (tr' <++< rwks)


data ControlKey = CK'ZUp
                | CK'ZDown
    deriving (Eq, Ord)

instance Game GameState (C.Controls ControlKey C.TriggerAggregate, Assets, Window) rng where
    runFrame (GameState v s w wks) (ctrls, _, _) g =
        let nv = updateViewer v ctrls in
        ((GameState nv s w wks), g)


mkState :: Int -> Int -> Int -> GameState
mkState cols rows layers =
    let view = Viewer 0 0 0 (0,0,layers-1) in
    let tiles = A3D.tabulate cols rows layers $ \(_, _, z) ->
         let tt = case z of
                      0 -> T.WholeSolid
                      1 -> T.FloorSolid
                      _ -> T.Free
         in
         T.Tile tt []
    in
    let wk = IdleWorker (Unemployed 0 (0,0,1) Builder Nothing "Birthday") in
    GameState view Schema tiles [wk]
