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
import Antiqua.Utils
import qualified Antiqua.Input.Controls as C
import Antiqua.Common
import Antiqua.Data.Graph
import qualified Antiqua.Pathing.Dijkstra as D
import Antiqua.Data.Coordinate

import Demiurge.Input.ControlMap
import qualified Demiurge.Tile as T
import Demiurge.Common
import qualified Demiurge.Data.Array3d as A3D
import qualified Demiurge.Drawing.Renderable as Demiurge
import qualified Demiurge.Entity as E
import Demiurge.Worker
import Demiurge.Plan

forbid :: String -> TaskPermission
forbid = TaskForbidden . Message
updateWorkers :: [EWorker] -> Terrain -> ([EWorker], Terrain)
updateWorkers wks ter =
    upOne [] wks ter
    where upOne up (x:left) t =
              let (wk', t') = processWorker x (up ++ left) t in
              upOne (wk':up) left t'
          upOne up [] t = (up, t)

processWorker :: EWorker -> [EWorker] -> Terrain -> (EWorker, Terrain)
processWorker (WorkingWorker wk) _ t =
    let tsk = getTask wk in
    case allowed tsk wk t of
        TaskPermitted -> perform tsk wk t
        TaskBlocked _ -> (pack $ unemploy (Message "Blocked") wk, t)
        TaskForbidden rsn -> (pack $ unemploy rsn wk, t)
processWorker wk _ t = (wk, t)

shiftTask :: Worker 'Working -> EWorker
shiftTask = undefined

goalToTasks :: Goal -> EWorker -> Terrain -> NonEmpty Task
goalToTasks (Build xyz) wk _ =
    let wPos = getPos wk in
    let adjPos = xyz ~~> nearestDir wPos xyz in
    (Navigate wPos adjPos, [Place xyz])


allowed :: Task -> Worker 'Working -> Terrain -> TaskPermission
allowed Drop wk _
    | (isJust . getResource . pack) wk = TaskPermitted
    | otherwise = forbid "Has no resource to drop"
allowed Gather wk t
    | (isJust . getResource . pack) wk = forbid "Inventory is full"
    | not $ hasResource t ((getPos . pack) wk) T.Stone = forbid "No resource here to pick up"
    | otherwise = TaskPermitted
allowed (Navigate _ _) _ _ = TaskPermitted
allowed (Path (x, _)) _ t
    | (not . isWalkable t) x = forbid "Can't walk there"
    | otherwise = TaskPermitted
allowed (Place xyz) _ t
    | isWholeSolid t xyz = forbid "Somethign is already build there"
    | otherwise = TaskPermitted

perform :: Task -> Worker 'Working -> Terrain -> (EWorker, Terrain)
perform Drop wk t =
    let wk' = (shiftTask . spendResource) wk in
    let t' = addResource t (getPos wk') T.Stone in
    (wk', t')
perform Gather wk t =
    let wk' = (shiftTask . giveResource T.Stone) wk in
    let t' = takeResource t (getPos wk') T.Stone in
    (wk', t')
perform (Navigate src dst) wk t =
    let path = pfind t src dst in
    let wk' = case path of
                  Left reason -> pack $ unemploy reason wk
                  Right p -> pack $ mapTask (\_ -> Path p) wk
    in
    (wk', t)
perform (Path (x, y:yx)) wk t =
    let wk' = mapTask (\_ -> Path (y, yx)) $ setPos x wk in
    (pack wk', t)
perform (Path (x, [])) wk t =
    let wk' = unemploy (Message "Task Finished") $ setPos x wk in
    (pack wk', t)
perform (Place xyz) wk t =
    let t' = modTile t xyz (\_ -> T.Tile T.WholeSolid []) in
    (pack wk, t')

data Schema = Schema
type Terrain = A3D.Array3d T.Tile
data GameState = GameState Viewer Schema Terrain [EWorker]
data Viewer = Viewer Int Int Int (Int,Int,Int)

clampView :: Viewer -> Viewer
clampView (Viewer x y z m@(xm, ym, zm)) =
    Viewer (clamp 0 xm x)
           (clamp 0 ym y)
           (clamp 0 zm z)
           m

updateViewer :: Viewer -> ControlMap C.TriggerAggregate -> Viewer
updateViewer (Viewer x y z clam) ctrls =
    let up = (select 0 (-1) . C.isPressed . from ctrls) (Get :: Index 'CK'ZUp) in
    let down = (select up 1 . C.isPressed . from ctrls) (Get :: Index 'CK'ZDown)in
    clampView $ Viewer x y (z + down) clam

class Coordinate c => World w c | w -> c where
    getTile :: w -> c -> Maybe T.Tile
    putTile :: w -> c -> T.Tile -> w

    resources :: w -> c -> [T.Resource]
    resources arr xy = fromMaybe T.emptyMs $ T.getResources <$> getTile arr xy

    addResource :: w -> c -> T.Resource -> w
    addResource arr xy r = modTile arr xy (T.addResource r)

    takeResource :: w -> c -> T.Resource -> w
    takeResource arr xy r = modTile arr xy (T.takeResource r)

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

    pfind :: w -> c -> c -> Either Reason (NonEmpty c)




instance World (A3D.Array3d T.Tile) XYZ where
    getTile = A3D.get
    putTile w xyz tile =
        let updated = A3D.put w xyz tile in
        if any T.isFree (getTile w (xyz ~~> D'Upward))
        then A3D.put updated (xyz ~~> D'Upward) $ T.Tile T.FloorSolid []
        else updated
    isStandable arr xy = any id $ do
        t <- getTile arr xy
        (return . T.isStandable t . getTile arr) (xy ~~> D'Downward)

    pfind arr src dst =
        case D.pfind arr src dst of
            Just (x:xs) -> Right (x, xs)
            Just [] -> Right (dst, [])
            Nothing -> (Left . Message) ("No Path from " ++ show src ++ show dst)


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




instance Game GameState (ControlMap C.TriggerAggregate, Assets, Window) rng where
    runFrame (GameState v s w wks) (ctrls, _, _) g =
        let (wks', w') = updateWorkers wks w in
        let nv = updateViewer v ctrls in
        ((GameState nv s w' wks'), g)


mkState :: Int -> Int -> Int -> GameState
mkState cols rows layers =
    let view = Viewer 0 0 1 (0,0,layers-1) in
    let tiles = A3D.tabulate cols rows layers $ \(_, _, z) ->
         let tt = case z of
                      0 -> T.WholeSolid
                      1 -> T.FloorSolid
                      _ -> T.Free
         in
         T.Tile tt []
    in
    let wk = Employed (E.Entity 0 (0,0,1) E.Builder Nothing) undefined (Navigate (0,0,1) (10,10,1), []) in
    GameState view Schema tiles [pack wk]
