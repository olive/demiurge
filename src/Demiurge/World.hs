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
import Antiqua.Data.Coordinate

import Demiurge.Input.ControlMap
import qualified Demiurge.Tile as T
import Demiurge.Common
import qualified Demiurge.Data.Array3d as A3D
import qualified Demiurge.Drawing.Renderable as Demiurge
import Demiurge.Utils

import Debug.Trace

data Job = Builder | Gatherer | Miner

data Task = Drop
          | Gather
          | Navigate XYZ XYZ
          | Path (NonEmpty XYZ)

data Goal = Build XYZ
type Reason = String

data Status = Working | Idle
data Worker k where
    Employed :: (k ~ Status)
             => Int              -- | id
             -> XYZ              -- | position
             -> Job              -- | job
             -> Maybe T.Resource -- | inventory
             -> Goal             -- | current goal
             -> NonEmpty Task    -- | current tasks
             -> Worker 'Working
    Unemployed :: (k ~ Status)
               => Int              -- | id
               -> XYZ              -- | position
               -> Job              -- | job
               -> Maybe T.Resource -- | inventory
               -> Reason           -- | why unemployed
               -> Worker 'Idle

data EWorker = WorkingWorker (Worker 'Working)
             | IdleWorker (Worker 'Idle)

data TaskPermission = TaskPermitted
                    | TaskBlocked EWorker
                    | TaskForbidden Reason

pack :: Worker t -> EWorker
pack e@(Employed _ _ _ _ _ _) = WorkingWorker e
pack u@(Unemployed _ _ _ _ _) = IdleWorker u

shiftTask :: Worker 'Working -> EWorker
shiftTask = undefined


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
        TaskBlocked _ -> (pack $ unemploy "Blocked" wk, t)
        TaskForbidden rsn -> (pack $ unemploy rsn wk, t)
processWorker wk _ t = (wk, t)


allowed :: Task -> Worker 'Working -> Terrain -> TaskPermission
allowed Drop wk _
    | (isJust . getResource) wk = TaskPermitted
    | otherwise = TaskForbidden "Has no resource to drop"
allowed Gather wk t
    | (isJust . getResource) wk = TaskForbidden "Inventory is full"
    | not $ hasResource t ((getPos . pack) wk) T.Stone = TaskForbidden "No resource here to pick up"
    | otherwise = TaskPermitted
allowed (Navigate _ _) _ _ = TaskPermitted
allowed (Path (x, _)) _ t
    | (not . isWalkable t) x = TaskForbidden "Can't walk there"
    | otherwise = TaskPermitted

perform :: Task -> Worker 'Working -> Terrain -> (EWorker, Terrain)
perform Drop wk t =
    let wk' = shiftTask $ spendResource wk in
    let t' = addResource t (getPos wk') T.Stone in
    (wk', t')
perform Gather wk t =
    let wk' = shiftTask $ giveResource T.Stone wk in
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
    let wk' = unemploy "Task Finished" $ setPos x wk in
    (pack wk', t)

getTask :: Worker 'Working -> Task
getTask (Employed _ _ _ _ _ tsk) = headOf tsk

mapTask :: (Task -> Task) -> Worker 'Working -> Worker 'Working
mapTask f (Employed i pos job rs gol tsk) = Employed i pos job rs gol (mapHead f tsk)

unemploy :: Reason -> Worker 'Working -> Worker 'Idle
unemploy rsn (Employed i pos job rs _ _) =
    trace rsn $ Unemployed i pos job rs rsn

getResource :: Worker t -> Maybe T.Resource
getResource (Employed _ _ _ r _ _) = r
getResource (Unemployed _ _ _ r _) = r

mapResource :: (T.Resource -> Maybe T.Resource) -> Worker t -> Worker t
mapResource f (Employed i pos job r gol tsks) =
    Employed i pos job (r >>= f) gol tsks
mapResource f (Unemployed i pos job r rsn) =
    Unemployed i pos job (r >>= f) rsn

spendResource :: Worker t -> Worker t
spendResource = mapResource (\_ -> Nothing)

giveResource :: T.Resource -> Worker t -> Worker t
giveResource r = mapResource (\_ -> Just r)

getPos :: EWorker -> XYZ
getPos (WorkingWorker (Employed _ pos _ _ _ _)) = pos
getPos (IdleWorker (Unemployed _ pos _ _ _)) = pos

setPos :: XYZ -> Worker t -> Worker t
setPos xyz (Employed i _ job rs gol tsk) = Employed i xyz job rs gol tsk
setPos xyz (Unemployed i _ job rs rsn) = Unemployed i xyz job rs rsn

workerToTile :: Job -> Status -> Antiqua.Tile CP437
workerToTile j st =
    let color = case st of
                    Working -> yellow
                    Idle -> white
    in
    case j of
        Builder -> Antiqua.Tile C'B black color
        Gatherer -> Antiqua.Tile C'G black color
        Miner -> Antiqua.Tile C'M black color

instance Demiurge.Renderable EWorker where
    render (WorkingWorker (Employed _ pos job _ _ _)) tr =
        let t1 = (drop3 pos, workerToTile job Working) in
        tr <+ t1
    render (IdleWorker (Unemployed _ pos job _ _)) tr =
        let t1 = (drop3 pos, workerToTile job Idle) in
        let t2 = (drop3 $ pos ~~> D'North,  Antiqua.Tile (:?) black white) in
        tr <++ [t1, t2]


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
        return $ T.isStandable t $ getTile arr (xy ~~> D'Downward)

    pfind arr src dst =
        case D.pfind arr src dst of
            Just (x:xs) -> Right (x, xs)
            Just [] -> Right (dst, [])
            Nothing -> Left ("No Path from " ++ show src ++ show dst)


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




instance Game GameState (C.Controls ControlKey C.TriggerAggregate, Assets, Window) rng where
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
    let wk = Employed 0 (0,0,1) Builder Nothing undefined (Navigate (0,0,1) (10,10,1), []) in
    GameState view Schema tiles [pack wk]
