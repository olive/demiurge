{-# OPTIONS_GHC -fno-warn-orphans #-}
module Demiurge.Worker where

import Prelude hiding (any)
import Control.Applicative((<$>))
import Data.Foldable(any)
import Data.Maybe

import Demiurge.Data.Graph
import Demiurge.Data.Coordinate
import qualified Demiurge.Pathing.Dijkstra as D
import qualified Demiurge.Tile as T
import Demiurge.Common
import Demiurge.Utils
import qualified Demiurge.Data.Array3d as A3D

data Job = Builder | Gatherer | Miner

data Major
data Minor

type PlanID = Int
data AGoal a c where
    Build :: Int -> PlanID -> c -> AGoal Major c
    Move :: Int ->  PlanID -> c -> AGoal Minor c
    Mine :: Int ->  PlanID -> c -> AGoal Major c
    Stock :: Int -> PlanID -> c -> c -> AGoal Major c

getGoalID :: AGoal a c -> Int
getGoalID (Build i _ _) = i
getGoalID (Move i _ _) = i
getGoalID (Mine i _ _) = i
getGoalID (Stock i _ _ _) = i


getParent :: AGoal a c -> PlanID
getParent (Build _ i _) = i
getParent (Move _ i _) = i
getParent (Mine _ i _) = i
getParent (Stock _ i _ _) = i


data ATask c = Pickup | Drop | BuildWall | Path [c] | Navigate c c
data APlan p = APlan [p] [p] (Maybe (APlan p))
data PlanList p = PlanList [p]

data AWorld = AWorld (A3D.Array3d T.Tile)
data WorldState w c s g t = WorldState w s [EWorker c g t]
getWorld :: WorldState w c s g t -> w
getWorld (WorldState w _ _) = w

setWorld :: w -> WorldState w c s g t -> WorldState w c s g t
setWorld w (WorldState _ s wks) = WorldState w s wks
type NonEmpty t = (t, [t])

data Working
data Idle
data Worker c g t k where
    Employed :: (Task t, Goal g)
             => Int -> c -> Job -> Maybe T.Resource
             -> g -> NonEmpty t
             -> Worker c g t Working
    Unemployed :: (Task t, Goal g)
               => Int -> c -> Job -> Maybe T.Resource
               -> Reason
               -> Worker c g t Idle

data TaskPermission c g t where
    Permitted :: TaskPermission c g t
    Blocked :: EWorker c g t -> TaskPermission c g t
    Forbidden :: Reason -> TaskPermission c g t

isHolding :: Worker c g t k -> Bool
isHolding (Employed _ _ _ rs _ _) = isJust rs
isHolding (Unemployed _ _ _ rs _) = isJust rs

getPos :: Worker c g t k -> c
getPos (Employed _ pos _ _ _ _) = pos
getPos (Unemployed _ pos _ _ _) = pos

dropItem :: Worker c g t k -> Worker c g t k
dropItem (Employed i pos job _ g tsk) = (Employed i pos job Nothing g tsk)
dropItem (Unemployed i pos job _ rsn) = (Unemployed i pos job Nothing rsn)

takeItem :: T.Resource -> Worker c g t k -> Worker c g t k
takeItem r (Employed i pos job _ g tsk) = Employed i pos job (Just r) g tsk
takeItem r (Unemployed i pos job _ rsn) = Unemployed i pos job (Just r) rsn

data EWorker c g t = IdleWorker (Worker c g t Idle)
                   | WorkingWorker (Worker c g t Working)

type Reason = String

packW :: Worker c g t Working -> EWorker c g t
packW = WorkingWorker

packI :: Worker c g t Idle -> EWorker c g t
packI = IdleWorker

pack :: Worker c g t k -> EWorker c g t
pack e@(Employed _ _ _ _ _ _) = packW e
pack u@(Unemployed _ _ _ _ _) = packI u

makeIdle :: Reason -> Worker c g t Working -> Worker c g t Idle
makeIdle rsn (Employed i pos job rs _ _) = Unemployed i pos job rs rsn

getId :: EWorker c g t -> Int
getId (WorkingWorker (Employed i _ _ _ _ _)) = i
getId (IdleWorker (Unemployed i _ _ _ _)) = i

tasks :: Worker c g t Working -> NonEmpty t
tasks (Employed _ _ _ _ _ tsks) = tsks

goal :: Worker c g t Working -> g
goal (Employed _ _ _ _ gol _) = gol

setGoal :: (Task t, Goal g)
        => g
        -> NonEmpty t
        -> Worker c g t Idle
        -> Worker c g t Working
setGoal gol tsks (Unemployed i pos job rs _) = Employed i pos job rs gol tsks

putWorker :: EWorker c g t
          -> WorldState w c s g t
          -> WorldState w c s g t
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws


getSchema :: WorldState w c s g t
          -> s
getSchema (WorldState _ s _) = s

putSchema :: s
          -> WorldState w c s g t
          -> WorldState w c s g t
putSchema s (WorldState w _ wks) = WorldState w s wks


getWorkers :: WorldState w c s g t -> [EWorker c g t]
getWorkers (WorldState _ _ wks) = wks

setWorkers :: WorldState w c s g t
           -> [EWorker c g t]
           -> WorldState w c s g t
setWorkers (WorldState w s _) wks = WorldState w s wks


coordinateTasks :: (World w c, Schema s, Task t, Goal g)
                => WorldState w c s g t
                -> WorldState w c s g t
coordinateTasks ws =
    let wks = getWorkers ws in
    foldl processWorker ws wks


processWorker :: (World w c, Schema s, Task t, Goal g)
              => WorldState w c s g t
              -> EWorker c g t
              -> WorldState w c s g t
processWorker ws (WorkingWorker wk) =
    performTasks ws wk
processWorker ws (IdleWorker wk) =
    findGoal ws wk


findGoal :: (Schema s, Task t, Goal g)
         => WorldState w c s g t
         -> Worker c g t Idle
         -> WorldState w c s g t
findGoal ws wk  = do
    let s = getSchema ws
    let wk' = employ s wk ws
    putWorker (packW wk') ws

giveGoal :: (Task t, Goal g, Schema s)
         => g
         -> Worker c g t Idle
         -> WorldState w c s g t
         -> Worker c g t Working
giveGoal gol wk (WorldState w s _) =
    let tsks = mkTasks s wk w in
    setGoal gol tsks wk

performTasks :: (World w c, Task t, Schema s, Goal g)
             => WorldState w c s g t
             -> Worker c g t Working
             -> WorldState w c s g t
performTasks ws wk@(Employed _ _ _ _ _ (t, _)) =
    case allowed t wk ws of
        Permitted -> let (ws', wk') = perform t wk ws in
                     putWorker wk' ws'
        Forbidden str -> let s = getSchema ws in
                         let (s', wk') = surrender s (packW wk) str in
                         let ws' = putSchema s' ws in
                         putWorker wk' ws'
        Blocked blk -> jam blk $ jam (packW wk) ws


jam :: (Schema s, Goal g, Task t)
    => EWorker c g t
    -> WorldState w c s g t
    -> WorldState w c s g t
jam (IdleWorker wk) ws =
    let s = getSchema ws in
    let moveCmd = move s in
    let wk' = giveGoal moveCmd wk ws in
    putWorker (packW wk') ws
jam (WorkingWorker wk) ws =
    let s = getSchema ws in
    let (s', wk') = surrender s (packW wk) "jam" in
    jam wk' (putSchema s' ws)


-- required that the current task has been complete
incrementTask :: (Task t, Schema s)
              => WorldState w c s g t
              -> Worker c g t Working
              -> WorldState w c s g t
incrementTask ws wk@(Employed i pos job rs g tsks) = do
    case (snd tsks, (isFinished . fst) tsks) of
        ([], True) -> do
            let nwk = packI $ makeIdle "Task Complete" wk
            let s = getSchema ws
            let ws' = putSchema (complete g s) ws
            putWorker nwk ws'
        ((x:xs), True) -> do
            let nwk = WorkingWorker (Employed i pos job rs g (x, xs))
            putWorker nwk ws
        _ -> ws

incrementTasks :: (Task t, Schema s)
               => WorldState w c s g t
               -> WorldState w c s g t
incrementTasks ws =
    let wks = getWorkers ws in
    foldl incr ws wks
    where incr st (WorkingWorker wk) = incrementTask st wk
          incr st _ = st

class Plan p where
    --isFinished :: p -> Bool

class Coordinate c => World w c | w -> c where
    getTile :: w -> c -> Maybe T.Tile
    putTile :: w -> c -> T.Tile -> w

    resources :: w -> c -> [T.Resource]
    resources arr xy = fromMaybe T.emptyMs $ T.getResources <$> getTile arr xy

    addResource :: w -> c -> T.Resource -> w
    addResource arr xy r = modTile arr xy (T.addResource r)

    takeResource :: w -> c -> T.Resource -> (Worker c g t k -> (w, Worker c g t k))
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


instance Graph (A3D.Array3d T.Tile) XYZ  where
    neighbors arr (x, y, z) =
        let ns = [(x-1, y, z),
                  (x+1, y, z),
                  (x, y+1, z),
                  (x, y-1, z)]
        in
        (\w -> (w, 1)) <$> filter (isStandable arr) ns

instance World (A3D.Array3d T.Tile) XYZ where
    getTile = A3D.get
    putTile = A3D.put

    isStandable arr xy = any id $ do
        t <- getTile arr xy
        return $ T.isStandable t $ getTile arr (xy |+| (0,0,-1))

    pfind arr src dst =
        case D.pfind arr src dst of
            Just path -> Right path
            Nothing -> Left "No Path"

class Task t where
    isFinished :: t -> Bool
    finish :: t -> t

    -- perform must increment the task for worker and if necessary
    -- set them to unemployed
    perform :: (World w c)
            => t
            -> Worker c g t Working
            -> WorldState w c s g t
            -> (WorldState w c s g t, EWorker c g t)
    allowed :: (World w c)
            => t
            -> Worker c g t Working
            -> WorldState w c s g t
            -> TaskPermission c g t

instance Coordinate c => Task (ATask c) where
    isFinished Pickup = True
    finish Pickup = Pickup
    perform Pickup emp ws =
        let (w', wk') = takeResource (getWorld ws) (getPos emp) T.Stone emp in
        (setWorld w' ws, pack wk')
    allowed Pickup emp ws =
        if (not.null) $ resources (getWorld ws) (getPos emp)
        then Permitted
        else Forbidden "test"

class Same g => Goal g where
    parent :: g -> PlanID

instance Same (AGoal g c) where
    same g1 g2 = getGoalID g1 == getGoalID g2

instance Goal (AGoal g c) where
    parent = getParent
class Schema s where
    mkTasks :: s -> Worker c g t Idle -> w -> NonEmpty t
    process :: s -> Worker c g t k -> (s, Worker c g t k)
    getPlan :: Plan p => s -> p
    complete :: g -> s -> s
    surrender :: s -> EWorker c g t -> Reason -> (s, EWorker c g t)
    -- probably needs entity and world to know where to move to
    move :: Goal g => s -> g
    employ :: s
           -> Worker c g t Idle
           -> WorldState w c s g t
           -> Worker c g t Working

instance Schema (PlanList (APlan (AGoal k XYZ))) where
    mkTasks (PlanList pls) (Unemployed _ _ _ _ _) world = undefined
instance Same (EWorker c g t) where
    same w1 w2 = getId w1 == getId w2

mkWorld :: (gg ~ AGoal k XYZ)
        => WorldState (A3D.Array3d T.Tile) XYZ (PlanList (APlan gg)) gg (ATask XYZ)
mkWorld = undefined

update :: (World w c, Goal g, Schema s, Task t)
       => WorldState w c s g t
       -> WorldState w c s g t
update ws = update $ (incrementTasks . coordinateTasks) ws

