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
data AGoal a c where
    Build :: c -> AGoal Major c
    Move :: c -> AGoal Minor c
    Mine :: c -> AGoal Major c
    Stock :: c -> c -> AGoal Major c


data ATask c = Pickup | Drop | Path [c] | Navigate c c
data APlan p = APlan [p] [p] (Maybe (APlan p))
data PlanList p = PlanList [p]

data AWorld = AWorld (A3D.Array3d T.Tile)
data WorldState w c s p g t = WorldState w s [EWorker c g t]
getWorld :: WorldState w c s p g t -> w
getWorld (WorldState w _ _) = w
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
          -> WorldState w c s p g t
          -> WorldState w c s p g t
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws


getSchema :: WorldState w c s p g t
          -> s
getSchema (WorldState _ s _) = s

putSchema :: s
          -> WorldState w c s p g t
          -> WorldState w c s p g t
putSchema s (WorldState w _ wks) = WorldState w s wks


getWorkers :: WorldState w c s p g t -> [EWorker c g t]
getWorkers (WorldState _ _ wks) = wks

setWorkers :: WorldState w c s p g t
           -> [EWorker c g t]
           -> WorldState w c s p g t
setWorkers (WorldState w s _) wks = WorldState w s wks


coordinateTasks :: (World w c, Schema s, Task t, Goal g, Plan p)
                => WorldState w c s p g t
                -> WorldState w c s p g t
coordinateTasks ws =
    let wks = getWorkers ws in
    foldl processWorker ws wks


processWorker :: (World w c, Schema s, Task t, Goal g, Plan p)
              => WorldState w c s p g t
              -> EWorker c g t
              -> WorldState w c s p g t
processWorker ws (WorkingWorker wk) =
    performTasks ws wk
processWorker ws (IdleWorker wk) =
    findGoal ws wk


findGoal :: (Schema s, Task t, Goal g, Plan p)
         => WorldState w c s p g t
         -> Worker c g t Idle
         -> WorldState w c s p g t
findGoal ws wk  = do
    let s = getSchema ws
    let wk' = employ s wk ws
    putWorker (packW wk') ws

giveGoal :: (Task t, Goal g, Schema s)
         => g
         -> Worker c g t Idle
         -> WorldState w c s p g t
         -> Worker c g t Working
giveGoal gol wk (WorldState w s _) =
    let tsks = mkTasks s wk w gol in
    setGoal gol tsks wk

performTasks :: (World w c, Task t, Schema s, Goal g)
             => WorldState w c s p g t
             -> Worker c g t Working
             -> WorldState w c s p g t
performTasks ws wk@(Employed _ _ _ _ _ (t, _)) =
    case allowed t wk ws of
        Permitted -> let (wk', ws') = perform t wk ws in
                     putWorker wk' ws'
        Forbidden str -> let s = getSchema ws in
                         let (s', wk') = surrender s (packW wk) str in
                         let ws' = putSchema s' ws in
                         putWorker wk' ws'
        Blocked blk -> jam blk $ jam (packW wk) ws


jam :: (Schema s, Goal g, Task t)
    => EWorker c g t
    -> WorldState w c s p g t
    -> WorldState w c s p g t
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
              => WorldState w c s p g t
              -> Worker c g t Working
              -> WorldState w c s p g t
incrementTask ws wk@(Employed i pos job rs g tsks) = do
    case snd tsks of
        [] -> do
            let nwk = packI $ makeIdle "Task Complete" wk
            let s = getSchema ws
            let ws' = putSchema (finish g s) ws
            putWorker nwk ws'
        (x:xs) -> do
            let nwk = WorkingWorker (Employed i pos job rs g (x, xs))
            putWorker nwk ws

incrementTasks :: (Task t, Schema s)
               => WorldState w c s p g t
               -> WorldState w c s p g t
incrementTasks ws =
    let wks = getWorkers ws in
    foldl incr ws wks
    where incr st (WorkingWorker wk) = incrementTask st wk
          incr st _ = st

class Plan p where
    isFinished :: p -> Bool

class Coordinate c => World w c | w -> c where
    getTile :: w -> c -> Maybe T.Tile
    putTile :: w -> c -> T.Tile -> w

    resources :: w -> c -> [T.Resource]
    resources arr xy = fromMaybe T.emptyMs $ T.getResources <$> getTile arr xy
    addResource :: w -> c -> T.Resource -> w
    addResource arr xy r = modTile arr xy (T.addResource r)
    hasResource :: w -> c -> T.Resource -> Bool
    hasResource w xy r = elem r $ resources w xy
    modTile :: w -> c -> (T.Tile -> T.Tile) -> w
    modTile w xy f = let p = f <$> getTile w xy in
                     fromMaybe w $ (putTile w xy)  <$> p

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
    -- perform must increment the task for worker and if necessary
    -- set them to unemployed
    perform :: (World w c)
            => t
            -> Worker c g t Working
            -> WorldState w c s p g t
            -> (EWorker c g t, WorldState w c s p g t)
    allowed :: (World w c)
            => t
            -> Worker c g t Working
            -> WorldState w c s p g t
            -> TaskPermission c g t

instance Coordinate c => Task (ATask c) where
    perform Pickup emp ws = undefined
    allowed Pickup emp ws =
        if (not.null) $ resources (getWorld ws) (getPos emp)
        then Permitted
        else Forbidden "test"

class Same g => Goal g where
    parent :: (Plan p) => g -> p

class Schema s where
    mkTasks :: s -> Worker c g t k -> w -> g -> NonEmpty t
    process :: s -> Worker c g t k -> (s, Worker c g t k)
    getPlan :: Plan p => s -> p
    finish :: g -> s -> s
    surrender :: s -> EWorker c g t -> Reason -> (s, EWorker c g t)
    -- probably needs entity and world to know where to move to
    move :: Goal g => s -> g
    employ :: s
           -> Worker c g t Idle
           -> WorldState w c s p g t
           -> Worker c g t Working

instance Same (EWorker c g t) where
    same w1 w2 = getId w1 == getId w2

update :: (World w c, Goal g, Plan p, Schema s, Task t)
       => WorldState w c s p g t
       -> WorldState w c s p g t
update ws = update $ (incrementTasks . coordinateTasks) ws

