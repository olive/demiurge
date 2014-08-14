module Demiurge.Worker where


import Demiurge.Common
import Demiurge.Utils
import qualified Demiurge.Data.Array3d as A3D

data Job = Builder | Gatherer | Miner

data Major
data Minor
data AGoal a where
    Build :: XYZ -> AGoal Major
    Move :: XYZ -> AGoal Minor
    Mine :: XYZ -> AGoal Major
    Stock :: XYZ -> XYZ -> AGoal Major

data ATask = Pickup | Drop | Path [XYZ] | Navigate XYZ XYZ
data APlan p = APlan [p] [p] (Maybe (APlan p))
data PlanList p = PlanList [p]
data Tile
data AWorld = AWorld (A3D.Array3d Tile)
data WorldState w c s p g t = WorldState w s [EWorker c g t]

type NonEmpty t = (t, [t])

data Working
data Idle
data Worker c g t k where
    Employed :: (Task t, Goal g)
             => Int -> XYZ -> Job -> g -> NonEmpty t -> Worker c g t Working
    Unemployed :: (Task t, Goal g)
               => Int -> XYZ -> Job -> Reason -> Worker c g t Idle

data EWorker c g t = IdleWorker (Worker c g t Idle)
                   | WorkingWorker (Worker c g t Working)

type Reason = String

packW :: Worker c g t Working -> EWorker c g t
packW = WorkingWorker

packI :: Worker c g t Idle -> EWorker c g t
packI = IdleWorker

pack :: Worker c g t k -> EWorker c g t
pack e@(Employed _ _ _ _ _) = packW e
pack u@(Unemployed _ _ _ _) = packI u

makeIdle :: Reason -> Worker c g t Working -> Worker c g t Idle
makeIdle rsn (Employed i pos job _ _ ) = Unemployed i pos job rsn

getId :: EWorker c g t -> Int
getId (WorkingWorker (Employed i _ _ _ _)) = i
getId (IdleWorker (Unemployed i _ _ _)) = i

tasks :: Worker c g t Working -> NonEmpty t
tasks (Employed _ _ _ _ tsks) = tsks

goal :: Worker c g t Working -> g
goal (Employed _ _ _ gol _) = gol

setGoal :: (Task t, Goal g)
        => g
        -> NonEmpty t
        -> Worker c g t Idle
        -> Worker c g t Working
setGoal gol tsks (Unemployed i pos job _) = Employed i pos job gol tsks

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


coordinateTasks :: (Schema s, Task t, Goal g, Plan p)
                => WorldState w c s p g t
                -> WorldState w c s p g t
coordinateTasks ws =
    let wks = getWorkers ws in
    foldl processWorker ws wks


processWorker :: (Schema s, Task t, Goal g, Plan p)
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

performTasks :: (Task t, Schema s, Goal g)
             => WorldState w c s p g t
             -> Worker c g t Working
             -> WorldState w c s p g t
performTasks ws wk@(Employed _ _ _ _ (t, _)) =
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
incrementTask ws wk@(Employed i pos job g tsks) = do
    case snd tsks of
        [] -> do
            let nwk = packI $ makeIdle "Task Complete" wk
            let s = getSchema ws
            let ws' = putSchema (finish g s) ws
            putWorker nwk ws'
        (x:xs) -> do
            let nwk = WorkingWorker (Employed i pos job g (x, xs))
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

data TaskPermission c g t where
    Permitted :: TaskPermission c g t
    Blocked :: EWorker c g t -> TaskPermission c g t
    Forbidden :: Reason -> TaskPermission c g t

class Eq r => Resource r where

class World w c | w -> c where
    getTile :: w -> c -> Tile
    putTile :: w -> c -> Tile -> w
    resources :: Resource r => w -> c -> [r]
    hasResource :: Resource r => w -> c -> r -> Bool
    putResource :: Resource r => w -> c -> r -> w
    modTile :: w -> c -> (Tile -> Tile) -> w
    isStandable :: w -> c -> Bool
    isWalkable :: w -> c -> Bool
    isFree :: w -> c -> Bool
    isWholeSolid :: w -> c -> Bool
    pfind :: w -> c -> c -> Either Reason [c]
class Same t => Task t where
    -- perform must increment the task for worker and if necessary
    -- set them to unemployed
    perform :: t
            -> Worker c g t Working
            -> WorldState w c s p g t
            -> (EWorker c g t, WorldState w c s p g t)
    allowed :: t
            -> Worker c g t Working
            -> WorldState w c s p g t
            -> TaskPermission c g t

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

update :: (Goal g, Plan p, Schema s, Task t)
       => WorldState w c s p g t
       -> WorldState w c s p g t
update ws = update $ (incrementTasks . coordinateTasks) ws

