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
data ASchema s = ASchema s
data Tile
data AWorld = AWorld (A3D.Array3d Tile)
data WorldState w s p g t = WorldState w s [EWorker g t]

type NonEmpty t = (t, [t])

data Working
data Idle
data Worker g t k where
    Employed :: (Task t, Goal g)
             => Int -> XYZ -> Job -> g -> NonEmpty t -> Worker g t Working
    Unemployed :: (Task t, Goal g)
               => Int -> XYZ -> Job -> Reason -> Worker g t Idle

data EWorker g t = IdleWorker (Worker g t Idle)
                 | WorkingWorker (Worker g t Working)

type Reason = String

packW :: Worker g t Working -> EWorker g t
packW = WorkingWorker

packI :: Worker g t Idle -> EWorker g t
packI = IdleWorker

pack :: Worker g t k -> EWorker g t
pack e@(Employed _ _ _ _ _) = packW e
pack u@(Unemployed _ _ _ _) = packI u

makeIdle :: Reason -> Worker g t Working -> Worker g t Idle
makeIdle rsn (Employed i pos job _ _ ) = Unemployed i pos job rsn

getId :: EWorker g t -> Int
getId (WorkingWorker (Employed i _ _ _ _)) = i
getId (IdleWorker (Unemployed i _ _ _)) = i

tasks :: Worker g t Working -> NonEmpty t
tasks (Employed _ _ _ _ tsks) = tsks

goal :: Worker g t Working -> g
goal (Employed _ _ _ gol _) = gol

setGoal :: (Task t, Goal g)
        => g
        -> NonEmpty t
        -> Worker g t Idle
        -> Worker g t Working
setGoal gol tsks (Unemployed i pos job _) = Employed i pos job gol tsks

putWorker :: EWorker g t
          -> WorldState w s p g t
          -> WorldState w s p g t
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws


getSchema :: WorldState w s p g t
          -> s
getSchema (WorldState _ s _) = s

putSchema :: s
          -> WorldState w s p g t
          -> WorldState w s p g t
putSchema s (WorldState w _ wks) = WorldState w s wks


getWorkers :: WorldState w s p g t -> [EWorker g t]
getWorkers (WorldState _ _ wks) = wks

setWorkers :: WorldState w s p g t -> [EWorker g t] -> WorldState w s p g t
setWorkers (WorldState w s _) wks = WorldState w s wks


coordinateTasks :: (Schema s, Task t, Goal g, Plan p)
                => WorldState w s p g t
                -> WorldState w s p g t
coordinateTasks ws =
    let wks = getWorkers ws in
    foldl processWorker ws wks


processWorker :: (Schema s, Task t, Goal g, Plan p)
              => WorldState w s p g t
              -> EWorker g t
              -> WorldState w s p g t
processWorker ws (WorkingWorker wk) =
    performTasks ws wk
processWorker ws (IdleWorker wk) =
    findGoal ws wk


findGoal :: (Schema s, Task t, Goal g, Plan p)
         => WorldState w s p g t
         -> Worker g t Idle
         -> WorldState w s p g t
findGoal ws wk  = do
    let s = getSchema ws
    let wk' = employ s wk ws
    putWorker (packW wk') ws

giveGoal :: (Task t, Goal g) => g -> Worker g t Idle -> Worker g t Working
giveGoal gol wk =
    let tsks = toOrder gol in
    setGoal gol tsks wk

performTasks :: (Task t, Schema s, Goal g)
             => WorldState w s p g t
             -> Worker g t Working
             -> WorldState w s p g t
performTasks ws wk@(Employed _ _ _ _ (t, _)) =
    case allowed t wk ws of
        Permitted -> let (wk', ws') = perform t wk ws in
                     putWorker wk' ws'
        Forbidden str -> let s = getSchema ws in
                         let (s', wk') = surrender s (packW wk) str in
                         let ws' = putSchema s' ws in
                         putWorker wk' ws'
        Blocked blk -> let ws' = jam blk ws in
                       let ws'' = jam (packW wk) ws' in
                       ws''

jam :: (Schema s, Goal g, Task t)
    => EWorker g t
    -> WorldState w s p g t
    -> WorldState w s p g t
jam (IdleWorker wk) ws =
    let s = getSchema ws in
    let moveCmd = move s in
    let wk' = giveGoal moveCmd wk in
    putWorker (packW wk') ws
jam (WorkingWorker wk) ws =
    let s = getSchema ws in
    let (s', wk') = surrender s (packW wk) "jam" in
    jam wk' (putSchema s' ws)


-- required that the current task has been complete
incrementTask :: (Task t, Schema s)
              => WorldState w s p g t
              -> Worker g t Working
              -> WorldState w s p g t
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
               => WorldState w s p g t
               -> WorldState w s p g t
incrementTasks ws =
    let wks = getWorkers ws in
    foldl incr ws wks
    where incr st (WorkingWorker wk) = incrementTask st wk
          incr st _ = st

class Plan p where
    isFinished :: p -> Bool

data TaskPermission g t where
    Permitted :: TaskPermission g t
    Blocked :: EWorker g t -> TaskPermission g t
    Forbidden :: Reason -> TaskPermission g t

class Same t => Task t where
    -- perform must increment the task for worker and if necessary
    -- set them to unemployed
    perform :: t
            -> Worker g t Working
            -> WorldState w s p g t
            -> (EWorker g t, WorldState w s p g t)
    allowed :: t
            -> Worker g t Working
            -> WorldState w s p g t
            -> TaskPermission g t

class Same g => Goal g where
    toOrder :: (Task t) => g -> NonEmpty t

class Schema s where
    mkTasks :: s -> Worker g t k -> w -> g -> NonEmpty t
    process :: s -> Worker g t k -> (s, Worker g t k)
    getPlan :: Plan p => s -> p
    finish :: g -> s -> s
    surrender :: s -> EWorker g t -> Reason -> (s, EWorker g t)
    -- probably needs entity and world to know where to move to
    move :: Goal g => s -> g
    employ :: s
           -> Worker g t Idle
           -> WorldState w s p g t
           -> Worker g t Working
instance Same (EWorker g t) where
    same w1 w2 = getId w1 == getId w2


update :: (Goal g, Plan p, Schema s, Task t)
       => WorldState w s p g t
       -> WorldState w s p g t
update ws = update $ (incrementTasks . coordinateTasks) ws

