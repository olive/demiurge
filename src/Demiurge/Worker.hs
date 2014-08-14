module Demiurge.Worker where

import Control.Monad.State

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
type AState a = State a a

type WS w s p g t = AState (WorldState w s p g t)


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

sputWorker :: EWorker g t
           -> WS w s p g t
sputWorker wk = do
    ws <- get
    return $ putWorker wk ws

getSchema :: WorldState w s p g t
          -> s
getSchema (WorldState _ s _) = s

putSchema :: s
          -> WorldState w s p g t
          -> WorldState w s p g t
putSchema s (WorldState w _ wks) = WorldState w s wks

mapSchema :: (s -> s) -> WS w s p g t
mapSchema f = do
    ws <- get
    let s = getSchema ws
    return $ putSchema (f s) ws

getWorkers :: WorldState w s p g t -> [EWorker g t]
getWorkers (WorldState _ _ wks) = wks

setWorkers :: WorldState w s p g t -> [EWorker g t] -> WorldState w s p g t
setWorkers (WorldState w s _) wks = WorldState w s wks

foldlS :: (a -> AState b) -> [a] -> AState b
foldlS _ [] = get
foldlS f (x:xs) = do
    f x
    foldlS f xs

coordinateTasks :: (Schema s, Task t, Goal g, Plan p) => WS w s p g t
coordinateTasks = do
    a <- get
    let wks = getWorkers a
    return $ foldl processWorker a wks


processWorker :: (Schema s, Task t, Goal g, Plan p) => WorldState w s p g t -> EWorker g t -> WorldState w s p g t
processWorker ws (WorkingWorker wk) =
    performTasks ws wk
processWorker ws (IdleWorker wk) =
    findGoal ws wk


findGoal :: (Schema s, Task t, Goal g, Plan p)
         => WorldState w s p g t
         -> Worker g t Idle
         -> WorldState w s p g t
findGoal ws wk  = do
    let plan = (getPlan . getSchema) ws
    let wk' = employ plan wk ws
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
jam (WorkingWorker wk@(Employed _ _ _ _ _)) ws =
    let s = getSchema ws in
    let (s', wk') = surrender s (packW wk) "jam" in
    jam wk' (putSchema s' ws)


-- required that the current task has been complete
incrementTask :: (Task t, Schema s) => Worker g t Working-> WS w s p g t
incrementTask (Employed i pos job g tsks) = do
    case snd tsks of
        [] -> do
          let nwk = IdleWorker (Unemployed i pos job "Task Complete")
          mapSchema $ finish g
          post g
          sputWorker nwk
        (x:xs) -> do
          let nwk = WorkingWorker (Employed i pos job g (x, xs))
          sputWorker nwk

class Plan p where
    isFinished :: p -> Bool
    employ :: p
           -> Worker g t Idle
           -> WorldState w s p g t
           -> Worker g t Working

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
    post :: g -> WS w s p g t

class Schema s where
    mkTasks :: s -> Worker g t k -> w -> g -> NonEmpty t
    process :: s -> Worker g t k -> (s, Worker g t k)
    getPlan :: Plan p => s -> p
    finish :: g -> s -> s
    surrender :: s -> EWorker g t -> Reason -> (s, EWorker g t)
    -- probably needs entity and world to know where to move to
    move :: Goal g => s -> g
instance Same (EWorker g t) where
    same w1 w2 = getId w1 == getId w2

updateState :: WS w s p g t
updateState = undefined

update :: WorldState w s p g t
       -> WorldState w s p g t
update ws = update $ evalState (updateState) ws

