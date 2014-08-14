module Demiurge.Worker where

import Control.Monad.State
import Data.Foldable

import Demiurge.Common
import Demiurge.Utils
import qualified Demiurge.Data.Array3d as A3D

data Job = Builder | Gatherer | Miner
data AGoal where
    Build :: XYZ -> AGoal
    Move :: XYZ -> AGoal
    Mine :: XYZ -> AGoal
    Stock :: XYZ -> XYZ -> AGoal

data ATask = Pickup | Drop | Path [XYZ] | Navigate XYZ XYZ
data APlan g = APlan [g] [g] (Maybe (APlan g))
data ASchema g = ASchema g
data Tile
data AWorld = AWorld (A3D.Array3d Tile)
data AWorldState w s g t k = AWorldState w s [Worker g t k]
type AState a = State a a

type WS w s g t k = AState (AWorldState w s g t k)
data Working
data Finished g = Finished g
data Idle
data Worker g t k where
    WorkerA :: (Task t, Goal g)
            => Int -> XYZ -> Job -> g -> NonEmpty t -> Worker g t Working
    WorkerB :: (Task t, Goal g)
            => Int -> XYZ -> Job -> Worker g t Idle
type NonEmpty t = (t, [t])


getId :: Worker g t k -> Int
getId (WorkerA i _ _ _ _) = i
getId (WorkerB i _ _ ) = i

tasks :: Worker g t Working -> NonEmpty t
tasks (WorkerA _ _ _ _ tsks) = tsks

goal :: Worker g t Working -> g
goal (WorkerA _ _ _ gol _) = gol

setGoal :: (Task t, Goal g)
        => g
        -> NonEmpty t
        -> Worker g t Idle
        -> Worker g t Working
setGoal gol tsks (WorkerB i pos job) = WorkerA i pos job gol tsks

putWorker :: (Goal g, Task t)
          => Worker g t k
          -> AWorldState w s g t k
          -> AWorldState w s g t k
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws

getWorkers :: AWorldState w s g t k -> [Worker g t k]
getWorkers (AWorldState _ _ wks) = wks

setWorkers :: AWorldState w s g t k -> [Worker g t k] -> AWorldState w s g t k
setWorkers (AWorldState w s _) wks = AWorldState w s wks

allTasks :: (Task t, Goal g) => WS w s g t k
allTasks = do
    a@(AWorldState _ _ wks) <- get
    foldlM thing a wks
    where thing _ w = doThing w

doThing :: (Task t, Goal g) => Worker g t k -> WS w s g t k
doThing w@(WorkerA _ _ _ _ _) = performTasks w
doThing w@(WorkerB _ _ _) = do
    ws <- get
    findGoal w ws

findGoal :: (Task t, Goal g)
         => Worker g t Idle
         -> AWorldState w s g t k
         -> WS w s g t k
findGoal = undefined

giveGoal :: Goal g => g -> Worker g t Idle -> Worker g t Working
giveGoal gol wk@(WorkerB _ _ _) =
    let tsks = toOrder gol in
    setGoal gol tsks wk

performTasks :: (Task t, Goal g) => Worker g t Working -> WS w s g t k
performTasks wk@(WorkerA _ _ _ _ (t, _)) = do
    ws <- get
    let (wk', ws') = perform t wk ws
    return $ putWorker wk' ws'


class Plan p where
    isFinished :: p -> Bool

class Same t => Task t where
    perform :: t
            -> Worker g t Working
            -> AWorldState w s g t k
            -> (Worker g t k, AWorldState w s g t k)

class Same g => Goal g where
    toOrder :: (Task t) => g -> NonEmpty t

class Schema s where
    mkTasks :: s -> Worker g t k -> w -> g -> NonEmpty t
    process :: s -> Worker g t k -> (s, Worker g t k)


instance Same (Worker g t k) where
    same w1 w2 = getId w1 == getId w2

