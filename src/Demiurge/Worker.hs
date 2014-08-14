module Demiurge.Worker where

import Control.Monad.State
import Data.Foldable

import Demiurge.Common
import Demiurge.Utils
import qualified Demiurge.Data.Array3d as A3D

data Job = Builder | Gatherer | Miner

data AGoal = Build XYZ AGoal
           | Move XYZ AGoal
           | Mine XYZ AGoal
           | Stock XYZ XYZ AGoal

data ATask = Pickup | Drop | Path [XYZ] | Navigate XYZ XYZ
data APlan g = APlan [g] [g] (Maybe (APlan g))
data ASchema g = ASchema g
data Tile
data AWorld = AWorld (A3D.Array3d Tile)
data AWorldState w s p g t = AWorldState w s [EWorker g t]
type AState a = State a a

type WS w s p g t = AState (AWorldState w s p g t)
data Working
data Idle
--data Worker g t k where
--    Employed :: (Task t, Goal g) =>
--               => Int -> XYZ -> Job -> g -> NonEmpty t -> Worker g t Working
--    Unemployed :: (Task t, Goal g)
--               => Int -> XYZ -> Job -> Worker g t Idle
type NonEmpty t = (t, [t])

data Status = Working | Idle
data Worker g t k where
    Employed :: (k ~ Status, Task t, Goal g)
             => Int -> XYZ -> Job -> g -> NonEmpty t -> Worker g t Working
    Unemployed :: (k ~ Status, Task t, Goal g)
               => Int -> XYZ -> Job -> Worker g t Idle

data EWorker g t = IdleWorker (Worker g t Idle) | WorkingWorker (Worker g t Working)

packW :: Worker g t Working -> EWorker g t
packW = WorkingWorker

packI :: Worker g t Idle -> EWorker g t
packI = IdleWorker

getId :: EWorker g t -> Int
getId (WorkingWorker (Employed i _ _ _ _)) = i
getId (IdleWorker (Unemployed i _ _ )) = i

tasks :: Worker g t Working -> NonEmpty t
tasks (Employed _ _ _ _ tsks) = tsks

goal :: Worker g t Working -> g
goal (Employed _ _ _ gol _) = gol

setGoal :: (Task t, Goal g)
        => g
        -> NonEmpty t
        -> Worker g t Idle
        -> Worker g t Working
setGoal gol tsks (Unemployed i pos job) = Employed i pos job gol tsks

putWorker :: EWorker g t
          -> AWorldState w s p g t
          -> AWorldState w s p g t
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws

getWorkers :: AWorldState w s p g t -> [EWorker g t]
getWorkers (AWorldState _ _ wks) = wks

setWorkers :: AWorldState w s p g t -> [EWorker g t] -> AWorldState w s p g t
setWorkers (AWorldState w s _) wks = AWorldState w s wks

allTasks :: (Schema s, Task t, Goal g, Plan p) => WS w s p g t
allTasks = do
    a@(AWorldState _ _ wks) <- get
    foldlM thing a wks
    where thing _ w = doThing w

doThing :: (Schema s, Task t, Goal g, Plan p) => EWorker g t -> WS w s p g t
doThing (WorkingWorker w@(Employed _ _ _ _ _)) = performTasks w
doThing (IdleWorker w@(Unemployed _ _ _)) = do
    ws <- get
    findGoal w ws

findGoal :: (Schema s, Task t, Goal g)
         => Worker g t Idle
         -> AWorldState w s p g t
         -> WS w s p g t
findGoal wk ws@(AWorldState _ s wks) =
    let plan = getPlan s in
    let wk' = employ plan wk in
    return $ putWorker (packW wk') ws

giveGoal :: Goal g => g -> Worker g t Idle -> Worker g t Working
giveGoal gol wk@(Unemployed _ _ _) =
    let tsks = toOrder gol in
    setGoal gol tsks wk

performTasks :: (Task t, Goal g) => Worker g t Working -> WS w s p g t
performTasks wk@(Employed _ _ _ _ (t, _)) = do
    ws <- get
    let (wk', ws') = perform t wk ws
    return $ putWorker wk' ws'


class Plan p where
    isFinished :: p -> Bool
    employ :: p
           -> Worker g t Idle
           -> Worker g t Working

class Same t => Task t where
    -- perform must increment the task for worker and if necessary
    -- set them to unemployed
    perform :: t
            -> Worker g t Working
            -> AWorldState w s p g t
            -> (EWorker g t, AWorldState w s p g t)

class Same g => Goal g where
    toOrder :: (Task t) => g -> NonEmpty t

class Schema s where
    mkTasks :: s -> Worker g t k -> w -> g -> NonEmpty t
    process :: s -> Worker g t k -> (s, Worker g t k)
    getPlan :: Plan p => s -> p

instance Same (EWorker g t) where
    same w1 w2 = getId w1 == getId w2

