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
data ASchema = ASchema
data Tile
data AWorld = AWorld (A3D.Array3d Tile)
data AWorldState w s g k = AWorldState w s [Worker k]
type AState a = State a a

type WS w s g k = AState (AWorldState w s g k)
data Working g = Working g [ATask]
data Idle = Idle
data Worker k = Worker Int XYZ Job k

getId :: Worker k -> Int
getId (Worker i _ _ _) = i

tasks :: Worker (Working g) -> [ATask]
tasks (Worker _ _ _ (Working _ tsks)) = tsks

goal :: Worker (Working g) -> g
goal (Worker _ _ _ (Working gol _)) = gol

setGoal :: Goal g => g -> [ATask] -> Worker Idle -> Worker (Working g)
setGoal gol tsks (Worker i pos job _) = Worker i pos job (Working gol tsks)

putWorker :: Goal g => Worker k -> AWorldState w s g k -> AWorldState w s g k
putWorker wk ws =
    (setWorkers ws . updateAt wk . getWorkers) ws

getWorkers :: AWorldState w s g k -> [Worker k]
getWorkers (AWorldState _ _ wks) = wks

setWorkers :: AWorldState w s g k -> [Worker k] -> AWorldState w s g k
setWorkers (AWorldState w s _) wks = AWorldState w s wks

allTasks :: Goal g => WS w s g k
allTasks = do
    a@(AWorldState _ _ wks) <- get
    foldlM thing a wks
    where thing ws w = do
              let w' = w
              let ws' = putWorker w' ws
              return ws'

giveGoal :: Goal g => g -> Worker g -> Worker g
giveGoal gol wk@(Worker _ _ _ Nothing _) =
    let tsks = toOrder gol in
    setGoal gol tsks wk

impossible :: a
impossible = undefined

refreshTasks :: Goal g => Working g -> Worker g
refreshTasks (Worker _ _ _ (Just gol) (x:xs)) = impossible




class Plan p where
    isFinished :: p -> Bool

class Same t => Task t where
    perform :: t
            -> Worker k
            -> WS w s g k

class Same g => Goal g where
    toOrder :: g -> [ATask]

class Schema s where
    mkTasks :: s -> Worker k -> w -> g -> [ATask]
    process :: s -> Worker k -> (s, Worker k)


instance Same (Worker k) where
    same w1 w2 = getId w1 == getId w2

