module Demiurge.Worker where

import Demiurge.Common


data Job = Builder | Gatherer | Miner
data AGoal where
    Build :: XYZ -> AGoal
    Move :: XYZ -> AGoal
    Mine :: XYZ -> Maybe AGoal -> AGoal
    Stock :: XYZ -> XYZ -> AGoal
    NoGoal :: AGoal

extract :: AGoal -> Maybe AGoal
extract (Mine _ g2) = g2
extract _ = Nothing

data ATask = Pickup | Drop | Path [XYZ] | Navigate XYZ XYZ | NoTask
data APlan = APlan [AGoal] [AGoal] (Maybe APlan)
data ASchema = ASchema
data Tile
data AWorld = AWorld [[Tile]]
data WorldState s w = WorldState s [Worker] w

class World w where

class Plan p where
    isFinished :: p -> Bool

class Task t where
    perform :: (Schema s, World w)
            => t
            -> Worker
            -> WorldState s w
            -> (Worker, WorldState s w)

class Schema s where
    mkTasks :: World w => s -> Worker -> w -> AGoal -> [ATask]
    employ :: s -> Worker -> (s, Worker)

data Worker = Worker XYZ Job [AGoal] [ATask]

