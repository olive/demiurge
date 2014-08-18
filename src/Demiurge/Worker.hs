module Demiurge.Worker where

import Antiqua.Common
import qualified Antiqua.Graphics.Tile as Antiqua
import Antiqua.Graphics.Color
import Antiqua.Graphics.TileRenderer
import Antiqua.Data.Coordinate
import Antiqua.Data.CP437

import Demiurge.Drawing.Renderable
import Demiurge.Common
import qualified Demiurge.Tile as T
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

jobToColor :: Status -> Color
jobToColor Working = yellow
jobToColor Idle = white

workerToTile :: Job -> Status -> Antiqua.Tile CP437
workerToTile j st =
    let code =
         case j of
             Builder -> C'B
             Gatherer -> C'G
             Miner -> C'M
    in
    Antiqua.Tile code black $ jobToColor st

instance Renderable EWorker where
    render (WorkingWorker (Employed _ pos job _ _ _)) tr =
        let t1 = (fstsnd pos, workerToTile job Working) in
        tr <+ t1
    render (IdleWorker (Unemployed _ pos job _ _)) tr =
        let t1 = (fstsnd pos, workerToTile job Idle) in
        let t2 = (fstsnd $ pos ~~> D'North,  Antiqua.Tile (:?) black white) in
        tr <++ [t1, t2]
