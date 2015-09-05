module Demiurge.Worker where

import Antiqua.Common
import qualified Antiqua.Graphics.Tile as Antiqua
import Antiqua.Graphics.Colors
import Antiqua.Graphics.TileRenderer
import Antiqua.Data.Coordinate
import Antiqua.Data.CP437

import Demiurge.Drawing.Renderable
import Demiurge.Common
import qualified Demiurge.Tile as T
import Demiurge.Utils
import qualified Demiurge.Entity as E


data WGoal = Major E.Job Int Goal | Minor Goal

data Task = Drop
          | Gather
          | Navigate XYZ XYZ
          | Path (NonEmpty XYZ)
          | Place XYZ

data Goal = Build XYZ

data Reason = Message String | GoalFail String Int deriving Show


data Status = Working | Idle

data Worker k where
    Employed :: (k ~ Status)
             => E.Entity
             -> WGoal            -- | current goal
             -> NonEmpty Task    -- | current tasks
             -> Worker 'Working
    Unemployed :: (k ~ Status)
               => E.Entity
               -> Reason           -- | why unemployed
               -> Worker 'Idle

data EWorker = WorkingWorker (Worker 'Working)
             | IdleWorker (Worker 'Idle)

data TaskPermission = TaskPermitted
                    | TaskBlocked EWorker
                    | TaskForbidden Reason



pack :: Worker t -> EWorker
pack e@(Employed _ _ _) = WorkingWorker e
pack u@(Unemployed _ _) = IdleWorker u

getTask :: Worker 'Working -> Task
getTask (Employed _ _ tsk) = headOf tsk

mapTask :: (Task -> Task) -> Worker 'Working -> Worker 'Working
mapTask f (Employed e gol tsk) = Employed e gol (mapHead f tsk)

getEntity :: EWorker -> E.Entity
getEntity (WorkingWorker (Employed e _ _)) = e
getEntity (IdleWorker (Unemployed e _)) = e


getResource :: EWorker -> Maybe T.Resource
getResource = E.getResource . getEntity

mapResource :: (T.Resource -> Maybe T.Resource) -> Worker t -> Worker t
mapResource f (Employed e gol tsks) =
    Employed (E.mapResource e f) gol tsks
mapResource f (Unemployed e rsn) =
    Unemployed (E.mapResource e f) rsn

spendResource :: Worker t -> Worker t
spendResource = mapResource (\_ -> Nothing)

giveResource :: T.Resource -> Worker t -> Worker t
giveResource r = mapResource (\_ -> Just r)

getPos :: EWorker -> XYZ
getPos (WorkingWorker (Employed e _ _)) = E.getPos e
getPos (IdleWorker (Unemployed e _)) = E.getPos e

setPos :: XYZ -> Worker t -> Worker t
setPos xyz (Employed e gol tsk) = Employed ((E.setPos xyz) e) gol tsk
setPos xyz (Unemployed e rsn) = Unemployed ((E.setPos xyz) e) rsn

jobToColor :: Status -> Color
jobToColor Working = yellow
jobToColor Idle = white

workerToTile :: E.Job -> Status -> Antiqua.Tile CP437
workerToTile j st =
    let code =
         case j of
             E.Builder -> C'B
             E.Gatherer -> C'G
             E.Miner -> C'M
    in
    Antiqua.Tile code black $ jobToColor st

instance Renderable EWorker where
    render (WorkingWorker (Employed (E.Entity _ pos job _) _ _)) tr =
        let t1 = (fstsnd pos, workerToTile job Working) in
        tr <+ t1
    render (IdleWorker (Unemployed (E.Entity _ pos job _) _)) tr =
        let t1 = (fstsnd pos, workerToTile job Idle) in
        let t2 = (fstsnd $ pos ~~> D'North,  Antiqua.Tile (:?) black white) in
        tr <++ [t1, t2]
