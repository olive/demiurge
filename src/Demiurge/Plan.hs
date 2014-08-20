module Demiurge.Plan where

import qualified Data.Map as Map

import Demiurge.Common
import Demiurge.Worker
import qualified Demiurge.Entity as E

data Plan where
    Plan :: (E.Job :-> [WGoal]) -> (E.Job -> WGoal) -> Plan

getMajor :: Plan -> E.Job :-> [WGoal]
getMajor (Plan mp _) = mp

getMinor :: Plan -> E.Job -> WGoal
getMinor (Plan _ mn) = mn
getGoals :: Plan -> E.Job -> [WGoal]
getGoals pl j = Map.findWithDefault ([getMinor pl j]) j (getMajor pl)

isComplete :: Plan -> Bool
isComplete (Plan mp _) = Map.null mp

distribute :: Plan -> Worker 'Idle -> Worker 'Working
distribute pl wk =
    let j = (E.getJob . getEntity . pack) wk in
    undefined -- let E.Jobs = getGoals
