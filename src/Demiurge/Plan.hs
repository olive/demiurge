module Demiurge.Plan where

import qualified Data.Map as Map

import Demiurge.Common
import Demiurge.Worker
import qualified Demiurge.Entity as E

data Plan where
    Plan :: [Int]               -- all major ids associated with this plan
         -> [Int]               -- completed major ids
         -> (E.Job :-> [WGoal]) -- major goals
         -> (E.Job -> Goal)     -- minor goals
         -> Maybe Plan          -- successor
         -> Plan

getMajor :: Plan -> E.Job :-> [WGoal]
getMajor (Plan _ _ mp _ _) = mp

getMinor :: Plan -> E.Job -> WGoal
getMinor (Plan _ _ _ mn _) = mn

getSuccessor :: Plan -> Maybe Plan
getSuccessor (Plan _ _ _ _ s) = s


getGoals :: Plan -> E.Job -> [WGoal]
getGoals pl j = Map.findWithDefault ([getMinor pl j]) j (getMajor pl)

isComplete :: Plan -> Bool
isComplete (Plan mp _ _) = Map.null mp

distribute :: Plan -> Worker 'Idle -> Worker 'Working
distribute pl wk =
    let j = (E.getJob . getEntity . pack) wk in
    undefined -- let E.Jobs = getGoals
