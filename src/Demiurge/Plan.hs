module Demiurge.Plan where

import qualified Data.Map as Map

import Demiurge.Common
import Demiurge.Worker
import qualified Demiurge.Entity as E

data Plan where
    Plan :: [Int]               -- all major ids associated with this plan
         -> [Int]               -- completed major ids
         -> (E.Job :-> [WGoal]) -- major goals
         -> (E.Job -> WGoal)    -- minor goals
         -> Maybe Plan          -- successor
         -> Plan
data Schema = Schema [Plan]

getMajor :: Plan -> E.Job :-> [WGoal]
getMajor (Plan _ _ mp _ _) = mp

getMinor :: Plan -> E.Job -> WGoal
getMinor (Plan _ _ _ mn _) = mn

getSuccessor :: Plan -> Maybe Plan
getSuccessor (Plan _ _ _ _ s) = s


getGoals :: Plan -> E.Job -> [WGoal]
getGoals pl j = Map.findWithDefault ([getMinor pl j]) j (getMajor pl)

isComplete :: Plan -> Bool
isComplete (Plan _ _ mp _ _) = Map.null mp

distribute :: Plan -> Worker 'Idle -> Worker 'Working
distribute pl wk =
    let goal = (E.getJob . getEntity . pack) wk in
    error "unimplemented" -- let E.Jobs = getGoals

reinsert :: WGoal -> Plan -> Plan
reinsert (Minor _) p = p
reinsert mj@(Major j _ _) (Plan all' left maj mn suc)  =
    Plan all' left (Map.insertWith (++) j [mj] maj) mn suc
