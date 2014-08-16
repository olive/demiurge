module Demiurge.Goal where

import Demiurge.Common

data Major
data Minor

type PlanID = Int
data AGoal a c where
    Build :: Int -> PlanID -> c -> AGoal Major c
    Move ::  Int -> PlanID -> c -> AGoal Minor c
    Mine ::  Int -> PlanID -> c -> AGoal Major c
    Stock :: Int -> PlanID -> c -> c -> AGoal Major c

getGoalID :: AGoal a c -> Int
getGoalID (Build i _ _) = i
getGoalID (Move i _ _) = i
getGoalID (Mine i _ _) = i
getGoalID (Stock i _ _ _) = i


getParent :: AGoal a c -> PlanID
getParent (Build _ i _) = i
getParent (Move _ i _) = i
getParent (Mine _ i _) = i
getParent (Stock _ i _ _) = i


class Same g => Goal g where
    parent :: g -> PlanID

instance Same (AGoal g c) where
    same g1 g2 = getGoalID g1 == getGoalID g2

instance Goal (AGoal g c) where
    parent = getParent
