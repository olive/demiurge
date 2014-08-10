{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}
module Demiurge.Blueprint where

import Demiurge.Common
import Demiurge.Goal
import Demiurge.Data.Graph

class Blueprint p g where
    generate :: (Goal g o t c , Graph a b c) => a b -> p -> GoalPool g c (o t) t




instance Blueprint Rect BGoal where
    generate gr r = undefined


