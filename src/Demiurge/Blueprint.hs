{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}
module Demiurge.Blueprint where

import Control.Applicative((<$>))
import Data.Maybe

import Demiurge.Common
import Demiurge.Goal
import qualified Demiurge.Pathing.Dijkstra as D
import Demiurge.Data.Graph

class Blueprint p g where
    generate :: (Goal g c, Graph a b c) => a b -> p -> [g c]




instance Blueprint Rect BGoal where
    generate gr r = undefined


