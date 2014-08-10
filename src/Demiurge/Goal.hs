{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts #-}
module Demiurge.Goal where

import Data.Maybe

import Demiurge.Task
import Demiurge.Order
import Demiurge.Common
import Demiurge.Data.Graph

import qualified Demiurge.Pathing.Dijkstra as D

class Goal g o t c | g -> o, g -> t, g -> c, t -> c, o -> c where
    toOrders :: (Task t, Order o t, Graph a b c)
             => g c
             -> a b
             -> OrderList t
    isReserved :: g c -> Bool
    reserve :: g c -> g c


data BGoal a = Build a Bool

res :: BGoal a -> BGoal a
res (Build x _) = Build x True

isRes :: BGoal a -> Bool
isRes (Build _ b) = b

mkOL :: ([Cell -> BTask Cell]) -> OrderList (BTask Cell)
mkOL f = TaskList f

instance Goal BGoal OrderList (BTask Cell) Cell where
    reserve = res
    isReserved = isRes
    toOrders (Build dst _) gr =
        let mkPath :: Cell -> BTask Cell
            mkPath pos = Path $ fromMaybe [] $ D.pfind gr pos dst
        in
        let mkPlace :: Cell -> BTask Cell
            mkPlace _ = Place dst
        in
        let mkTask :: [Cell -> BTask Cell]
            mkTask = [mkPath, mkPlace]
        in
        mkOL mkTask
