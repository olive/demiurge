{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}
module Demiurge.Blueprint where

import Control.Applicative((<$>))
import Data.Maybe

import Demiurge.Common
import Demiurge.Order
import Demiurge.Task
import qualified Demiurge.Pathing.Dijkstra as D
import Demiurge.Data.Graph

class Blueprint p c o t | o t -> c where
    generate :: (Order o t, Task t, Graph a b c) => a b -> p -> [o t]


data Rect = Rect Int Int Int Int

rectToBorder :: Rect -> [Cell]
rectToBorder (Rect x y width height) = do
    [(i, j) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]

mkOL :: ([Cell -> Thing Cell]) -> OrderList (Thing Cell)
mkOL f = TaskList f f

instance Blueprint Rect Cell OrderList (Thing Cell) where
    generate gr r =
        let border = rectToBorder r in
        let mkPath :: Cell -> (Cell -> Thing Cell)
            mkPath dst pos = Path $ fromMaybe [] $ D.pfind gr pos dst
        in
        let mkBuild :: Cell -> (Cell -> Thing Cell)
            mkBuild dst _ = Build dst
        in
        let mkThing :: Cell -> [Cell -> Thing Cell]
            mkThing dst = [mkPath dst, mkBuild dst]
        in
        let t = mkThing <$> border in
        (mkOL <$> t)


