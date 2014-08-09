{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Demiurge.Blueprint where

import Control.Applicative((<$>))
import Data.Maybe

import Demiurge.Common
import Demiurge.Order
import Demiurge.Task
import qualified Demiurge.Pathing.Dijkstra as D
import Demiurge.World
import Demiurge.Data.Graph
import Demiurge.Data.Array2d

class Blueprint c p o where
    generate :: (Order o t, Task t, Graph a b c) => a b -> p -> [o]


data Rect = Rect Int Int Int Int

rectToBorder :: Rect -> [Cell]
rectToBorder (Rect x y width height) = do
    [(i, j) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]

mkOL :: (Cell -> Thing Cell) -> OrderList (Thing Cell)
mkOL f = TaskList [f] [f]

instance Blueprint Cell Rect (OrderList (Thing Cell)) where
    generate gr r =
        let border = rectToBorder r in
        let mkPath :: Cell -> Cell -> Thing Cell

            mkPath dst pos = Path $ fromMaybe [] $ D.pfind gr pos dst in
        let t = mkPath <$> border in
        let r :: [OrderList (Thing Cell)]
            r = mkOL <$> t in
        r


