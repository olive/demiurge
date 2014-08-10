module Demiurge.Common where

type Cell = (Int,Int)

type GoalPool o = [o]


data Rect = Rect Int Int Int Int

rectToBorder :: Rect -> [Cell]
rectToBorder (Rect x y width height) = do
    [(i, j) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]
