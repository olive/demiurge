module Demiurge.Common where

type Cell = (Int,Int)

data GoalPool g c o t = GoalPool [g c]

gpAppend :: g c -> GoalPool g c o t -> GoalPool g c o t
gpAppend x (GoalPool xs) = GoalPool (x:xs)

data Rect = Rect Int Int Int Int

rectToBorder :: Rect -> [Cell]
rectToBorder (Rect x y width height) = do
    [(i, j) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]
