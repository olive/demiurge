module Demiurge.Common where

type XYZ = (Int,Int,Int)

type XY = (Int,Int)

data Rect = Rect Int Int Int Int

rectToBorder :: Int -> Rect -> [XYZ]
rectToBorder k (Rect x y width height) = do
    [(i, j, k) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]
