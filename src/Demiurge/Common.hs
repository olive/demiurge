{-# OPTIONS_GHC -fno-warn-orphans #-}
module Demiurge.Common where

import qualified Data.Map as Map
import Antiqua.Data.Coordinate

type a :-> b = Map.Map a b


type XYZ = (Int,Int,Int)
data Direction3 = D'North | D'South | D'East | D'West | D'Upward | D'Downward


instance Coordinate XYZ where
    (x, y, z) |+| (a, b, c) = (a + x, b + y, c + z)
    (x, y, z) |-| (a, b, c) = (a - x, b - y, c - z)
    neg (x, y, z) = (-x, -y, -z)


instance Space XYZ Direction3 where
    (x, y, z) ~~> D'North    = (x, y-1,z)
    (x, y, z) ~~> D'South    = (x, y+1,z)
    (x, y, z) ~~> D'West     = (x-1, y,z)
    (x, y, z) ~~> D'East     = (x+1, y,z)
    (x, y, z) ~~> D'Upward   = (x, y,z+1)
    (x, y, z) ~~> D'Downward = (x, y,z-1)



data Rect = Rect Int Int Int Int

rectToBorder :: Int -> Rect -> [XYZ]
rectToBorder k (Rect x y width height) = do
    [(i, j, k) | i <- [x..x+width],
                 j <- [y..y+height],
                 i == 0 || j == 0 || i == width - 1 || j == height - 1]



class Same a where
    same :: a -> a -> Bool

nearestDir :: XYZ -> XYZ -> Direction3
nearestDir (x, y, _) (a, b, _)
    | abs (x - a) > abs (y - b) =
            if x < a
            then D'West
            else D'East
    | otherwise =
            if y < b
            then D'North
            else D'South
