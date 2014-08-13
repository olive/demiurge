{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Demiurge.Data.Coordinate where

import Demiurge.Common

class Coordinate c where
    (|+|) :: c -> c -> c
    (|-|) :: c -> c -> c
    adj :: c -> [c]

instance Coordinate XYZ where
    (x, y, z) |+| (a, b, c) = (a + x, b + y, c + z)
    (x, y, z) |-| (a, b, c) = (a - x, b - y, c - z)
    adj (x, y, z) = [(x-1, y, z), (x+1, y, z), (x, y+1, z), (x, y-1, z)]
