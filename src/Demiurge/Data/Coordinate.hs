module Demiurge.Data.Coordinate where

import Demiurge.Common

class (Ord c, Eq c) => Coordinate c where
    (|+|) :: c -> c -> c
    (|-|) :: c -> c -> c

instance Coordinate XYZ where
    (x, y, z) |+| (a, b, c) = (a + x, b + y, c + z)
    (x, y, z) |-| (a, b, c) = (a - x, b - y, c - z)
