{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Demiurge.Data.Coordinate where

import Demiurge.Common

class Coordinate c where
    (|+|) :: c -> c -> c
    (|-|) :: c -> c -> c
    adj :: c -> [c]

instance Coordinate Cell where
    (x, y) |+| (w, z) = (w + x, y + z)
    (x, y) |-| (w, z) = (x - w, y - z)
    adj (x, y) = [(x-1, y), (x+1, y), (x, y+1), (x, y-1)]
