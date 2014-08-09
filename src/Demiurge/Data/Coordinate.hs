module Demiurge.Data.Coordinate where


class Coordinate c where
    (|+|) :: c -> c -> c
    (|-|) :: c -> c -> c
    adj :: c -> [c]
