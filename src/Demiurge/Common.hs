module Demiurge.Common where

type XYZ = (Int,Int,Int)

type XY = (Int,Int)

data Rect = Rect Int Int Int Int

rectToBorder :: Int -> Rect -> [XYZ]
rectToBorder k (Rect x y width height) = do
    [(i, j, k) | i <- [x..x+width],
              j <- [y..y+height],
              i == 0 || j == 0 || i == width - 1 || j == height - 1]

{-

data AB = A | B

data Thing a where
    ThingA :: Int -> Thing A
    ThingB :: Int -> Int -> Thing B

data Something = SomeA (Thing A) | SomeB (Thing B)

data Foo = Foo [Something]

class Blah b where
    blah :: b
         -> Thing A
         -> Thing B

instance Blah Foo where
    blah _ (ThingA i) = ThingB i i

what :: Blah b => b -> Something -> Foo
what bl (SomeA th)  = Foo [SomeB $ blah bl th]

-}
