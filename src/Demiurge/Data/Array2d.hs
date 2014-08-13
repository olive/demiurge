module Demiurge.Data.Array2d(
    Array2d(..),
    get,
    zipWithIndex,
    tabulate,
    find,
    foldl,
    getOrElse,
    put,
    inRange,
    (<$*>)
) where

import Prelude hiding (all, foldl)
import Control.Applicative
import Data.Maybe
import qualified Data.Vector as Vec

import Demiurge.Common
import Demiurge.Utils

type Col = Int
type Row = Int
-- | A two dimensional array.
data Array2d elt = Array2d Col Row (Vec.Vector elt)

instance Functor Array2d where
    fmap f (Array2d cols rows v) = Array2d cols rows $ f <$> v

toVec :: Array2d a -> Vec.Vector a
toVec (Array2d _ _ v) = v

toCoords :: Array2d a -> Int -> XY
toCoords (Array2d cols _ _) k = rawToCoords cols k

rawToCoords :: Int -> Int -> XY
rawToCoords cols k = (k `mod` cols, k `quot` cols)

fromCoords :: Array2d a -> XY -> Int
fromCoords (Array2d cols _ _) (i, j) = i + j * cols

unsafeGet :: Array2d a -> Int -> a
unsafeGet (Array2d _ _ v) k = v Vec.! k

-- | Get an element by coordinates or Nothing if the index is out of range.
get :: Array2d a -> XY -> Maybe a
get a c =
    select (Just $ unsafeGet a $ fromCoords a c)
           Nothing
           (not . inRange a $ c)

put :: Array2d a -> XY -> a -> Array2d a
put a@(Array2d cols rows v) c t =
    let i = fromCoords a c in
    let inserted = v Vec.// [(i,t)] in
    select a
           (Array2d cols rows inserted)
           (not . inRange a $ c)


-- | Get an element or return an alternative value.
getOrElse :: Array2d a -> a -> XY -> a
getOrElse arr e c = fromMaybe e (get arr c)

-- | Computes fmap with index.
(<$*>) :: (XY -> a -> b) -> Array2d a -> Array2d b
f <$*> arr@(Array2d cols rows v) = Array2d cols rows $ mapped
    where zipped = Vec.zip (Vec.fromList [0..(cols*rows)-1]) v
          mapped = (\(k, p) -> let (x, y) = toCoords arr k in f (x, y) p) <$> zipped

-- | Zips each element of the array with its index.
zipWithIndex :: Array2d a -> Array2d (XY, a)
zipWithIndex arr = (,) <$*> arr

-- | Fold left with index.
foldl :: (a -> (XY, b) -> a) -> a -> Array2d b -> a
foldl f x arr = Vec.foldl f x $ (toVec . zipWithIndex) arr

-- | Finds the index and element satisfying a predicate.
find :: (a -> Bool) -> Array2d a -> Maybe (XY, a)
find f arr =
    let (Array2d _ _ zipped) = zipWithIndex arr in
    Vec.find (\(_, e) -> f e) zipped

-- | Creates a new Array2d from a generating function.
tabulate :: Col -> Row -> (XY -> a) -> Array2d a
tabulate cols rows f =
    let vec = Vec.generate (cols*rows) (\k -> f $ rawToCoords cols k) in
    Array2d cols rows vec

-- | Returns true iff the coordinates are within the bounds of the array.
inRange :: Array2d a -> XY -> Bool
inRange (Array2d cols rows _) (i, j) =
    (i >= 0 && i <= cols - 1 && j >= 0 && j <= rows - 1)
