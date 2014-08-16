module Demiurge.Data.Array3d(
    Array3d,
    cols3,
    rows3,
    layers3,
    get,
    tabulate,
    put,
    getLayer,
    inRange
) where

import Prelude hiding (all, foldl, concat)
import Control.Applicative
import Data.Maybe
import Control.Monad
import qualified Data.Vector as Vec

import Demiurge.Common
import qualified Antiqua.Data.Array2d as A2D

type Col = Int
type Row = Int
type Layer = Int
-- | A three dimensional array.
data Array3d elt = Array3d Col Row Layer (Vec.Vector (A2D.Array2d elt))

cols3 :: Array3d a -> Col
cols3 (Array3d col _ _ _) = col

rows3 :: Array3d a -> Row
rows3 (Array3d _ row _ _) = row

layers3 :: Array3d a -> Layer
layers3 (Array3d _ _ lays _) = lays

instance Functor Array3d where
    fmap f (Array3d cols rows lay v) =
        Array3d cols rows lay $ (\arr -> f <$> arr) <$> v

vecGet :: Vec.Vector a -> Int -> Maybe a
vecGet v i = v Vec.!? i

get :: Array3d a -> XYZ -> Maybe a
get (Array3d _ _ _ vec) (x, y, z) =
    join $ (\a -> A2D.get a (x, y)) <$> vecGet vec z

put :: Array3d a -> XYZ -> a -> Array3d a
put a@(Array3d cols rows lays vec) (x, y, z) t =
    let getted = vecGet vec z in
    let vv = (\arr -> A2D.put arr (x, y) t) <$> getted in
    let pp = (\arr -> vec Vec.// [(z,arr)]) <$> vv in
    fromMaybe a $ (Array3d cols rows lays) <$> pp

getLayer :: Array3d a -> Int -> Maybe (A2D.Array2d a)
getLayer (Array3d _ _ _ vec) z = vecGet vec z

tabulate :: Col -> Row -> Layer -> (XYZ -> a) -> Array3d a
tabulate cols rows layers f =
    let vec = Vec.generate layers (\z -> A2D.tabulate cols rows (\(x, y) -> f (x, y, z))) in
    Array3d cols rows layers vec

inRange :: Array3d a -> XYZ -> Bool
inRange (Array3d cols rows layers _) (i, j, k) =
    (i >= 0 && i <= cols - 1 && j >= 0 && j <= rows - 1 && k >= 0 && k <= layers - 1)
