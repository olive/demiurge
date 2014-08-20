module Demiurge.Utils where

import Demiurge.Common



updateAt :: Same a => a -> [a] -> [a]
updateAt xx yys =
    let helper x acc (y:ys) = if same x y
                              then (x:ys ++ acc)
                              else helper x (y:acc) ys
        helper x acc [] = x:acc
    in
    helper xx [] yys

fstsnd :: (a, b, c) -> (a, b)
fstsnd (x, y, _) = (x, y)
