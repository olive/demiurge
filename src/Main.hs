{-# LANGUAGE GADTs #-}

module Main where

import Demiurge.Common()
import Demiurge.World
import Demiurge.Data.Graph()
import Demiurge.Pathing.Dijkstra()
import Demiurge.Builder
import Demiurge.Data.Coordinate
import Demiurge.Utils


update :: (c ~ GetC t, Task t, Coordinate c, World w c, Order o)
       => [Builder c o t]
       -> w
       -> OrderPool o t
       -> ([Builder c o t], w, OrderPool o t)
update bs world pool =
    let (bs', w', p') = fold3 bs world pool manageTask in
    let (bs'', p'') = fold2 bs' p' manageOrder in
    (bs'', w', p'')

main :: IO ()
main = putStrLn "test"
