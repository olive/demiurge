{-# LANGUAGE GADTs #-}

module Main where

import Demiurge.Common
import Demiurge.World
import Demiurge.Data.Graph()
import Demiurge.Pathing.Dijkstra()
import Demiurge.Builder
import Demiurge.Data.Coordinate
import Demiurge.Utils
import Demiurge.Blueprint()
import Demiurge.Task
import Demiurge.Order

-- take a builder and do their current task
manageTask :: (c ~ GetC t, World w c, Order o, Task t)
           => Builder c o t
           -> w
           -> OrderPool o t
           -> (Builder c o t, w, OrderPool o t)
manageTask b@(Builder xy ord t) w pool =
    if allowed t b w
    then perform t b w pool
    else (Builder xy noneO noneT, w, rewind ord : pool)

-- check to see that a builder has orders. if not, give them one if one is available
manageOrder :: (Order o, Task t)
            => Builder c o t
            -> OrderPool o t
            -> (Builder c o t, OrderPool o t)
manageOrder b pool =
    if (((isNoneO . getOrd) &&& (isNoneT . getTsk)) b)
    then giveOrd b pool
    else (b, pool)

giveOrd :: Order o => Builder c o t -> OrderPool o t -> (Builder c o t, OrderPool o t)
giveOrd b [] = (b, [])
giveOrd b (x:xs) = (order x b, xs)





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
