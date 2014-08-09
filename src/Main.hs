{-# LANGUAGE GADTs #-}

module Main where

import Demiurge.Common
import Demiurge.World
import Demiurge.Data.Graph()
import Demiurge.Data.Array2d
import Demiurge.Pathing.Dijkstra()
import Demiurge.Builder
import Demiurge.Data.Coordinate
import Demiurge.Utils
import Demiurge.Blueprint
import Demiurge.Task
import Demiurge.Order

-- take a builder and do their current task
manageTask :: (c ~ GetC t, World w c, Order o t, Task t)
           => Builder c o t
           -> w
           -> OrderPool o t
           -> (Builder c o t, w, OrderPool o t)
manageTask b@(Builder xy ord t rs) w pool =
    if allowed t b w
    then perform t b w pool
    else (Builder xy noneO noneT rs, w, rewind ord : pool)

-- check to see that a builder has orders. if not, give them one if one is available
manageOrder :: (Order o t, Task t)
            => Builder c o t
            -> OrderPool o t
            -> (Builder c o t, OrderPool o t)
manageOrder b pool =
    if (((isNoneO . getOrd) &&& (isNoneT . getTsk)) b)
    then giveOrd b pool
    else (b, pool)

giveOrd :: Order o t => Builder c o t -> OrderPool o t -> (Builder c o t, OrderPool o t)
giveOrd b [] = (b, [])
giveOrd b (x:xs) = (order x b, xs)

genWorld :: ([Builder Cell OrderList (Thing Cell)],
             OrderPool OrderList (Thing Cell),
             Array2d Tile)
genWorld =
    let world = tabulate 10 10 Free (\_ -> Free) in
    let rect = Rect 1 1 10 10 in
    let op = generate world rect in
    let bs = [] in
    (bs, op, world)

update :: (c ~ GetC t, Task t, Coordinate c, World w c, Order o t)
       => [Builder c o t]
       -> w
       -> OrderPool o t
       -> ([Builder c o t], w, OrderPool o t)
update bs world pool =
    let (bs', w', p') = fold3 bs world pool manageTask in
    let (bs'', p'') = fold2 bs' p' manageOrder in
    (bs'', w', p'')

main :: IO ()
main = do
    let (builds, opool, world) = genWorld
    let up bs w op =
         let (bs', w', op') = update bs w op in
         up bs' w' op'
    up builds world opool
    return ()
