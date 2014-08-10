{-# LANGUAGE GADTs #-}

module Main where

import Data.Maybe

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
import Demiurge.Goal
-- take a builder and do their current task
manageTask :: (c ~ GetC t, World w c, Goal g c, Order o t, Task t)
           => Builder c g o t
           -> w
           -> GoalPool (g c)
           -> (Builder c g o t, w, GoalPool (g c))
manageTask b@(Builder xy goal ord t rs) w pool =
    if allowed t b w
    then perform t b w pool
    else (Builder xy goal noneO noneT rs, w, goal : pool)

-- check to see that a builder has orders. if not, give them one if one is available
manageOrder :: (Order o t, Task t, Goal g c)
            => Builder c g o t
            -> GoalPool (g c)
            -> (Builder c g o t, GoalPool (g c))
manageOrder b pool =
    if (((isNoneO . getOrd) &&& (isNoneT . getTsk)) b)
    then giveGoal b pool
    else (b, pool)

giveGoal :: (Order o t, Goal g c)
        => Builder c g o t
        -> GoalPool (g c)
        -> (Builder c g o t, GoalPool (g c))
giveGoal b gs =
    let (b', gs') = case splitFind (not . isReserved) gs of
                        Just (found, rest) -> (goal found b, reserve found : rest)
                        Nothing -> (b, gs)
    in
    (b', gs')

genWorld :: ([Builder Cell a OrderList (BTask Cell)],
             GoalPool (BGoal Cell),
             Array2d Tile)
genWorld =
    let world = tabulate 10 10 (Tile Free []) (\_ -> (Tile Free [])) in
    let rect = Rect 1 1 10 10 in
    let op = generate world rect in
    let bs = [] in
    (bs, op, world)

update :: (c ~ GetC t, Task t, Coordinate c, World w c, Order o t, Goal g c)
       => [Builder c g o t]
       -> w
       -> GoalPool (g c)
       -> ([Builder c g o t], w, GoalPool (g c))
update bs world pool =
    let (bs', w', p') = fold3 bs world pool manageTask in
    let (bs'', p'') = fold2 bs' p' manageOrder in
    (bs'', w', p'')

main :: IO ()
main = do
    let (builds, gpool, world) = genWorld
    let up bs w gp =
         let (bs', w', gp') = update bs w gp in
         up bs' w' gp'
    up builds world gpool
    return ()
