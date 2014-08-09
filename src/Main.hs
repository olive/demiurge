module Main where

import Demiurge.Common()
import Demiurge.World
import Demiurge.Data.Graph()
import Demiurge.Pathing.Dijkstra()
import Demiurge.Builder
import Demiurge.Data.Coordinate

fold3 :: [a] -> b -> c -> (a -> b -> c -> (a, b, c)) -> ([a], b, c)
fold3 xs w y f = foldl (\(acc, w', y') x -> let (p, q, r) = f x w' y' in (p:acc, q, r)) ([], w, y) xs

update :: (Coordinate c, World w c, Order o)
       => [Builder c o]
       -> w
       -> OrderPool o
       -> ([Builder c o], w, OrderPool o)
update bs world pool =
    fold3 bs world pool manageTask

main :: IO ()
main = putStrLn "test"
