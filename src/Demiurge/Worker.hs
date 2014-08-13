module Demiurge.Worker where

type Cell = (Int,Int,Int)

data GoalPool g c o t = GoalPool [g c]

