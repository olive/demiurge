module Main where

import Demiurge.Common


data Task = Move Cell | None

class Order o where
    next :: o -> (o, Task)
    rewind :: o
    none :: o

data Builder order = Builder Cell order Task

finishO :: Order o => Builder o -> Builder o
finishO = order none

finishT :: Builder o -> Builder o
finishT = task None

order :: Order o => o -> Builder o -> Builder o
order o (Builder c _ tsk) = Builder c o tsk

move :: Cell -> Builder o -> Builder o
move c (Builder _ ord tsk) = Builder c ord tsk

task :: Task -> Builder o -> Builder o
task tsk (Builder c ord _) = Builder c ord tsk

processTask :: Task -> Builder o -> Builder o
processTask None b = b
processTask (Move c) b = finishT $ move c b

isAllowed :: Task -> Builder o -> Builder o
isAllowed = undefined

incrOrder :: Order o => Builder o -> Builder o
incrOrder (Builder pos ord None) =
    let (ord', tsk) = next ord in
    Builder pos ord' tsk

main :: IO ()
main = putStrLn "test"
