module Demiurge.Builder where

import Demiurge.Data.Coordinate
import Demiurge.World

type OrderPool o = [o]

manageTask :: (World w c, Order o)
           => Builder c o
           -> w
           -> OrderPool o
           -> (Builder c o, w, OrderPool o)
manageTask (Builder xy ord (Path (pos:ps))) w pool =
    if (free w pos)
    then (Builder pos ord (Path ps), w, pool)
    else (Builder xy none None, w, ord:pool)
manageTask (Builder xy ord (Path [])) w pool =
    let (no, tsk) = next ord in
    (Builder xy no tsk, w, pool)
manageTask (Builder xy ord None) w pool =
    let (no, tsk) = next ord in
    (Builder xy no tsk, w, pool)

data Task c = Path [c] | None

--class Task t where
--    perform :: World w c => w -> Builder c

class Order o where
    next :: o -> (o, Task c)
    rewind :: o
    none :: o

data Builder c o = Builder c o (Task c)

finishO :: (Coordinate c, Order o) => Builder c o -> Builder c o
finishO = order none

finishT :: Builder c o -> Builder c o
finishT = task None

order :: Order o => o -> Builder c o -> Builder c o
order o (Builder c _ tsk) = Builder c o tsk

move :: Coordinate c => c -> Builder c o -> Builder c o
move c (Builder _ ord tsk) = Builder c ord tsk

task :: Task c -> Builder c o -> Builder c o
task tsk (Builder c ord _) = Builder c ord tsk

incrOrder :: Order o => Builder c o -> Builder c o
incrOrder (Builder pos ord None) =
    let (ord', tsk) = next ord in
    Builder pos ord' tsk

