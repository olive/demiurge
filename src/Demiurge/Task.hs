{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies #-}
module Demiurge.Task where

import Prelude hiding (mod)
import Demiurge.World
import Demiurge.Builder
import Demiurge.Common

data BTask a = Path [a]
             | Place a
             | Collect
             | None

class Task t where
    type GetC t
    perform :: (c ~ GetC t, World w c)
            => t
            -> Builder c g o t
            -> w
            -> GoalPool g c (o t) t
            -> (Builder c g o t, w, GoalPool g c (o t) t)
    allowed :: (c ~ GetC t, World w c)
            => t
            -> Builder c g o t
            -> w
            -> Bool
    noneT :: t
    isNoneT :: t -> Bool

taskIsNone :: BTask a -> Bool
taskIsNone None = True
taskIsNone _ = False

instance Task (BTask a) where
    type GetC (BTask a) = a
    noneT = None
    isNoneT = taskIsNone
    allowed (Path (x:_)) _ w = free w x
    allowed (Path []) _ _ = True
    allowed (Place pos) b w = free w pos && hasStone b
    allowed None _ _ = True
    perform (Path (x:xs)) b w pool = (task (Path xs) $ move x b, w, pool)
    perform (Path []) b w pool = (task None b, w, pool)
    perform (Place pos) b w pool = (spendStone b, mod w pos (setType Solid), pool)
    perform None b w pool = (b, w, pool)

