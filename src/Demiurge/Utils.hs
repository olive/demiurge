module Demiurge.Utils where

select :: Bool -> a -> a -> a
select b f t = if b then t else f

(&&&) :: (a -> Bool) -> (a -> Bool ) -> a -> Bool
(&&&) f g x = all id [f x, g x]
