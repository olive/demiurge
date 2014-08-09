module Demiurge.Utils where

select :: Bool -> a -> a
select b f t = if b then t else f
