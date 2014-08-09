module Demiurge.Utils where

select :: a -> a -> Bool -> a
select f t b = if b then t else f

(&&&) :: (a -> Bool) -> (a -> Bool ) -> a -> Bool
(&&&) f g x = all id [f x, g x]

(|@|) :: (a, b) -> c -> (a, b, c)
(|@|) (x, y) z = (x, y, z)

fold3 :: [a] -> b -> c -> (a -> b -> c -> (a, b, c)) -> ([a], b, c)
fold3 xs w y f = foldl (\(acc, w', y') x -> let (p, q, r) = f x w' y' in (p:acc, q, r)) ([], w, y) xs

fold2 :: [a] -> b -> (a -> b -> (a, b)) -> ([a], b)
fold2 xs w f = foldl (\(acc, w') x -> let (p, q) = f x w' in (p:acc, q)) ([], w) xs
