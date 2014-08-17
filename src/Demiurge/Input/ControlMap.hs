{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module Demiurge.Input.ControlMap where

import Control.Applicative
import GHC.TypeLits


data True
data False

type family TypeEqF a b where
  TypeEqF a a = True
  TypeEqF a b = False

type TypeNeq a b = TypeEqF a b ~ False

data Vec :: * -> Nat -> * where
  Nil :: Vec a 0
  Cons :: a -> Vec a n -> Vec a (n+1)

data Getter (i :: Nat) = Get

class ((<=) i (n - 1)) => GetVec a (n::Nat) (i::Nat) where
    get :: Vec a n -> Getter i -> a

index :: ((<=) i (n - 1), TypeNeq 0 i) => Vec a n -> Getter i -> a
index (Cons x tl) _ = x
index Nil _ = undefined

--instance GetVec a 1 0 where
--    get (Cons x _) _ = x
--
--instance (TypeNeq n 1) => GetVec a n 0 where
--    get (Cons x _) _ = x
--
--instance ((<=) i (n - 1), TypeNeq 0 i, GetVec a n (i - 1)) => GetVec a n i where
--    get (Cons _ tl) _ = get tl (Get  :: Getter (i - 1))



data ControlKey = CK'ZUp
                | CK'ZDown
                | CK'Up
                | CK'Down
                | CK'Left
                | CK'Right

    deriving (Eq, Ord)

class Integrable a b where


data ControlMap a = ControlMap (a, a, a, a, a)

data Index (i :: ControlKey) = Getb

sequenceCtrl :: Monad m => ControlMap (m a) -> m (ControlMap a)
sequenceCtrl (ControlMap (x1, x2, x3, x4, x5)) = do
    x1' <- x1
    x2' <- x2
    x3' <- x3
    x4' <- x4
    x5' <- x5
    return $ ControlMap (x1', x2', x3', x4', x5')

class Indexed a i b | a i -> b where
    from :: a -> Index i -> b

instance Indexed (ControlMap a) 'CK'ZUp a where
    from (ControlMap (x, _, _, _, _)) _ = x

instance Indexed (ControlMap a) 'CK'ZDown a where
    from (ControlMap (_, x, _, _, _)) _ = x

instance Functor ControlMap where
    fmap f (ControlMap (x1, x2, x3, x4, x5)) =
        ControlMap (f x1, f x2, f x3, f x4, f x5)
