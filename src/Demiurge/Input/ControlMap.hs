{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module Demiurge.Input.ControlMap where

import Control.Applicative

import Antiqua.Input.Controls as C

data ControlKey = CK'ZUp
                | CK'ZDown
                | CK'Up
                | CK'Down
                | CK'Left
                | CK'Right

    deriving (Eq, Ord)

data ControlMap a = ControlMap (a, a)

data Index (i :: ControlKey) = Get

sequenceCtrl :: Monad m => ControlMap (m a) -> m (ControlMap a)
sequenceCtrl (ControlMap (x1, x2)) = do
    x1' <- x1
    x2' <- x2
    return $ ControlMap (x1', x2')

class Indexed a i b | a i -> b where
    from :: a -> Index i -> b

instance Indexed (ControlMap a) CK'ZUp a where
    from (ControlMap (x, _)) _ = x

instance Indexed (ControlMap a) CK'ZDown a where
    from (ControlMap (_, x)) _ = x

instance Functor ControlMap where
    fmap f (ControlMap (x1, x2)) =
        ControlMap (f x1, f x2)

instance C.Controls (ControlMap C.TriggerAggregate) where
    updateControls mp win =
        sequenceCtrl $ (flip update) win <$> mp
