{-# LANGUAGE RankNTypes, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Demiurge.Resource where


data Resource = Stone

data LatentResource = Latent Resource Bool
