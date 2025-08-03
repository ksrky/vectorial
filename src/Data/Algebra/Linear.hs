{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Algebra.Linear where

import Data.Num.Linear
import GHC.Generics
import Data.Unrestricted.Linear
import Prelude.Linear
import Prelude qualified

class (Ring r, AdditiveGroup v) => Module r v where
    (*>) :: r %1 -> v %1 -> v

-- | @compose . decompose = id@
class (Ring r, Module r (v a)) => FreeModule r v a where
    decompose :: v a %1 -> [(r, a)]
    generate  :: [(r, a)] %1 -> v a

class GSeparable f where
    gbasis :: [f a]

instance GSeparable V1 where
    gbasis = []

instance GSeparable U1 where
    gbasis = [U1]

instance (GSeparable a, GSeparable b) => GSeparable (a :+: b) where
    gbasis = Prelude.map L1 gbasis Prelude.++ Prelude.map R1 gbasis

instance (GSeparable a, GSeparable b) => GSeparable (a :*: b) where
    gbasis = [a :*: b | a <- gbasis, b <- gbasis]

instance GSeparable a => GSeparable (M1 i c a) where
    gbasis = Prelude.map M1 gbasis

instance Separable c => GSeparable (K1 i c) where
    gbasis = Prelude.map K1 basis

class Movable a => Separable a where
    basis :: [a]
    default basis :: (Generic a, GSeparable (Rep a)) => [a]
    basis = Prelude.map to gbasis

instance Separable ()

instance Separable Bool

instance (Separable a, Separable b) => Separable (a, b)

instance Separable a => Separable (Maybe a)

instance (Separable a, Separable b) => Separable (Either a b) where
