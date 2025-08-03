{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Algebra.Linear where

import Data.Num.Linear
import GHC.Generics
import Data.Unrestricted.Linear
import Prelude.Linear
import Prelude qualified

class (Ring r, AdditiveGroup v) => Module r v | v -> r where
    (*>) :: r %1 -> v %1 -> v

class GBasis f where
    gbasis :: [f a]

instance GBasis V1 where
    gbasis = []

instance GBasis U1 where
    gbasis = [U1]

instance (GBasis a, GBasis b) => GBasis (a :+: b) where
    gbasis = Prelude.map L1 gbasis Prelude.++ Prelude.map R1 gbasis

instance (GBasis a, GBasis b) => GBasis (a :*: b) where
    gbasis = [a :*: b | a <- gbasis, b <- gbasis]

instance GBasis a => GBasis (M1 i c a) where
    gbasis = Prelude.map M1 gbasis

instance Basis c => GBasis (K1 i c) where
    gbasis = Prelude.map K1 basis

class (Eq a, Movable a) => Basis a where
    basis :: [a]
    default basis :: (Generic a, GBasis (Rep a)) => [a]
    basis = Prelude.map to gbasis

instance Basis ()

instance Basis Bool

instance (Basis a, Basis b) => Basis (a, b)

instance Basis a => Basis (Maybe a)

instance (Basis a, Basis b) => Basis (Either a b) where

-- | @compose . decompose = id@
class (Module r (v a), Basis a) => FreeModule r v a | v -> r where
    decompose :: v a -> [(r, a)]
    compose :: [(r, a)] -> v a
