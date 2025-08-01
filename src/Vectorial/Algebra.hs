{-# LANGUAGE NoImplicitPrelude #-}

module Vectorial.Algebra where

import Prelude (Monoid (..), Semigroup (..))

class Monoid a => Additive a where
    zero :: a
    (+) :: a -> a -> a
    negate :: a -> a
    zero = mempty
    (+) = (<>)

class Additive a => Ring a where
    (*) :: a -> a -> a
    one :: a

class (Ring a, Additive v) => Module a v where
    (*>) :: a -> v -> v
