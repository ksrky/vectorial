module Vectorial.Vector where

import Data.Complex
import Data.Foldable
import Prelude           hiding ((*>))
import Vectorial.Algebra qualified as A

type RR = Double

newtype CC = CC (Complex RR)
    deriving (Show, Eq, Num, Fractional, Floating)

(!+) :: RR -> RR -> CC
(!+) x y = CC (x :+ y)

instance Semigroup CC where
    (CC x) <> (CC y) = CC (x + y)

instance Monoid CC where
    mempty = CC (0 :+ 0)

instance A.Additive CC where
    negate (CC x) = CC (negate x)

instance A.Ring CC where
    (CC x) * (CC y) = CC (x * y)
    one = CC (1 :+ 0)

newtype V a = V [(CC, a)]
    deriving (Show, Eq, Functor, Monoid)

instance Eq a => Semigroup (V a) where
    V xs <> V ys = V [(c1 + c2, x) | (c1, x) <- xs, (c2, y) <- ys, x == y]

instance Eq a => A.Additive (V a) where
    zero = mempty
    (+) = (<>)
    negate (V xs) = V [(- c, x) | (c, x) <- xs]

instance Eq a => A.Module CC (V a) where
    (*>) :: CC -> V a -> V a
    (*>) c (V v) = V $ map (\(c', x) -> (c * c', x)) v

class EqMonad m where
    return :: Eq a => a -> m a
    (>>=) :: (Eq a, Eq b) => m a -> (a -> m b) -> m b

instance EqMonad V where
    return x = V [(1, x)]
    (>>=) (V xs) f = fold [c A.*> f x | (c, x) <- xs]
