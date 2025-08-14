{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Vectorial.Vector.Internal where

import Data.Complex
import Prelude         hiding ((*>))
import Vectorial.Monad

type RR = Double

newtype CC = CC (Complex RR)
    deriving (Show, Eq, Num, Fractional, Floating)

instance Semigroup CC where
    CC x <> CC y = CC (x + y)

newtype V a = V {unV :: [(CC, a)]}
    deriving (Monoid)

v :: [(CC, a)] -> V a
v = V

instance Eq a => Semigroup (V a) where
    V xs <> V ys = V (foldr (uncurry addV) ys xs)
      where
        addV :: CC -> a -> [(CC, a)] -> [(CC, a)]
        addV c x [] = [(c, x)]
        addV c1 x ((c2, y) : bys)
            | x == y    = (c1 + c2, y) : bys
            | otherwise = (c2, y) : addV c1 x bys

negate :: V a -> V a
negate (V xs) = V [(- c, x) | (c, x) <- xs]

(*>) :: CC -> V a -> V a
(*>) c (V xs) = V $ map (\(c1, x) -> (c * c1, x)) xs

instance RMonad V where
    type Restrict V a = (Eq a)
    return x = V [(1, x)]
    (>>=) :: forall a b. (Restrict V b) => V a -> (a -> V b) -> V b
    (>>=) (V xs) f = Prelude.foldr (\(c, x) v' -> (c *> f x) <> v') mempty xs
