{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Vectorial.Vector where

import Data.Algebra.Linear
import Data.Complex
import Data.List.Linear
import Prelude qualified
import Prelude.Linear
import Unsafe.Linear       qualified as Unsafe

type RR = Double

newtype CC = CC (Complex RR)
    deriving (Show, Prelude.Eq, Prelude.Num)

instance Consumable CC where
    consume (CC (x :+ y)) = consume x <> consume y

instance Dupable CC where
    dup2 (CC (x :+ y)) = withDup (dup2 x) (dup2 y)
      where
        withDup :: (RR, RR) %1 -> (RR, RR) %1 -> (CC, CC)
        withDup (x1, x2) (y1, y2) = (CC (x1 :+ y1), CC (x2 :+ y2))

instance Movable CC where
    move (CC (x :+ y)) = case (move x, move y) of
        (Ur x', Ur y') -> Ur (CC (x' :+ y'))

instance Eq CC where
    CC (x1 :+ x2) == CC (y1 :+ y2) = x1 == y1 && x2 == y2

instance Prelude.Semigroup CC where
    CC x <> CC y = CC (x Prelude.+ y)

instance Prelude.Monoid CC where
    mempty = CC (0 :+ 0)

instance Additive CC where
    CC (x1 :+ y1) + CC (x2 :+ y2) = CC ((x1 + x2) :+ (y1 + y2))

instance AddIdentity CC where
    zero = 0

instance AdditiveGroup CC where
    negate (CC (x :+ y)) = CC (negate x :+ negate y)

instance Multiplicative CC where
    (CC (x1 :+ y1)) * (CC (x2 :+ y2)) = withDup (dup2 x1) (dup2 y1) (dup2 x2) (dup2 y2)
      where
        withDup :: (RR, RR) %1 -> (RR, RR) %1 -> (RR, RR) %1 -> (RR, RR) %1 -> CC
        withDup (x1', x1'') (y1', y1'') (x2', x2'') (y2', y2'') =
            CC ((x1' * x2' - y1' * y2') :+ (x1'' * y2'' + y1'' * x2''))

instance MultIdentity CC where
    one = CC (1 :+ 0)

instance Semiring CC

instance Ring CC

instance FromInteger CC where
    fromInteger n = CC (fromInteger n :+ 0)

instance Fractional CC where
    fromRational r = CC (fromRational r :+ 0)
    CC (x1 :+ y1) / CC (x2 :+ y2) =
        let denom = x2 * x2 + y2 * y2
        in CC ((x1 * x2 + y1 * y2) / denom :+ (y1 * x2 - x1 * y2) / denom)

instance Floating CC where
    pi = CC Prelude.pi
    exp (CC z) = CC (Prelude.exp z)
    log (CC z) = CC (Prelude.log z)
    sin (CC z) = CC (Prelude.sin z)
    cos (CC z) = CC (Prelude.cos z)
    asin (CC z) = CC (Prelude.asin z)
    acos (CC z) = CC (Prelude.acos z)
    atan (CC z) = CC (Prelude.atan z)
    sinh (CC z) = CC (Prelude.sinh z)
    cosh (CC z) = CC (Prelude.cosh z)
    asinh (CC z) = CC (Prelude.asinh z)
    acosh (CC z) = CC (Prelude.acosh z)
    atanh (CC z) = CC (Prelude.atanh z)


instance Semigroup CC where
    (<>) = (+)

instance Monoid CC where
    mempty = 0

newtype V a = V [(CC, a)]
    deriving (Show, Eq)

instance (Eq a, Movable a) => Additive (V a) where
    V xs + V ys = case (move xs, move ys) of
        (Ur xs', Ur ys') -> Prelude.foldr add (V ys') xs'
      where
        add :: (CC, a) -> V a -> V a
        add (c, x) (V xs') = V (addV c x xs')

        addV :: CC -> a -> [(CC, a)] -> [(CC, a)]
        addV c x [] = [(c, x)]
        addV c1 x ((c2, y) : bys)
            | x == y    = (c1 + c2, y) : bys
            | otherwise = (c2, y) : addV c1 x bys

instance (Eq a, Movable a) => AddIdentity (V a) where
    zero = V []

instance (Eq a, Movable a) => AdditiveGroup (V a) where
    negate (V xs) = V (map (\(c, x) -> (negate c, x)) xs)

instance (Eq a, Movable a) => Module CC (V a) where
    (*>) c (V v) = case move c of
        Ur c1 -> V $ map (\(c', x) -> (c1 * c', x)) v

class EqMonad m where
    return :: Eq a => a -> m a
    (>>=)  :: (Eq a, Eq b, Movable b) => m a %1 -> (a %1 -> m b) %1 -> m b

infixl 1 >>=

instance EqMonad V where
    return x = V [(1, x)]
    (>>=) :: forall a b. (Eq a, Eq b, Movable b) => V a %1 -> (a %1 -> V b) %1 -> V b
    (>>=) = Unsafe.coerce bind
      where
        bind :: V a -> (a -> V b) -> V b
        bind (V xs) f = Prelude.foldr (\(c, x) v -> (c *> f x) + v) zero xs
