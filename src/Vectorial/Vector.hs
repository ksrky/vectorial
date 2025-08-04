{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Vectorial.Vector where

import Data.Algebra.Linear
import Data.Complex
import Data.List.Linear
import Prelude qualified
import Prelude.Linear
import Unsafe.Linear       qualified as Unsafe
import Vectorial.Monad

type RR = Double

newtype CC = CC (Complex RR)
    deriving (Show, Prelude.Eq, Prelude.Num, Fractional, Floating)

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
    CC (x1 :+ y1) * CC (x2 :+ y2) = withDup (dup2 x1) (dup2 y1) (dup2 x2) (dup2 y2)
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

magn :: CC %1 -> RR
magn c = case move c of Ur (CC z) -> magnitude z

newtype V a = V [(CC, a)]
    deriving (Show, Eq)

instance (Eq a, Dupable a) => Additive (V a) where
    V xs + V ys = V (foldr (uncurry addV) ys xs)
      where
        addV :: CC %1 -> a %1 -> [(CC, a)] %1 -> [(CC, a)]
        addV c x [] = [(c, x)]
        addV c1 x ((c2, y) : bys) = case (dup2 x, dup2 y) of
            ((x', x''), (y', y'')) ->
                case x' == y' of
                    True  -> lseq (consume x'') ((c1 + c2, y'') : bys)
                    False -> (c2, y'') : addV c1 x'' bys

instance (Eq a, Dupable a) => AddIdentity (V a) where
    zero = V []

instance (Eq a, Dupable a) => AdditiveGroup (V a) where
    negate (V xs) = V (map (\(c, x) -> (negate c, x)) xs)

instance (Eq a, Dupable a) => Module CC (V a) where
    (*>) c (V v) = case move c of
        Ur c1 -> V $ map (\(c', x) -> (c1 * c', x)) v

instance (Eq a, Dupable a) => FreeModule CC V a where
    decompose (V xs) = xs
    generate = V

instance RMonad V where
    return x = V [(1, x)]
    (>>=) :: forall a b. (Eq b, Dupable b) => V a %1 -> (a %1 -> V b) %1 -> V b
    (>>=) = Unsafe.toLinear2 bind
      where
        bind :: V a -> (a %1 -> V b) -> V b
        bind (V xs) f = Prelude.foldr (\(c, x) v -> (c *> f x) + v) zero xs
