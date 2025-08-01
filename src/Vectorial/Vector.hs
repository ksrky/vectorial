{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Vectorial.Vector where

import Data.Complex
import Data.List.Linear
import Data.Monoid.Linear
import Data.Num.Linear
import Prelude qualified
import Prelude.Linear

type RR = Double

newtype CC = CC (Complex RR)
    deriving (Show, Prelude.Num)

instance Consumable CC where
    consume = undefined

instance Dupable CC where
    dup2 = undefined

instance Movable CC where
    move = undefined

instance Eq CC where
    CC (x1 :+ x2) == CC (y1 :+ y2) = x1 == y1 && x2 == y2

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
    (/)= undefined

instance Floating CC where
    pi = CC (pi :+ 0)


instance Semigroup CC where
    (<>) = (+)

instance Monoid CC where
    mempty = 0


newtype V a = V [(CC, a)]
    deriving (Show, Eq)

instance Eq a => Semigroup (V a) where
    (<>) = undefined -- V xs <> V ys = V [(c1 + c2, x) | (c1, x) <- xs, (c2, y) <- ys, x == y]

instance Eq a => Monoid (V a) where
    mempty = V []

instance Eq a => Additive (V a) where
    (+) = (<>)

instance Eq a => AddIdentity (V a) where
    zero = V []

instance Eq a => AdditiveGroup (V a) where
    negate (V xs) = V (map (\(c, x) -> (negate c, x)) xs)

class (Ring a, AdditiveGroup v) => Module a v where
    (*>) :: a -> v %1 -> v

instance Eq a => Module CC (V a) where
    (*>) c (V v) = V $ map (\(c', x) -> (c * c', x)) v

class EqMonad m where
    return :: Eq a => a -> m a
    (>>=)  :: (Eq a, Eq b) => m a %1 -> (a %1 -> m b) %1 -> m b

infixl 1 >>=

instance EqMonad V where
    return x = V [(1, x)]
    (>>=) (V xs) f = undefined xs f -- foldMap (\(c, x) -> (\c' -> c' *> f x) (move c)) xs
