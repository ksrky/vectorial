{-# LANGUAGE LinearTypes  #-}
{-# LANGUAGE TypeFamilies #-}

module Vectorial.Monad where

import Data.Kind
import Prelude   hiding (Monad (..))

class RMonad m where
    type Restrict m a :: Constraint
    return :: a -> m a
    (>>=)  :: Restrict m b => m a -> (a -> m b) -> m b

infixl 1 >>=

(|*|) :: (RMonad m, Restrict m (a, b)) => m a -> m b -> m (a, b)
(|*|) m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (x1, x2)

join :: (RMonad m, Restrict m a) => m (m a) -> m a
join mma = mma >>= id

inward :: (RMonad m, Restrict m (a, m b)) => m (a, b) -> m (a, m b)
inward mab = mab >>= \(a, b) -> return (a, return b)
