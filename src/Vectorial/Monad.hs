{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Vectorial.Monad where

import Data.Kind

class RMonad m where
    type Restrict m a :: Constraint
    return :: a %1 -> m a
    (>>=)  :: Restrict m b => m a %1 -> (a %1 -> m b) %1 -> m b

infixl 1 >>=

(|*|) :: (RMonad m, Restrict m (a, b)) => m a %1 -> m b %1 -> m (a, b)
(|*|) m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (x1, x2)
