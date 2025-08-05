{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Vectorial.Monad where

import Prelude.Linear

class RMonad m where
    return :: (Dupable a) => a %1 -> m a
    (>>=)  :: (Eq a, Dupable a, Eq b, Dupable b) => m a %1 -> (a %1 -> m b) %1 -> m b

infixl 1 >>=

(|*|) :: (RMonad m, Eq a, Eq b, Dupable a, Dupable b) => m a %1 -> m b %1 -> m (a, b)
(|*|) m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (x1, x2)
