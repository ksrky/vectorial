{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Vectorial.Gates where

import Data.Algebra.Linear
import Data.Bool.Linear
import Data.Complex
import Prelude.Linear
import Vectorial.Projection
import Vectorial.Vector     as V

ket0 :: RMonad m => m Bool
ket0 = return False

ket1 :: RMonad m => m Bool
ket1 = return True

ketPlus :: V Bool
ketPlus = V [(1 / sqrt 2, False), (1 / sqrt 2, True)]

ketMinus :: V Bool
ketMinus = V [(1 / sqrt 2, False), (- (1 / sqrt 2), True)]

pauliX :: Bool %1 -> V Bool
pauliX False = return True
pauliX True  = return False

pauliZ :: Bool %1 -> V Bool
pauliZ False = return False
pauliZ True  = negate (return True)

hadamard :: Bool %1 -> V Bool
hadamard False = ketPlus
hadamard True  = ketMinus

phaseS :: Bool %1 -> V Bool
phaseS False = return False
phaseS True  = V [(exp (CC (0 :+ pi / 2)), True)]

phaseT :: Bool %1 -> V Bool
phaseT False = return False
phaseT True  = V [(exp (CC (0 :+ pi / 4)), True)]

cnot :: Bool %1 -> Bool %1 -> V (Bool, Bool)
cnot False False = return (False, False)
cnot False True  = return (False, True)
cnot True False  = return (True, True)
cnot True True   = return (True, False)

cz :: Bool %1 -> Bool %1 -> V (Bool, Bool)
cz False False = return (False, False)
cz False True  = return (False, True)
cz True False  = return (True, False)
cz True True   = negate $ return (True, True)

swap :: (Basis a, Basis b) => a %1 -> b %1 -> V (b, a)
swap x y = return (y, x)

controlU :: (Bool %1 -> V Bool) -> Bool %1 -> Bool %1 -> V (Bool, Bool)
controlU _ False False = return (False, False)
controlU _ False True  = return (False, True)
controlU u True False  = return True |*| u False
controlU u True True   = return True |*| u True

meas :: V Bool -> IO Bool
meas = simulate
