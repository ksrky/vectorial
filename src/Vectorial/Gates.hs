{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

module Vectorial.Gates where

import Data.Bool.Linear
import Data.Complex
import Prelude.Linear
import Vectorial.Vector as V

ket0 :: V Bool
ket0 = V.return False

ket1 :: V Bool
ket1 = V.return True

ketPlus :: V Bool
ketPlus = V [(1 / sqrt 2, False), (1 / sqrt 2, True)]

ketMinus :: V Bool
ketMinus = V [(1 / sqrt 2, False), (- (1 / sqrt 2), True)]

pauliX :: Bool %1 -> V Bool
pauliX False = V.return True
pauliX True  = V.return False

pauliZ :: Bool %1 -> V Bool
pauliZ False = V.return False
pauliZ True  = negate (V.return True)

hadamard :: Bool %1 -> V Bool
hadamard False = ketPlus
hadamard True  = ketMinus

phaseS :: Bool %1 -> V Bool
phaseS False = V.return False
phaseS True  = V [(exp (CC (0 :+ pi / 2)), True)]

phaseT :: Bool %1 -> V Bool
phaseT False = V.return False
phaseT True  = V [(exp (CC (0 :+ pi / 4)), True)]

cnot :: Bool %1 -> Bool %1 -> V (Bool, Bool)
cnot False False = V.return (False, False)
cnot False True  = V.return (False, True)
cnot True False  = V.return (True, True)
cnot True True   = V.return (True, False)

cz :: Bool %1 -> Bool %1 -> V (Bool, Bool)
cz False False = V.return (False, False)
cz False True  = V.return (False, True)
cz True False  = V.return (True, False)
cz True True   = negate $ V.return (True, True)

manualHH :: V Bool
manualHH = V.do
    x <- ket0
    x' <- hadamard x
    hadamard x'

-- bellCircuit' :: V Bool
-- bellCircuit' = ket0 V.>>= hadamard V.>>= \q1 -> (ket0 V.>>= \q2 -> cnot q1 q2)

bellCircuit :: V (Bool, Bool)
bellCircuit = V.do
    q1 <- ket0
    -- q1' <- hadamard q1
    q2 <- ket0
    cnot q1 q2

groverStep :: V Bool
groverStep = V.do
    q <- ket0
    q1 <- hadamard q      -- Create superposition
    q2 <- pauliZ q1       -- Oracle (phase flip)
    hadamard q2
