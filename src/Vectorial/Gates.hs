module Vectorial.Gates where

import Data.Complex
import Vectorial.Vector as V
import Vectorial.Algebra as A

ket0 :: V Bool
ket0 = V.return False

ket1 :: V Bool
ket1 = V.return True

ketPlus :: V Bool
ketPlus = V [(1 / sqrt 2, False), (1 / sqrt 2, True)]

ketMinus :: V Bool
ketMinus = V [(1 / sqrt 2, False), (- (1 / sqrt 2), True)]

pauliX :: Bool -> V Bool
pauliX False = V.return True
pauliX True  = V.return False

pauliZ :: Bool -> V Bool
pauliZ False = V.return False
pauliZ True  = A.negate $ V.return True

hadamard :: Bool -> V Bool
hadamard False = ketPlus
hadamard True  = ketMinus

phaseS :: Bool -> V Bool
phaseS False = V.return False
phaseS True  = V [(exp (CC (0 :+ pi / 2)), True)]

phaseT :: Bool -> V Bool
phaseT False = V.return False
phaseT True  = V [(exp (CC (0 :+ pi / 4)), True)]

cnot :: Bool -> Bool -> V (Bool, Bool)
cnot False False = V.return (False, False)
cnot False True  = V.return (False, True)
cnot True False  = V.return (True, True)
cnot True True   = V.return (True, False)

cz :: Bool -> Bool -> V (Bool, Bool)
cz False False = V.return (False, False)
cz False True  = V.return (False, True)
cz True False  = V.return (True, False)
cz True True   = A.negate $ V.return (True, True)
