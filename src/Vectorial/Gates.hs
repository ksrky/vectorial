module Vectorial.Gates where

import Data.Complex
import Vectorial.Syntax
import Vectorial.Vector as V

ket0 :: VPattern
ket0 = V.return (PBit False)

ket1 :: VPattern
ket1 = V.return (PBit True)

ketPlus :: VPattern
ketPlus = V [(1 / sqrt 2, PBit False), (1 / sqrt 2, PBit True)]

ketMinus :: VPattern
ketMinus = V [(1 / sqrt 2, PBit False), (- (1 / sqrt 2), PBit True)]

patTerm :: Pattern -> Term
patTerm (PBit b)      = Bit b
patTerm (PVar x)      = Var x
patTerm (PPair p1 p2) = Pair (V.return $ patTerm p1) (V.return $ patTerm p2)

pauliX :: Iso
pauliX = [ (PBit False, V.return (PBit True))
         , (PBit True, V.return (PBit False))
         ]

pauliZ :: Iso
pauliZ = [ (PBit False, V.return (PBit False))
         , (PBit True, V.return (PBit True))
         ]

hadamard :: Iso
hadamard = [ (PBit False, ketPlus)
           , (PBit True, ketMinus)
           ]

phaseS :: Iso
phaseS = [ (PBit False, V.return (PBit False))
         , (PBit True, V [(exp (CC (0 :+ pi / 2)), PBit True)])
         ]

phaseT :: Iso
phaseT = [ (PBit False, V.return (PBit False))
         , (PBit True, V [(exp (CC (0 :+ pi / 4)), PBit True)])
         ]

cnot :: Iso
cnot = [ (PPair (PBit False) (PBit False), V.return (PPair (PBit False) (PBit False)))
       , (PPair (PBit False) (PBit True), V.return (PPair (PBit False) (PBit True)))
       , (PPair (PBit True) (PBit False), V.return (PPair (PBit True) (PBit True)))
       , (PPair (PBit True) (PBit True), V.return (PPair (PBit True) (PBit False)))
       ]

cz :: Iso
cz = [ (PPair (PBit False) (PBit False), V.return (PPair (PBit False) (PBit False)))
     , (PPair (PBit False) (PBit True), V.return (PPair (PBit False) (PBit True)))
     , (PPair (PBit True) (PBit False), V.return (PPair (PBit True) (PBit False)))
     , (PPair (PBit True) (PBit True), V [(-1, PPair (PBit True) (PBit True))])
     ]
