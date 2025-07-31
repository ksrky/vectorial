{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Vectorial.Syntax where

import Vectorial.Vector

data Term
    = Bit Bool
    | Var String
    | IsoApp Iso VTerm
    | Pair VTerm VTerm
    | Let Pattern  VTerm VTerm
    deriving (Show, Eq)

type VTerm = V Term

data Pattern
    = PBit Bool
    | PVar String
    | PPair Pattern Pattern
    deriving (Show, Eq, Ord)

type VPattern = V Pattern

type Iso = [(Pattern, VPattern)]

data Type
    = Qubit
    | Tensor Type Type
    deriving (Show, Eq)

data IsoType = IsoType Type Type
    deriving (Show, Eq)
