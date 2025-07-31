{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RebindableSyntax #-}

module Vectorial.Eval where

import Data.Foldable
import Data.Maybe
import Prelude           hiding (return, (*>), (>>=))
import Vectorial.Algebra
import Vectorial.Syntax
import Vectorial.Vector  as V

type Ctx = [(String, Pattern)]

eval :: Ctx -> Term -> VPattern
eval _ (Bit b) = return $ PBit b
eval ctx (Var x) | Just p <- lookup x ctx = return p
eval _ (Var _) = error "Variable not found"
eval ctx (Pair vt1 vt2) = do
    p1 <- evalV ctx vt1
    p2 <- evalV ctx vt2
    return $ PPair p1 p2
eval ctx (IsoApp iso vt) = do
    p <- evalV ctx vt
    fromMaybe mempty (lookup p iso)
eval ctx (Let pat vt1 vt2) = do
    p1 <- evalV ctx vt1
    case match pat p1 of
        Just ctx' -> evalV (ctx' ++ ctx) vt2
        Nothing   -> mempty

evalV :: Ctx -> VTerm -> VPattern
evalV ctx (V vt) = foldMap (\(c, t) -> c *> eval ctx t) vt

match :: Pattern -> Pattern -> Maybe Ctx
match (PBit b1) (PBit b2) | b1 == b2 = Just []
match (PVar x) p = Just [(x, p)]
match (PPair p1 p2) (PPair q1 q2) = (++) <$> match p2 q2 <*> match p1 q1
match _ _ = Nothing
