module Vectorial.Check where

{-import Vectorial.Syntax
import Vectorial.Vector qualified as V

type Ctx = [(String, Type)]

lookupCtx :: Ctx -> String -> Maybe (Type, Ctx)
lookupCtx ctx x = go [] ctx
  where
    go _ [] = Nothing
    go left ((y, ty) : right)
        | x == y    = Just (ty, left ++ right)
        | otherwise = go ((y, ty) : left) right

infer :: Ctx -> Term -> Maybe (Type, Ctx)
infer ctx (Bit _) = Just (Qubit, ctx)
infer ctx (Var x) = lookupCtx ctx x
infer ctx (Pair vt1 vt2) = do
    (ty1, ctx1) <- inferV ctx vt1
    (ty2, ctx2) <- inferV ctx1 vt2
    return (Tensor ty1 ty2, ctx2)
infer ctx (IsoApp iso vt) = do
    (IsoType ty1 ty2) <- inferIso ctx iso
    ctx' <- checkV ctx vt ty1
    return (ty2, ctx')
infer ctx (Let p vt body) = do

    undefined

inferV :: Ctx -> VTerm -> Maybe (Type, Ctx)
inferV ctx (V.V vt) = undefined

inferIso :: Ctx -> Iso -> Maybe IsoType
inferIso = undefined

checkV :: Ctx -> VTerm -> Type -> Maybe Ctx
checkV = undefined

check :: Ctx -> Term -> Type -> Ctx
check ctx (Var x) ty = undefined
check ctx t ty       = undefined
-}
