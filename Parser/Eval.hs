{-# LANGUAGE PatternSynonyms #-}
module Eval where

--import Expr(Ident(..), noLoc)
import Core
import Error

pattern CPrim :: String -> Core
pattern CPrim s = CValue (HNF (HPrim s))

pattern CArray :: [Value] -> Core
pattern CArray vs = CValue (HNF (HArray vs))

pattern CUnOp :: String -> Value -> Core
pattern CUnOp op v = CApply (CPrim op) (CValue v)

pattern CBinOp :: String -> Value -> Value -> Core
pattern CBinOp op v1 v2 = CApply (CPrim op) (CArray [v1, v2])

pattern VInt :: Integer -> Value
pattern VInt i = HNF (HInt i)

pattern CInt :: Integer -> Core
pattern CInt i = CValue (VInt i)

-------------

-- Reduce until we reach HNF
eval :: Core -> Core
eval e | isHNF e = e
       | otherwise = evalSteps e

isHNF :: Core -> Bool
isHNF (CValue (HNF _)) = True
isHNF _ = False

-- Take some evaluation steps
evalSteps :: Core -> Core
evalSteps = evalSucceeds . evalPrimOps

-- succeeds{v}  -->  v
evalSucceeds :: Core -> Core
evalSucceeds = f
  where
    f (CSucceeds e@CValue{}) = f e
    -- XXX add WRONG
    f e = composOp f e

-- Reduce applications of primops
evalPrimOps :: Core -> Core
evalPrimOps = f
  where
    f (CUnOp  "pre'-'" (VInt i)) = CInt $ -i
    f (CBinOp "in'+'"  v1 v2) = arith (+) v1 v2
    f (CBinOp "in'-'"  v1 v2) = arith (-) v1 v2
    f (CBinOp "in'*'"  v1 v2) = arith (*) v1 v2
    f (CBinOp "in'/'"  (VInt i1) (VInt i2)) | i2 == 0 = CFail
                                            | otherwise = CInt $ i1 - i2
    f (CBinOp "in'<'"   v1 v2) = cmp (<)  v1 v2
    f (CBinOp "in'<='"  v1 v2) = cmp (<=) v1 v2
    f (CBinOp "in'>'"   v1 v2) = cmp (>)  v1 v2
    f (CBinOp "in'>='"  v1 v2) = cmp (>=) v1 v2
    f e = composOp f e

    arith op (VInt i1) (VInt i2) = CInt $ op i1 i2
    arith _ _ _ = unimplemented
    
    cmp op (VInt i1) (VInt i2) | op i1 i2 = CInt i1
                               | otherwise = CFail
    cmp _ _ _ = unimplemented
