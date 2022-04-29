{-# LANGUAGE PatternSynonyms #-}
module Core where
import Control.Arrow(second)
import Control.Monad.State.Strict

import Expr
import Desugar(predefs, blockToExpr)
import Error

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar [Core]
  | CIf Core Core Core
  | CFor Core Core
  | CDef Heap Core
  deriving (Show)

type Heap = [Ident]

data Value = Var Ident | HNF HNF
  deriving (Show)

data HNF
  = VInt Integer
  | VRat Rational
  | VPrim String
  | VArray [Value]
  | VLam Ident Core
  | VRec Ident Core
  | VType Value      -- really a lambda
  deriving (Show)

pattern CInt :: Integer -> Core
pattern CInt x = CValue (HNF (VInt x))
pattern CRat :: Rational -> Core
pattern CRat x = CValue (HNF (VRat x))
pattern CVar :: Ident -> Core
pattern CVar x = CValue (Var x)

type C = State Int

seqC :: [Core] -> Core
seqC acs =
  case concatMap flat acs of
    [] -> impossible acs
    [c] -> c
    cs -> CSeq cs
  where
    flat (CSeq cs) = concatMap flat cs
    flat c = [c]

newIdent :: C Ident
newIdent = undefined

exprToCore :: Expr -> Core
exprToCore = flip evalState 1 . core

blockToExprs :: Block -> [Expr]
blockToExprs (BExpr e) = [e]
blockToExprs (BExprs es) = es

core :: Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = CSeq <$> mapM core es
core (ApplyS e1 e2) = core $ ApplyD (Variable (Ident noLoc "succeeds")) (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (If3 e1 e2 e3) = CIf <$> core e1 <*> core (blockToExpr e2) <*> core (blockToExpr e3)
core (For2 e1 e2) = CFor <$> core e1 <*> core (blockToExpr e2)
core (Unify e1 e2) = CUnify <$> core e1 <*> core e2
core e@Type{} = val e
core e@Choice{} = CBar <$> mapM core (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core e@Lambda{} = val e
core e@Any = val e
core Fail = pure $ CBar []
core e = impossible e

val :: Expr -> C Core
val e = do
  (cs, v) <- value e
  pure $ seqC $ cs ++ [CValue v]

value :: Expr -> C ([Core], Value)
value (LitInt i) = pure ([], HNF $ VInt i)
value (LitRat i) = pure ([], HNF $ VRat i)
value (Variable i@(Ident _ s)) | i `elem` predefs = pure ([], HNF $ VPrim s)
                               | otherwise = pure ([], Var i)
value (Array b) = do
  (css, vs) <- unzip <$> mapM value (blockToExprs b)
  pure (concat css, HNF $ VArray vs)
value (Lambda i e) = do
  e' <- core e
  pure ([], HNF $ VLam i e')
value (Type e) = second (HNF . VType) <$> value e
value Any = pure ([], HNF $ VPrim "any")
value e = do
  e' <- core e
  v <- newIdent
  pure ([CUnify (CVar v) e'], Var v)
  
