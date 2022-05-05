{-# LANGUAGE PatternSynonyms #-}
module Core(exprToCore, coreToExpr) where
import Control.Arrow(second)
import Control.Monad.State.Strict

import Print
import Expr
import Desugar(predefs)
import Error

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar [Core]
  | CIf Core Core
  | CFor Core
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

{-
pattern CInt :: Integer -> Core
pattern CInt x = CValue (HNF (VInt x))
pattern CRat :: Rational -> Core
pattern CRat x = CValue (HNF (VRat x))
-}
pattern CVar :: Ident -> Core
pattern CVar x = CValue (Var x)

type C = State ([Ident], Int)

seqC :: [Core] -> Core
seqC acs =
  case concatMap flat acs of
    [] -> impossible acs
    [c] -> c
    cs -> CSeq cs
  where
    flat (CSeq cs) = concatMap flat cs
    flat c = [c]

newTmp :: C Ident
newTmp = do
  (is, n) <- get
  let i = Ident noLoc ("$c" ++ show n)
  put (i:is, n+1)
  pure i

getTmps :: C [Ident]
getTmps = do
  (is, n) <- get
  put ([], n)
  pure is

exprToCore :: Expr -> Core
exprToCore = flip evalState ([], 1) . core

core :: Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = CSeq <$> mapM core es
core (ApplyS e1 e2) = core $ ApplyD (Variable (Ident noLoc "succeeds")) (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (Unify e1 e2) = CUnify <$> core e1 <*> core e2
{-
core e@Type{} = val e
core (Def is e) = do
  e' <- core e
  ts <- getTmps
  pure $ CDef (is ++ ts) e'
core e@Lambda{} = val e
core (IfC e1 e2) = CIf <$> core e1 <*> core e2
core (ForC e) = CFor <$> core e
-}
core e@Choice{} = CBar <$> mapM core (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
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
value (Array es) = do
  (css, vs) <- unzip <$> mapM value es
  pure (concat css, HNF $ VArray vs)
{-
value (Lambda i e) = do
  e' <- core e
  pure ([], HNF $ VLam i e')
value (Type e) = second (HNF . VType) <$> value e
-}
value Any = pure ([], HNF $ VPrim "any")
value e = do
  e' <- core e
  v <- newTmp
  pure ([CUnify (CVar v) e'], Var v)
  
------

instance Pretty Core where
  pPrintPrec l p = pPrintPrec l p . coreToExpr

coreToExpr :: Core -> Expr
coreToExpr (CValue v) = valueToExpr v
coreToExpr (CUnify e1 e2) = Unify (coreToExpr e1) (coreToExpr e2)
coreToExpr (CSeq es) = Seq (map coreToExpr es)
coreToExpr (CApply e1 e2) = ApplyD (coreToExpr e1) (coreToExpr e2)
coreToExpr (CBar []) = Fail
coreToExpr (CBar es) = foldr1 Choice $ map coreToExpr es
{-
coreToExpr (CIf e1 e2) = IfC (coreToExpr e1) (coreToExpr e2)
coreToExpr (CFor e1) = ForC (coreToExpr e1)
coreToExpr (CDef is e) = Def is (coreToExpr e)
-}

valueToExpr :: Value -> Expr
valueToExpr (Var i) = Variable i
valueToExpr (HNF e) = hnfToExpr e

hnfToExpr :: HNF -> Expr
hnfToExpr (VInt i) = LitInt i
hnfToExpr (VRat i) = LitRat i
hnfToExpr (VPrim s) = Variable $ Ident noLoc s
hnfToExpr (VArray es) = Array $ map valueToExpr es
--hnfToExpr (VLam i e) = Lambda i $ coreToExpr e
hnfToExpr VRec{} = undefined
--hnfToExpr (VType e) = Type $ valueToExpr e
