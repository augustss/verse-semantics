{-# LANGUAGE PatternSynonyms #-}
module Core(exprToCore, coreToExpr) where
--import Control.Arrow(second)
import Control.Monad.State.Strict

import Print
import Expr
import Desugar(predefs, getVisible)
import Error
--import Debug.Trace

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar [Core]
  | COne Core
  | CAll Core
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

cEmpty :: Core
cEmpty = CValue $ HNF $ VArray []

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

newTmp :: C Ident
newTmp = do
  n <- get
  let i = Ident noLoc ("$c" ++ show n)
  put $! n+1
  pure i

exprToCore :: Expr -> Core
exprToCore = flip evalState 1 . core

core :: Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = seqC <$> mapM core es
core (ApplyS e1 e2) = core $ ApplyD (eVar "succeeds") (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (Unify e1 e2) = CUnify <$> core e1 <*> core e2
core e@Typedef{} = val e
core e@Choice{} = CBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i e) = CUnify (CVar i) <$> core e
core (Range e) = core $ ApplyD (eVar "range") e
core e@Any = val e
core Fail = pure $ CBar []
core (For2 e1 e2) = do
  e2' <- thunk e2
--  traceM $ show (e2, e2', seqE [e1, e2'])
  core $ ApplyD (eVar "all") (seqE [e1, e2'])
core (If3 e1 e2 e3) = do
  e2' <- thunk e2
  e3' <- thunk e3
  l <- COne <$> core (seqE [e1, e2'])
  r <- core e3'
  let fn = COne $ CBar [l, r]
  pure $ CApply fn cEmpty
core e@Function{} = val e
core e = impossible e

coreD :: Expr -> C Core
coreD e = CDef (getVisible e) <$> core e

val :: Expr -> C Core
val e = CValue <$> value e

value :: Expr -> C Value
value (LitInt i) = pure (HNF $ VInt i)
value (LitRat i) = pure (HNF $ VRat i)
value (Variable i@(Ident _ s)) | i `elem` predefs = pure (HNF $ VPrim s)
                               | otherwise = pure (Var i)
value (Array es) = HNF . VArray <$> mapM value es
value (Typedef e) = do
  i <- newTmp
  HNF . VType . HNF . VLam i <$> coreD (ApplyD e (Variable i))
value (Function (Define x AnyT) [] b) = HNF . VLam x <$> coreD b
value Any = pure (HNF $ VPrim "any")
value e = internalError $ "value: not a value" ++ show e
  
eVar :: String -> Expr
eVar = Variable . Ident noLoc

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ Function (Define i AnyT) [] e

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
coreToExpr (COne e) = ApplyS (eVar "one") (coreToExpr e)
coreToExpr (CAll e) = ApplyS (eVar "all") (coreToExpr e)
coreToExpr (CDef is e) = seqE $ map (\ i -> Define i AnyT) is ++ [coreToExpr e]

valueToExpr :: Value -> Expr
valueToExpr (Var i) = Variable i
valueToExpr (HNF e) = hnfToExpr e

hnfToExpr :: HNF -> Expr
hnfToExpr (VInt i) = LitInt i
hnfToExpr (VRat i) = LitRat i
hnfToExpr (VPrim s) = Variable $ Ident noLoc s
hnfToExpr (VArray es) = Array $ map valueToExpr es
hnfToExpr (VLam i e) = Function (Define i AnyT) [] $ coreToExpr e
hnfToExpr (VType e) = valueToExpr e -- XXX
hnfToExpr VRec{} = undefined
--hnfToExpr (VType e) = Type $ valueToExpr e
