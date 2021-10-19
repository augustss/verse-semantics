module Desugar(desugar) where
import Control.Monad.State.Strict

import qualified CoreExpr as C
import ParseExpr

-----
foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM f z xs = foldM (flip f) z (reverse xs)
-----

type D = State Int

newIdent :: D Ident
newIdent = do
  i <- get
  put $! i+1
  pure $ Ident $ "x#" ++ show i

desugar :: Expr -> C.Expr
desugar e = evalState (dsExpr e) 1

dsExpr :: Expr -> D C.Expr
dsExpr (Def x) = pure $ C.Def x
dsExpr (Var x) = pure $ C.Var x
dsExpr (Int i) = pure $ C.Int i
dsExpr (Unify e1 e2) = C.Unify <$> dsExpr e1 <*> dsExpr e2
dsExpr (Apply e1 e2) = C.Apply <$> dsExpr e1 <*> dsExpr e2
dsExpr (Call e1 e2) = C.Call <$> dsExpr e1 <*> dsExpr e2
dsExpr (Lambda p e) = dsLambda p e
dsExpr (Alt e1 e2) = C.Alt <$> dsExpr e1 <*> dsExpr e2
dsExpr (Array es) = C.Array <$> mapM dsExpr es
dsExpr (If e1 e2 e3) = C.If <$> dsExpr e1 <*> dsExpr e2 <*> dsExpr e3
dsExpr (For e1 e2) = C.For <$> dsExpr e1 <*> dsExpr e2
dsExpr (Let e1 e2) = C.Let <$> dsExpr e1 <*> dsExpr e2
dsExpr (Do e) = C.Do <$> dsExpr e
dsExpr (Seq es) = C.Seq <$> mapM dsExpr es
--- macros below
dsExpr (Define p e) = dsDefine p e
dsExpr (HasType x t) = dsExpr $ Define x (Range t)
dsExpr (Range t) = do x <- newIdent; dsExpr $ Do $ Seq [Def x, Apply t (Var x)]
dsExpr (TypeDef e) = do x <- Var <$> newIdent; dsExpr $ Lambda x $ Unify x e
dsExpr (Where e1 e2) = dsExpr $ Apply (Array [e1, e2]) (Int 1)
dsExpr (Case e es) = do
  x <- Var <$> newIdent
  let eArm a r = do y <- Var <$> newIdent; pure $ If (Define y $ Apply a x) y r
  dsExpr =<< (Let (Define x e) <$> foldrM eArm eColonFalse es)

eColonFalse :: Expr
eColonFalse = Range $ Var $ Ident "false"

dsLambda :: Pat -> Expr -> D C.Expr
dsLambda (Var i) e = C.Lambda i <$> dsExpr e
dsLambda p e = do
  x <- newIdent
  C.Lambda x <$> dsExpr (Seq [Unify (Var x) p, e])

dsDefine :: Pat -> Expr -> D C.Expr
dsDefine p@(Var x) e = dsExpr $ Seq [Def x, Unify p e]
dsDefine (HasType p t) e = dsDefine p (Apply t e)
dsDefine (Call f a) e = do
  x <- Var <$> newIdent
  dsDefine f $ Lambda x $ Seq [ Unify x a, e]
dsDefine (Array ps) e = do
  x <- Var <$> newIdent
  dsExpr $ Seq $ Define x e : zipWith (\ i p -> Define p $ Apply x (Int i)) [0..] ps
dsDefine p e = error $ "dsDefine: " ++ show p
