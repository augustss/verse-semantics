module Desugar(Expr(..), desugar) where
import Control.Monad.State.Strict
import Text.PrettyPrint.HughesPJClass

import qualified ParseExpr as P
import ParseExpr(Ident(..))

-- This generates a lot of unreadable junk
desugarCall :: Bool
desugarCall = False

desugarSeq :: Bool
desugarSeq = False

-----
foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM f z xs = foldM (flip f) z (reverse xs)
-----

-- After desugaring
data Expr
  = Var Ident                        -- x
  | Int Integer                      -- i
  | Define Ident Expr                -- x := e
  | Range Expr                       -- :t
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Lambda Ident Expr                -- x => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  | Let Expr Expr                    -- let (e1) in e2
  | Do Expr                          -- do e
  -- The rest could be desugared
  | Call Expr Expr                   -- e1[e2]
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  deriving (Eq, Ord, Show)

instance Pretty Expr where
  pPrintPrec l p = pPrintPrec l p . eToPE

eToPE :: Expr -> P.Expr
eToPE (Var x) = P.Var x
eToPE (Int i) = P.Int i
eToPE (Define x e) = P.Define (P.Var x) (eToPE e)
eToPE (Range e) = P.Range (eToPE e)
eToPE (Unify e1 e2) = P.Unify (eToPE e1) (eToPE e2)
eToPE (Apply e1 e2) = P.Apply (eToPE e1) (eToPE e2)
eToPE (Lambda x e) = P.Lambda (P.Var x) (eToPE e)
eToPE (Alt e1 e2) = P.Alt (eToPE e1) (eToPE e2)
eToPE (Array es) = P.Array (map eToPE es)
eToPE (If e1 e2 e3) = P.If (eToPE e1) (eToPE e2) (eToPE e3)
eToPE (For e1 e2) = P.For (eToPE e1) (eToPE e2)
eToPE (Let e1 e2) = P.Let (eToPE e1) (eToPE e2)
eToPE (Do e) = P.Do (eToPE e)
eToPE (Call e1 e2) = P.Call (eToPE e1) (eToPE e2)
eToPE (Seq es) = P.Seq (map eToPE es)

-------

type D = State Int

newIdent :: D Ident
newIdent = do
  i <- get
  put $! i+1
  pure $ Ident $ "x#" ++ show i

desugar :: P.Expr -> Expr
desugar e = evalState (dsExpr e) 1

dsExpr :: P.Expr -> D Expr
dsExpr (P.Var x) = pure $ Var x
dsExpr (P.Int i) = pure $ Int i
dsExpr (P.Define d e) = dsDefine d e
dsExpr (P.Range t) = Range <$> dsExpr t
dsExpr (P.Unify e1 e2) = Unify <$> dsExpr e1 <*> dsExpr e2
dsExpr (P.Apply (P.Var (P.Ident "operator'&&'")) (P.Array [e1, e2])) = dsExpr $ P.Seq [e1, e2]
dsExpr (P.Apply (P.Var (P.Ident "operator'||'")) (P.Array [e1, e2])) = dsOr e1 e2
dsExpr (P.Apply e1 e2) = Apply <$> dsExpr e1 <*> dsExpr e2
dsExpr (P.Call e1 e2) = dsCall e1 e2
dsExpr (P.Lambda p e) = dsLambda p e
dsExpr (P.Alt e1 e2) = Alt <$> dsExpr e1 <*> dsExpr e2
dsExpr (P.Array es) = Array <$> mapM dsExpr es
dsExpr (P.If e1 e2 e3) = If <$> dsExpr e1 <*> dsExpr e2 <*> dsExpr e3
dsExpr (P.For e1 e2) = For <$> dsExpr e1 <*> dsExpr e2
dsExpr (P.Let e1 e2) = Let <$> dsExpr e1 <*> dsExpr e2
dsExpr (P.Do e) = Do <$> dsExpr e
dsExpr (P.Seq es) = dsSeq es
--- macros below
dsExpr (P.HasType x t) = dsDefine x (P.Range t)
dsExpr (P.TypeDef e) = do x <- newIdent; (Lambda x . Unify (Var x)) <$> dsExpr e
dsExpr (P.Where e1 e2) = dsExpr $ P.Apply (P.Array [e1, e2]) (P.Int 0)
dsExpr (P.Case e es) = do
  x <- P.Var <$> newIdent
  let eArm a r = do y <- P.Var <$> newIdent; pure $ P.If (P.Define y $ P.Apply a x) y r
  dsExpr =<< (P.Let (P.Define x e) <$> foldrM eArm eColonFalse es)
dsExpr (P.Match p e) = dsMatch p e
dsExpr e@P.DefIn{} = error $ "dsExpr " ++ show e

eColonFalse :: P.Expr
eColonFalse = P.Range $ P.Var $ Ident "false"

eWrong :: P.Expr
eWrong = P.Var $ Ident "wrong"

dsLambda :: P.Pat -> P.Expr -> D Expr
dsLambda p e = do
  x <- newIdent
  Lambda x <$> dsExpr (P.Seq [P.Match p (P.Var x), e])

dsDefine :: P.Def -> P.Expr -> D Expr
dsDefine (P.Var x) e = Define x <$> dsExpr e
dsDefine (P.HasType d t) e = dsDefine d (P.Apply t e)
dsDefine (P.Call f a) e = dsDefine f (P.Lambda a e)
dsDefine (P.Array ps) e = do
  x <- P.Var <$> newIdent
  dsExpr $ P.Seq $ P.Define x e : zipWith (\ i p -> P.Define p $ P.Apply x (P.Int i)) [0..] ps
dsDefine p _ = error $ "dsDefine: " ++ show p

--dsAnd :: Expr -> Expr -> D Expr
--dsAnd e1 e2 = dsExpr $ If e1 e2 eColonFalse
--dsAnd e1 e2 = dsExpr $ Seq [e1, e2]

dsOr :: P.Expr -> P.Expr -> D Expr
dsOr e1 e2 = do
  x <- P.Var <$> newIdent
  dsExpr $ P.If (P.Define x e1) x e2

dsCall :: P.Expr -> P.Expr -> D Expr
dsCall e1 e2
  | not desugarCall = Call <$> dsExpr e1 <*> dsExpr e2
  | otherwise = do
  f <- P.Var <$> newIdent
  a <- P.Var <$> newIdent
  x <- P.Var <$> newIdent
  dsExpr $ P.Seq [P.Define f e1, P.Define a e2, P.If (P.Define x $ P.Apply f a) x eWrong]

dsSeq :: [P.Expr] -> D Expr
dsSeq es | not desugarSeq = Seq <$> mapM dsExpr es
         | otherwise = dsExpr $ P.Apply (P.Array es) (P.Int $ toInteger $ length es - 1)

dsMatch :: P.Pat -> P.Expr -> D Expr
dsMatch (P.Array ps) e = do
  x <- P.Var <$> newIdent
  dsExpr $ P.Seq $ P.Define x e : zipWith (\ i p -> P.Match p $ P.Apply x (P.Int i)) [0..] ps
dsMatch (P.HasType d t) e = dsExpr $ P.Define (P.HasType d t) e
dsMatch (P.Define d e1) e2 = dsExpr $ P.Define d $ {-P.Do $-} P.Unify e1 e2
dsMatch (P.Range t) e = dsExpr $ P.Apply t e
dsMatch (P.Seq es) e = dsExpr $ P.Seq $ init es ++ [P.Match (last es) e]
dsMatch (P.Where p e1) e2 = dsExpr $ P.Where (P.Match p e2) e1
dsMatch (P.Let e1 p) e2 = dsExpr $ P.Let e1 (P.Match p e2)
dsMatch e1 e2 = dsExpr $ P.Do $ P.Unify e1 e2
