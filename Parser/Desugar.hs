module Desugar where
import Control.Monad.State.Strict

import Expr
import Error
import Epic.Print

desugar :: Expr -> Expr
desugar = flip evalState 1 . desugarS

type D = State Int

desugarS :: Expr -> D Expr
desugarS = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array b) = Array <$> block b
    expr (Seq es) = Seq <$> mapM expr es
    expr (Call e1 e2) = Call <$> expr e1 <*> expr e2
    expr (Index e1 e2) = Index <$> expr e1 <*> expr e2
    expr e@EffAttr{} = syntaxError $ "attribute not allowed: " ++ prettyShow e
    expr (PrefixOp op e) = expr $ call "pre" op e
    expr (PostfixOp e op) = expr $ call "post" op e
    expr (InfixOp e1 (Ident _ "=>") e2) = expr $ Function e1 [] (BExprs [e2])
    expr (InfixOp e1 (Ident _ ":=") e2) = desugarDef e1 e2
    expr (InfixOp e1 (Ident _ ":") e2) = desugarColon e1 e2
    expr (InfixOp e1 op e2) = expr $ call "in" op $ Array $ BExprs [e1, e2]
    expr (If1 e) = inVar (exprOf e) >>= \ (t, e') -> expr $ If2 e' (BExpr t)
    expr (If2 e1 e2) = expr $ If3 e1 e2 (BExpr $ Array $ BExprs [])
    expr (If2E e1 e2) = inVar e1 >>= \ (t, e') -> expr $ If3 e' (BExpr t) e2
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> block e2 <*> block e3
    expr (For1 e) = inVar (exprOf e) >>= \ (t, e') -> expr $ For2 e' (BExpr t)
    expr (For2 e1 e2) = For2 <$> expr e1 <*> block e2
    expr (Let e1 e2) = Let <$> expr e1 <*> block e2
    expr (Do e) = Do <$> block e
    expr (Case1 b) = newIdent "d" >>= \ i -> expr $ InfixOp (tAny i) (Ident noLoc "=>") $ Case2 (Variable i) b
      where tAny i = InfixOp (Variable i) (Ident noLoc ":") (Variable (Ident noLoc "any"))
    expr (Case2 e1 e2) = desugarCase e1 e2
    expr (Function e1 fs e2) = Function <$> expr e1 <*> pure fs <*> block e2

    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

    exprOf (BExpr e) = e
    exprOf (BExprs es) = Seq es

    call p (Ident l s) e = con (Variable (Ident l (p ++ "'" ++ s ++ "'"))) e
      where con | s `elem` ["/","=","<>","<",">","<=",">="] = Index
                | otherwise = Call

newIdent :: String -> D Ident
newIdent s = do
  n <- get
  put $! n+1
  pure $ Ident noLoc $ "$" ++ s ++ show n

inVar :: Expr -> D (Expr, Expr)
inVar e@Variable{} = pure (e, e)
inVar e = do
  i <- newIdent "d"
  pure (Variable i, define i e)

noLoc :: SourcePos
noLoc = initialPos ""

desugarCase :: Expr -> Block -> D Expr
desugarCase e@Variable{} (BExprs es) = desugarS $ foldr mkOr eFail es
  where mkOr a r = If2E (Index a e) (BExpr r)
desugarCase Variable{} _ = internalError
desugarCase e b = do
  (t, e') <- inVar e
  ec <- desugarCase t b
  pure $ Seq [e', ec]

-- Failure, i.e., :false, i.e., :()
eFail :: Expr
eFail = PrefixOp (Ident noLoc ":") (Array (BExprs []))

define :: Ident -> Expr -> Expr
define i e = InfixOp (Variable i) (Ident noLoc ":=") e

desugarColon :: Expr -> Expr -> D Expr
desugarColon l t = desugarDef l (PrefixOp (Ident noLoc ":") t)

desugarDef :: Expr -> Expr -> D Expr
desugarDef (Variable i) e = define i <$> desugarS e
desugarDef (InfixOp l (Ident _ ":") t) e = desugarDef l $ Call t e  -- XXX Is this correct
desugarDef f@Call{} e = desugarFunDef f [] e
desugarDef f@EffAttr{} e = desugarFunDef f [] e
--desugarDef (Array (BExprs ls)) e = do
  

desugarFunDef :: Expr -> [Eff] -> Expr -> D Expr
desugarFunDef (EffAttr f a) as e = desugarFunDef f (a:as) e
desugarFunDef (Call f a) as e = desugarFunDef f [] $ Function a (reverse as) (BExprs [e])
desugarFunDef (Variable f) [] e = define f <$> desugarS e
desugarFunDef Variable{} _ _ = internalError
desugarFunDef f _ _ = syntaxError $ "bad function definition: " ++ prettyShow f
