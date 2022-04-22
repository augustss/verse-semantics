module Desugar where
import Control.Monad.State.Strict
import Data.Maybe

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
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    expr (PrefixOp (Ident l "!") e) = expr $ If2 e $ BExpr $ eFail l
    expr (PrefixOp (Ident l op) e) = expr $ call "pre" l op e
    expr (PostfixOp e (Ident l op)) = expr $ call "post" l op e
    expr (InfixOp e1 (Ident l op) e2) =
      case op of
        "=>" -> expr $ Function e1 [] (BExprs [e2])
        ":=" -> desugarDef l e1 e2
        ":"  -> desugarColon l e1 e2
        "&&" -> expr $ Seq [e1, e2]
        "||" -> expr $ If2E e1 (BExpr e2)
        _    -> expr $ call "in" l op $ Array $ BExprs [e1, e2]
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

    call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='"] = Index
                | otherwise = Call
            s' = p ++ "'" ++ s ++ "'"

newIdent :: String -> D Ident
newIdent s = do
  n <- get
  put $! n+1
  pure $ Ident noLoc $ "$" ++ s ++ show n

inVarM :: Expr -> D (Expr, Maybe Expr)
inVarM e@Variable{} = pure (e, Nothing)
inVarM e = do
  i <- newIdent "d"
  pure (Variable i, Just $ define noLoc i e)

inVar :: Expr -> D (Expr, Expr)
inVar e = (\ (e', me) -> (e', fromMaybe e' me)) <$> inVarM e

desugarCase :: Expr -> Block -> D Expr
desugarCase e@Variable{} (BExprs es) = desugarS $ foldr mkOr (eFail noLoc) es
  where mkOr a r = If2E (Index a e) (BExpr r)
desugarCase Variable{} _ = internalError
desugarCase e b = do
  (t, e') <- inVar e
  ec <- desugarCase t b
  pure $ Seq [e', ec]

-- Failure, i.e., :false, i.e., :()
eFail :: Loc -> Expr
eFail l = PrefixOp (Ident l ":") (Array (BExprs []))

eAssign :: Loc -> Expr
eAssign l = Variable (Ident l "$assign")

define :: Loc -> Ident -> Expr -> Expr
define l i e = InfixOp (Variable i) (Ident l ":=") e

desugarColon :: Loc -> Expr -> Expr -> D Expr
desugarColon l x t = desugarDef l x (PrefixOp (Ident l ":") t)

desugarDef :: Loc -> Expr -> Expr -> D Expr
desugarDef l (Variable i) e = define l i <$> desugarS e
desugarDef _ (InfixOp x (Ident l ":") t) e = desugarDef l x $ Call t e  -- XXX Is this correct
desugarDef l f@Call{} e = desugarFunDef l f [] e
desugarDef l f@EffAttr{} e = desugarFunDef l f [] e
desugarDef l (PostfixOp x q@(Ident _ "?")) e = desugarDef l x (PostfixOp e q)
desugarDef l (PostfixOp x (Ident _ "^")) e = desugarS $ Call (eAssign l) $ Array $ BExprs [x, e]
desugarDef l (Array (BExprs xs)) e = do
  (v, me) <- inVarM e
  es <- zipWithM (\ x i -> desugarDef l x (Index v (LitInt i))) xs [0..]
  chk <- desugarS $ PrefixOp (Ident l "!") $ Index v (LitInt (toInteger (length xs)))  -- Check that list ends correctly
  pure $ Seq $ maybeToList me ++ [chk] ++ es
-- What else is allowed?  LitInt and LitRat would be easy.
desugarDef l x _ = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x

desugarFunDef :: Loc -> Expr -> [Eff] -> Expr -> D Expr
desugarFunDef l (EffAttr f a) as e = desugarFunDef l f (a:as) e
desugarFunDef l (Call f a) as e = desugarFunDef l f [] $ Function a (reverse as) (BExprs [e])
desugarFunDef l (Variable f) [] e = define l f <$> desugarS e
desugarFunDef _ Variable{} _ _ = internalError
desugarFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f
