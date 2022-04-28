module Desugar(desugar, desugarLight, desugarFunction, scope, simplify) where
import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace

import Expr
import Error
import Print hiding (first)

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------

desugar :: Expr -> Expr
desugar = scope . desugarFunction . desugarLight

desugarLight :: Expr -> Expr
desugarLight = flip evalState 1 . desugarS

type D = State Int

desugarS :: Expr -> D Expr
desugarS = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array b) = Array <$> block b
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) | ()/=() = expr $ ApplyD eSucceeds $ ApplyD e1 e2
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    expr (PrefixOp (Ident l "!") e) = expr $ If2 e $ BExpr $ eFail l
    expr (PrefixOp (Ident l ":") e) = --Range <$> expr e
      newIdent "d" >>= \ i -> expr $ ApplyD e (tAny l i)
    expr (PrefixOp (Ident l op) e) = expr $ call "pre" l op e
    expr (PostfixOp e (Ident l op)) = expr $ call "post" l op e
    expr (InfixOp e1 (Ident l op) e2) =
      case op of
        "=>" -> expr $ Function e1 [] (BExprs [e2])
        ":=" -> desugarDef l e1 e2
        ":"  -> desugarColon l e1 e2
        "&&" -> expr $ seqE [e1, e2]
        "||" -> expr $ If2E e1 (BExpr e2)
        "="  -> Unify <$> expr e1 <*> expr e2
        "where" -> newIdent "w" >>= \ i -> expr $ Seq [define l i e1, e2, Variable i]
        _    -> expr $ call "in" l op $ Array $ BExprs [e1, e2]
    expr (If1 e) = inVar (blockToExpr e) >>= \ (t, e') -> expr $ If2 e' (BExpr t)
    expr (If2 e1 e2) = expr $ If3 e1 e2 (BExpr $ Array $ BExprs [])
    expr (If2E e1 e2) = inVar e1 >>= \ (t, e') -> expr $ If3 e' (BExpr t) e2
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> block e2 <*> block e3
    expr (For1 e) = inVar (blockToExpr e) >>= \ (t, e') -> expr $ For2 e' (BExpr t)
    expr (For2 e1 e2) = For2 <$> expr e1 <*> block e2
    expr (Let e1 e2) = Let <$> expr e1 <*> block e2
    expr (Do e) = Do <$> block e
    expr (Case1 b) =
      newIdent "d" >>= \ i -> expr $ InfixOp (tAny noLoc i) (Ident noLoc "=>") $ Case2 (Variable i) b
    expr (Case2 e1 e2) = desugarCase e1 e2
    expr (Function e1 fs e2) = Function <$> expr e1 <*> pure fs <*> block e2
    expr (Typedef b) = newIdent "d" >>= \ i -> Type . Lambda i <$> expr (blockToExpr b)
    expr Any = pure Any
      
    expr e@Def{} = impossible e
    expr e@Unify{} = impossible e
--    expr e@Range{} = impossible e
    expr e@Type{} = impossible e
    expr e@Lambda{} = impossible e

    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

    call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='"] = ApplyD
                | otherwise = ApplyS
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
  where mkOr a r = If2E (ApplyD a e) (BExpr r)
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

eSucceeds :: Expr
eSucceeds = Variable (Ident noLoc "succeeds")

define :: Loc -> Ident -> Expr -> Expr
define l i e = InfixOp (Variable i) (Ident l ":=") e

tAny :: Loc -> Ident -> Expr
tAny l i = define l i Any

desugarColon :: Loc -> Expr -> Expr -> D Expr
desugarColon l x t = desugarDef l x (PrefixOp (Ident l ":") t)

desugarDef :: Loc -> Expr -> Expr -> D Expr
desugarDef l (Variable i) e = define l i <$> desugarS e
desugarDef _ (InfixOp x (Ident l ":") t) e = desugarDef l x $ ApplyS t e  -- XXX Is this correct
desugarDef l f@ApplyS{} e = desugarFunDef l f [] e
desugarDef l f@EffAttr{} e = desugarFunDef l f [] e
desugarDef l (PostfixOp x q@(Ident _ "?")) e = desugarDef l x (PostfixOp e q)
desugarDef l (PostfixOp x (Ident _ "^")) e = desugarS $ ApplyS (eAssign l) $ Array $ BExprs [x, e]
desugarDef l (Array (BExprs xs)) e = do
  (v, me) <- inVarM e
  es <- zipWithM (\ x i -> desugarDef l x (ApplyD v (LitInt i))) xs [0..]
  chk <- desugarS $ PrefixOp (Ident l "!") $ ApplyD v (LitInt (toInteger (length xs)))  -- Check that list ends correctly
  pure $ Seq $ maybeToList me ++ [chk] ++ es
desugarDef l (InfixOp x (Ident _ "->") y) (PrefixOp (Ident _ ":") t) = do
  i <- newIdent "d"
  ex <- desugarDef l x (Variable i)
  ey <- desugarDef l y (ApplyD t (tAny l i))
  pure $ Seq [ex, ey]
-- What else is allowed?  LitInt and LitRat would be easy.
desugarDef l x _ = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x

desugarFunDef :: Loc -> Expr -> [Eff] -> Expr -> D Expr
desugarFunDef l (EffAttr f a) as e = desugarFunDef l f (a:as) e
desugarFunDef l (ApplyS f a) as e = desugarFunDef l f [] $ Function a (reverse as) (BExprs [e])
desugarFunDef l (Variable f) [] e = define l f <$> desugarS e
desugarFunDef _ Variable{} _ _ = internalError
desugarFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f

--------------------

-- Insert defs
scope :: Expr -> Expr
scope = scopeCheck . scopeDef

scopeDef :: Expr -> Expr
scopeDef = insDef . scopeExpr

scopeDefB :: Block -> Block
scopeDefB (BExpr e) = BExpr $ scopeDef e
scopeDefB (BExprs es) = BExpr $ scopeDef $ Seq es

scopeCheck :: Expr -> Expr
scopeCheck e =
  case freeVars e \\ predef of
    [] -> e
    xs -> trace ("Undefined " ++ prettyShow xs)
          e

predef :: [Ident]
predef = map (Ident noLoc) ["int", "any", "nat", "float", "string"]

freeVars :: Expr -> [Ident]
freeVars ae = execState (free ae) []
  where
    free e@(Variable v) = do modify (\ s -> union [v] s); pure e
    free (Lambda v e) = do e' <- free e; modify (\ s -> s \\ [v]); pure e'
    free (Def vs e)  = do e' <- freeB e; modify (\ s -> s \\ vs); pure (Def vs e')
    free e = compos free e
    freeB (BExpr e) = BExpr <$> free e
    freeB (BExprs es) = BExprs <$> mapM free es

insDef :: ([Ident], Expr) -> Expr
insDef (is, e) | anySame is = error $ "Multiple definition " ++ prettyShow is
               | otherwise = def is (BExpr e)

def :: [Ident] -> Block -> Expr
def [] e = blockToExpr e
def is e = Def is e

blockToExpr :: Block -> Expr
blockToExpr (BExpr e) = e
blockToExpr (BExprs es) = Seq es

scopeExpr :: Expr -> ([Ident], Expr)
scopeExpr e@LitInt{} = ([], e)
scopeExpr e@LitRat{} = ([], e)
scopeExpr e@Variable{} = ([], e)
scopeExpr (Array b) = second Array $ scopeBlock b
scopeExpr (Seq es) = second Seq $ scopeExprs es
scopeExpr (ApplyS e1 e2) = (is1 ++ is2, ApplyS e1' e2') where (is1, e1') = scopeExpr e1; (is2, e2') = scopeExpr e2
scopeExpr (ApplyD e1 e2) = (is1 ++ is2, ApplyD e1' e2') where (is1, e1') = scopeExpr e1; (is2, e2') = scopeExpr e2
scopeExpr (Unify e1 e2) = (is1 ++ is2, Unify e1' e2') where (is1, e1') = scopeExpr e1; (is2, e2') = scopeExpr e2
scopeExpr (EffAttr e r) = (is, EffAttr e' r) where (is, e') = scopeExpr e
--scopeExpr (PrefixOp op e) = (is, PrefixOp e') where (is, e') = scopeExpr e
--scopeExpr (PostfixOp e r) = (is, PostfixOp e' r) where (is, e') = scopeExpr e
scopeExpr (InfixOp (Variable x) (Ident _ ":=") e) = (x:xs, e'')
  where (xs, e') = scopeExpr e
        e'' = Unify (Variable x) e'
--scopeExpr (InfixOp e1 (Ident l "|") e2) = ([], In XXXX
--scopeExpr InfixOp
--scopeExpr If1
--scopeExpr If2
scopeExpr (If3 e1 e2 e3) = ([], If3 e1'' e2'' e3')
  where (xs, e1') = scopeExpr e1
        e2' = scopeDefB e2
        e3' = scopeDefB e3
        e1'' = def xs $ BExprs [e1', exs]
        e2'' = lambdas xs e2'
        exs = Array (BExprs (map Variable xs))
scopeExpr (For2 e1 e2) = ([], For2 e1'' e2'')
  where (xs, e1') = scopeExpr e1
        e2' = scopeDefB e2
        e1'' = def xs $ BExprs [e1', exs]
        e2'' = lambdas xs e2'
        exs = Array (BExprs (map Variable xs))
scopeExpr (Let _e _b) = unimplemented
scopeExpr (Do _b) = unimplemented
scopeExpr (Function p r e) = ([], Function p r $ scopeDefB e)
scopeExpr (Type (Lambda v e)) = ([], Type $ Lambda v $ def xs $ BExpr $ Unify (Variable v) e')
  where (xs, e') = scopeExpr e
--scopeExpr (Range e) = second Range $ scopeExpr e
scopeExpr (Lambda v e) = ([], Lambda v $ scopeDef e)
scopeExpr Any = ([], Any)
scopeExpr e = error $ "scopeExpr " ++ show e

scopeBlock :: Block -> ([Ident], Block)
scopeBlock (BExpr e) = second BExpr $ scopeExpr e
scopeBlock (BExprs es) = second BExprs $ scopeExprs es

scopeExprs :: [Expr] -> ([Ident], [Expr])
scopeExprs es = first concat $ unzip $ map scopeExpr es

lambdas :: [Ident] -> Block -> Block
lambdas is e = BExpr $ Lambda v $ seqE [Unify (Array (BExprs (map Variable is))) (Variable v), blockToExpr e]
  where v = Ident noLoc "$v"  -- XXX

seqE :: [Expr] -> Expr
seqE = mk . concatMap flat
  where flat (Seq es) = es
        flat e = [e]
        mk [e] = e
        mk es = Seq es

desugarFunction :: Expr -> Expr
desugarFunction = flip evalState 1 . desugarFunctionS

desugarFunctionS :: Expr -> D Expr
desugarFunctionS = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array b) = Array <$> block b
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> block e2 <*> block e3
    expr (For2 e1 e2) = For2 <$> expr e1 <*> block e2
    expr (Let e1 e2) = Let <$> expr e1 <*> block e2
    expr (Do e) = Do <$> block e
    expr (Case2 e1 e2) = Case2 <$> expr e1 <*> block e2
    expr (Function e1 fs e2) = function e1 fs e2
    expr e@Def{} = impossible e
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (InfixOp e1 op e2) = InfixOp <$> expr e1 <*> pure op <*> expr e2
--    expr (Range e) = Range <$> expr e
    expr (Type e) = Type <$> expr e
    expr (Lambda v e) = Lambda v <$> expr e
    expr Any = pure Any
    expr e = impossible e

    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

    function e1 fs e2 = do
      i <- newIdent "z"
      e1' <- expr e1
      e2' <- expr $ blockToExpr e2
      let e2'' = foldr ApplyS e2' $ map Variable fs
      pure $ Lambda i $ seqE [Unify e1' (Variable i), e2'']

------------

simplify :: Expr -> Expr
simplify = simp
  where --simp (Unify v@(Variable _) (Range e)) = Seq [ApplyD (simp e) v, v]
    simp (Unify e Any) = simp e
    simp (ApplyD (Variable (Ident _ "any")) e) = simp e
    simp e = composOp simp e
