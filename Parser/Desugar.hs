module Desugar(desugar, simplify, predefs, blockToExprs) where
import Control.Arrow(first, second)
import Control.Monad.State.Strict
--import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import GHC.Stack

import Expr
import Error
import Print hiding (first)

scope = undefined
makeUniq = undefined
simplify = undefined
desugarFunction = undefined

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------

desugar :: Expr -> Expr
desugar = eval . (desugarDo <=< desugarS)
  where eval = flip evalState 1

type D = State Int

desugarS :: Expr -> D Expr
desugarS = expr
  where
    expr :: HasCallStack => Expr -> D Expr
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array b) = Array <$> block b
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) | ()/=() = expr $ ApplyD eSucceeds $ ApplyD e1 e2
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    expr (PrefixOp (Ident l "!") e) = If3 <$> expr e <*> pure (BExpr Fail) <*> pure (BExpr Unit)
    expr (PrefixOp (Ident _ ":") e) = Range <$> expr e
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
        "|"  -> Choice <$> expr e1 <*> expr e2
        "where" -> newIdent "w" >>= \ i -> expr $ Seq [define l i e1, e2, Variable i]
        _    -> expr $ call "in" l op $ Array $ BExprs [e1, e2]
    expr (If1 e) = inVar (blockToExpr e) >>= \ (t, e') -> expr $ If2 e' (BExpr t)
    expr (If2 e1 e2) = expr $ If3 e1 e2 (BExpr Unit)
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
    expr (Typedef e) = Typedef <$> block e

    expr e@Define{} = impossible e
    expr e@Choice{} = impossible e
    expr e@Unify{} = impossible e
    expr e@Range{} = impossible e
    expr e@Any{} = impossible e

{-
    expr e@Def{} = impossible e
    expr e@Type{} = impossible e
    expr e@Lambda{} = impossible e
    expr e@IfC{} = impossible e
    expr e@ForC{} = impossible e
    expr Any = pure Any
    expr Fail = pure Fail
    expr (Define i e) = Define i <$> expr e
-}

    block :: HasCallStack => Block -> D Block
    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

    call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='"] = ApplyD
                | otherwise = ApplyS
            s' = p ++ "'" ++ s ++ "'"

newInt :: D Int
newInt = do
  n <- get
  put $! n+1
  pure n

newIdent :: String -> D Ident
newIdent s = do
  n <- newInt
  pure $ Ident noLoc $ "$" ++ s ++ show n

inVarM :: Expr -> D (Expr, Maybe Expr)
inVarM e@Variable{} = pure (e, Nothing)
inVarM e = do
  i <- newIdent "d"
  pure (Variable i, Just $ define noLoc i e)

inVar :: Expr -> D (Expr, Expr)
inVar e = (\ (e', me) -> (e', fromMaybe e' me)) <$> inVarM e

desugarCase :: Expr -> Block -> D Expr
desugarCase e@Variable{} (BExprs es) = desugarS $ foldr mkOr Fail es
  where mkOr a r = If2E (ApplyD a e) (BExpr r)
desugarCase Variable{} _ = internalError
desugarCase e b = do
  (t, e') <- inVar e
  ec <- desugarCase t b
  pure $ Seq [e', ec]

eAssign :: Loc -> Expr
eAssign l = Variable (Ident l "$assign")

eSucceeds :: Expr
eSucceeds = Variable (Ident noLoc "succeeds")

define :: Loc -> Ident -> Expr -> Expr
define _l i e = Define i e

tAny :: Loc -> Ident -> Expr
tAny l i = define l i AnyT

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
  (i, ex) <-
    case x of
      Variable i -> pure (i, [])
      _ -> do
        i <- newIdent "d"
        ex <- desugarDef l x (Variable i)
        pure (i, [ex])
  ey <- desugarDef l y (ApplyD t (tAny l i))
  pure $ seqE $ ey : ex
-- What else is allowed?  LitInt and LitRat would be easy.
desugarDef l x _ = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x

desugarFunDef :: Loc -> Expr -> [Eff] -> Expr -> D Expr
desugarFunDef l (EffAttr f a) as e = desugarFunDef l f (a:as) e
desugarFunDef l (ApplyS f a) as e = desugarFunDef l f [] $ Function a (reverse as) (BExprs [e])
desugarFunDef l (Variable f) [] e = define l f <$> desugarS e
desugarFunDef _ Variable{} _ _ = internalError
desugarFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f

blockToExpr :: Block -> Expr
blockToExpr = seqE . blockToExprs

blockToExprs :: Block -> [Expr]
blockToExprs (BExpr e) = [e]
blockToExprs (BExprs es) = es

predefs :: [Ident]
predefs = map (Ident noLoc)
  [ "int", "any", "nat", "float", "string", "false"
  , "in'+'", "in'-'", "in'*'", "in'/'"
  , "in'<'", "in'<='", "in'>'", "in'>='", "in'<>'"
  , "in'..'", "in'->'"
  , "pre'-'"
  , "post'^'", "post'?'"
  , "succeeds", "decides", "iterates", "io"
  , "$assign"
  ]

--------------------

-- Remove let/do and possible name clashes.
-- XXX name clash removal not implemented
desugarDo :: Expr -> D Expr
desugarDo = compos f
  where f (Do b) = f $ blockToExpr b
        f (Let e b) = f $ seqE (e : blockToExprs b)
        f e = compos f e

--------------------

{-

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
{-
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (Type e) = Type <$> expr e
    expr (Define i e) = Define i <$> expr e
    expr (Choice e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (Lambda v e) = Lambda v <$> expr e
    expr Any = pure Any
    expr Fail = pure Fail
-}
    expr e = impossible e

    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

    function e1 fs e2 = do
      i <- newIdent "z"
      e1' <- expr e1
      e2' <- expr $ blockToExpr e2
      let e2'' = foldr ApplyS e2' $ map Variable fs
      pure $ Lambda i $ seqE [Unify e1' (Variable i), e2'']

--------------------

-- Insert defs
scope :: Expr -> Expr
scope = uncurry Def . flip evalState 1 . scopeExpr

scopeDef :: Expr -> D Expr
scopeDef ee = def <$> scopeExpr ee
  where
    def :: ([Ident], Expr) -> Expr
    def ([], e) = e
    def (is, e) = Def is e

exprToBlock :: Expr -> Block
exprToBlock (Seq es) = BExprs es
exprToBlock e = BExpr e

scopeExpr :: Expr -> D ([Ident], Expr)
scopeExpr e@LitInt{} = pure ([], e)
scopeExpr e@LitRat{} = pure ([], e)
scopeExpr e@Variable{} = pure ([], e)
scopeExpr (Array b) = second Array <$> scopeBlock b
scopeExpr (Seq es) = second Seq <$> scopeExprs es
scopeExpr (ApplyS e1 e2) = do
  (is1, e1') <- scopeExpr e1
  (is2, e2') <- scopeExpr e2
  pure (is1 ++ is2, ApplyS e1' e2')
scopeExpr (ApplyD e1 e2) = do
  (is1, e1') <- scopeExpr e1
  (is2, e2') <- scopeExpr e2
  pure (is1 ++ is2, ApplyD e1' e2')
scopeExpr (Unify e1 e2) = do
  (is1, e1') <- scopeExpr e1
  (is2, e2') <- scopeExpr e2
  pure (is1 ++ is2, Unify e1' e2')
scopeExpr (If3 e1 e2 e3) = do
  e1' <- scopeDef $ Seq [e1, thunk $ blockToExpr e2]
  e3' <- scopeDef $ thunk $ blockToExpr e3
  pure ([], IfC e1' e3')
scopeExpr (For2 e1 e2) = do
  e1' <- scopeDef $ Seq [e1, thunk $ blockToExpr e2]
  pure ([], ForC e1')
scopeExpr (Typedef e) = do
  v <- newIdent "s"
  (_, e') <- scopeExpr $ Lambda v $ blockToExpr e
  pure ([], Type e')
scopeExpr (Define x e) = do
  (xs, e') <- scopeExpr e
  let e'' = Unify (Variable x) e'
  pure (x:xs, e'')
scopeExpr (Choice e1 e2) = do
  e1' <- scopeDef e1
  e2' <- scopeDef e2
  pure ([], Choice e1' e2')
scopeExpr (Lambda v e) = do
  e' <- scopeDef e
  pure ([], Lambda v e')
scopeExpr Any = pure ([], Any)
scopeExpr Fail = pure ([], Fail)
scopeExpr e = impossible e

scopeBlock :: Block -> D ([Ident], Block)
scopeBlock (BExpr e) = second BExpr <$> scopeExpr e
scopeBlock (BExprs es) = second BExprs <$> scopeExprs es

scopeExprs :: [Expr] -> D ([Ident], [Expr])
scopeExprs es = first concat <$> unzip <$> mapM scopeExpr es

thunk :: Expr -> Expr
thunk = Lambda dummyIdent
  where dummyIdent = Ident noLoc "_"

------------

-- Make all variable names unique by appending a number.
-- Check for undefined identifiers.
-- Drop do&let
makeUniq :: Expr -> Expr
makeUniq e = evalState (uniqE globals e) 1
  where
    globals :: Env
    globals = M.fromList $ zip predefs predefs

type Env = M.Map Ident Ident

envFor :: Expr -> D Env
envFor e = do
  let is = getVisible e
  when (anySame is) $
    traceM ("Multiple definitions among " ++ show is)
  envIdents is

envIdents :: [Ident] -> D Env
envIdents is = do
  let mku (Ident l s) = Ident l . ((s ++ "#") ++) . show <$> newInt
  us <- mapM mku is
  pure $ M.fromList $ zip is us

uniqB :: Env -> Block -> D Block
uniqB env b = exprToBlock <$> uniqE env (blockToExpr b)

uniqE :: Env -> Expr -> D Expr
uniqE env e = do
  ext <- envFor e
  uniqE' (M.union ext env) e

uniqE' :: Env -> Expr -> D Expr
uniqE' env = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@(Variable i@(Ident l _)) = do
      case M.lookup i env of
        Nothing -> trace ("Undefined " ++ show l ++ ": " ++ show i) $ pure e
        Just i' -> pure $ Variable i'
    expr (Array b) = Array <$> block b
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr (If3 e1 e2 e3) = do (e1', e2') <- blockX e1 e2; If3 e1' e2' <$> uniqB env e3
    expr (For2 e1 e2)   = do (e1', e2') <- blockX e1 e2; pure $ For2 e1' e2'
    expr (Let e1 e2)    = xLet <$> blockX e1 e2
    expr (Do e)         = xDo <$> uniqB env e
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (Typedef e) = Typedef <$> block e
    expr (Define i e) = Define (fromMaybe undefined $ M.lookup i env) <$> expr e
    expr (Choice e1 e2) = Choice <$> uniqE env e1 <*> uniqE env e2
    expr (Lambda v e) = do
      env' <- envIdents [v]
      Lambda (env' M.! v) <$> uniqE (M.union env' env) e
    expr Any = pure Any
    expr Fail = pure Fail
    expr e = impossible e

    xDo = blockToExpr
    xLet (e, b) = seqE [e, blockToExpr b]

    blockX e b = do
      env' <- envFor e
      let env'' = M.union env' env
      e' <- uniqE' env'' e
      b' <- uniqB env'' b
      pure (e', b')

    block (BExpr e) = BExpr <$> expr e
    block (BExprs es) = BExprs <$> mapM expr es

-- Get all visible identifiers from i := e
getVisible :: Expr -> [Ident]
getVisible LitInt{} = []
getVisible LitRat{} = []
getVisible Variable{} = []
getVisible (Array e) = getVisible (blockToExpr e)
getVisible (Seq es) = concatMap getVisible es
getVisible (ApplyS e1 e2) = getVisible e1 ++ getVisible e2
getVisible (ApplyD e1 e2) = getVisible e1 ++ getVisible e2
getVisible If3{} = []
getVisible For2{} = []
getVisible (Let _ e) = getVisible (blockToExpr e)
getVisible Do{} = []
getVisible (Unify e1 e2) = getVisible e1 ++ getVisible e2
getVisible (Type e) = getVisible e
getVisible (Define i e) = i : getVisible e
getVisible Choice{} = []
getVisible Lambda{} = []
getVisible Any = []
getVisible Fail = []
getVisible e = impossible e

------------

simplify :: Expr -> Expr
simplify = simp
  where --simp (Unify v@(Variable _) (Range e)) = Seq [ApplyD (simp e) v, v]
    simp (Unify e Any) = simp e
    simp (ApplyD (Variable (Ident _ "any")) e) = simp e
    simp e = composOp simp e
-}
