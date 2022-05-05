module Desugar(desugar, simplify, predefs, getVisible) where
--import Control.Arrow(first, second)
import Control.Monad.State.Strict
--import Data.List
--import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace
import GHC.Stack

import Expr
import Error
import Print hiding (first)

simplify = undefined

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------

desugar :: Expr -> Expr
desugar = eval . (anfS <=< desugarFunctionS <=< desugarDoS <=< scopeCheck <=< desugarS)
  where eval = flip evalState 1

type D = State Int

desugarS :: Expr -> D Expr
desugarS = expr
  where
    expr :: HasCallStack => Expr -> D Expr
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) | ()/=() = expr $ ApplyD eSucceeds $ ApplyD e1 e2
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    expr (PrefixOp (Ident _ "!") e) = If3 <$> expr e <*> pure Fail <*> pure Unit
    expr (PrefixOp (Ident _ ":") e) = Range <$> expr e
    expr (PrefixOp (Ident l op) e) = expr $ call "pre" l op e
    expr (PostfixOp e (Ident l op)) = expr $ call "post" l op e
    expr (InfixOp e1 (Ident l op) e2) =
      case op of
        "=>" -> expr $ Function e1 [] e2
        ":=" -> desugarDef l e1 e2
        ":"  -> desugarColon l e1 e2
        "&&" -> expr $ seqE [e1, e2]
        "||" -> expr $ If2E e1 e2
        "="  -> Unify <$> expr e1 <*> expr e2
        "|"  -> Choice <$> expr e1 <*> expr e2
        "where" -> newIdent "w" >>= \ i -> expr $ Seq [define l i e1, e2, Variable i]
        _    -> expr $ call "in" l op $ Array [e1, e2]
    expr (If1 e) = inVar e >>= \ (t, e') -> expr $ If2 e' t
    expr (If2 e1 e2) = expr $ If3 e1 e2 Unit
    expr (If2E e1 e2) = inVar e1 >>= \ (t, e') -> expr $ If3 e' t e2
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> expr e2 <*> expr e3
    expr (For1 e) = inVar e >>= \ (t, e') -> expr $ For2 e' t
    expr (For2 e1 e2) = For2 <$> expr e1 <*> expr e2
    expr (Let e1 e2) = Let <$> expr e1 <*> expr e2
    expr (Do e) = Do <$> expr e
    expr (Case1 b) =
      newIdent "d" >>= \ i -> expr $ InfixOp (tAny noLoc i) (Ident noLoc "=>") $ Case2 (Variable i) b
    expr (Case2 e1 e2) = desugarCase e1 e2
    expr (Function e1 fs e2) = Function <$> expr e1 <*> pure fs <*> expr e2
    expr (Block es) = expr $ seqE es
    expr (Typedef e) = Typedef <$> expr e
    expr (Define i e) = Define i <$> expr e
    expr (Range e) = Range <$> expr e
    expr (Choice e1 e2) = Choice <$> expr e1 <*> expr e2
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr Any = pure Any

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
desugarCase e@Variable{} (Block es) = desugarS $ foldr mkOr Fail es
  where mkOr a r = If2E (ApplyD a e) r
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
desugarDef l (PostfixOp x (Ident _ "^")) e = desugarS $ ApplyS (eAssign l) $ Array [x, e]
desugarDef l (Array xs) e = do
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
desugarFunDef l (ApplyS f a) as e = desugarFunDef l f [] $ Function a (reverse as) e
desugarFunDef l (Variable f) [] e = define l f <$> desugarS e
desugarFunDef _ Variable{} _ _ = internalError
desugarFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f

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
desugarDoS :: Expr -> D Expr
desugarDoS = f
  where f (Do b) = f b
        f (Let e b) = f $ seqE [e, b]
        f e = compos f e

--------------------

desugarFunctionS :: Expr -> D Expr
desugarFunctionS = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@Variable{} = pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> expr e2 <*> expr e3
    expr (For2 e1 e2) = For2 <$> expr e1 <*> expr e2
--    expr (Let e1 e2) = Let <$> expr e1 <*> expr e2
--    expr (Do e) = Do <$> expr e
    expr (Function e1 fs e2) = function e1 fs e2
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (Define i e) = Define i <$> expr e
    expr (Choice e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (Range e1) = Range <$> expr e1
    expr Any = pure Any
    expr e = impossible e

    function e1@(Define _ AnyT) fs e2 = Function e1 fs <$> expr e2
    function e1 fs e2 = do
      i <- newIdent "z"
      e1' <- expr e1
      e2' <- expr e2
      pure $ Function (Define i AnyT) fs $ seqE [Unify e1' (Variable i), e2']

{-

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

------------

simplify :: Expr -> Expr
simplify = simp
  where --simp (Unify v@(Variable _) (Range e)) = Seq [ApplyD (simp e) v, v]
    simp (Unify e Any) = simp e
    simp (ApplyD (Variable (Ident _ "any")) e) = simp e
    simp e = composOp simp e
-}

-- Make all Array take value arguments
anfS :: Expr -> D Expr
anfS = anf
  where
    anf e@Array{} = val e
    anf e = compos anf e
    val e = do
      (es, v) <- value e
      pure $ seqE $ es ++ [v]
    value e@LitInt{} = pure ([], e)
    value e@LitRat{} = pure ([], e)
    value e@Variable{} = pure ([], e)
    value (Array es) = do
      (ess, vs) <- unzip <$> mapM value es
      pure (concat ess, Array vs)
    value e@Function{} = pure ([], e)
    value e@Typedef{} = pure ([], e)
    value e@Any{} = pure ([], e)
    value e = do
      i <- newIdent "a"
      e' <- anf e
      pure ([Define i e'], Variable i)

------------

-- Get all visible identifiers from i := e
getVisible :: Expr -> [Ident]
getVisible LitInt{} = []
getVisible LitRat{} = []
getVisible Variable{} = []
getVisible (Array es) = concatMap getVisible es
getVisible (Seq es) = concatMap getVisible es
getVisible (ApplyS e1 e2) = getVisible e1 ++ getVisible e2
getVisible (ApplyD e1 e2) = getVisible e1 ++ getVisible e2
getVisible If3{} = []
getVisible For2{} = []
getVisible (Let _ e) = getVisible e
getVisible Do{} = []
getVisible (Unify e1 e2) = getVisible e1 ++ getVisible e2
getVisible (Typedef e) = []
getVisible (Define i e) = i : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible Any = []
getVisible Function{} = []
getVisible e = impossible e

--------------

data ScopeErr
  = ErrMultiple [Ident]
  | ErrUndefined Ident
  | ErrShadow Ident
--  deriving (Show)

scopeCheck :: Expr -> D Expr
scopeCheck e = do
  let errs = scopeErrs (S.fromList predefs) (Do e)
  case [ is | ErrMultiple is <- errs ] of
    [] -> pure ()
    is : _ -> error $ "scopeCheck: Multiply defined " ++ show is
  case [ i | ErrUndefined i <- errs ] of
    [] -> pure ()
    -- Make it a trace instead of an error for now
    is -> traceM $ "scopeCheck: warning undefined " ++ show is
  case [ i | ErrShadow i <- errs ] of
    [] -> pure e
    -- Make it a trace instead of an error for now
    iis -> trace ("scopeCheck: warning shadowing " ++ show iis) $
           -- XXX Here we should patch up the shadowing problem
           pure e

scopeErrs :: S.Set Ident -> Expr -> [ScopeErr]
scopeErrs s = expr
  where
    expr LitInt{} = []
    expr LitRat{} = []
    expr (Variable i) | i `S.member` s = []
                      | otherwise = [ErrUndefined i]
    expr (Array es) = concatMap expr es
    expr (Seq es) = concatMap expr es
    expr (ApplyS e1 e2) = expr e1 ++ expr e2
    expr (ApplyD e1 e2) = expr e1 ++ expr e2
    expr (If3 e1 e2 e3) = errs ++ scopeErrs s' e1 ++ scopeErrs s' (Do e2) ++ expr (Do e3)
      where (errs, s') = defs e1
    expr (For2 e1 e2) = errs ++ scopeErrs s' e1 ++ scopeErrs s' (Do e2)
      where (errs, s') = defs e1
    expr (Let e1 e2) = errs ++ scopeErrs s' e1  ++ scopeErrs s' (Do e2)
      where (errs, s') = defs e1
    expr (Do e) = errs ++ scopeErrs s' e
      where (errs, s') = defs e
    expr (Function e1 fs e2) = errs ++ scopeErrs s' e1  ++ scopeErrs s' (Do e2)
      where (errs, s') = defs e1
    expr (Unify e1 e2) = expr e1 ++ expr e2
    expr (Define i e) = expr e
    expr (Choice e1 e2) = expr (Do e1) ++ expr (Do e2)
    expr (Range e1) = expr e1
    expr Any = []
    expr e = impossible e
    
    defs :: Expr -> ([ScopeErr], S.Set Ident)
    defs e =
      let is = getVisible e
          errM = if anySame is then [ErrMultiple is] else []
          errS = [ ErrShadow i | i <- is, i `S.member` s ]
          s' = foldr S.insert s is
      in  (errM ++ errS, s')
