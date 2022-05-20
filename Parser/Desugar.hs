{-# LANGUAGE TupleSections #-}
module Desugar(desugar, simplify, primOps, getVisible) where
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
import Misc

desugar :: Expr -> Expr
desugar = eval . (anfS <=< hackRange <=< desugarFunctionS <=< desugarDoS <=< scopeCheck <=< desugarS)
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
    --expr (ApplyS e1 e2) | ()/=() = expr $ ApplyD eSucceeds $ ApplyD e1 e2
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
    expr AnyT = pure AnyT

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

--eSucceeds :: Expr
--eSucceeds = Variable (Ident noLoc "succeeds")

define :: Loc -> Ident -> Expr -> Expr
define _l i e = Define i e

tAny :: Loc -> Ident -> Expr
tAny l i = define l i AnyT

desugarColon :: Loc -> Expr -> Expr -> D Expr
desugarColon l x t = desugarDef l x (PrefixOp (Ident l ":") t)

desugarDef :: Loc -> Expr -> Expr -> D Expr
desugarDef _ (InfixOp x (Ident l ":") t) e = desugarDef l x $ ApplyS t e  -- XXX Is this correct
desugarDef l f@ApplyS{} e = desugarFunDef l f [] e
desugarDef l f@EffAttr{} e = desugarFunDef l f [] e
desugarDef l x e = desugarBind l x e

desugarBind :: Loc -> Expr -> Expr -> D Expr
desugarBind l (Variable i) e = define l i <$> desugarS e
desugarBind l (PostfixOp x q@(Ident _ "?")) e = desugarBind l x (PostfixOp e q)
desugarBind l (PostfixOp x (Ident _ "^")) e = desugarS $ ApplyS (eAssign l) $ Array [x, e]
desugarBind l (Array ls) e = do
  xs <- mapM (const $ newIdent "d") ls
  let eun = Unify (Array (map (tAny l) xs)) e
  els <- zipWithM (\ lhs x -> desugarBind l lhs (Variable x)) ls xs
  pure $ Seq $ eun : els
desugarBind l (InfixOp x (Ident _ "~>") y) (PrefixOp (Ident _ ":") t) = do
  (i, ex) <-
    case x of
      Variable i -> pure (i, [])
      _ -> do
        i <- newIdent "d"
        ex <- desugarBind l x (Variable i)
        pure (i, [ex])
  ey <- desugarBind l y (ApplyD t (tAny l i))
  pure $ seqE $ ey : ex
desugarBind l lhs@(InfixOp _ (Ident _ "~>") _) e =
  desugarBind l lhs (PrefixOp (Ident noLoc ":") (Typedef e))
-- What else is allowed?  LitInt and LitRat would be easy.
desugarBind l x y = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x ++ ", RHS=" ++ prettyShow y

desugarFunDef :: Loc -> Expr -> [Eff] -> Expr -> D Expr
desugarFunDef l (EffAttr f a) as e = desugarFunDef l f (a:as) e
desugarFunDef l (ApplyS f a) as e = desugarFunDef l f [] $ Function a (reverse as) e
desugarFunDef l (Variable f) [] e = define l f <$> desugarS e
desugarFunDef _ Variable{} _ _ = internalError
desugarFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f

-- Definitions that should go in a Prelude
prelude :: [Ident]
prelude = map (Ident noLoc)
  [ "int", "float", "string", "any", "nat", "false"]

-- Primitives
primOps :: [Ident]
primOps = map (Ident noLoc)
  [ "int#", "float#", "string#"
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
    expr (Choice e1 e2) = Choice <$> expr e1 <*> expr e2
    expr (Range e1) = Range <$> expr e1
    expr (Typedef e1) = Typedef <$> expr e1
    expr AnyT = pure AnyT
    expr e = impossible e

    function (Define i a) fs e2 | isAnyT a = Function (Define i AnyT) fs <$> expr e2
      -- XXX remove "any" when we have a simplifier.
      where isAnyT AnyT = True
            isAnyT (Range (Variable (Ident _ "any"))) = True
            isAnyT _ = False
    function e1 fs e2 = do
      i <- newIdent "z"
      e1' <- expr e1
      e2' <- expr e2
      pure $ Function (Define i AnyT) fs $ seqE [Unify e1' (Variable i), e2']

-- Make all Array take value arguments, as well as ApplyS/ApplyD
anfS :: Expr -> D Expr
anfS = anf
  where
    anf e@Array{} = val e
    anf (ApplyS e1 e2) = app ApplyS e1 e2
    anf (ApplyD e1 e2) = app ApplyD e1 e2
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
    value e@Function{} = ([],) <$> anf e
    value e@Typedef{} = ([],) <$> anf e
    value e@AnyT{} = pure ([], e)
    value (Define i e) = do
      -- Special version of next case; no need for a new variable
      e' <- anf e
      pure ([Define i e'], Variable i)
    value e = do
      i <- newIdent "a"
      e' <- anf e
      pure ([Define i e'], Variable i)
    app con e1 e2 = do
      (es1, e1') <- value e1
      (es2, e2') <- value e2
      pure $ seqE $ es1 ++ es2 ++ [con e1' e2']

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
getVisible (Typedef _) = []
getVisible (Define i e) = i : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible AnyT = []
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
  let errs = scopeErrs (S.fromList $ prelude ++ primOps) (Do e)
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
    expr (Function e1 _ e2) = errs ++ scopeErrs s' e1  ++ scopeErrs s' (Do e2)
      where (errs, s') = defs e1
    expr (Unify e1 e2) = expr e1 ++ expr e2
    expr (Define _ e) = expr e
    expr (Choice e1 e2) = expr (Do e1) ++ expr (Do e2)
    expr (Range e1) = expr e1
    expr (Typedef e1) = expr (Do e1)
    expr AnyT = []
    expr e = impossible e

    defs :: Expr -> ([ScopeErr], S.Set Ident)
    defs e =
      let is = getVisible e
          errM = if anySame is then [ErrMultiple is] else []
          errS = [ ErrShadow i | i <- is, i `S.member` s ]
          s' = foldr S.insert s is
      in  (errM ++ errS, s')

------------

simplify :: Expr -> Expr
simplify = simp
  where
    simp (Unify e AnyT) = simp e
    simp (ApplyD (Variable (Ident _ "any")) e) = simp e
    simp (ApplyD (Typedef e1) e2) = simp $ Unify e1 e2
    --simp (Range (Typedef e)) = simp e   -- Always?
    simp e = composOp simp e

{-

e1:e2  <-->  e1 := :e2
e1:=e2 <-->  e1:typedef{e2}

:typedef{e} <--> e

e1 := e2  -->  e1 := :typedef{e2}  -->  e1:typedef{e2}

-}

hackRange :: Expr -> D Expr
hackRange = f
  where
    f (Range e) = do
      r <- newIdent "r"
      e' <- f e
      pure $ ApplyD e' (tAny noLoc r)
    f e = compos f e
