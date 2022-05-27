{-# LANGUAGE TupleSections #-}
module Desugar(desugar, simplify, primOps, getVisible) where
--import Control.Arrow(first, second)
import Control.Monad.State.Strict
--import Data.List
--import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Debug.Trace
import GHC.Stack

import Expr
import Error
import Print hiding (first, colon)
import Misc

isLiteral :: Expr -> Bool
isLiteral LitInt{} = True
isLiteral LitRat{} = True
isLiteral _ = False

doTrace :: Bool
doTrace = False

-------

desugar :: Expr -> Expr
desugar = eval . (anfS <=< dsDo <=< scopeCheck <=< dsD Eval <=< dropParens)
  where eval = flip evalState 1

data Context = Eval | Abs
  deriving (Show, Eq)

type D = State Int

type SExpr = Expr   -- Simple Expr: only has some of the constructors

dropParens :: Expr -> D Expr
dropParens = f
  where f (Parens e) = f e
        f e = compos f e

-- This follows the D transformation in calculus.ltx
dsD :: Context -> Expr -> D SExpr
dsD _ctx = expr
  where
    expr :: HasCallStack => Expr -> D Expr
    expr e | doTrace && trace ("dsD " ++ prettyShow e) False = undefined

    -- Basic forms
    -- D[k] = k
    expr e | isLiteral e = pure e
    -- D[x] = x
    expr e@Variable{} = pure e
    -- D[e1,...,en] = ???
    -- FIX D: update for splices
    expr (Array es) = arrSplice $ exprElems es
    -- D[e1;...;en] = D[e1]; ...; D[en]
    expr (Seq es) = seqE <$> mapM expr es
    -- D[e1(e2)] = D[e1](D[e2])
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    -- D[e1[e2]] = D[e1][D[e2]]
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    -- Attributes are only allow for function definitions
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    -- D[e1 = e2] = D[e1] = D[e2], also change constructor
    expr (InfixOp e1 (Ident _ "=") e2) = Unify <$> expr e1 <*> expr e2
    -- D[e1 | e2] = D[e1] | D[e2], also change constructor
    expr (InfixOp e1 (Ident _ "|") e2) = Choice <$> expr e1 <*> expr e2

    -- Bindings
    -- D [lhs : t] = Lt[lhs] D[t]
    expr (InfixOp lhs (Ident l ":") t) = dsLHSColon l lhs =<< expr t
    -- D [lhs := e] = L[lhs] D[e]
    expr (InfixOp lhs (Ident l ":=") e) = dsLHS l lhs =<< expr e
    -- Function bindings recognized by dsLHS

    -- Functions
    -- D[e1 => e2] = D[fn(e1){e2}]
    expr (InfixOp e1 (Ident _ "=>") e2) = expr $ Function [(e1, [])] e2
    -- See below
    expr (Function [(e, [])] b) = function e b
    expr (Function [(e, r:rs)] b) = expr $ Function [(e, rs)] $ applyEffect r b
    -- D[fn a1 a2 ... {b}] = D[fn a1 (fn a2 ... {e})]
    expr (Function (a:as) b) = expr $ Function [a] $ Function as b

    -- Types
    -- D[:t] = : D[t], also change constructor
    expr (PrefixOp (Ident _ ":") t) = Range <$> expr t
    -- D[typedef{e}] = typedef{D[e]}
    expr (Typedef e) = Typedef <$> expr e

    -- Conditionals
    -- D[if{e}] = D[if(e) else false]
    expr (If1 e) = expr $ If2E e Unit
    -- D[if(e1)then e2] = D[if(e1)then e2 else false]
    expr (If2 e1 e2) = expr $ If3 e1 e2 Unit
    -- D[if(e1)else e2] = D[if(x:e1)then x else e2]
    expr (If2E e1 e2) = inVar e1 >>= \ (t, e') -> expr $ If3 e' t e2
    -- D[if(e1)then e2 else e3] = if(D[e1])then D[e2] else D[e3]
    expr (If3 e1 e2 e3) = If3 <$> expr e1 <*> expr e2 <*> expr e3

    -- For
    -- D[for{e}] = D[for(x:=e)in x]
    expr (For1 e) = inVar e >>= \ (t, e') -> expr $ For2 e' t
    -- D[for(e1)in e2] = for(D[e1]) in D[e2]
    expr (For2 e1 e2) = For2 <$> expr e1 <*> expr e2

    -- Operators
    -- D[!e] = D[if(e)then :false else false]
    expr (PrefixOp (Ident _ "!") e) = If3 <$> expr e <*> pure Fail <*> pure Unit
    -- D[op e] = op D[e]
    expr (PrefixOp (Ident l op) e) = expr $ call "pre" l op e
    -- D[e op] = D[e] op
    expr (PostfixOp e (Ident l op)) = expr $ call "post" l op e
    expr (InfixOp e1 (Ident l op) e2) =
      case op of
        -- D[e1 && e2] = D[e1]; D[e2]
        "&&" -> expr $ seqE [e1, e2]
        -- D[e1 || e2] = D[if(e1)else e2]
        "||" -> expr $ If2E e1 e2
        -- D[e1 where e2] = D[x:= e1; e2; x]
        "where" -> newIdent "w" >>= \ x -> expr $ Seq [define l x e1, e2, Variable x]
        -- D[e1 op e2] = D[e1] op D[e2]
        _    -> expr $ call "in" l op $ Array [e1, e2]

    -- Let, do, case
    -- 'let' kept until after scope check
    expr (Let e1 e2) = Let <$> expr e1 <*> expr e2
    -- 'do' kept until after scope check
    expr (Do e) = Do <$> expr e  -- XXX scope check?
    -- D[case {b}] = D[(x:any) => case(x)of b]
    expr (Case1 b) =
      newIdent "d" >>= \ i -> expr $ InfixOp (tAny noLoc i) (Ident noLoc "=>") $ Case2 (Variable i) b
    -- D[case(e)of b] = ...
    expr (Case2 e1 e2) = dsCase e1 e2

    -- Misc
    expr (Block es) = expr $ seqE es

    -- FIX D: add
    -- D[option{}] = false
    expr (Option Nothing) = pure Unit
    -- D[option{e}] = D[if(x:=e)then truth(e)
    expr (Option (Just e)) = inVar e >>= \ (t, e') -> expr $ If2 e' (ApplyD eTruth t)

    -- Make it idempotent
    expr (Define i e) = Define i <$> expr e
    expr (Choice e1 e2) = Choice <$> expr e1 <*> expr e2
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr AnyT = pure AnyT

    expr e = impossible e

    -- Pick the appropriate form of apply for operators
    call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='"] = ApplyD
                | otherwise = ApplyS
            s' = p ++ "'" ++ s ++ "'"

    -- Handle function(e){b}
    -- For nicer desugaring, handle ":any" argument specially
    function (Define i a) b | isAnyT a = Function [(Define i AnyT, [])] <$> expr b
      -- XXX remove "any" when we have a simplifier.
      where isAnyT AnyT = True
            isAnyT (Range (Variable (Ident _ "any"))) = True
            isAnyT _ = False
    -- D[fn(e){b}] = fn(x:any){M[e]x; D[do b]}
    function e b = do
      x <- newIdent "z"
      e' <- dsD Abs e
      b' <- expr (Do b)
      pure $ Function [(Define x AnyT, [])] $ seqE [Unify e' (Variable x), b']

    -- Splice together ArrayElems
    arrSplice :: [ArrayElem] -> D SExpr
    arrSplice [EElems es] = Array <$> mapM expr es  -- no splices
    arrSplice as = applyPrim "concat#" <$> Array <$> mapM (expr . f) as
      where f (EElems es) = Array es
            f (ESplice e) = e

data ArrayElem = EElems [Expr] | ESplice Expr
  deriving (Show)

-- Handle an array element, it can be ..e or e
exprElems :: [Expr] -> [ArrayElem]
exprElems = grp . map cls
  where cls (PrefixOp (Ident _ "..") e) = Left e
        cls e = Right e
        grp [] = []
        grp (Left e : as) = ESplice e : grp as
        grp as =
          let (rs, bs) = span isRight as
          in  EElems [ e | Right e <- rs ] : grp bs

applyPrim :: String -> SExpr -> SExpr
applyPrim s e = ApplyS (Variable (Ident noLoc s)) e

applyPrimD :: String -> SExpr -> SExpr
applyPrimD s e = ApplyD (Variable (Ident noLoc s)) e

-- XXX do something special?
applyEffect :: Ident -> Expr -> Expr
applyEffect i e = ApplyD (Variable i) e

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

inVarC :: Expr -> (Expr -> D Expr) -> D Expr
inVarC e@Variable{} k = k e
inVarC e k = do
  i <- newIdent "d"
  ke <- k (Variable i)
  pure $ seqE [define noLoc i e, ke]

dsCase :: Expr -> Block -> D Expr
-- D[case x of e1; ... en] = e1[x] || ... || en[x]
dsCase e@Variable{} (Block es) = dsD Eval $ foldr mkOr Fail es
  where mkOr a r = If2E (ApplyD a e) r
dsCase Variable{} _ = internalError
-- D[case e of b] = x=D[e]; D[case x of b]
dsCase e b = do
  e' <- dsD Eval e
  inVarC e' $ \ x -> dsCase x b

eAssign :: Loc -> Expr
eAssign l = Variable (Ident l "assign#")

eTruth :: Expr
eTruth = Variable (Ident noLoc "truth")

define :: Loc -> Ident -> Expr -> Expr
define _l i e = Define i e

tAny :: Loc -> Ident -> Expr
tAny l i = define l i AnyT

-- Desugar a definition e1 := e2
-- L
dsLHS :: Loc -> Expr -> SExpr -> D SExpr
dsLHS l f e | isFunDef f = dsFunDef l f [] e
dsLHS l x e = dsL l x e

-- Desugar a definition e1 : e2
dsLHSColon :: Loc -> Expr -> SExpr -> D SExpr
dsLHSColon l e t | isFunDef e = dsFunDefType l e [] =<< dsD Eval t
-- L[lhs1 ~> lhs2] (:t) = L[lhs1] Lt[lhs2] t 
dsLHSColon l (InfixOp lhs1 (Ident _ "~>") lhs2) t = do
  e <- dsLHSColon l lhs2 t
  dsLHS l lhs1 e
dsLHSColon l e t = do
  x <- newIdent "d"
  t' <- dsD Eval t
  e' <- dsL l e (ApplyD t' (tAny l x))
  pure $ Seq [e', Variable x]

isFunDef :: Expr -> Bool
isFunDef ApplyS{} = True
isFunDef EffAttr{} = True
isFunDef (InfixOp f (Ident _ ":") _) = isFunDef f
isFunDef _ = False

-- Desugar a definition lhs := e
dsL :: Loc -> Expr -> SExpr -> D SExpr
dsL _ e1 e2 | doTrace && trace ("dsL " ++ prettyShow (e1, e2)) False = undefined
-- L[x] e = x := e
dsL l (Variable x) e = pure $ define l x e
-- L[:t] e = D[t][e]
dsL l (PrefixOp colon@(Ident _ ":") t) e = do
  x <- newIdent "x"
  dsL l (InfixOp (Variable x) colon t) e
-- L[l:t] e = l := D[t][e]
dsL l (InfixOp x (Ident _ ":") t) e = do
  t' <- dsD Eval t
  dsL l x (ApplyD t' e)
-- FIX L: use option
-- L[e?] = ????
dsL _l (PostfixOp _lhs (Ident _ "?")) _e = undefined
-- L[e1^] e = assign(D[e1], e)
dsL l (PostfixOp e1 (Ident _ "^")) e = do
  e1' <- dsD Eval e1
  pure $ ApplyS (eAssign l) $ Array [e1', e]
-- FIX L: update for splices
-- L[lhs1, ... lhsn] = ...
dsL l (Array lhss) e = dsLArr l lhss e
-- L[lhs ~> lhs2] e = L[lhs ~> lhs2] (: typedef{e})
dsL l lhs@(InfixOp _ (Ident _ "~>") _) e = dsLHSColon l lhs (Typedef e)
-- What else is allowed?  LitInt and LitRat would be easy.
dsL l x y = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x ++ ", RHS=" ++ prettyShow y

-- Handle ..l, l
dsLArr :: Loc -> [Expr] -> SExpr -> D SExpr
dsLArr l lhss e =
  case exprElems lhss of
    -- L[lhs0,...,lhsn] e = (x0:any,...,xn:any) = e; L[lhs0]x0; ...; L[lhsn]xn
    [EElems ls] -> do
      xs <- mapM (const $ newIdent "d") ls
      let eun = Unify (Array (map (tAny l) xs)) e
      els <- zipWithM (\ lhs x -> dsL l lhs (Variable x)) ls xs
      pure $ Seq $ els ++ [eun]

    [ESplice lhs] ->
      dsL l lhs e
    
    [EElems ls1, ESplice lhs] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent "d"; pure (Variable v, [define l v e])
      let
        len1 = toInteger $ length ls1
        a1  = applyPrim "takeL#" $ Array [LitInt len1, v]
        e'  = applyPrim "dropL#" $ Array [LitInt len1, v]
      v' <- newIdent "d"
      let
        bv' = define l v' e'
      m1 <- dsL l (Array ls1) a1
      mm <- dsL l lhs e'
      pure $ Seq $ bv ++ [m1, bv', mm, v]

    [ESplice lhs, EElems ls2] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent "d"; pure (Variable v, [define l v e])
      let
        len2 = toInteger $ length ls2
        a2  = applyPrim "takeR#" $ Array [LitInt len2, v]
        e'' = applyPrim "dropR#" $ Array [LitInt len2, v]
      mm <- dsL l lhs e''
      m2 <- dsL l (Array ls2) a2
      pure $ Seq $ bv ++ [mm, m2, v]

    [EElems ls1, ESplice lhs, EElems ls2] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent "d"; pure (Variable v, [define l v e])
      let
        len1 = toInteger $ length ls1
        len2 = toInteger $ length ls2
        a1  = applyPrim "takeL#" $ Array [LitInt len1, v]
        e'  = applyPrim "dropL#" $ Array [LitInt len1, v]
      v' <- newIdent "d"
      let
        bv' = define l v' e'
        a2  = applyPrim "takeR#" $ Array [LitInt len2, Variable v']
        e'' = applyPrim "dropR#" $ Array [LitInt len2, Variable v']
      m1 <- dsL l (Array ls1) a1
      mm <- dsL l lhs e''
      m2 <- dsL l (Array ls2) a2
      pure $ Seq $ bv ++ [m1, bv', mm, m2, v]

    _ -> syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow (Array lhss) ++ ", should only have one ..e"

dsFunDef :: Loc -> Expr -> [Eff] -> Expr -> D SExpr
dsFunDef l (InfixOp f (Ident _ ":") t) as e = dsFunDef l f as (ApplyS t e)
dsFunDef l (EffAttr f a) as e = dsFunDef l f (a:as) e
dsFunDef l (ApplyS f a) as e = dsFunDef l f [] $ Function [(a, reverse as)] e
dsFunDef l (Variable f) [] e = define l f <$> dsD Eval e
dsFunDef _ Variable{} _ _ = internalError
dsFunDef l f _ _ = syntaxError l $ "bad function definition: " ++ prettyShow f

dsFunDefType :: Loc -> Expr -> [Eff] -> SExpr -> D SExpr
dsFunDefType l (EffAttr f a) as t = dsFunDefType l f (a:as) t
-- XXX This does not support dependent types yet
dsFunDefType l (ApplyS f a) [] t = do
  a' <- tyOf a
  dsFunDefType l f [] $ applyPrimD "in'->'" $ Array [a', t]
dsFunDefType _l (ApplyS _f _a) _as _e = unimplemented "function type with effects"
dsFunDefType l (Variable f)  [] t = pure $ define l f $ Range t
dsFunDefType _ Variable{} _ _ = internalError
dsFunDefType l f _ _ = syntaxError l $ "bad function type definition: " ++ prettyShow f

tyOf :: Expr -> D SExpr
tyOf (InfixOp _ (Ident _ ":") t) = dsD Eval t
tyOf (PrefixOp (Ident _ ":") t) = dsD Eval t
tyOf e = Typedef <$> dsD Abs e

-- Definitions that should go in a Prelude
prelude :: [Ident]
prelude = map (Ident noLoc)
  [ "int", "float", "string", "any", "nat", "false"
  , "in'..'", "in'->'"
  , "in'<'", "in'<='", "in'>'", "in'>='"
  ]

-- Primitives
primOps :: [Ident]
primOps = map (Ident noLoc)
  [ "isInt#", "isFloat#", "isString#", "isArr#", "isFcn#"
  , "in'+'", "in'-'", "in'*'", "in'/'"
  , "in'<>'"
  , "pre'-'"
  , "post'^'", "post'?'"
  , "succeeds", "decides", "iterates", "io"
  , "assign#"
  , "concat#", "takeL#", "dropL#", "takeR#", "dropR#"
  ]

--------------------

-- Remove let/do and possible name clashes.
-- XXX name clash removal not implemented
-- XXX Also get rid of lingering Range.
--     They should probably not exist in the first place.
dsDo :: Expr -> D Expr
dsDo = f
  where
    -- D[do b] = D[b]
    f (Do b) = f b
    -- D[let(e)in b] = D[e]; D[b]
    f (Let e b) = f $ seqE [e, b]
    f (Range e) = do
      r <- newIdent "r"
      e' <- f e
      pure $ Seq [ApplyD e' (tAny noLoc r), Variable r]
    f e = compos f e

--------------------
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
    expr (Function [] e2) = scopeErrs s (Do e2)
    expr (Function ((a,_):ars) e2) = errs ++ scopeErrs s' (Function ars e2)
      where (errs, s') = defs a
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
