{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module FrontEnd.Desugar(
  desugar,
  primOps, covariantId, dsScope,
  simplify,
  exprToCore,
  simpCore,
  ) where
import Control.Monad
import Control.Monad.State.Strict
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack
import Epic.List
import Epic.Print
--import FrontEnd.Desugar
import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags

-- TODO:
--  reduce variables passed to body in if/for
--  do beta reduction for inlined primitives

desugar :: Flags -> Expr -> Expr
--desugar flgs | trace ("desugar: " ++ show flgs) False = undefined
desugar flgs = eval flgs .
            (traceDS "lower"      <=< lower    <=<
             traceDS "simp"       <=< simp     <=<
             traceDS "addScope"   <=< addScope <=<
             traceDS "dsD"        <=< dsD      <=<
             traceDS "addDeref"   <=< addDeref <=<
             traceDS "dsSmall"    <=< dsSmall  <=<
             traceDS "dropParens" <=< dropParens)
  where
    tr = fTraceDesugar flgs
    traceDS :: String -> Expr -> D Expr
    traceDS msg e | tr = trace ("---- " ++ msg ++ "\n" ++ prettyShow e) $
                         pure e
                  | otherwise = pure e

------

type D = State DState

-- XXX Do we really need the distinction between abstracttion and evaluation
-- context?
-- Right now it is used to guide desugaring to avoid an uninstantiated exist
-- in the function desugaring.

data DState = DState { nextNo :: !Int, context :: !DContext, dflags :: !Flags }
  deriving (Show)
data DContext = DAbstract | DEval
  deriving (Show)

newInt :: D Int
newInt = do
  ds <- get
  let n = nextNo ds
  put $! ds { nextNo = n+1 }
  pure n

newIdent :: Loc -> String -> D Ident
newIdent l s = do
  n <- newInt
  pure $ Ident l $ "$" ++ s ++ show n

withContext :: DContext -> D a -> D a
withContext c da = do
  oldc <- gets context
  modify $ \ ds->ds{ context = c }
  a <- da
  modify $ \ ds->ds{ context = oldc }
  pure a

---------------------

dropParens :: Expr -> D Expr
dropParens = f
  where f (Parens e) = f e
        f (InfixOp (InfixOp (Variable i1) (Ident l2 ":") e2) (Ident l3  "=") e3) =
          f $ InfixOp (InfixOp (Variable i1) (Ident l2 ":") e2) (Ident l3 ":=") e3
        f e = compos f e

---------------------

eval :: Flags -> D Expr -> Expr
eval flgs = flip evalState DState{ nextNo = 1, context = DEval, dflags = flgs }

-- Desugar into Small Source Verse
dsSmall :: Expr -> D Expr
dsSmall = ds
  where
    ds :: Expr -> D Expr
    -- Application and unification
    ds (InfixOp e1 (Op "where") e2) = do
      x <- newIdent (getLoc e1) "x"
      ds $ seqE [DefineE x e1, e2, Variable x]
    ds (InfixOp e1@Variable{} (Op "=") e2) = ds $ Unify e1 e2
    ds (InfixOp e1            (Op "=") e2) = do x <- newIdent (getLoc e1) "x"; ds $ seqE [DefineE x e1, Unify (Variable x) e2]
    ds (ApplyD  e1 e2) = join (apply ApplyD <$> ds e1 <*> ds e2)
    ds (ApplyS  e1 e2) = join (apply applyS <$> ds e1 <*> ds e2)
      where applyS x y = Succeeds (ApplyD x y)

    -- Bindings
    ds (InfixOp e1 o@(Op ":")  e2) = ds =<< defn e1 (PrefixOp o e2)
    ds (InfixOp e1   (Op ":=") e2) = ds =<< defn e1 e2

    -- Function notation
    ds (Typedef e) = do y <- newIdent (getLoc e) "y"; ds $ Function [(DefineE y e, [])] (Variable y)
    ds (InfixOp e1 (Op "=>") e2) = ds $ Function [(e1, [])] e2
    ds (Function (a:as@(_:_)) b) = ds $ Function [a] $ Function as b
    -- ds Function [] ...
    -- XXX effects
    ds (If1 e) = ds $ If2E e eFalse
    ds (If2 e1 e2) = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2) = do x <- newIdent (getLoc e1) "x"; ds $ If3 (DefineE x e1) (Variable x) e2
    ds (For1 e) = do x <- newIdent (getLoc e) "x"; ds $ For2 (DefineE x e) (Variable x)

    -- Operators
    ds (PrefixOp (Op "not") e) = do e' <- ds e; pure $ If3 e' eFail eFalse
    ds (PrefixOp (Op ":") e) = Range <$> ds e
    ds (PrefixOp (Ident l op) e) = ds =<< call "pre" l op e
    ds (PostfixOp e (Op "?")) = Range <$> ds e
    ds (PostfixOp e (Ident l op)) = ds =<< call "post" l op e
    ds (InfixOp e1 (Op "|") e2) = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2) = ds $ Seq [e1, e2]                  -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 eFail) eFail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2) = ds $ If2E e1 $ If2E e2 eFail
    ds (InfixOp e1 (Ident l op) e2) = ds =<< call "in" l op (Array [e1, e2])

    -- Array
    ds (Array es) = arraySplice =<< mapM elm es
      where elm (PrefixOp (Ident l "..") e) = PrefixOp (Ident l "..") <$> ds e
            elm e = ds e

    -- Let do case
    ds (Case1 b) = do
      let l = getLoc b
      x <- Variable <$> newIdent l "x"
      ds $ Function [(InfixOp x (Op ":") eAny, [])] $ Case2 x b
    ds (Case2 _ _) = undefined
    ds (Block es) = ds $ seqE es

    ds (Seq es) = seqE <$> mapM ds es
    ds (HasType e1 e2) = HasType <$> ds e1 <*> ds e2

    -- Misc
    ds (Variable (Ident l "_")) = DefineV <$> newIdent l "u"
    ds (Option Nothing) = pure eFalse
    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (DefineE t e) (Array [Variable t])

    -- one, all
    ds (Macro1 (Ident _ "one") [] e) = ds $ If2E e eFail
    ds (Macro1 (Ident _ "all") [] e) = ds $ For1 e

    ds x = compos ds x

type Value = Expr

apply :: (Value -> Value -> Expr) -> Expr -> Expr -> D Expr
-- val1[e2]  -->
apply con e1 e2 | isValue e1 = apply1 con e1 e2   -- Easy special case.  Not really needed
-- e1[e2]  -->  f:=e1; f[e2]
apply con e1 e2 = do
  f <- newIdent (getLoc e1) "f"
  r <- apply1 con (Variable f) e2
  pure $ seqE [DefineE f e1, r]

apply1 :: (Value -> Value -> Expr) -> Value -> Expr -> D Expr
-- val1[val2] 
apply1 con x1 e2 | isValue e2 = apply2 con x1 e2   -- Easy special case.  Not really needed
-- val1[e2]  -->  a:=e2; val1[a]
apply1 con x1 e2 = do
  a <- newIdent (getLoc e2) "a"
  r <- apply2 con x1 (Variable a)
  pure $ seqE [DefineE a e2, r]

-- val1[val2]  --> 
apply2 :: (Value -> Value -> Expr) -> Value -> Value -> D Expr
apply2 con x1 x2 = pure $ con x1 x2

defn :: Expr -> Expr -> D Expr
-- Rule: (i := e) -->  (i := e)
defn (Variable i) e = pure $ DefineE i e
-- Rule: (f(a) := e)  -->  (f := function(a){e})
-- Rule: (p<a> := e)  -->  ...
defn p e | Just (f, a, rs) <- getFun p = defn f (Function [(a, rs)] e)
-- Rule: (e1:e2 := e)  -->  (e1 := e:e2)
defn (InfixOp e1 (Op ":") e2) e = defn e1 (HasType e e2)
-- Rule: (:e1) := e2  XXX Allowed?
defn (PrefixOp (Op ":") e1) e2 = pure $ ApplyD e1 e2   -- ApplyD or ApplyS?
--defn (EffAttr e1 r) e v = defn e1 (applyEff [r] e) v
-- Rule: (p?) := e  -->  p := option{e}
--defn (PostfixOp p (Ident _ "?")) e = defn p (Option $ Just e)
-- Rule: (p1,...) := e  -->  (x1:any,...) = e; p1 := x1; ...
defn (Array ps) e = defnArray ps e
-- Rule (p1 -> p2) := e  -->  p1 := x1; p2 := x2; (x1 -> x2) := e
defn (InfixOp (Variable x1) (Op "->") (Variable x2)) e = pure $ DefineIE x1 x2 e
defn (InfixOp x1@Variable{} op@(Op "->") p2) e = do
  x2 <- Variable <$> newIdent (getLoc p2) "x"
  r2 <- defn p2 x2
  r  <- defn (InfixOp x1 op x2) e
  pure $ seqE [r2, r]
defn (InfixOp p1 op@(Op "->") p2) e = do
  x1 <- Variable <$> newIdent (getLoc p2) "x"
  r1 <- defn p1 x1
  r  <- defn (InfixOp x1 op p2) e
  pure $ seqE [r1, r]
defn p _ = error $ "Bad LHS to := " ++ prettyShow p
--defn p _ = impossible p

-- Return function, argument, and attributes
getFun :: Expr -> Maybe (Expr, Expr, [Ident])
getFun = gf []
  where
    gf rs (EffAttr e r) = gf (r:rs) e
    gf rs (ApplyS f a) = Just (f, a, reverse rs)
    gf _ _ = Nothing

eFalse :: Expr
eFalse = Array []

eFail :: Expr
eFail = Range eFalse

eAny :: Expr
eAny = Variable (Ident noLoc "any")

defnArray :: [Expr] -> Expr -> D Expr
defnArray ps e = do
  let var p = do
        let (wrap, ip) =
              case p of
                PrefixOp (Ident l "..") p' -> (PrefixOp (Ident l ".."), p')
                _ -> (id, p)
        case ip of
          Variable v ->
            pure (Nothing, wrap (DefineV v))
          _ -> do
            x <- newIdent (getLoc p) "x"
            pure (Just (Variable x, ip), wrap (DefineV x))
  (xps, es) <- unzip <$> mapM var ps
  arr <- arraySplice es
  let (xs, ps') = unzip $ catMaybes xps
  bs <- zipWithM defn ps' xs
--  traceM ("*** " ++ show bs)
  pure $ seqE $ bs ++ [InfixOp arr (Op "=") e]
      
arraySplice :: [Expr] -> D Expr
arraySplice as =
--  trace ("--- " ++ show (as, arrayElems as)) $
  case arrayElems as of
    []          -> pure $ Array []
    e:es        -> app (arr e) $ map arr es
  where arr (EElems es) = Array es
        arr (ESplice e) = e
        app r [] = pure r
        app r (e : es) = do
          t <- newIdent noLoc "t"
          rest <- app (Variable t) es
          pure $ seqE [DefineV t, eAppend r e (Variable t), rest]

eAppend :: Expr -> Expr -> Expr -> Expr
eAppend x y z = ApplyD (Variable (Ident noLoc "append$")) (Array [x, y, z])

data ArrayElem = EElems [Expr] | ESplice Expr
  deriving (Show)

-- Handle an array element, it can be ..e or e
arrayElems :: [Expr] -> [ArrayElem]
arrayElems = grp . map cls
  where cls (PrefixOp (Ident _ "..") e) = Left e
        cls e = Right e
        grp [] = []
        grp (Left e : as) = ESplice e : grp as
        grp as =
          let (rs, bs) = span isRight as
          in  EElems [ e | Right e <- rs ] : grp bs

---------------------------------------------------------------------------------

-- All cases, but the last, can be removed.
-- They are just there to avoid introducing unused existentials.
dsD :: Expr -> D Expr
dsD e | isValue e = pure e
dsD e@(ApplyD f a) | isValue f && isValue a = pure e
dsD e@(HasType f a) | isValue f && isValue a = pure e
dsD (Unify x e) | isValue x = Unify x <$> dsD e
dsD (DefineV x) = pure (DefineV x)
dsD (DefineE x e) = DefineE x <$> dsD e
dsD (For2 e1 e2) = For2 <$> dsD e1 <*> dsD e2
dsD (If3 e1 e2 e3) = If3 <$> dsD e1 <*> dsD e2 <*> dsD e3
dsD (Macro1 m rs e) = Macro1 m rs <$> dsD e
dsD (Array ts) = Array <$> mapM dsD ts
dsD (Seq []) = pure (Array [])
dsD (Seq [t]) = dsD t
dsD (Seq (t:ts)) = seqE <$> sequence [dsD t, dsD (Seq ts)]
dsD e = do
  x <- newIdent (getLoc e) "i"
  existsV [x] <$> dsM x e

dsM :: Ident -> Expr -> D Expr
-- Rule:  i |> k       -->  i = k
dsM i k | isLiteral k = pure $ unifyV i k
-- Rule:  i |> x       -->  i = x
dsM i x@Variable{} = pure $ unifyV i x
-- Rule:  i |> f[x]    -->  i = f[x]
--dsM i fa@(ApplyD f a) | isValue f && isValue a = pure $ unifyV i fa
--dsM i e@(ApplyD f a) | isValue f && isValue a = pure $ unifyV i e
--                     | otherwise = undefined -- invariant broken
dsM i (ApplyD f a) = unifyV i <$> (ApplyD <$> dsD f <*> dsD a)
-- Rule:  i |> x = t   -->  x = (i |> t)
dsM i (Unify x t) | isValue x = Unify x <$> dsM i t
-- Rule:  i |> x:any  --> x := i
dsM i (DefineV x) = pure $ DefineE x (Variable i)
-- Rule:  i |> x := t  -->  x := (i |> t)
dsM i (DefineE x t) = DefineE x <$> dsM i t
-- Rule:  i |> (j->x) := t  -->  j := i; x := (i |> t)
dsM i (DefineIE j x t) = do
  t' <- dsM i t
  pure $ seqE [DefineE j (Variable i), DefineE x t']
-- Rule:  i |> :t      -->  D(t)[i]
dsM i (Range t) = ApplyD <$> dsD t <*> pure (Variable i)
-- Rule:  i |> t1; t2  -->  D(t1); i |> t2
dsM i (Seq []) = dsM i (Array [])
dsM i (Seq [t]) = dsM i t
dsM i (Seq (t:ts)) = seqE <$> sequence [dsD t, dsM i (Seq ts)]
-- Rule:  i |> t1 | t2 -->  (i |> t1) | (i |> t2)
dsM i (Choice t1 t2) = Choice <$> dsM i t1 <*> dsM i t2
-- XXX verify

-- Rule:  i |> (t1,...,tn)  -->  exists x1 ... xn . x1 |> t1; ...; xn |> tn; i = (x1,...,xn)
dsM i (Array ts) = do
  xs <- mapM (\ t -> newIdent (getLoc t) "x") ts
  bs <- zipWithM dsM xs ts
  pure $ existsV xs $ seqE $ bs ++ [unifyV i $ Array $ map Variable xs]

--dsM i (Array ts) = (unifyV i . Array) <$> mapM dsD ts
dsM i (If3 e1 e2 e3) = If3 <$> dsD e1 <*> dsM i e2 <*> dsM i e3
dsM i (For2 e1 e2) = unifyV i <$> (For2 <$> dsD e1 <*> dsD e2)
dsM i (Function [(t1, r)] t2) = do
  c <- gets context
  dsFunction c i t1 r t2
dsM i af@(HasType a f) | isValue f && isValue a = pure $ unifyV i af
dsM i (Macro1 m rs e) = unifyV i . Macro1 m rs <$> dsD e  -- XXX
dsM _ e = impossible e

dsFunction :: DContext -> Ident -> Expr -> [Eff] -> Expr -> D Expr
dsFunction DEval i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  t1' <- withContext DAbstract $ dsM x t1
  (t2', mt3) <-
    case t2 of
      HasType e t -> (,) <$> dsD e  <*> (Just <$> dsD t)
      _           -> (,) <$> dsD t2 <*> pure Nothing
  pure $ unifyV i $  -- Do the unification?
         TLam x effs t1' t2' mt3
dsFunction DAbstract i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  y <- newIdent (getLoc t1) "y"
  z <- newIdent (getLoc t1) "z"
  t1' <- dsM x t1
  (t2', mt3) <- withContext DEval $
    case t2 of
      HasType e t -> (,) <$> dsM y e  <*> (Just <$> dsD t)
      _           -> (,) <$> dsM y t2 <*> pure Nothing
  pure $ TLam x effs (DefineE z t1') (Seq [DefineE y (ApplyD (Variable i) (Variable z)), t2']) mt3

unifyV :: Ident -> Expr -> Expr
unifyV i e = Unify (Variable i) e

existsV :: [Ident] -> Expr -> Expr
existsV is e = --seqE $ map (\ i -> Define i AnyT) is ++ [e]
               Exists is e

-- Pick the appropriate form of apply for operators
call :: String -> Loc -> String -> Expr -> D Expr
call p l s e = do
  ver <- gets (fVerify . dflags)
  let
    -- For verification, use ApplyS.  At runtime, skip the test.
    con | ver && s' `elem` [
                     "pre'+'","pre'-'",
                     "in'+'","in'-'","in'*'"] = ApplyS
        | s' `elem` ["in'/'","pre'!'","post'?'",
                     "pre'^'", "pre'[]'", "post'^'",  -- no need for succeeds
                     "pre'+'","pre'-'",  -- XXX not really right
                     "in'+'","in'-'","in'*'",  -- XXX not really right
                     "in'+='", "in'-='", "in'*='", "in'/='", "in'.='",
                     "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='",
                     "length","in'..'"] = ApplyD
        | otherwise = ApplyS
    s' = p ++ "'" ++ s ++ "'"
  pure $ con (Variable (Ident l s')) e

----------------------------------------------

dsScope :: Flags -> Expr -> Expr
dsScope flgs = eval flgs . (lower <=< addScope)

addScope :: Expr -> D Expr
addScope e = scope (S.fromList $ prel ++ primOps) (Do e)
 where prel = if Ident noLoc "PRELUDE" `elem` getVisible e then [] else preludeIds

_knownEffects :: [Ident]
_knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts", "covariant"
  ]

_isLambdaEffect :: Ident -> Bool
_isLambdaEffect i = elem i [
  covariantId
  ]

covariantId :: Ident
covariantId = Ident noLoc "covariant"

errUndefined :: [Ident] -> D ()
errUndefined =
  mapM_ (\ i@(Ident l _) -> traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i))

errShadow :: [Ident] -> D ()
errShadow =
  mapM_ (\ i@(Ident l _) -> traceM $ "scopeCheck: warning shadowing " ++ prettyShow (l, i))

errMultiple :: [[Ident]] -> D ()
errMultiple =
  mapM_ (\ is -> error $ "scopeCheck: Multiply defined " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

scope :: S.Set Ident -> Expr -> D Expr
scope sc = expr
  where
    expr e@LitInt{} = pure e
    expr e@LitRat{} = pure e
    expr e@(Variable i) | i `S.member` sc = pure e
                        | otherwise = do errUndefined [i]; pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
{-
    expr (ApplyEff is e) = do
      errUndefined (is \\ knownEffects)
      ApplyEff is <$> expr e
-}
    expr (If3 e1 e2 e3) = do
      (e1', sc') <- defs sc e1
      If3 e1' <$> scopeD sc' e2 <*> exprD e3
    expr (For2 e1 e2) = do
      (e1', sc') <- defs sc e1
      For2 e1' <$> scopeD sc' e2
    expr (Do e) = exprD e
    expr (Let e1 e2) = do
      (e1', sc') <- defs sc e1
      e2' <- scopeD sc' e2      
      pure $ seqE [e1', e2']
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (DefineV i) = pure $ Variable i
    expr (DefineE i e) = Unify (Variable i) <$> expr e
    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr (Macro1 m [] e1) = Macro1 m [] <$> exprD e1
    expr Macro1 {} = unimplemented "Macro1 with effects"
    expr (HasType e1 e2) = HasType <$> expr e1 <*> expr e2
    expr (TLam i r e1 e2 me3) = do
      (e1', sc') <- defs (S.insert i sc) e1
      TLam i r e1' <$> scopeD sc' e2 <*> traverse expr me3
    expr (Exists _ e) = expr e
    expr (Lam i e) = Lam i <$> scopeD (S.insert i sc) e
    expr e = impossible e

    exprD e = fst <$> defs sc e
    scopeD s e = fst <$> defs s e

    defs :: S.Set Ident -> Expr -> D (Expr, S.Set Ident)
    defs as e = do
      let is = getVisible e
          errM = filter ((> 1) . length) $ group $ sort is
          errS = [ i | i <- is, i `S.member` sc ]
          s' = foldr S.insert as is
      e' <- scope s' e
      errMultiple errM
      errShadow errS
      pure (Exists is e', s')


-- Get all visible identifiers from i := e
getVisible :: HasCallStack => Expr -> [Ident]
getVisible LitInt{} = []
getVisible LitRat{} = []
getVisible Variable{} = []
getVisible (Array es) = concatMap getVisible es
getVisible (Seq es) = concatMap getVisible es
getVisible (ApplyS e1 e2) = getVisible e1 ++ getVisible e2
getVisible (ApplyD e1 e2) = getVisible e1 ++ getVisible e2
getVisible (ApplyEff _ _e) = [] -- getVisible e
getVisible If3{} = []
getVisible For2{} = []
getVisible (Let _ e) = getVisible e
getVisible Do{} = []
getVisible (Unify e1 e2) = getVisible e1 ++ getVisible e2
--getVisible (Typedef _) = []
getVisible Macro1 {} = []
getVisible (DefineV i) = [i]
getVisible (DefineE i e) = i : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible Function{} = []
getVisible (Exists is e) = is ++ getVisible e
getVisible (HasType e1 e2) = getVisible e1 ++ getVisible e2
getVisible TLam{} = []
getVisible Lam{} = []
getVisible e = impossible e

getVar :: HasCallStack => Expr -> [Ident]
getVar LitInt{} = []
getVar LitRat{} = []
getVar Variable{} = []
getVar (Array es) = concatMap getVar es
getVar (Seq es) = concatMap getVar es
getVar (ApplyS e1 e2) = getVar e1 ++ getVar e2
getVar (ApplyD e1 e2) = getVar e1 ++ getVar e2
getVar (ApplyEff _ _e) = [] -- getVar e
getVar If3{} = []
getVar For2{} = []
getVar (Let _ e) = getVar e
getVar Do{} = []
getVar (Unify e1 e2) = getVar e1 ++ getVar e2
getVar Macro1 {} = []
getVar (DefineV _) = []
getVar (DefineE _ e) = getVar e
getVar (DefineIE _ _ e) = getVar e
getVar Choice{} = []
getVar (Set _ _ e) = getVar e
getVar (MVar i t e) = i : maybe [] getVar t ++ maybe [] getVar e
getVar (Range e) = getVar e
getVar Function{} = []
getVar TLam{} = []
getVar (Exists _ e) = getVar e
getVar (HasType e t) = getVar e ++ getVar t
getVar Lam{} = []
getVar e = impossible e

-- Definitions that should go in a Prelude
preludeIds :: [Ident]
preludeIds = map (Ident noLoc . fst) preludeFuncs

-- Primitives
primOps :: [Ident]
primOps = map (Ident noLoc)
  [ "isInt$", "isFlt$", "isStr$", "isPtr$", "isArr$", "isFcn$"
  , "in'+'", "in'-'", "in'*'", "in'/'"
  , "in'<'", "in'<='", "in'>'", "in'>='"
  , "in'<>'"
  , "pre'-'"
  , "pre'+'"
  , "post'?'"
  , "concat$", "takeL$", "dropL$", "takeR$", "dropR$", "cons$"
  , "length"
  , "known$"  -- This is a horrible hack
  , "alloc$", "read$", "write$"
  , "in'..'"
  , "in'+='", "in'-='", "in'*='", "in'/='"
  , "print$"
  , "append$"
  ]

------------------------

simp :: Expr -> D Expr
simp = simpUnify {- <=< simpUnused-} <=< simpAny

-- Simplify any[e]  -->  e
simpAny :: Expr -> D Expr
simpAny = pure . f
  where f (ApplyD (Variable (Ident _ "any")) e) = f e
        f e = composOp f e

-- Simplify x = (e1; ...; en)  -->  e1; ...; x = en
simpUnify :: Expr -> D Expr
simpUnify = pure . f
  where f (Unify v (Seq (Snoc xs x))) | isValue v = Seq $ xs ++ [Unify v x]
        f e = composOp f e

-- XXX assumes no name shadowing
_simpUnused :: Expr -> D Expr
_simpUnused e = pure $ removeUnused unused e
  where unused = [ i | (i, [Uni]) <- M.toList $ findUses e, i `notElem` preludeIds, i `notElem` primOps ]

data Use = Uni | Other deriving (Show)

-- Find out how variables are used.
-- A variable can be used on either side of a unification, or somewhere else.
-- Existentials that are only used once in a unification, can be removed.
findUses :: Expr -> M.Map Ident [Use]
findUses = flip execState M.empty . f
  where f e@(Variable i) = do modify (M.insertWith (++) i [Other]); pure e
        f (Unify ei@(Variable i) e) = do modify (M.insertWith (++) i [Uni]); Unify ei <$> f e
        f e = compos f e

removeUnused :: [Ident] -> Expr -> Expr
removeUnused unused = f
  where f (Unify (Variable i) e) | i `elem` unused = f e
        f (Exists is e) = Exists (filter (`notElem` unused) is) (f e)
        f e = composOp f e

simplify :: Expr -> Expr
simplify e = e

-------------------------

addDeref :: Expr -> D Expr
addDeref = pure . exprD S.empty
  where
    expr _ e@LitInt{} = e
    expr _ e@LitRat{} = e
    expr s e@(Variable i) | i `S.member` s = applyPrimD "read$" e
                          | otherwise = e
    expr s (Array es) = Array $ map (expr s) es
    expr s (Seq es) = Seq $ map (expr s) es
    expr s (ApplyS e1 e2) = ApplyS (expr s e1) (expr s e2)
    expr s (ApplyD e1 e2) = ApplyD (expr s e1) (expr s e2)
    expr s (ApplyEff is e) = ApplyEff is (expr s e)
    expr s (If3 e1 e2 e3) = If3 (expr s' e1) (expr s' e2) (exprD s e3)
      where s' = defs s e1
    expr s (For2 e1 e2) = For2 (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Let e1 e2) = Let (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Do e) = Do (exprD s e)
    expr s (Function [(a,rs)] e2) = Function [(a, rs)] (exprD s' e2)
      where s' = defs s a
    expr s (Unify e1 e2) = Unify (expr s e1) (expr s e2)
    expr _ (DefineV i) = DefineV i
    expr s (DefineE i e) = DefineE i (expr s e)
    expr s (DefineIE i j e) = DefineIE i j (expr s e)
    expr s (Choice e1 e2) = Choice (exprD s e1) (exprD s e2)
    expr s (Set e1 (Ident l sop) e2) = set s e1 op (expr s e2)
      where op = Ident l ("in'" ++ sop ++ "'")
    expr s (MVar i (Just t) (Just e)) = DefineE i $ ApplyD (applyPrimD "new" (expr s t)) (expr s e)
    expr s (Range e1) = Range (expr s e1)
--    expr s (Typedef e1) = Typedef (exprD s e1)
    expr s (Macro1 m rs e1) = Macro1 m rs (exprD s e1)
    expr s (TLam i rs e1 e2 me3) = TLam i rs (expr s' e1) (expr s' e2) (expr s' <$> me3)
      where s' = defs s e1
    expr s (Exists is e) = Exists is (expr s e)
    expr s (HasType e t) = HasType (expr s e) (expr s t)
    expr _ e = impossible e

    exprD s e = expr (defs s e) e

    set s e1 (Ident l "in'='") e2 = set s e1 (Ident l "write$") e2
    set s e1@(Variable i) op@(Ident l _) e2
      | i `S.member` s = ApplyD (Variable op) $ Array [e1, e2]
      | otherwise = syntaxError l $ "set variable must be declared with var: " ++ prettyShow i
    set s (ApplyD e1@(Variable i) ei) (Ident l sop) e2
      | i `S.member` s = ApplyD (Variable (Ident l (sop++"[]"))) $ Array [e1, ei, e2]
      | otherwise = syntaxError l $ "set variable must be declared with var: " ++ prettyShow i
    set _ e1 _ _ = syntaxError (getLoc e1) $ "set LHS not valid: " ++ prettyShow e1

    defs :: S.Set Ident -> Expr -> S.Set Ident
    defs s e = S.union s (S.fromList (getVar e))

    applyPrimD s e = ApplyD (Variable (Ident noLoc s)) e

---------------------------------

-- Convert Big Core to Core
lower :: Expr -> D Expr
lower e@LitInt{} = pure e
lower e@LitRat{} = pure e
lower e@(Variable (Ident _ s)) = do
  mexp <- lowerPrimOp s
  pure $ fromMaybe e mexp
lower (Array es) = Array <$> mapM lower es
lower e@Wrong{} = pure e
lower (Seq es) = seqE <$> mapM lower es
lower (ApplyS e1 e2) = lowerSucceeds =<< lower (ApplyD e1 e2)
lower (ApplyD e1 e2) = ApplyD <$> lower e1 <*> lower e2
lower (ApplyEff _rs _e) = undefined
lower (Unify e1 e2) = Unify <$> lower e1 <*> lower e2
lower (Choice e1 e2) = Choice <$> lower e1 <*> lower e2
lower (For2 (Exists is e1) e2) = join $ lowerFor is <$> lower e1 <*> lower e2
lower (If3 (Exists is e1) e2 e3) = join $ lowerIf is <$> lower e1 <*> lower e2 <*> lower e3
lower (Macro1 (Ident _ "all") [] e) = lowerAll =<< lower e
lower (Macro1 (Ident _ "one") [] e) = lowerOne =<< lower e
lower (Macro1 (Ident _ "succeeds") [] e) = lowerSucceeds =<< lower e
lower (Macro1 (Ident _ "decides") [] e) = lowerDecides =<< lower e
lower (Macro1 (Ident _ "assume") [] e) = lowerAssume =<< lower e
lower (Exists is e) = lExists is <$> lower e
lower (TLam i rs (Exists is e1) e2 me3) = join $ lowerTLam i rs is <$> lower e1 <*> lower e2 <*> traverse lower me3
lower (HasType e t) = join $ lowerHasType <$> lower e <*> lower t
lower (Lam i e) = Lam i <$> lower e
lower e = impossible e

-- Lower a for loop
lowerFor :: [Ident] -> Expr -> Expr -> D Expr
lowerFor is e1 e2 = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerForSplit is e1 e2
   else
    lowerForAll is e1 e2

-- Lower for loop using split
-- TODO: special case 'for{e}'
lowerForSplit :: [Ident] -> Expr -> Expr -> D Expr
lowerForSplit vs e1 e2 = do
  let l = getLoc e1
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  h <- newIdent l "h"   -- h = ge, but passed to split to avoid recursion
  let fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
      e1' = lExists vs $ Seq [e1, evs]  -- domain + array of free variables
      be  = Split (eForce (Variable y)) fe (Variable h)
      fe = eThunk $ Array []
      ge = Lam x $ Lam y $ Lam h $ lExists fvs $ Seq
             [ Unify (Variable x) evs
             , ApplyD (EPrim "cons$") (Array [e2, be])
             ]
  pure $ Split e1' fe ge

-- Lower for loop using all
lowerForAll :: [Ident] -> Expr -> Expr -> D Expr
lowerForAll (i:is) (Unify (Variable i') e) (Variable i'') | i == i' && i == i'' =
  -- Simple special case: for{e} = all{e}
  pure $ eAll (lExists is e)
lowerForAll is e1 e2 = do
  vv <- newIdent (getLoc e1) "v"
  let ev = Variable vv
      ea = eAll $ lExists is $ Seq [e1, eThunk e2]
  pure $ Exists [vv] $ Seq [Unify ev ea, ApplyD (EPrim "mapAp$") ev]

lowerIf :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIf is e1 e2 e3 = do
  noLambdaIf <- gets (fNoLambdaIf . dflags)
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  if verif then
    lowerIfVerify is e1 e2 e3
   else if noLambdaIf then
    lowerIfNoLambda is e1 e2 e3
   else if useSplit then
    lowerIfSplit is e1 e2 e3
   else
    lowerIfOne is e1 e2 e3

lowerIfVerify :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIfVerify is e1 e2 e3 = pure $ If3 (Exists is e1) e2 e3

lowerIfNoLambda :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIfNoLambda vs e1 e2 e3 = do
  y <- newIdent (getLoc e1) "y"
  let vy = Variable y
      fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
  pure $ Exists [y] $ Seq
           [ Unify vy (eOne $ lExists vs (Seq [e1, evs])
                              `Choice`
                              LitInt 0)
           , lExists fvs (Seq [Unify vy evs, e2])
             `Choice`
             Seq [Unify vy (LitInt 0), e3]
           ]

-- TODO: special case 'if{}'
lowerIfSplit :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIfSplit vs e1 e2 e3 = do
  x <- newIdent (getLoc e1) "x"
  let fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
      e1' = lExists vs $ Seq [e1, evs]  -- domain + array of free variables      
      fe = eThunk e3
      ge = Lam x $ Lam underscore $ Lam underscore $ lExists fvs $ Seq
             [ Unify (Variable x) evs
             , e2
             ]
  pure $ Split e1' fe ge

lowerIfOne :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIfOne is e1 e2 e3 = do
  let e1e2 = lExists is $ Seq [e1, eThunk e2]
  pure $ eForce $ eOne $ Choice e1e2 (eThunk e3)

lowerTLam :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLam i rs is e1 e2 me3 = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerTLamVerify i rs is e1 e2 me3
   else
    lowerTLamRun i rs is e1 e2 me3

-- XXX what about _rs
lowerTLamVerify :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLamVerify i _rs is e1 e2 me3 = do
  (e2', e2'') <-
    case me3 of
      Nothing -> pure (e2, e2)
      Just t -> do
        x <- newIdent (getLoc t) "x"
        pure (ApplyD t e2, Exists [x] $ ApplyD t (Variable x))
  pure $ Seq
    [ eVerify $ Lam i $ lExists is $ Seq [eAssume e1, eAssert e2']
    ,           Lam i $ lExists is $ Seq [        e1, eAssume e2'']
    ]

-- XXX use all of rs
lowerTLamRun :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLamRun i rs is e1 e2 me3 = do
  e2' <- maybe (pure e2) (\ t -> lowerSucceeds (ApplyD t e2)) me3
  let covariant = covariantId `elem` rs  || True -- XXX
  if null is && e1 == Array [] then
    pure $ Lam i e2'   -- Simple special case
   else
    if covariant then
      pure $ Lam i $ lExists is (seqE [e1, e2])
    else
      Lam i <$> lowerIf is e1 e2 DomainFail

lowerHasType :: Expr -> Expr -> D Expr
lowerHasType e t = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerHasTypeVerify e t
   else
    lowerSucceeds (ApplyD t e)

lowerHasTypeVerify :: Expr -> Expr -> D Expr
lowerHasTypeVerify e t = do
  x <- newIdent (getLoc t) "x"
  pure $ Seq [ eVerify $ eAssert $ ApplyD t e, Exists [x] $ ApplyD t (Variable x) ]

lowerAll :: Expr -> D Expr
lowerAll e = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerAllSplit e
   else
    pure $ eAll e

lowerAllSplit :: Expr -> D Expr
lowerAllSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  h <- newIdent l "h"   -- h = ge, but passed to split to avoid recursion
  let xs = Split (eForce $ Variable y) fe (Variable h)
      fe = eThunk $ Array []
      ge = Lam x $ Lam y $ Lam h $
             ApplyD (EPrim "cons$") (Array [Variable x, xs])
  pure $ Split e fe ge

lowerOne :: Expr -> D Expr
lowerOne e = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerOneSplit e
   else
    pure $ eOne e

lowerOneSplit :: Expr -> D Expr
lowerOneSplit e = do
  v <- newIdent (getLoc e) "v"
  pure $ Split e (eThunk Fail) (Lam v $ Lam underscore $ Lam underscore $ Variable v)

lowerSucceeds :: Expr -> D Expr
lowerSucceeds e = do
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  if verif then
    pure $ eAssert e
   else if useSplit then
    lowerSucceedsSplit e
   else
    pure $ Macro1 (Ident noLoc "succeeds") [] e

lowerSucceedsSplit :: Expr -> D Expr
lowerSucceedsSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  pure $ Split e
               (eThunk $ Wrong "succeeds-fail") $
               Lam x $ Lam y $ Lam underscore $
                   Split (eForce (Variable y))
                         (eThunk (Variable x))
                         (Lam underscore $ Lam underscore $ Lam underscore $ Wrong "succeed-many")

lowerDecides :: Expr -> D Expr
lowerDecides e = do
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  if verif then
    unimplemented "verify-decides"
   else if useSplit then
    lowerDecidesSplit e
   else
    pure $ Macro1 (Ident noLoc "succeeds") [] e

lowerDecidesSplit :: Expr -> D Expr
lowerDecidesSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  pure $ Split e
               (eThunk Fail) $
               Lam x $ Lam y $ Lam underscore $
                   Split (eForce (Variable y))
                         (eThunk (Variable x))
                         (Lam underscore $ Lam underscore $ Lam underscore $ Wrong "decides-many")

lowerAssume :: Expr -> D Expr
lowerAssume e = pure $ eAssume e

-- Some "primops" will be expanded into code.
-- This should really be part of the prelude.
-- For verification, we need a different expansion.
lowerPrimOp :: String -> D (Maybe Expr)
lowerPrimOp s = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerPrimOpVerif s
   else
    lowerPrimOpRun s

lowerPrimOpVerif :: String -> D (Maybe Expr)
lowerPrimOpVerif s = do
  me <- lowerPrimOpRun s
  case me of
    Just (EPrim p) | Just ises <- lookup p verifyPrelude -> pure $ Just $ toLam ises
    r -> pure r
 where
    arg = Ident noLoc "arg"
    toLam (   [a],   es) = Lam a   $ Seq es
    toLam (as@[_,_], es) = Lam arg $ lExists as $ Seq $ Unify (Array $ map Variable as) (Variable arg) : es
    toLam _ = undefined  -- shouldn't happen

lowerPrimOpRun :: String -> D (Maybe Expr)
lowerPrimOpRun s =
  case lookup s preludeFuncs of
    r@Just{} -> pure r
    Nothing  ->
      if Ident noLoc s `elem` primOps then
        pure $ Just $ EPrim s
      else
        pure $ Nothing

preludeFuncs :: [(String, Expr)]
preludeFuncs =
  [("any", typ [])                                             -- x => x
  ,("nat", typ [app "isInt$" vx, app2 "in'>='" vx (LitInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt$" vx])                              -- x => int#[x]; x
  ,("in'->'", arrowV)
  ,("false", Array [])                                      -- ()
  ,("new", newV)
  ,("post'^'", EPrim "read$")
  ,("in'.='", EPrim "write$")
  ,("mapAp", EPrim "mapAp$")
  ]
  where typ es = Lam x $ seqE $ es ++ [Variable x]
        vx = Variable x
        app f v = ApplyD (EPrim f) v
        app2 f v1 v2 = ApplyD (EPrim f) (Array [v1, v2])

        arrowV =
          Lam st $
            Exists [s, t] $
            Seq [
              Unify (Array [Variable s, Variable t]) (Variable st),
              Lam g $ Lam y $
                Exists [sy, gsy] $
                Seq [
                  app "isFcn$" (Variable g),
                  Unify (Variable  sy) (ApplyD (Variable s) (Variable y)),
                  Unify (Variable gsy) (ApplyD (Variable g) (Variable sy)),
                  ApplyD (Variable t) (Variable gsy)
                  ]
              ]
        [st, s, t, g, y, sy, gsy, x, _xy] =
           map (Ident noLoc . ("$$" ++)) ["st","s","t","g","y","sy","gsy","x", "xy"]

        newV =
          Lam t $ Lam x $
            Exists [y] $
            Seq [
              Unify (Variable y) (ApplyD (Variable t) (Variable x)),
              app "alloc$" (Variable y)
              ]

verifyPrelude :: [(String, ([Ident], [Expr]))]
verifyPrelude =
  [ arithBinOpInt  "in'+'"
  , arithBinOpInt  "in'-'"
  , arithBinOpInt  "in'*'"
  , arithBinOpIntC "in'/'" yNe0
  , arithUnOpInt   "pre'-'"
  , arithUnOpInt   "pre'+'"
  , cmpBinOpInt    "in'<'"
  , cmpBinOpInt    "in'<='"
  , cmpBinOpInt    "in'>'"
  , cmpBinOpInt    "in'>='"
  , cmpBinOpInt    "in'<>'"
  ]
  where
    arithBinOpInt  p = (p, arithBinOpInt' [] p)
    arithBinOpIntC p c = (p, arithBinOpInt' [c] p)
    arithBinOpInt' c p = ([x, y],
      [ cInt vx, cInt vy] ++ c ++
      [ eAssume $ Exists [z] $ Seq [Unify vz (ApplyD (EPrim p) (Array [vx, vy])), cInt vz, vz] ])

    cmpBinOpInt  p = (p, cmpBinOpInt' p)
    cmpBinOpInt' p = ([x, y], 
      [ cInt vx, cInt vy, ApplyD (EPrim p) (Array [vx, vy]), eAssume (Seq [cInt vx, vx]) ])

    yNe0 = ApplyD (EPrim "in'<>'") (Array [vy, LitInt 0])

    arithUnOpInt p =
      (p, ([x], [ cInt vx, eAssume (Exists [z] $ Seq [Unify vz (ApplyD (EPrim p) vx), cInt vz, vz]) ]))

    cInt e = ApplyD (EPrim "isInt$") e
    x = Ident noLoc "$$x"
    y = Ident noLoc "$$y"
    z = Ident noLoc "$$z"
    vx = Variable x
    vy = Variable y
    vz = Variable z

-- After lowering there are no funny scopes, so empty existential
-- are no longer necessary.
lExists :: [Ident] -> Expr -> Expr
lExists [] e = e
lExists is e = Exists is e

underscore :: Ident
underscore = Ident noLoc "_"

eThunk :: Expr -> Expr
eThunk = Lam (Ident noLoc "_")

eForce :: Expr -> Expr
eForce e = ApplyD e (Array [])

eAll :: Expr -> Expr
eAll = Macro1 (Ident noLoc "all") []

eOne :: Expr -> Expr
eOne = Macro1 (Ident noLoc "one") []

eAssert :: Expr -> Expr
eAssert = Macro1 (Ident noLoc "assert") []

eAssume :: Expr -> Expr
eAssume = Macro1 (Ident noLoc "assume") []

eVerify :: Expr -> Expr
eVerify = Macro1 (Ident noLoc "verify") []

-- Used to create the array of free variables passed from the domain to the range
-- of for/if.  If it's just a single variable, don't use an array.
fvArray :: [Expr] -> Expr
fvArray [e] = e
fvArray es = Array es

-----------

-- TODO? Add checks
exprToCore :: Flags -> Expr -> Core
exprToCore _ = id

simpCore :: Core -> Core
simpCore = id

----------------------

-- Functions that only work on the core subset of Expr
getFree :: Core -> [Ident]
getFree = Epic.List.nub . fvs 
  where
    fvs (Variable i) = [i]
    fvs (LitInt _) = []
    fvs (LitRat _ _) = []
    fvs (EPrim _) = []
    fvs (Array es) = concatMap fvs es
    fvs (Lam i e) = filter (/= i) (fvs e)
    fvs (Unify e1 e2) = fvs e1 ++ fvs e2
    fvs (ApplyD e1 e2) = fvs e1 ++ fvs e2
    fvs (Seq es) = concatMap fvs es
    fvs (Choice e1 e2) = fvs e1 ++ fvs e2
    fvs (Exists is e) = filter (`notElem` is) (fvs e)
    fvs (Wrong _) = []
    fvs (Macro1 _ _ e) = fvs e
    fvs (Split e1 e2 e3) = fvs e1 ++ fvs e2 ++ fvs e3
    fvs e = error $ "getFree: " ++ prettyShow e
