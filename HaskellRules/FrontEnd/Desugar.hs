{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
module FrontEnd.Desugar(
  desugar,
  dsScope,
  getFree, substMany, getAllVars,
  ) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Either
import Data.List
--import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack
import Epic.List
import Epic.Print
import Epic.Uniplate(universeBi, transform)
--import FrontEnd.Desugar
import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags

-- QUESTIONS:
--  x:int='a'   fail or wrong?, tests L93, L95

-- TODO:
--  Add Length
--  Add Err

-- TODO:
--  x:t=v is syntactic sugar for x:=(:t=v) and
--  :t=v is a special form meaning it's not the same as (:t)=v, which is just unification.
--  desugar function effects
  
desugar :: Flags -> Expr -> Expr
--desugar flgs | trace ("desugar: " ++ show flgs) False = undefined
desugar flgs = eval flgs .
            (-- simplification
             traceDS "simpler"    <=< simpler   <=<  -- verifier breaks without this
             traceDS "simplify"   <=< simplify  <=<
             -- desugaring
             traceDS "primops"    <=< primops   <=<
             traceDS "lower"      <=< lower     <=<
             traceDS "addScope"   <=< addScope  <=<
             traceDS "lowerApply" <=< lowerApply<=<
             traceDS "dsD"        <=< dsDx      <=<
             traceDS "addDeref"   <=< addDeref  <=<
             traceDS "dsSmall"    <=< dsSmall   <=<
             traceDS "addPrelude" <=< addPrelude <=<
             traceDS "syntaxFixes" <=< syntaxFixes)
  where
{-
    hack = (traceDS "dsD"        <=< dsD       <=<
            traceDS "addDeref"   <=< addDeref  <=<
            traceDS "dsSmall"    <=< dsSmall   <=<
            traceDS "syntaxFixes" <=< syntaxFixes)
-}
    tr = fTraceDesugar flgs
    traceDS :: String -> Expr -> D Expr
    traceDS msg e | tr = trace ("---- " ++ msg ++ "\n" ++ prettyShow e) $
                         pure e
                  | otherwise = pure e
    addPrelude e = pure $ addUsed prel e
    prel = spl $ eval flgs $ syntaxFixes $ snd $ fPrelude flgs

    -- Split the prelude into an association list
    spl (Array ds) = map (\ e -> (nameOf e, e)) ds
    spl e = impossible e

    -- Find the name of a definition
    nameOf (InfixOp lhs (Ident _ ":=") _) = lhsName lhs
    nameOf e = impossible e
    lhsName :: Expr -> Ident
    lhsName (EffAttr e _) = lhsName e
    lhsName (ApplyS e _) = lhsName e
    lhsName (Variable i) = i
    lhsName e = impossible e

-- Hackily add prelude identifiers used in e
addUsed :: [(Ident, Expr)] -> Expr -> Expr
addUsed prel = loop []
  where
    loop vs e =
      let is = allIdents e
          ps = filter (\ (i, _) -> i `elem` is && i `notElem` vs) prel
      in  case ps of
            [] -> e
            ies -> loop (map fst ies ++ vs) $ seqE (map snd ies ++ [e])

allIdents :: Expr -> [Ident]
allIdents = universeBi . transform fixops
  where
    -- Fix operator names
    fixops (InfixOp e1 (Ident l s) e2) = InfixOp e1 (Ident l ("in'" ++ s ++ "'")) e2
    fixops (PrefixOp (Ident l s) e2) = PrefixOp (Ident l ("pre'" ++ s ++ "'")) e2
    fixops (PostfixOp e1 (Ident l s)) = PostfixOp e1 (Ident l ("post'" ++ s ++ "'"))
    -- If there's an MVar, we need "new"
    fixops e@MVar{} = ApplyS (Variable (Ident noLoc "new")) e
    fixops e = e

------

type D = State DState

data DState = DState { nextNo :: !Int, dflags :: !Flags }
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

{-
withContext :: DContext -> D a -> D a
withContext c da = do
  oldc <- gets context
  modify $ \ ds->ds{ context = c }
  a <- da
  modify $ \ ds->ds{ context = oldc }
  pure a
-}

---------------------

-- Do various early changes:
--  * (e)       -->  e             parens are there to stop the next from possibly firing
--  * e1:e2=e3  -->  e1:e2 := e3   XXX should we do this?
--  * (e1,...)  -->  array{e1,...} no need to distingush them anymore
--  * x&y:e     -->  array{x&y:e}  if outside an array
--                   x:e; y:e      if inside an array
syntaxFixes :: Expr -> D Expr
syntaxFixes = pure . f
  where f :: Expr -> Expr
        f (Parens e) = f e
        f (InfixOp (InfixOp (Variable i1) o@(Op ":") e2) (Ident l3  "=") e3) =
          f $ InfixOp (InfixOp (Variable i1) o e2) (Ident l3 ":=") e3
        f (Tuple es) = f (Array es)
        f (Array es) = Array $ concatMap g es
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":" ) _) = f (Array [e])  -- PAMP1
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":=") _) = f (Array [e])  -- PAMP1
        f e = composOp f e

        -- PAMP2
        g :: Expr -> [Expr]
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":" ) rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":=") rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g e = [f e]

---------------------

eval :: Flags -> D Expr -> Expr
eval flgs = flip evalState DState{ nextNo = 1, dflags = flgs }

-- Desugar into Small Source Verse
dsSmall :: Expr -> D Expr
dsSmall = ds
  where
    ds :: Expr -> D Expr
    -- Application and unification
    ds (InfixOp e1 (Op "where") e2) = do
      x <- newIdent (getLoc e1) "x"
      ds $ seqE [DefineE x e1, e2, Variable x]
    ds (ApplyS  e1 e2) = join (apply applyS <$> ds e1 <*> ds e2)
      where applyS x y = Succeeds (ApplyD x y)

{-
    -- n-ary unification
    ds (InfixOp e1 (Op "=") e2) = do e1' <- ds e1; e2' <- ds e2; dsU [e1', e2']
    ds (Macro1 (Ident _ "in'='") [] (Blk es)) = dsU =<< mapM ds es
-}
    ds (InfixOp e1 (Op "=") e2) = Unify <$> ds e1 <*> ds e2

    ds (ApplyD  e1 e2) = join (apply ApplyD <$> ds e1 <*> ds e2)

    -- Bindings
    ds (InfixOp e1 o@(Op ":")  e2) = ds =<< defn e1 (PrefixOp o e2)  -- PCOLONT
    ds (InfixOp e1   (Op ":=") e2) = ds =<< defn e1 e2

    -- Function notation
    ds (Typedef e) = do x <- newIdent (getLoc e) "x"; ds $ Function [(DefineE x e, [invariantId])] (Variable x)
    ds (InfixOp e1 (Op "=>") e2) = ds $ Function [(e1, [invariantId])] e2
    ds (Function (a:as@(_:_)) b) = ds $ Function [a] $ Function as b
-- not yet
--  ds (Function [(e, ps@(_:_))] b) = ds $ Function [(e, [])] $ Check ps b
    ds (Function [(e1, effs)] e2) = do
           e1' <- ds e1
           e2' <- ds e2
           effs' <- checkEffs effs
           pure $ Function [(e1', effs')] e2'

    -- Conditional and for-loop notation
    ds (If1 e) = ds $ If2E e eFalse
    ds (If2 e1 e2) = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2) = do x <- newIdent (getLoc e1) "x"; ds $ If3 (DefineE x e1) (Variable x) e2
    ds (For1 e) = do x <- newIdent (getLoc e) "x"; ds $ For2 (DefineE x e) (Variable x)

    -- Operators
    ds (PrefixOp (Op "not") e) = do e' <- ds e; pure $ If3 e' Fail eFalse
    ds (PrefixOp (Op ":") e) = Range <$> ds e
    ds (PrefixOp (Ident l op) e) = ds =<< call "pre" l op e
    ds (PostfixOp e (Op "?")) = Range <$> ds e
    ds (PostfixOp e (Ident l op)) = ds =<< call "post" l op e
    ds (InfixOp e1 (Op "|") e2) = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2) = ds $ Seq [e1, e2]                  -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 Fail) Fail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2) = ds $ If2E e1 $ If2E e2 Fail
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
    ds (Blk es) = ds $ seqE es

    ds (Seq es) = seqE <$> mapM ds es
    ds (OfType e1 e2) = OfType <$> ds e1 <*> ds e2

    -- Misc
    ds (Variable (Ident l "_")) = DefineV <$> newIdent l "u"
    ds (Option Nothing) = pure eFalse
    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (DefineE t e) (Array [Variable t])

    -- one, all
    -- XXX why do we do this?
    ds (Macro1 (Ident _ "one") [] e) = ds $ If2E e Fail
    ds (Macro1 (Ident _ "all") [] e) = ds $ For1 e

    ds (Macro1 (Ident _ "first") [] e) = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    ds (Exists xs b) = ds $ foldr (\ v e -> seqE [DefineV v, e]) b xs

    ds x = compos ds x

{- no n-ary unification for now
    dsU [] = pure $ Range $ Variable $ Ident noLoc "any$"
    dsU [e] = pure e
    dsU ees@(e:es) = do
      let findVar _ []= Nothing
          findVar xs (y@(Variable _) : ys) = Just (y, xs ++ ys)
          findVar xs (y:ys) = findVar (xs ++ [y]) ys
      case findVar [] ees of
        Nothing -> do
          x <- newIdent (getLoc e) "x"
          pure $ Seq $ DefineE x e : map (Unify (Variable x)) es
        Just (x, xs) -> pure $ Seq $ map (Unify x) xs
-}

checkEffs :: [Eff] -> D [Eff]
checkEffs = mapM checkEff
  where checkEff (Ident _ "invariant") = pure invariantId
        checkEff e | e `elem` knownEffects = pure e
                   | otherwise = errorMessage $ "unknown effect: " ++ show e

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
defn (Variable (Ident _ "_")) e = do
  x <- newIdent (getLoc e) "u"
  pure $ DefineE x e
defn (Variable i) e = pure $ DefineE i e
-- Rule: (f(a) := e)  -->  (f := function(a){e})
-- Rule: (p<a> := e)  -->  ...
defn p e | Just (f, a, rs) <- getFun p = defn f (Function [(a, rs)] e)
-- Rule: (e1:e2 := e)  -->  (e1 := hasType(e2){e})
defn (InfixOp e1 (Op ":") e2) e = defn e1 (OfType e e2)
-- Rule: (:e2) := e  -->  (x:e2) := e, x fresh
defn (PrefixOp op@(Op ":") e2) e = do
  u <- newIdent (getLoc e2) "u"
  defn (InfixOp (Variable u) op e2) e
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
defn p _ = errorMessage $ "Bad LHS to := " ++ prettyShow p
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

eAny :: Expr
eAny = Variable (Ident noLoc "any$")

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
          pure $ seqE [eAppend r e t, rest]

eAppend :: Expr -> Expr -> Ident -> Expr
eAppend (Array xs) (Array ys) z = DefineE z (Array (xs ++ ys))
eAppend x y z = Seq [DefineV z, ApplyD (Variable (Ident noLoc "append$")) (Array [x, y, Variable z])]

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

context :: DState -> Desugar
context = fDesugar . dflags

dsDx :: Expr -> D Expr
dsDx e = do
  how <- gets context
  case how of
    DS1 -> dsD_1 e
    DS2 -> dsD_2 e
    DS3 -> dsD_3 e
    DS5 -> dsD_5 e
    DS6 -> dsD_6 e
    DS7 -> dsD_7 e

dsD_1 :: Expr -> D Expr
dsD_1 e | isValue e = pure e  -- DCONST DVAR
dsD_1 (Choice e1 e2) = Choice <$> dsD_1 e1 <*> dsD_1 e2
dsD_1 (ApplyD e1 e2) = ApplyD <$> dsD_1 e1 <*> dsD_1 e2
dsD_1 (Unify e1 e2) = Unify <$> dsD_1 e1 <*> dsD_1 e2
dsD_1 (DefineV x) = pure (DefineV x)
dsD_1 (DefineE x e) = DefineE x <$> dsD_1 e
dsD_1 (For2 e1 e2) = For2 <$> dsD_1 e1 <*> dsD_1 e2
dsD_1 (If3 e1 e2 e3) = If3 <$> dsD_1 e1 <*> dsD_1 e2 <*> dsD_1 e3
dsD_1 (Macro1 m rs e) = Macro1 m rs <$> dsD_1 e
dsD_1 (Array ts) = Array <$> mapM dsD_1 ts
dsD_1 (Seq []) = pure (Array [])
dsD_1 (Seq [t]) = dsD_1 t
dsD_1 (Seq (t:ts)) = seqE <$> sequence [dsD_1 t, dsD_1 (Seq ts)]
dsD_1 (OfType e t) = OfType <$> dsD_1 e <*> dsD_1 t
dsD_1 Fail = pure Fail
dsD_1 (Function [(t1, effs)] t2) = do
  x <- newIdent (getLoc t1) "x"
  t1' <- dsM_1 x t1
  t2' <- dsD_1 t2
  pure $ TLam x effs t1' t2'
dsD_1 e@Range{} = dsDM e
dsD_1 e@DefineIE{} = dsDM e
dsD_1 (Lam x e) = Lam x <$> dsD_1 e
dsD_1 e = impossible e

-- Use M to desugar
dsDM :: Expr -> D Expr
dsDM e = do
  x <- newIdent (getLoc e) "i"
  existsV [x] <$> dsM_1 x e

dsM_1 :: Ident -> Expr -> D Expr
-- Rule:  i |> k       -->  i = k
dsM_1 i k | isLiteral k = pure $ unifyV i k
-- Rule:  i |> x       -->  i = x
dsM_1 i x@Variable{} = pure $ unifyV i x
-- Rule:  i |> f[x]    -->  i = f[x]
--dsM_1 i fa@(ApplyD f a) | isValue f && isValue a = pure $ unifyV i fa
--dsM_1 i e@(ApplyD f a) | isValue f && isValue a = pure $ unifyV i e
--                     | otherwise = undefined -- invariant broken
dsM_1 i (ApplyD f a) = unifyV i <$> (ApplyD <$> dsD_1 f <*> dsD_1 a)
-- Rule:  i |> x = t   -->  x = (i |> t)
dsM_1 i (Unify t1 t2) = Unify <$> dsM_1 i t1 <*> dsM_1 i t2
-- Rule:  i |> x:any  --> x := i
dsM_1 i (DefineV x) = pure $ DefineE x (Variable i)
-- Rule:  i |> x := t  -->  x := (i |> t)
dsM_1 i (DefineE x t) = DefineE x <$> dsM_1 i t
-- Rule:  i |> (j->x) := t  -->  j := i; x := (i |> t)
dsM_1 i (DefineIE j x t) = do
  t' <- dsM_1 i t
  pure $ seqE [DefineE j (Variable i), DefineE x t']
-- Rule:  i |> :t      -->  D(t)[i]
dsM_1 i (Range t) = ApplyD <$> dsD_1 t <*> pure (Variable i)
-- Rule:  i |> t1; t2  -->  D(t1); i |> t2
dsM_1 i (Seq []) = dsM_1 i (Array [])
dsM_1 i (Seq [t]) = dsM_1 i t
dsM_1 i (Seq (t:ts)) = seqE <$> sequence [dsD_1 t, dsM_1 i (Seq ts)]
-- Rule:  i |> t1 | t2 -->  (i |> t1) | (i |> t2)
dsM_1 i (Choice t1 t2) = Choice <$> dsM_1 i t1 <*> dsM_1 i t2
-- Rule:  i |> (t1,...,tn)  -->  exists x1 ... xn . x1 |> t1; ...; xn |> tn; i = (x1,...,xn)
dsM_1 i (Array ts) = do
  xs <- mapM (\ t -> newIdent (getLoc t) "x") ts
  bs <- zipWithM dsM_1 xs ts
  pure $ existsV xs $ seqE [ unifyV i $ Array $ map Variable xs, Array bs]

dsM_1 i (If3 e1 e2 e3) = If3 <$> dsD_1 e1 <*> dsM_1 i e2 <*> dsM_1 i e3
dsM_1 i (For2 e1 e2) = unifyV i <$> (For2 <$> dsD_1 e1 <*> dsD_1 e2)

dsM_1 i (Function [(t1, effs)] t2) = do
  x <- newIdent (getLoc t1) "x"
  y <- newIdent (getLoc t1) "y"
  z <- newIdent (getLoc t1) "z"
  t1' <- dsM_1 x t1
  t2' <- dsM_1 y t2
  pure $ TLam x effs (DefineE z t1') (Seq [DefineE y (ApplyD (Variable i) (Variable z)), t2'])

dsM_1 i (OfType a f) = OfType <$> dsM_1 i a <*> dsD_1 f
dsM_1 i (Macro1 m rs e) = Macro1 m rs <$> dsM_1 i e  -- XXX really?
dsM_1 i Fail = pure $ unifyV i Fail
dsM_1 i (Lam x e) = unifyV i . Lam x <$> dsD_1 e
dsM_1 i (Let e1 e2) = Let <$> dsD_1 e1 <*> dsM_1 i e2
dsM_1 _ e = impossible e

{-
dsFunction :: DContext -> Ident -> Expr -> [Eff] -> Expr -> D Expr
-- This is highly dubious
dsFunction DEval i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  t1' <- withContext DAbstract $ dsM_1 x t1
  t2' <- dsD_1 t2
  pure $ unifyV i $  -- Do the unification?
         TLam x effs t1' t2'
dsFunction DAbstract i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  y <- newIdent (getLoc t1) "y"
  z <- newIdent (getLoc t1) "z"
  t1' <- dsM_1 x t1
  t2' <- dsM_1 y t2
  pure $ TLam x effs (DefineE z t1') (Seq [DefineE y (ApplyD (Variable i) (Variable z)), t2'])
-}

unifyV :: Ident -> Expr -> Expr
unifyV i e = Unify (Variable i) e

existsV :: [Ident] -> Expr -> Expr
existsV is e = --seqE $ map (\ i -> Define i AnyT) is ++ [e]
               Exists is e

-- Pick the appropriate form of apply for operators
call :: String -> Loc -> String -> Expr -> D Expr
call p l s e = do
  ver <- gets (not . fAssumeVerified . dflags)
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

knownEffects :: [Ident]
knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts", "transacts", "open"
  ] ++ [invariantId]

_isLambdaEffect :: Ident -> Bool
_isLambdaEffect i = elem i [
  invariantId
  ]

invariantId :: Ident
invariantId = Ident noLoc "closed"

openId :: Ident
openId = Ident noLoc "open"

errUndefined :: [Ident] -> D ()
errUndefined is = do
  flg <- gets dflags
  if fNoWarn flg then
    case is of
      [] -> pure ()
      i@(Ident l _) : _ -> errorMessage $ "undefined: " ++ prettyShow (l, i)
   else
    mapM_ (\ i@(Ident l _) -> traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i)) is

errShadow :: [(Ident, Ident)] -> D ()
errShadow is = do
  flg <- gets dflags
  if fNoWarn flg then
    case is of
      [] -> pure ()
      (i@(Ident li _), (Ident lj _)) : _ -> errorMessage $ "shadowing: " ++ prettyShow (li, i, lj)
   else
    mapM_ (\ (i@(Ident li _), (Ident lj _)) -> traceM $ "warning shadowing " ++ prettyShow (li, i, lj)) is

errMultiple :: [[Ident]] -> D ()
errMultiple =
  mapM_ (\ is -> errorMessage $ "multiply defined: " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

dsScope :: Flags -> Expr -> Expr
dsScope flgs = eval flgs . (primops <=< addScope)

addScope :: Expr -> D Expr
addScope e = scope (S.fromList primOps) (Block e)

scope :: S.Set Ident -> Expr -> D Expr
scope sc = expr
  where
    expr e@Lit{} = pure e
    expr e@(Variable i) | i `S.member` sc = pure e
                        | otherwise = do errUndefined [i]; pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    expr (If3 e1 e2 e3) = do
      (is, e1', sc') <- defs' sc e1
      If3B is e1' <$> scopeD' sc' e2 <*> exprD e3
    expr (For2 e1 e2) = do
      (is, e1', sc') <- defs' sc e1
      For2B is e1' <$> scopeD' sc' e2
    expr (Block e) = exprD e
    expr (Let e1 e2) = do
      (e1', sc') <- defs sc e1
      let Exists is e1'' = e1'
      e2' <- scope sc' e2
      pure $ Exists is $ seqE [e1'', e2']
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (DefineV i) = pure $ Variable i
    expr (DefineE i e) = Unify (Variable i) <$> expr e
    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr (Macro1 (Ident l "assume") [] e1) = Macro1 (Ident l "assume") [] <$> expr e1
    expr (Macro1 m@(Ident _ "lowered") [] e1) = Macro1 m [] <$> exprD' e1
    expr (Macro1 m [] e1) = Macro1 m [] <$> exprD e1
    expr Macro1 {} = unimplemented "Macro1 with effects"
    expr (OfType e1 e2) = OfType <$> exprD e1 <*> exprD e2
    expr (TLam i r e1 e2) = do
      (e1', sc') <- defs (S.insert i sc) e1
      TLam i r e1' <$> scopeD sc' e2
    expr (Exists _ e) = expr e
    expr (Lam i e) = Lam i <$> scopeD' (S.insert i sc) e
    expr Fail = pure Fail
    expr (Forall is e) = Forall is <$> scopeD' sc' e
      where sc' = foldr S.insert sc is
    expr e = impossible e

    exprD e = fst <$> defs sc e
    scopeD s e = fst <$> defs s e

    -- Like scopeD, but does not return a top level Exists []
    scopeD' s e = dropEmptyExists <$> scopeD s e
    exprD' e = dropEmptyExists <$> exprD e

    dropEmptyExists (Exists [] e) = e
    dropEmptyExists e = e

    defs :: S.Set Ident -> Expr -> D (Expr, S.Set Ident)
    defs as e = do
      (is, e', s) <- defs' as e
      pure (Exists is e', s)

    defs' :: S.Set Ident -> Expr -> D ([Ident], Expr, S.Set Ident)
    defs' as e = do
      let is = getVisible e
          errM = filter ((> 1) . length) $ group $ sort is
          errS = [ (i, j) | i <- is, i `S.member` sc, j <- filter (== i) (S.toList sc) ]
          s' = foldr S.insert as is
      e' <- scope s' e
      errMultiple errM
      errShadow errS
      pure (is, e', s')

-- Get all visible identifiers from i := e
getVisible :: HasCallStack => Expr -> [Ident]
getVisible Lit{} = []
getVisible Variable{} = []
getVisible (Array es) = concatMap getVisible es
getVisible (Seq es) = concatMap getVisible es
getVisible (ApplyS e1 e2) = getVisible e1 ++ getVisible e2
getVisible (ApplyD e1 e2) = getVisible e1 ++ getVisible e2
getVisible (If3 {}) = []
-- This is just wrong.  Variables defined in e are not visible outside the 'if' getVisible (If3 e _ _) = getVisible e
getVisible For2{} = []
getVisible (Let _ e) = getVisible e
getVisible Block{} = []
getVisible (Unify e1 e2) = getVisible e1 ++ getVisible e2
--getVisible (Typedef _) = []
getVisible (Macro1 (Ident _ "assume") _ e) = getVisible e
getVisible Macro1 {} = []
getVisible (DefineV i) = [i]
getVisible (DefineE i e) = i : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible Function{} = []
getVisible (Exists is e) = is ++ getVisible e
getVisible (Forall _is _e) = []  -- Forall is a new scope
getVisible (OfType _ _) = []
getVisible TLam{} = []
getVisible Lam{} = []
getVisible Fail = []
getVisible DomainFail = []
getVisible e = impossible e

getVar :: HasCallStack => Expr -> [Ident]
getVar Lit{} = []
getVar Variable{} = []
getVar (Array es) = concatMap getVar es
getVar (Seq es) = concatMap getVar es
getVar (ApplyS e1 e2) = getVar e1 ++ getVar e2
getVar (ApplyD e1 e2) = getVar e1 ++ getVar e2
-- getVar If3{} = []
getVar (If3 e _ _) = getVar e
getVar For2{} = []
getVar (Let _ e) = getVar e
getVar Block{} = []
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
getVar (Forall _ e) = getVar e
getVar (OfType e t) = getVar e ++ getVar t
getVar Lam{} = []
getVar Fail = []
getVar DomainFail = []
getVar e = impossible e

-- Primitives
primOps :: [Ident]
primOps = map (Ident noLoc)
  [ "isInt$", "isRat$", "isChr$", "isF32$", "isF64$", "isStr$", "isPtr$", "isArr$", "isFcn$"

  , "intAdd$", "intSub$", "intMul$", "intDiv$", "intNeg$", "intPlus$"
  , "intLT$", "intLE$", "intGT$", "intGE$", "intNE$"

  , "ratAdd$", "ratSub$", "ratMul$", "ratDiv$", "ratNeg$", "ratPlus$"
  , "ratLT$", "ratLE$", "ratGT$", "ratGE$", "ratNE$"

  , "f32Add$", "f32Sub$", "f32Mul$", "f32Div$", "f32Neg$", "f32Plus$"
  , "f32LT$", "f32LE$", "f32GT$", "f32GE$", "f32NE$"

  , "f64Add$", "f64Sub$", "f64Mul$", "f64Div$", "f64Neg$", "f64Plus$"
  , "f64LT$", "f64LE$", "f64GT$", "f64GE$", "f64NE$"

  , "post'?'"
  , "concat$", "cons$"
  , "length$"
  , "alloc$", "read$", "write$"
  , "in'..'"
  , "in'+='", "in'-='", "in'*='", "in'/='"
  , "print$"
  , "append$"
  , "known$"  -- This is a horrible hack
  , "any$"
  , "fail$"
  , "err$"
  , "arrLen$"
  , "arrConc$"
  ]

------------------------

{-
-- The invariant for generating core is that there is always an Exists
-- in the following constructs:
--  If3
--  For2
-- If there isn't, this pass will insert an empty exists.
addExist :: Expr -> D Expr
addExist = pure . f
  where
    f (If3 e1 e2 e3) = If3 (ex (f e1)) (f e2) (f e3)
    f (For2 e1 e2)   = For2 (ex (f e1)) (f e2)
    f e = composOp f e
    ex e@Exists{} = e
    ex e = Exists [] e
-}

------------------------

simplify :: Expr -> D Expr
simplify expr = do
  simpl <- gets (fSimplify . dflags)
  if not simpl then
    pure expr
   else do
    let loop :: Int -> Expr -> D Expr
        loop 0 e = trace "simplifier timed out" $ pure e
        loop n e = do
          e' <- oneSimplifyPass e
          if e == e' then
            pure e
           else
            loop (n-1) e'
    loop 25 expr

oneSimplifyPass :: Expr -> D Expr
oneSimplifyPass expr = do
  tr <- gets (fTraceDesugar . dflags)
  let traceDS :: String -> Expr -> D Expr
      traceDS msg e | tr = trace ("---- " ++ msg ++ "\n" ++ prettyShow e) $
                           pure e
                    | otherwise = pure e
  (traceDS "elimExist"  <=< elimExist <=<
   traceDS "alias"      <=< simpAlias <=<
   traceDS "simpler"    <=< simpler   <=<
   traceDS "inlineVal"  <=< inlineVal <=<
   pure ) expr

inlineVal :: Expr -> D Expr
inlineVal expr = do
  simpl <- gets (fSimplify . dflags)
  if simpl then
    pure $ inlineVal' expr
   else
    pure expr
    
inlineVal' :: Expr -> Expr
inlineVal' expr = evalState (inl expr) []
  where
    inl :: Expr -> State [(Ident, Expr)] Expr
    inl (Seq (e@(Unify (Variable x) v) : es)) | shouldInline x v = do
--      traceM $ "extend " ++ prettyShow (x, v)
      v' <- inl v
      es' <- withVal (x, v') (inl (Seq es))
      pure $ seqE [e, es']
    inl e@(Variable x) = do
      m <- get
      case lookup x m of
--        Just v | trace ("found " ++ prettyShow (x, v)) False -> undefined
        Just v | not (isLam v) -> pure v
        _                      -> pure e
    inl (ApplyD f@(Variable x) a) = do
      m <- get
      case lookup x m of
        Just elam@(Lam i e) | closed elam && i /= x ->
          inl $ Exists [i] $ Seq [ Unify (Variable i) a, e]
        _ -> ApplyD <$> inl f <*> inl a
    inl (Exists is e) = Exists is <$> withNotIn is  (inl e)
    inl (Forall is e) = Forall is <$> withNotIn is  (inl e)
    inl (Lam i e)     = Lam    i  <$> withNotIn [i] (inl e)
    inl e = compos inl e

    isLam Lam{} = True
    isLam _ = False
    shouldInline x (Lam _ _) = unIdent x `elem` ["int", "any"]
    shouldInline _ e = isValue e

    withVal :: (Ident, Expr) -> State [(Ident, Expr)] a -> State [(Ident, Expr)] a
    withVal b ma = do
      m <- get
      pure $ evalState ma (b:m)
    withNotIn :: [Ident] -> State [(Ident, Expr)] a -> State [(Ident, Expr)] a
    withNotIn is ma = do
      m <- get
      let m' = filter ((`notElem` is) . fst) m
--      when (m /= m') $ traceM $ "dropped " ++ prettyShow (m, m')
      pure $ evalState ma m'

-- Eliminate existentials of the form
--  Exists x . ... x=e ...
-- where the x=e is the only occurrence of x.
elimExist :: Expr -> D Expr
elimExist expr = do
  simpl <- gets (fSimplify . dflags)
  if simpl then
    pure $ elimE expr
   else
    pure expr
  where
    elimE (Exists [] e) = elimE e
    elimE (Exists (x:xs) e) =
      let e' = elimE (Exists xs e)
      in  case elimX x e' of
            Nothing  -> lExists [x] e'
            Just e'' -> e''
    elimE e = composOp elimE e

--    elimX x _ | trace ("----------------" ++ prettyShow x) False = undefined
    elimX x ex =
      let --elm e | unIdent x == "in'+'", trace ("e=" ++ prettyShow e) False = undefined
          elm (Unify (Variable y) e) | x == y = do tell (Sum (1::Int)); elm e
          elm e@(Variable y) | x == y = do tell (Sum 2); pure e
          elm e@(If3 e1 _ _) | occurs e1 = do tell (Sum 2); pure e
          elm e@(Split e1 _ _) | occurs e1 = do tell (Sum 2); pure e
          elm e = compos elm e
          occurs e = execWriter (elm e) /= 0  -- does x occur in e
      in  case runWriter (elm ex) of
--            xxx | unIdent x == "in'+'", trace ("runWriter " ++ prettyShow (x, xxx)) False -> undefined
            (e', Sum n) | n <= 1 -> Just e'
            _                    -> Nothing

instance Pretty a => Pretty (Sum a) where
  pPrint (Sum a) = text "Sum" <+> pPrint a

simpler :: Expr -> D Expr
simpler expr = do
  -- Always remove silly uses of any$
  expr' <- simpValue <=< simpAny $ expr
  simpl <- gets (fSimplify . dflags)
  if simpl then
    simpUnify expr'
   else
    pure expr'

-- Simplify  v; e  -->  e
simpValue :: Expr -> D Expr
simpValue = pure . simpValue'

simpValue' :: Expr -> Expr
simpValue' = f
  where f (Seq (Snoc es e)) = seqE $ map f (Snoc (filter (not . isValue') es) e)
        f e = composOp f e
        isValue' Lam{} = True   -- Also remove useless lambdas
        isValue' e = isValue e

{- Cannot do this everywhere, e.g., If3 relies on existentials
-- Simplify  exists . e  -->  e
simpExists :: Expr -> D Expr
simpExists = pure . f
  where f (Exists [] e) = f e
        f e = composOp f e
-}

-- Simplify any[e]  -->  e
simpAny :: Expr -> D Expr
simpAny = pure . f
  where f (ApplyD (Variable (Ident _ "any")) e) = f e  -- This should go away
        f (ApplyD (EPrim "any$") e) = f e
        f (EPrim "fail$") = Fail
        f e = composOp f e

-- Simplify x = (e1; ...; en)  -->  e1; ...; x = en
--          x = (y = e)  -->  x = y; y = e
simpUnify :: Expr -> D Expr
simpUnify = pure . f
  where f (Unify v (Seq (Snoc xs x))) | isValue v = f $ Seq $ xs ++ [Unify v x]
        f (Unify e1 (Unify v e2)) | isValue v = f $ Seq [Unify e1 v, Unify v e2]
        f (Seq es) = seqE $ map f es
        f e = composOp f e

-- If we have a unification x=y, and x&y are bound in the same existential
-- then we can get rid of one of the variables.
simpAlias :: Expr -> D Expr
simpAlias expr = do
  simpl <- gets (fSimplify . dflags)
  if simpl then
    pure (f expr)
   else
    pure expr
  where f (Exists is ee) =
          -- The xys list are the identifiers where the x is among the existential
          -- bindings and x is bound locally to y.
          -- We will replace x by y, remove x from the bound variables, and remove the binding.
          let e = f ee
              xys = uniq [] $ Epic.List.nub [ xy | (x, y) <- localUnify e, xy <- pickBetter x y ]
              -- pickBetter x y | trace ("pickBetter " ++ show (x, y)) False = undefined
              pickBetter x y | x == y = []
              pickBetter x y | not xlocal && not ylocal = []
                             |     xlocal && not ylocal = [(x, y)]
                             | not xlocal &&     ylocal = [(y, x)]
                             where xlocal = x `elem` is; ylocal = y `elem` is
              -- Both are local, try to pick the one with the better name to remain.
              pickBetter x y | isTempIdent x || not (isTempIdent y) = [(x, y)]
                             | isTempIdent y || not (isTempIdent x) = [(y, x)]
                             | x < y = [(x, y)]
                             | otherwise = [(y, x)]
              is' = filter (`notElem` map fst xys) is
              e' = substMany [(x, Variable y) | (x, y) <- xys] $ dropUnify xys e
          in  --trace (show (is, xys)) $
              lExists is' e'
        -- Special hack to keep existentials in If3
        f (If3 (Exists is e1) e2 e3) = If3 (Exists is (f e1)) (f e2) (f e3)
        f (If3 e1 e2 e3) = If3 (f e1) (f e2) (f e3)
        f e = composOp f e

uniq :: [Ident] -> [(Ident, Ident)] -> [(Ident, Ident)]
uniq _ [] = []
uniq u (x@(a,b):xs) | a `elem` u || b `elem` u = uniq u xs
                    | otherwise = x : uniq (a:b:u) xs

dropUnify :: [(Ident, Ident)] -> Expr -> Expr
dropUnify xys (Seq es) = seqE (seqDrop (map (dropUnify xys) $ concatMap flat es))
  where flat (Seq xs) = concatMap flat xs
        flat x = [x]
dropUnify xys (Unify (Variable x) (Variable y)) | (x, y) `elem` xys = Variable y
                                                | (y, x) `elem` xys = Variable x
dropUnify _ e = e

seqDrop :: [Expr] -> [Expr]
seqDrop [] = []
seqDrop [e] = [e]
seqDrop (v:es@(_:_)) | isValue v = seqDrop es
seqDrop (e:es) = e : seqDrop es

localUnify :: Expr -> [(Ident, Ident)]
localUnify (Seq es) = concatMap localUnify es
localUnify (Unify (Variable x) (Variable y)) = [(x, y)]
localUnify _ = []

isTempIdent :: Ident -> Bool
isTempIdent (Ident _ ('$':_)) = True
isTempIdent _ = False

-------------------------

addDeref :: Expr -> D Expr
addDeref = pure . exprD S.empty
  where
    expr _ e@Lit{} = e
    expr s e@(Variable i) | i `S.member` s = applyPrimD "read$" e
                          | otherwise = e
    expr s (Array es) = Array $ map (expr s) es
    expr s (Seq es) = Seq $ map (expr s) es
    expr s (ApplyS e1 e2) = ApplyS (expr s e1) (expr s e2)
    expr s (ApplyD e1 e2) = ApplyD (expr s e1) (expr s e2)
    expr s (If3 e1 e2 e3) = If3 (expr s' e1) (expr s' e2) (exprD s e3)
      where s' = defs s e1
    expr s (For2 e1 e2) = For2 (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Let e1 e2) = Let (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Block e) = Block (exprD s e)
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
    expr s (TLam i rs e1 e2) = TLam i rs (expr s' e1) (expr s' e2)
      where s' = defs s e1
    expr s (Exists is e) = Exists is (expr s e)
    expr s (OfType e t) = OfType (expr s e) (expr s t)
    expr _ Fail = Fail
    expr s (Lam i e) = Lam i (expr s e)
    expr _ e@EPrim{} = e
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

-- Applications have to be lowered before scope insertion
-- so existential get inserted in the right place.
lowerApply :: Expr -> D Expr
lowerApply = f
  where
    f (ApplyS e1 e2) = Succeeds <$> (ApplyD <$> f e1 <*> f e2)
    f (OfType e t) = do
      verif <- gets (fVerify . dflags)
      if verif then
        OfType <$> f e <*> f t
       else
        Succeeds <$> (ApplyD <$> f t <*> f e)
    f e = compos f e

-- Convert Big Core to Core
lower :: Expr -> D Expr
lower e@Lit{} = pure e
lower e@Variable{} = pure e
lower (Array es) = Array <$> mapM lower es
lower e@Wrong{} = pure e
lower (Seq es) = seqE <$> mapM lower es
lower (ApplyD e1 e2) = ApplyD <$> lower e1 <*> lower e2
lower (Unify e1 e2) = Unify <$> lower e1 <*> lower e2
lower (Choice e1 e2) = Choice <$> lower e1 <*> lower e2
lower (For2B is e1 e2) = join $ lowerFor is <$> lower e1 <*> lower e2
lower (If3B is e1 e2 e3) = join $ lowerIf is <$> lower e1 <*> lower e2 <*> lower e3
lower (Macro1 (Ident _ "all") [] e) = lowerAll =<< lower e
lower (Macro1 (Ident _ "one") [] e) = lowerOne =<< lower e
lower (Succeeds e) = lowerSucceeds =<< lower e
lower (Macro1 (Ident _ "decides") [] e) = lowerDecides =<< lower e
lower (Macro1 (Ident _ "assume") [] e) = lowerAssume =<< lower e
lower (Macro1 (Ident _ "verify") [] e) = lowerVerify =<< lower e
lower (Macro1 (Ident _ "assert") [] e) = lowerAssert =<< lower e
lower (Macro1 (Ident _ "lowered") [] e) = pure e
lower (Exists is e) = lExists is <$> lower e
lower (TLam i rs (Exists is e1) e2) = join $ lowerTLam i rs is <$> lower e1 <*> lower e2
lower (OfType e t) = join $ lowerOfType <$> lower e <*> lower t
lower (Lam i e) = Lam i <$> lower e
lower Fail = pure Fail
lower (Forall is e) = Forall is <$> lower e
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
lowerIfVerify is e1 e2 e3 = pure $ If3B is e1 e2 e3

lowerIfNoLambda :: [Ident] -> Expr -> Expr -> Expr -> D Expr
lowerIfNoLambda vs e1 e2 e3 = do
  y <- newIdent (getLoc e1) "y"
  let vy = Variable y
      fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
  pure $ Exists [y] $ Seq
           [ Unify vy (eOne $ lExists vs (Seq [e1, evs])
                              `Choice`
                              Lit (LitInt 0))
           , lExists fvs (Seq [Unify vy evs, e2])
             `Choice`
             Seq [Unify vy (Lit (LitInt 0)), e3]
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

lowerTLam :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> D Expr
lowerTLam i rs is e1 e2 = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerTLamVerify i rs is e1 e2
   else
    lowerTLamRun i rs is e1 e2

-- XXX what about _rs
lowerTLamVerify :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> D Expr
lowerTLamVerify i rs is e1 e2 = do
  (e2', e2'') <-
    case e2 of
      OfType e t -> do
        x <- newIdent (getLoc t) "x"
        pure (ApplyD t e, Exists [x] $ ApplyD t (Variable x))
      _ -> pure (e2, e2)
  -- XXX This whole thing is wrong.  Function effects should be handled in some
  -- consistent way.
  if hasEff "succeeds" rs then
    pure $ lowerTLamVerifySucceeds i is e1 e2' e2''
   else if hasEff "decides" rs then
    pure $ lowerTLamVerifyDecides i is e1 e2' e2''
   else if hasEff "decides" rs then
    pure $ lowerTLamVerifyFails i is e1 e2' e2''
   else
    -- Assume "succeeds"
    pure $ lowerTLamVerifySucceeds i is e1 e2' e2''

hasEff :: String -> [Ident] -> Bool
hasEff r rs = Ident noLoc r `elem` rs

lowerTLamVerifyFails :: Ident -> [Ident] -> Expr -> Expr -> Expr -> Expr
lowerTLamVerifyFails i is e1 e2' e2'' =
  -- Lam i $ lExists is $ Seq [ e1, eDecide e2'']
  Seq
    [ eVerify $ Lam i $ lExists is $ Seq [eAssume e1, eFails  e2']
    ,           Lam i $ lExists is $ Seq [        e1,         e2'']
    ]

lowerTLamVerifyDecides :: Ident -> [Ident] -> Expr -> Expr -> Expr -> Expr
lowerTLamVerifyDecides i is e1 e2' e2'' =
  -- Lam i $ lExists is $ Seq [ e1, eDecide e2'']
  Seq
    [ eVerify $ Lam i $ lExists is $ Seq [eAssume e1, eDecide e2']
    ,           Lam i $ lExists is $ Seq [        e1,         e2'']
    ]

lowerTLamVerifySucceeds :: Ident -> [Ident] -> Expr -> Expr -> Expr -> Expr
lowerTLamVerifySucceeds i is e1 e2' e2'' =
  Seq
    [ eVerify $ Lam i $ lExists is $ Seq [eAssume e1, eAssert e2']
    ,           Lam i $ lExists is $ Seq [        e1, eAssume e2'']
    ]

-- XXX use all of rs
lowerTLamRun :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> D Expr
lowerTLamRun i rs is e1 e2 = do
  -- XXX This inserts Succeeds late, and scope insertion has already happened.
  -- XXX This might be wrong.
  let invariant = --invariantId `elem` rs  || True -- XXX
                  openId `notElem` rs
  if null is && e1 == Array [] then
    pure $ Lam i e2   -- Simple special case
   else
    if invariant then
      pure $ Lam i $ lExists is (seqE [e1, e2])
    else
      Lam i <$> lowerIf is e1 e2 DomainFail

lowerOfType :: Expr -> Expr -> D Expr
lowerOfType e t = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerOfTypeVerify e t
   else
    lowerSucceeds (ApplyD t e)

lowerOfTypeVerify :: Expr -> Expr -> D Expr
lowerOfTypeVerify e t = do
  x <- newIdent (getLoc t) "x"
  pure $ Seq [ eVerify $ eAssert $ ApplyD t e, eAssume $ Forall [x] $ ApplyD t (Variable x) ]

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
  asmVerif <- gets (fAssumeVerified . dflags)
  how <- gets context
  if verif then
    case how of
      DS1 -> pure $ eAssert e
      DS2 -> pure $ eAssert e
      DS3 -> pure $ Succeeds e
      DS5 -> pure $ eAssert e  -- XXX ???
      DS6 -> pure $ eAssert e  -- XXX ???
      DS7 -> pure $ eAssert e  -- XXX ???
   else if asmVerif then
    pure $ e
   else if useSplit then
    lowerSucceedsSplit e
   else
    pure $ Succeeds e

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
  -- if verif then
  --   unimplemented "verify-decides"
  --  else
  if useSplit && not verif then
    lowerDecidesSplit e
  else
    pure $ eDecide  e

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

lowerVerify :: Expr -> D Expr
lowerVerify e = pure $ eVerify e

lowerAssert :: Expr -> D Expr
lowerAssert e = pure $ eAssert e



-----------------

primops :: Expr -> D Expr
primops = f
  where
    f (Variable (Ident _ s)) | Ident noLoc s `S.member` prim = pure $ EPrim s
    f e = compos f e
    prim = S.fromList primOps

{-
  where
    f (ApplyD g@(Variable (Ident _ s)) v) = do
      mfunc <- lowerPrimOp s
      v' <- f v
      case mfunc of
        Nothing -> pure $ ApplyD g v'
        Just func -> pure $ app func v'
    f e@(Variable (Ident _ s)) = do
      mfunc <- lowerPrimOp s
      pure $ maybe e toLam mfunc
    f e = compos f e

    app ([a], es) v | isValue v = substMany [(a, v)] $ seqE es
    app ([a1,a2], es) (Array [v1,v2]) | isValue v1 && isValue v2 = substMany [(a1, v1), (a2, v2)] $ seqE es
    app func v = ApplyD (toLam func) v

    arg = Ident noLoc "arg"
    toLam :: Func -> Core
    toLam (   [],    es) = seqE es
    toLam (   [a],   es) = Lam a   $ seqE es
    toLam (as@[_,_], es) = Lam arg $ lExists as $ seqE $ Unify (Array $ map Variable as) (Variable arg) : es
    toLam _ = undefined  -- shouldn't happen

-- Some "primops" will be expanded into code.
-- This should really be part of the prelude.
-- For verification, we need a different expansion.
lowerPrimOp :: String -> D (Maybe Func)
lowerPrimOp s = do
  if Ident noLoc s `elem` primOps then
    pure $ Just ([], [EPrim s])
  else
    pure $ Nothing

type Func = ([Ident], [Core])

lowerPrimOpVerif :: String -> D (Maybe Func)
lowerPrimOpVerif s = do
  me <- lowerPrimOpRun s
  case me of
    Just ([], [EPrim p]) | Just func <- lookup p verifyPrelude -> pure $ Just func
    Nothing              | Just func <- lookup s verifyPrelude -> pure $ Just func
    r -> pure r

lowerPrimOpRun :: String -> D (Maybe Func)
lowerPrimOpRun s =
  case lookup s preludeFuncs of
    r@Just{} -> pure r
    Nothing  ->
      if Ident noLoc s `elem` primOps then
        pure $ Just ([], [EPrim s])
      else
        pure $ Nothing

-- Definitions that should go in a Prelude
preludeIds :: [Ident]
preludeIds = map (Ident noLoc . fst) preludeFuncs

preludeFuncs :: [(String, Func)]
preludeFuncs =
  [("any", typ [])                                             -- x => x
  ,("nat", typ [app "isInt$" vx, app2 "in'>='" vx (Lit (LitInt 0))]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt$" vx])                              -- x => int#[x]; x
  ,("rat", typ [app "isRat$" vx])                              -- x => rat#[x]; x
  ,("char", typ [app "isChr$" vx])                             -- x => char#[x]; x
  ,("string", typ [app "isStr$" vx])                           -- x => string#[x]; x
  ,("in'->'", arrowV)
  ,("false", bare $ Array [])                                      -- ()
  ,("new", newV)
  ,("post'^'", bare $ EPrim "read$")
  ,("in'.='", bare $ EPrim "write$")
  ,("mapAp", bare $ EPrim "mapAp$")
  ]
  where bare e = ([], [e])
        typ es = ([x], es ++ [Variable x])
        vx = Variable x
        app f v = ApplyD (EPrim f) v
        app2 f v1 v2 = ApplyD (EPrim f) (Array [v1, v2])

        arrowV = ([s, t],
              [ Lam g $ Lam y $
                Exists [sy, gsy] $
                Seq [
                  app "isFcn$" (Variable g),
                  Unify (Variable  sy) (ApplyD (Variable s) (Variable y)),
                  Unify (Variable gsy) (ApplyD (Variable g) (Variable sy)),
                  ApplyD (Variable t) (Variable gsy)
                  ]
              ])
        [s, t, g, y, sy, gsy, x, _xy] =
           map (Ident noLoc . ("$$" ++)) ["s","t","g","y","sy","gsy","x", "xy"]

        newV = ([t],
                [ Lam x $
                  Exists [y] $
                  Seq [
                    Unify (Variable y) (ApplyD (Variable t) (Variable x)),
                    app "alloc$" (Variable y)
                    ]
                ])

verifyPrelude :: [(String, Func)]
verifyPrelude =
  [ arithBinOpInt  "in'+'" "intAdd$"
  , arithBinOpInt  "in'-'" "intSub$"
  , arithBinOpInt  "in'*'" "intMul$"
  , arithBinOpIntC "in'/'" "intDiv$" yNe0
  , arithUnOpInt   "pre'-'" "intNeg$"
  , arithUnOpInt   "pre'+'" "intPlus$"
  , cmpBinOpInt    "in'<'"  "intLT$"
  , cmpBinOpInt    "in'<='" "intLE$"
  , cmpBinOpInt    "in'>'"  "intGT$"
  , cmpBinOpInt    "in'>='" "intGE$"
  , cmpBinOpInt    "in'<>'" "intNE$"
  ]
  where
    arithBinOpInt  p s = (p, arithBinOpInt' [] s)
    arithBinOpIntC p s c = (p, arithBinOpInt' [c] s)
    arithBinOpInt' c p = ([x, y],
      [ cInt vx, cInt vy] ++ c ++
      [ eAssume $ Exists [z] $ Seq [Unify vz (ApplyD (EPrim p) (Array [vx, vy])), cInt vz, vz] ])

    cmpBinOpInt  p s = (p, cmpBinOpInt' s)
    cmpBinOpInt' p = ([x, y],
      [ cInt vx, cInt vy, ApplyD (EPrim p) (Array [vx, vy]), eAssume (Seq [cInt vx, vx]) ])

    yNe0 = ApplyD (EPrim "intNE$") (Array [vy, Lit (LitInt 0)])

    arithUnOpInt p s =
      (p, ([x], [ cInt vx, eAssume (Exists [z] $ Seq [Unify vz (ApplyD (EPrim s) vx), cInt vz, vz]) ]))

    cInt e = ApplyD (EPrim "isInt$") e
    x = Ident noLoc "$$x"
    y = Ident noLoc "$$y"
    z = Ident noLoc "$$z"
    vx = Variable x
    vy = Variable y
    vz = Variable z
-}

-----------------------------------------------

-- After lowering there are no funny scopes, so empty existential
-- are no longer necessary.
lExists :: [Ident] -> Expr -> Expr
lExists [] e = e
lExists is (Exists is' e) = lExists (is ++ is') e
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

eDecide :: Expr -> Expr
eDecide = Macro1 (Ident noLoc "decides") []

eFails :: Expr -> Expr
eFails = Macro1 (Ident noLoc "fails") []


-- Used to create the array of free variables passed from the domain to the range
-- of for/if.  If it's just a single variable, don't use an array.
fvArray :: [Expr] -> Expr
fvArray [e] = e
fvArray es = Array es

----------------------

-- Functions that only work on the core subset of Expr
getFree :: Core -> [Ident]
getFree = Epic.List.nub . fvs
  where
    fvs (Variable i) = [i]
    fvs (Lit _) = []
    fvs (EPrim _) = []
    fvs (Array es) = concatMap fvs es
    fvs (Lam i e) = filter (/= i) (fvs e)
    fvs (Unify e1 e2) = fvs e1 ++ fvs e2
    fvs (ApplyD e1 e2) = fvs e1 ++ fvs e2
    fvs (Seq es) = concatMap fvs es
    fvs (Choice e1 e2) = fvs e1 ++ fvs e2
    fvs (Exists is e) = filter (`notElem` is) (fvs e)
    fvs (Forall is e) = filter (`notElem` is) (fvs e)
    fvs (Wrong _) = []
    fvs (Macro1 _ _ e) = fvs e
    fvs (Split e1 e2 e3) = fvs e1 ++ fvs e2 ++ fvs e3
    fvs (If3 (Exists is e1) e2 e3) = fvs (Exists is (Seq [e1, e2])) ++ fvs e3
    fvs Fail = []
    fvs DomainFail = []
    fvs e = impossible e

closed :: Core -> Bool
closed = null . getFree

-- XXX binders
getAllVars :: Core -> [Ident]
getAllVars = Epic.List.nub . execWriter . vars
  where vars e@(Variable i) = do tell [i]; pure e
        vars e@(Lam i e') = do tell [i]; _ <- vars e'; pure e
        vars e@(Exists is e') = do tell is; _ <- vars e'; pure e
        vars e@(Forall is e') = do tell is; _ <- vars e'; pure e
        vars TLam{} = undefined
        vars e = compos vars e

substMany :: [(Ident, Core)] -> Core -> Core
substMany [] = id
substMany sb = sub
  where
    bs = getFree $ Seq $ map snd sb
    sub :: Core -> Core
    sub v@(Variable i) | Just b <- lookup i sb = b
                       | otherwise = v
    sub e@Lit{} = e
    sub e@EPrim{} = e
    sub (Array es) = Array (map sub es)
    sub (Lam i e) = binder i (Lam i) e
    sub (Unify e1 e2) = Unify (sub e1) (sub e2)
    sub (ApplyD e1 e2) = ApplyD (sub e1) (sub e2)
    sub (Seq es) = Seq (map sub es)
    sub (Choice e1 e2) = Choice (sub e1) (sub e2)
    sub (Exists [] e) = Exists [] (sub e)
    sub (Exists (i:is) e) = binder i (exists1 i) (Exists is e)
    sub (Forall [] e) = Forall [] (sub e)
    sub (Forall (i:is) e) = binder i (forall1 i) (Forall is e)
    sub e@Wrong{} = e
    sub (Macro1 i rs e) = Macro1 i rs (sub e)
    sub (Split e1 e2 e3) = Split (sub e1) (sub e2) (sub e3)
    sub (If3 (Exists is e1) e2 e3) =
      let (is', e1', e2') = if3Hack sub is e1 e2
      in  If3 (Exists is' e1') e2' (sub e3)
    sub Fail = Fail
    sub DomainFail = DomainFail
    sub e = impossible e

    binder :: Ident -> (Expr -> Expr) -> Expr -> Expr
    binder i con e | Just _ <- lookup i sb = substMany (filter ((/= i) . fst) sb) (con e)
                   | i `notElem` bs = con (sub e)
                   | otherwise = sub $ alphaConvert bs (con e)

    exists1 i (Exists is e) = Exists (i:is) e
    exists1 _ _ = undefined

    forall1 i (Forall is e) = Forall (i:is) e
    forall1 _ _ = undefined

if3Hack :: (Expr -> Expr) -> [Ident] -> Expr -> Expr -> ([Ident], Expr, Expr)
if3Hack f is e1 e2 =
  case f (Exists is (Array [e1, e2])) of
    Exists is' (Array [e1', e2']) -> (is', e1', e2')
--    Array [e1', e2'] -> ([], e1', e2')
    e -> impossible e

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> Core -> Core
alphaConvert vs = alpha []
  where
    alpha :: [(Ident, Ident)] -> Core -> Core
    alpha m (Variable i) = Variable $ fromMaybe i $ lookup i m
    alpha _ e@Lit{} = e
    alpha _ e@EPrim{} = e
    alpha m (Array es) = Array (map (alpha m) es)
    alpha m (Lam i e) = Lam i' $ alpha (add (i, i') m) e where i' = fresh i
    alpha m (Unify e1 e2) = Unify (alpha m e1) (alpha m e2)
    alpha m (Seq es) = Seq (map (alpha m) es)
    alpha m (ApplyD e1 e2) = ApplyD (alpha m e1) (alpha m e2)
    alpha m (Choice e1 e2) = Choice (alpha m e1) (alpha m e2)
    alpha m (Macro1 i rs e) = Macro1 i rs (alpha m e)
    alpha m (Exists is e) = Exists is' (alpha m' e)
      where is' = map fresh is
            m' = foldr add m $ zip is is'
    alpha _ e@Wrong{} = e
    alpha m (Split e f g) = Split (alpha m e) (alpha m f) (alpha m g)
    alpha m (If3 (Exists is e1) e2 e3) =
      let (is', e1', e2') = if3Hack (alpha m) is e1 e2
      in  If3 (Exists is' e1') e2' (alpha m e3)
    alpha _ Fail = Fail
    alpha _ e = impossible e

    add ii@(i, i') m | i == i' = m
                     | otherwise = ii : m

    fresh i@(Ident l s) | i `notElem` vs = i
                        | otherwise = fresh $ Ident l (s ++ "'")

-------------------------------------------------------------------

dsD_3 :: Expr -> D Expr
dsD_3 e@Lit{} = pure e
dsD_3 e@Variable{} = pure e
dsD_3 (ApplyD e1 e2) = ApplyD <$> dsD_3 e1 <*> dsD_3 e2
dsD_3 (Unify e1 e2) = Unify <$> dsD_3 e1 <*> dsD_3 e2
dsD_3 (Choice e1 e2) = Choice <$> dsD_3 e1 <*> dsD_3 e2
dsD_3 e@(DefineV _) = pure e
dsD_3 (DefineE x e) = DefineE x <$> dsD_3 e
dsD_3 (Seq []) = pure (Array [])
dsD_3 (Seq [t]) = dsD_3 t
dsD_3 (Seq (t:ts)) = seqE <$> sequence [dsD_3 t, dsD_3 (Seq ts)]
dsD_3 (Array ts) = Array <$> mapM dsD_3 ts
dsD_3 (OfType t1 t2) = OfType <$> dsD_3 t1 <*> dsD_3 t2
dsD_3 e@Fail = pure e
dsD_3 (Range t) = do
  i  <- newIdent (getLoc t) "i"
  existsV [i] <$> dsM_3 (Range t) i
dsD_3 e@Function{} = eVerify <$> dsF_3 e
-- Added
dsD_3 (If3 e1 e2 e3) = If3 <$> dsD_3 e1 <*> dsD_3 e2 <*> dsD_3 e3
dsD_3 (Succeeds e)   = eAssert <$> dsD_3 e
dsD_3 (Macro1 m rs e) = Macro1 m rs <$> dsD_3 e
dsD_3 (Lam x e) = Lam x <$> dsD_3 e
dsD_3 e = impossible e

dsF_3 :: Expr -> D Expr
dsF_3 (Function [(t1, _effs)] t2) = do
  y <- newIdent (getLoc t1) "y"
  t1' <- dsM_3 t1 y
  t2' <- dsF_3 t2
  pure $ Lam y $ seqE [eAssume t1', t2']
dsF_3 _z@(OfType t ty) = do
  t'  <- dsD_3 t
  ty' <- dsD_3 ty
  aty <- dsA_3 ty
  pure $ seqE [eAssert (ApplyD ty' t'), {- trace ("dsF_3: " ++ prettyShow z ++ " aty = " ++ prettyShow aty) -} aty]
dsF_3 t = do
  t' <- dsD_3 t
  pure $ seqE [eAssert t', t']

dsA_3 :: Expr -> D Expr
dsA_3 t = do
  r <- newIdent (getLoc t) "r"
  t' <- dsD_3 (Range t)
  pure $ Forall [r] $ seqE [eAssume (Unify (Variable r) t'), Variable r]

dsM_3 :: Expr -> Ident -> D Expr
dsM_3 ((Function [(Range t1, _effs)] (Range t2))) f = do
  i <- newIdent (getLoc t1) "i"
  i' <- newIdent (getLoc t1) "i'"
  z <- newIdent (getLoc t2) "z"
  t1' <- dsM_3 (Range t1) i'
  t2' <- dsM_3 (Range t2) z
  pure $ seqE [eVerify $ Lam i' $ seqE [DefineE i t1', Succeeds $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]
              ,          Lam i' $ seqE [DefineE i t1', eAssume  $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]
              ]
-- dsM_3 (Range (Function [(t1, _effs)] t2)) f = do
--   i <- newIdent (getLoc t1) "i"
--   i' <- newIdent (getLoc t1) "i'"
--   z <- newIdent (getLoc t2) "z"
--   t1' <- dsM_3 t1 i'
--   t2' <- dsM_3 t2 z
--   pure $ seqE [eVerify $ Lam i' $ seqE [DefineE i t1', Succeeds $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]
--               ,          Lam i' $ seqE [DefineE i t1', eAssume  $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]
--               ]
dsM_3 (Range t) i = ApplyD <$> dsD_3 t <*> pure (Variable i)
dsM_3 (DefineE x t) i = DefineE x <$> dsM_3 t i
dsM_3 (Unify t1 t2) i = Unify <$> dsM_3 t1 i <*> dsM_3 t2 i
dsM_3 (Seq []) i = dsM_3 (Array []) i
dsM_3 (Seq [t]) i = dsM_3 t i
dsM_3 (Seq (t:ts)) i = seqE <$> sequence [dsD_3 t, dsM_3 (Seq ts) i]
dsM_3 (Choice t1 t2) i = Choice <$> dsM_3 t1 i <*> dsM_3 t2 i
dsM_3 (If3 e1 e2 e3) i = If3 <$> dsD_3 e1 <*> dsM_3 e2 i <*> dsM_3 e3 i
dsM_3 (OfType t1 t2) i = OfType <$> dsM_3 t1 i <*> dsD_3 t2
dsM_3 t i = unifyV i <$> dsD_3 t

------------------------------------------------------------------------------------
-- | Adding DS for "fig 10: Mode-based Translation from SmallSource to Core."
------------------------------------------------------------------------------------

data DsMode = I | V deriving (Eq, Ord, Show)
data DsEff  = Suc | Dec deriving (Eq, Ord, Show)

dsD_2 :: Expr -> D Expr
dsD_2 e@Lit{} = pure e
dsD_2 e@Variable{}   = pure e
dsD_2 (ApplyD e1 e2) = ApplyD <$> dsD_2 e1 <*> dsD_2 e2
dsD_2 (Unify e1 e2)  = Unify  <$> dsD_2 e1 <*> dsD_2 e2
dsD_2 (Choice e1 e2) = Choice <$> dsD_2 e1 <*> dsD_2 e2
dsD_2 e@(DefineV _)  = pure e
dsD_2 (DefineE x e)  = DefineE x <$> dsD_2 e
dsD_2 (Seq [])       = pure (Array [])
dsD_2 (Seq [t])      = dsD_2 t
dsD_2 (Seq (t:ts))   = seqE <$> sequence [dsD_2 t, dsD_2 (Seq ts)]
dsD_2 (Array ts)     = Array <$> mapM dsD_2 ts
dsD_2 (OfType t1 t2) = do {e <- dsD_2 t1; ofType_2 e t2 }
dsD_2 e@Fail         = pure e
dsD_2 (Range t)      = do i  <- newIdent (getLoc t) "i"
                          existsV [i] <$> dsM_2 I (Range t) i
dsD_2 t@Function{}   = seqE <$> sequence [ eVerify <$> dsV_2 Suc t, dsI_2 Suc t ]
-- Added
dsD_2 (If3 e1 e2 e3)  = If3 <$> dsD_2 e1 <*> dsD_2 e2 <*> dsD_2 e3
dsD_2 (Succeeds e)    = eAssert <$> dsD_2 e
dsD_2 (Macro1 m rs e) = Macro1 m rs <$> dsD_2 e
dsD_2 (Lam x e)       = Lam x <$> dsD_2 e
dsD_2 e               = impossible e

_domainExpr :: Expr -> ([Ident], Expr)
_domainExpr = go
  where
    go (Exists xs e) = (ys, Exists xs e')            where (ys, e')   = go e
    go (Seq es)      = (concat xss, Seq es')         where (xss, es') = unzip (go <$> es)
    go (Blk es)      = (concat xss, Blk es')         where (xss, es') = unzip (go <$> es)
    go (DefineE x e) = (x:ys, Unify (Variable x) e') where (ys, e')   = go e
    go (Unify e1 e2) = (xs1 ++ xs2, Unify e1' e2')   where (xs1, e1') = go e1
                                                           (xs2, e2') = go e2
    go e             = ([], e)


dsV_2 :: DsEff -> Expr -> D Expr
dsV_2 fx (Function [(t1,_effs)] t2) = do
   i <- newIdent (getLoc t1) "i"
   t1' <- dsM_2 V t1 i
   t2' <- dsV_2 (bodyEff fx _effs) t2
   pure $ Lam i $ seqE [{- ASSUME-INPUT-direct-implies eAssume -} t1', t2']
dsV_2 _  (OfType  t1 t2)  = do { e <- dsD_2 t1; vOfType_2 e t2 }
dsV_2 Suc t               = eAssert <$> dsD_2 t
dsV_2 Dec t               = eDecide <$> dsD_2 t

bodyEff :: DsEff -> [Eff] -> DsEff
bodyEff fx rs
  | hasEff "succeeds" rs = Suc
  | hasEff "decides"  rs = Dec
  | otherwise            = fx

dsI_2 :: DsEff -> Expr -> D Expr
dsI_2 fx (Function [(t1,_effs)] t2) = do
   i <- newIdent (getLoc t1) "i"
   t1' <- dsM_2 I t1 i
   t2' <- dsI_2 (bodyEff fx _effs) t2
   pure $ Lam i $ seqE [t1', t2']
dsI_2 _ (OfType _ t2)    = iOfType_2 t2
dsI_2 Suc t              = eAssume <$> dsD_2 t
dsI_2 Dec t              =             dsD_2 t

ofType_2 :: Expr -> Expr -> D Expr
ofType_2 e t = seqE <$> sequence [vOfType_2 e t, iOfType_2 t]

vOfType_2 :: Expr -> Expr -> D Expr
vOfType_2 e t = do
  t' <- dsD_2 t
  pure $ eVerify (eAssert (ApplyD t' e))

iOfType_2 :: Expr -> D Expr
iOfType_2 t = do
  r <- newIdent (getLoc t) "r"
  t' <- dsD_2 (Range t)
  pure $ Forall [r] $ seqE [eAssume (Unify (Variable r) t'), Variable r]


dsM_2 :: DsMode -> Expr -> Ident -> D Expr
dsM_2 V ((Function [(t1, _effs)] t2)) f = do
  i <- newIdent (getLoc t1) "i"
  i' <- newIdent (getLoc t1) "i'"
  z <- newIdent (getLoc t2) "z"
  t1' <- dsM_2 I t1 i'
  t2' <- dsM_2 V t2 z
  pure $ Lam i' $ seqE [DefineE i t1', eAssume $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]

dsM_2 I ((Function [(t1, _effs)] t2)) f = do
  i <- newIdent (getLoc t1) "i"
  i' <- newIdent (getLoc t1) "i'"
  z <- newIdent (getLoc t2) "z"
  t1' <- dsM_2 V t1 i'
  t2' <- dsM_2 I t2 z
  pure $ eVerify $ Lam i' $ seqE [{- ASSUME-INPUT-direct-implies  eAssume -} (DefineE i t1'), eAssert $ seqE [DefineE z (ApplyD (Variable f) (Variable i)), t2']]

dsM_2 _ (Range t)       i = ApplyD    <$> dsD_2 t <*> pure (Variable i)
dsM_2 m (DefineE x t)   i = DefineE x <$> dsM_2 m t i
-- dsM_2 m (Unify t1 t2)   i = Unify     <$> dsM_2 m t1 i <*> dsM_2 m t2 i
dsM_2 m (Unify t1 t2)   i = do { t1' <- dsM_2 m t1 i; t2' <- dsM_2 m t2 i; pure (Seq [t1', t2'])}
dsM_2 m (Seq [])        i = dsM_2 m (Array []) i
dsM_2 m (Seq [t])       i = dsM_2 m t i
dsM_2 m (Seq (t:ts))    i = seqE      <$> sequence [dsD_2 t, dsM_2 m (Seq ts) i]
dsM_2 m (Choice t1 t2)  i = Choice    <$> dsM_2 m t1 i <*> dsM_2 m t2 i
dsM_2 m (If3 e1 e2 e3)  i = If3       <$> dsD_2 e1     <*> dsM_2 m e2 i <*> dsM_2 m e3 i
dsM_2 _ (OfType _t1 _t2)  _i = error "TODO" -- OfType    <$> dsM_3 t1 i <*> dsD_3 t2
dsM_2 _ t               i = unifyV i <$> dsD_2 t

dsD_5 :: Expr -> D Expr
dsD_5 e = dsM_5 e Nothing

dsM_5 :: Expr -> Maybe Ident -> D Expr
dsM_5 (Function [(t1, _effs)] t2) Nothing = do
  i <- newIdent (getLoc t1) "i"
  r <- newIdent (getLoc t2) "r"
  t1' <- dsM_5 t1 (Just i)
  t2' <- dsD_5 t2
  t2'' <- dsM_5 t2 Nothing
  pure $ seqE [eVerify $ Lam i $ seqE [t1', eAssert t2'],
               Lam i $ If3 (Exists [] t1')
                           (Forall [r] $ seqE [eAssume (unifyV r t2''), Variable r])
                           (eVerify (eAssert Fail))
              ]
dsM_5 (Function [(t1, _effs)] t2) (Just f) = do
  i <- newIdent (getLoc t1) "i"
  j <- newIdent (getLoc t1) "j"
  r <- newIdent (getLoc t2) "r"
  z <- newIdent (getLoc t2) "z"
  t1' <- dsM_5 t1 (Just i)
  t2' <- dsM_5 t2 (Just z)
  let defZ = DefineE z $ ApplyD (Variable f) (Variable j)
  pure $ seqE [eVerify $ Lam i $ seqE [DefineE j t1', eAssert $ seqE [defZ, t2']],
               Lam i $ If3 (Exists [j] (unifyV j t1'))
                           (Forall [r] $ seqE [eAssume $ seqE [defZ, unifyV r t2'], Variable r])
                           (ApplyD (Variable f) (Variable i))
              ]
dsM_5 (OfType t1 t2) i = do
  y <- newIdent (getLoc t1) "y"
  r <- newIdent (getLoc t2) "r"
  t1' <- dsM_5 t1 i
  t2' <- dsD_5 t2
  t2'' <- dsD_5 (Range t2)
  pure $ seqE [DefineE y t1',
               eVerify $ eAssert $ ApplyD t2' (Variable y),
               Forall [r] $ seqE [eAssume $ DefineE r t2'', Variable r]
              ]
dsM_5 (Range t) Nothing = do
  x <- newIdent (getLoc t) "x"
  t' <- dsD_5 t
  pure $ Exists [x] $ ApplyD t' (Variable x)
dsM_5 (Range t) (Just x) = do
  t' <- dsD_5 t
  pure $ ApplyD t' (Variable x)

dsM_5 (DefineE x t) i = DefineE x <$> dsM_5 t i
dsM_5 (Unify t1 t2) i = Unify <$> dsM_5 t1 i <*> dsM_5 t2 i
dsM_5 (Seq [])      i = dsM_5 (Array []) i
dsM_5 (Seq (Snoc ts t)) i = seqE <$> sequence (Snoc (map dsD_5 ts) (dsM_5 t i))
dsM_5 (Choice t1 t2) i = Choice <$> dsM_5 t1 i <*> dsM_5 t2 i
dsM_5 (If3 e1 e2 e3) i = If3    <$> dsD_5 e1   <*> dsM_5 e2 i <*> dsM_5 e3 i

dsM_5 (Array ts) Nothing = Array <$> mapM dsD_5 ts
dsM_5 (Array ts) (Just x) = do
  xs <- mapM (\ t -> newIdent (getLoc t) "x") ts
  bs <- zipWithM dsM_5 ts (map Just xs)
  pure $ Exists xs $ seqE [ unifyV x $ Array $ map Variable xs, Array bs]

dsM_5 (DefineIE j x t) (Just i) = do
  t' <- dsM_5 t (Just i)
  pure $ seqE [DefineE j (Variable i), DefineE x t']
dsM_5 (DefineIE j x t) Nothing = do
  Exists [j] <$> dsM_5 (DefineE x t) (Just j)

dsM_5 t (Just x) = unifyV x <$> dsD_5 t
dsM_5 Fail Nothing = pure Fail
dsM_5 e@(Lit _) Nothing = pure e
dsM_5 e@(Variable _) Nothing = pure e
dsM_5 (ApplyD t1 t2) Nothing = ApplyD <$> dsD_5 t1 <*> dsD_5 t2
dsM_5 e _ = error $ "dsM_5: unimplemented " ++ show e

----------------------------------------

dsD_6 :: Expr -> D Expr
dsD_6 e = do
  x <- newIdent (getLoc e) "x"
  Exists [x] <$> dsM_6 e x

dsM_6 :: Expr -> Ident -> D Expr
dsM_6 (Function [(t1, effs)] t2) f = do
  i <- newIdent (getLoc t1) "i"
  j <- newIdent (getLoc t1) "j"
  r <- newIdent (getLoc t2) "r"
  z <- newIdent (getLoc t2) "z"
  t1' <- dsM_6 t1 i
  t2' <- dsM_6 t2 z
  let defZ = DefineE z $ ApplyD (Variable f) (Variable j)
      defVerif = eVerify $ Lam i $ seqE [DefineE j t1', eAssert $ seqE [defZ, t2']]
      defForall = Forall [r] $ seqE [eAssume $ seqE [defZ, unifyV r t2'], Variable r]
  verif <- gets (fVerify . dflags)
  if invariantId `elem` effs then
    -- MCFUN
    if verif then do
      pure $ seqE [defVerif,
                   Lam i $ seqE [ DefineE j t1', defForall ]
                  ]
     else do
      pure $ Lam i $ seqE [ DefineE j t1', defZ, t2' ]
   else do
    -- MOFUN
    if verif then
      pure $ seqE [defVerif,
                   Lam i $ If3 (Exists [j] (unifyV j t1'))
                               defForall
                               (ApplyD (Variable f) (Variable i))
                  ]
     else
      -- A hack to simplify function(x:any$){e} to simply \ x . ... e ...
      -- This is only to make things more readable.
      case t1 of
        DefineE x (Range (Variable (Ident _ "any$"))) -> do
          let defZ' = DefineE z $ ApplyD (Variable f) (Variable x)
          pure $ Lam x $ seqE [defZ', t2']
        _ ->
          pure $ Lam i $ If3 (Exists [j] (unifyV j t1'))
                             (seqE [defZ, t2'])
                             (ApplyD (Variable f) (Variable i))
dsM_6 (OfType t1 t2) i = do
  y <- newIdent (getLoc t1) "y"
  t1' <- dsM_6 t1 i
  t2' <- dsD_6 t2
  verif <- gets (fVerify . dflags)
  if verif then do
    r <- newIdent (getLoc t2) "r"
    t2'' <- dsD_6 (Range t2)
    pure $ seqE [DefineE y t1',
                 eVerify $ eAssert $ ApplyD t2' (Variable y),
                 Forall [r] $ seqE [eAssume $ DefineE r t2'', Variable r]
                ]
  else
    pure $ seqE [DefineE y t1', Succeeds (ApplyD t2' t1')]

dsM_6 (Range t) x = do
  t' <- dsD_6 t
  pure $ ApplyD t' (Variable x)

dsM_6 (DefineE x t) i = DefineE x <$> dsM_6 t i
dsM_6 (Unify t1 t2) i = Unify <$> dsM_6 t1 i <*> dsM_6 t2 i
dsM_6 (Seq [])      i = dsM_6 (Array []) i
dsM_6 (Seq (Snoc ts t)) i = seqE <$> sequence (Snoc (map dsD_6 ts) (dsM_6 t i))
dsM_6 (Choice t1 t2) i = Choice <$> dsM_6 t1 i <*> dsM_6 t2 i
dsM_6 (If3 e1 e2 e3) i = If3    <$> dsD_6 e1   <*> dsM_6 e2 i <*> dsM_6 e3 i

dsM_6 (Array ts) i = do
  xs <- mapM (\ t -> newIdent (getLoc t) "x") ts
  bs <- zipWithM dsM_6 ts xs
  pure $ Exists xs $ seqE [ unifyV i $ Array $ map Variable xs, Array bs]

dsM_6 (DefineIE j x t) i = do
  t' <- dsM_6 t i
  pure $ seqE [DefineE j (Variable i), DefineE x t']

dsM_6 Fail i = pure $ unifyV i Fail
dsM_6 e@(Lit _) i = pure $ unifyV i e
dsM_6 e@(Variable _) i = pure $ unifyV i e
dsM_6 (ApplyD t1 t2) i = unifyV i <$> (ApplyD <$> dsD_6 t1 <*> dsD_6 t2)

dsM_6 (Succeeds e) i = eAssert <$> dsM_6 e i
dsM_6 e@(Macro1 (Ident _ "lowered") [] _) i = pure $ unifyV i e
dsM_6 (Macro1 m rs e) i = Macro1 m rs <$> dsM_6 e i

dsM_6 e _ = error $ "dsM_6: unimplemented " ++ show e

-----------

data Ident_7 = Solve_7 Ident | Infer_7 Ident
  deriving (Show)

instance Pretty Ident_7 where pPrint = text . show

-- Copy solve/infer to a new identifier
as_7 :: Ident_7 -> Ident -> Ident_7
as_7 (Solve_7 _) i = Solve_7 i
as_7 (Infer_7 _) i = Infer_7 i

identOf_7 :: Ident_7 -> Ident
identOf_7 (Solve_7 i) = i
identOf_7 (Infer_7 i) = i

opposite :: Ident_7 -> Ident -> Ident_7
opposite (Solve_7 _) i = Infer_7 i
opposite (Infer_7 _) i = Solve_7 i

dsD_7 :: Expr -> D Expr
{-
-- To not make the desugared version ridiculosely large,
-- do some minor short-cuts here.
dsD_7 e@(Lit _) = pure e
dsD_7 e@(Variable _) = pure e
dsD_7 (Array ts) = Array <$> mapM dsD_7 ts
-- End short-cuts
-}
dsD_7 e = do
  x <- newIdent (getLoc e) "x"
  Exists [x] <$> dsM_7 e (Solve_7 x)

dsA_7 :: Ident -> Ident_7 -> D Expr
dsA_7 _ (Solve_7 f) = (\ z -> Exists [z] $ Variable z) <$> newIdent (getLoc (Variable f)) "z"
dsA_7 x (Infer_7 f) = pure $ ApplyD (Variable f) (Variable x)

-- dsA_7def z x f  ===  DefineE z (dsA_7 x f)
-- but eliminating an existential.
dsA_7def :: Ident -> Ident -> Ident_7 -> D Expr
--dsA_7def z x f = Define z <$> dsA_7 x f
dsA_7def z _ (Solve_7 _) = pure $ DefineV z
dsA_7def z x (Infer_7 f) = pure $ DefineE z $ ApplyD (Variable f) (Variable x)

dsM_7 :: Expr -> Ident_7 -> D Expr
dsM_7 (Function [(t1, effs)] t2) f = do
  i <- newIdent (getLoc t1) "i"
  j <- newIdent (getLoc t1) "j"
  r <- newIdent (getLoc t2) "r"
  z <- newIdent (getLoc t2) "z"
  inv <- gets (fInvert . dflags)
  t1' <-
    if inv then
      dsM_7 t1 (opposite f i)
    else
      dsM_7 t1 (Infer_7 i)
  t2' <- dsM_7 t2 (as_7 f  z)
  defZ <- dsA_7def z j f
  apFtoI <- dsA_7 i f
  let defVerif = eVerify $ Lam i $ seqE [DefineE j t1', eAssert $ seqE [defZ, t2']]
      defForall = Forall [r] $ seqE [eAssume $ seqE [defZ, unifyV r t2'], Variable r]
  verif <- gets (fVerify . dflags)
  if invariantId `elem` effs then
    -- MCFUN
    if verif then do
      pure $ seqE [defVerif,
                   Lam i $ seqE [ DefineE j t1', defForall ]
                  ]
     else do
      pure $ Lam i $ seqE [ DefineE j t1', defZ, t2' ]
   else do
    -- MOFUN
    if verif then
      pure $ seqE [defVerif,
                   Lam i $ If3 (Exists [j] (unifyV j t1'))
                               defForall
                               apFtoI
                  ]
     else
      -- A hack to simplify function(x:any$){e} to simply \ x . ... e ...
      -- This is only to make things more readable.
{-
      case t1 of
        DefineE x (Range (Variable (Ident _ "any$"))) -> do
          let defZ' = DefineE z $ ApplyD (Variable f) (Variable x)
          pure $ Lam x $ seqE [defZ', t2']
        _ ->
-}
          pure $ Lam i $ If3 (Exists [j] (unifyV j t1'))
                             (seqE [defZ, t2'])
                             apFtoI
dsM_7 (OfType t1 t2) i = do
  y <- newIdent (getLoc t1) "y"
  t1' <- dsM_7 t1 i
  t2' <- dsD_7 t2
  verif <- gets (fVerify . dflags)
  if verif then do
    r <- newIdent (getLoc t2) "r"
    t2'' <- dsD_7 (Range t2)
    pure $ seqE [DefineE y t1',
                 eVerify $ eAssert $ ApplyD t2' (Variable y),
                 Forall [r] $ seqE [eAssume $ unifyV r t2'', Variable r]
                ]
  else
    pure $ seqE [DefineE y t1', Succeeds (ApplyD t2' (Variable y))]

dsM_7 (Range t) x = do
  t' <- dsD_7 t
  pure $ ApplyD t' (Variable (identOf_7 x))

dsM_7 (DefineV x)   i = pure $ DefineE x (Variable (identOf_7 i))
dsM_7 (DefineE x t) i = DefineE x <$> dsM_7 t i
dsM_7 (Unify t1 t2) i = Unify <$> dsM_7 t1 i <*> dsM_7 t2 i
dsM_7 (Seq [])      i = dsM_7 (Array []) i
dsM_7 (Seq (Snoc ts t)) i = seqE <$> sequence (Snoc (map dsD_7 ts) (dsM_7 t i))
dsM_7 (Choice t1 t2) i = Choice <$> dsM_7 t1 i <*> dsM_7 t2 i
dsM_7 (If3 e1 e2 e3) i = If3    <$> dsD_7 e1   <*> dsM_7 e2 i <*> dsM_7 e3 i

dsM_7 (Array ts) i = do
  xs <- mapM (\ t -> newIdent (getLoc t) "x") ts
  bs <- zipWithM dsM_7 ts (map (as_7 i) xs)
  pure $ Exists xs $ seqE [ unifyV (identOf_7 i) $ Array $ map Variable xs, Array bs]

dsM_7 (DefineIE j x t) i = do
  --traceM (show (j, x, t, i))
  t' <- dsM_7 (DefineE x t) i
  pure $ seqE [DefineE j (Variable (identOf_7 i)), t']

dsM_7 (For2 t1 t2) i = do
  t1' <- dsD_7 t1
  t2' <- dsD_7 t2
  pure $ unifyV (identOf_7 i) $ For2 t1' t2'

dsM_7 Fail i = pure $ unifyV (identOf_7 i) Fail
dsM_7 e@(Lit _) i = pure $ unifyV (identOf_7 i) e
dsM_7 e@(Variable _) i = pure $ unifyV (identOf_7 i) e
dsM_7 (ApplyD t1 t2) i = unifyV (identOf_7 i) <$> (ApplyD <$> dsD_7 t1 <*> dsD_7 t2)
-- This is a hack for succeeds of applications
dsM_7 (Succeeds (ApplyD t1 t2)) i = do
  t1' <- dsD_7 t1
  t2' <- dsD_7 t2
  pure $ unifyV (identOf_7 i) $ Succeeds $ ApplyD t1' t2'

-- This isn't right.  We might have to keep ApplyS around longer.
{-
dsM_7 (Succeeds e) i = do
  e' <- dsM_7 e i
  lowerSucceeds e'
-}

dsM_7 e@(Macro1 (Ident _ "lowered") [] _) i = pure $ unifyV (identOf_7 i) e
dsM_7 (Macro1 m rs e) i = Macro1 m rs <$> dsM_7 e i

dsM_7 e _ = error $ "dsM_7: unimplemented " ++ show e
