{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack
import Epic.List
import Epic.Print
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
            (traceDS "dropPrel"   <=< dropPrel  <=<
             traceDS "alias"      <=< simpAlias <=<
             traceDS "simpler"    <=< simpler   <=<
             traceDS "primops"    <=< primops   <=<
             traceDS "lower"      <=< lower     <=<
             traceDS "addScope"   <=< addScope  <=<
             traceDS "lowerApply" <=< lowerApply<=<
             traceDS "dsD"        <=< dsD       <=<
             traceDS "addDeref"   <=< addDeref  <=<
             traceDS "dsSmall"    <=< dsSmall   <=<
             traceDS "addPrelude" <=< addPrelude<=<
             traceDS "dropParens" <=< dropParens)
  where
    hack = (traceDS "dsD"        <=< dsD       <=<
            traceDS "addDeref"   <=< addDeref  <=<
            traceDS "dsSmall"    <=< dsSmall   <=<
            traceDS "dropParens" <=< dropParens)

    tr = fTraceDesugar flgs
    traceDS :: String -> Expr -> D Expr
    traceDS msg e | tr = trace ("---- " ++ msg ++ "\n" ++ prettyShow e) $
                         pure e
                  | otherwise = pure e
    addPrelude e = pure $ Array [prel, e]
    prel = eval flgs $ dropParens $ snd $ fPrelude flgs
    prelIds = getVisible $ eval flgs $ hack prel
    dropPrel = pure . dropUnusedIds prelIds

-- Drop unused prelude identifiers.
-- Assumes input expression is of the for exists is . < <prels>, e >
dropUnusedIds :: [Ident] -> Expr -> Expr
dropUnusedIds pids (Exists is (Array [Array ps, e])) =
  let used = clos [] (getFree e)
        where clos us [] = us
              clos us (u : uis) = clos (u:us) (maybe [] getFree (lookup u pds) ++ uis)
              pds = [(p, d) | Unify (Variable p) d <- ps ]
      unused = pids \\ used
      usedPrel (Unify (Variable p) _) = p `notElem` unused
      usedPrel _ = True
  in  lExists (is \\ unused) $ seqE $ filter usedPrel ps ++ [e]
dropUnusedIds _ (Array [Array [], e]) = e
dropUnusedIds _ e =
  trace (prettyShow e) $
  impossible e

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

-- Do various early changes:
--  * (e)       -->  e             parens are there to stop the next from possibly firing
--  * e1:e2=e3  -->  e1:e2 := e3   XXX should we do this?
--  * (e1,...)  -->  array{e1,...} no need to distingush them anymore
--  * x&y:e     -->  x:e; y:e      if outside an array/tuple
--                   ..(x:e, y:e)  if inside an array/tuple
dropParens :: Expr -> D Expr
dropParens = f False
  where f a (Parens e) = f a e
        f a (InfixOp (InfixOp (Variable i1) o@(Op ":") e2) (Ident l3  "=") e3) =
          f a $ InfixOp (InfixOp (Variable i1) o e2) (Ident l3 ":=") e3
        f a (Tuple es) = f a (Array es)
        f _ (Array es) = Array <$> mapM (f True) es
        f a (InfixOp p@(InfixOp _ (Op "&") _) o@(Op ":" ) e) = f a =<< amp a p o e
        f a (InfixOp p@(InfixOp _ (Op "&") _) o@(Op ":=") e) = f a =<< amp a p o e
        f _ e = compos (f False) e

{- This code does not duplicate e, but it doesn't agree with Tim's implementation.
        amp a p o e@Variable{} = do
          let es = Array $ map (\ x -> InfixOp x o e) (getAmp p)
          pure $ if a then PrefixOp (Op "..") es else es
        amp a p o e = do
          x <- newIdent (getLoc e) "x"
          e' <- amp a p o (Variable x)
          pure $ Let (DefineE x e) e'
-}
        amp a p o e = do
          let es = Array $ map (\ x -> InfixOp x o e) (getAmp p)
          pure $ if a then PrefixOp (Op "..") es else es

        getAmp (InfixOp p1 (Op "&") p2) = getAmp p1 ++ getAmp p2
        getAmp x@(Variable _) = [x]
        getAmp e = errorMessage $ "Bad use of & " ++ prettyShow e
          
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
    ds (InfixOp e1 (Op "=") e2) = do e1' <- ds e1; e2' <- ds e2; dsU [e1', e2']
    ds (Macro1 (Ident _ "in'='") [] (Block es)) = dsU =<< mapM ds es
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
    ds (Macro1 (Ident _ "one") [] e) = ds $ If2E e Fail
    ds (Macro1 (Ident _ "all") [] e) = ds $ For1 e

    ds (Macro1 (Ident _ "first") [] e) = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    ds x = compos ds x

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
defn (InfixOp e1 (Op ":") e2) e = defn e1 (HasType e e2)
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

-- All cases, but the last, can be removed.
-- They are just there to avoid introducing unused existentials.
dsD :: Expr -> D Expr
dsD e | isValue e = pure e
dsD e@(ApplyD f a) | isValue f && isValue a = pure e
dsD e@(HasType f a) | isValue f && isValue a = pure e
dsD (Unify x e) | isValue x = Unify x <$> dsD e
dsD (DefineV x) = pure (DefineV x)
dsD (DefineE x e@Function{}) = do
  c <- gets context
  case c of
    -- In an evaluation context, just define the function normally.
    -- This isn't necessary, but avoids an extra exists.
    DEval ->     existsV [x] <$> dsM x e
    DAbstract -> DefineE x <$> dsD e
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
--dsM i af@(HasType a f) | isValue f && isValue a = pure $ unifyV i af
dsM i (HasType a f) = unifyV i <$> (HasType <$> dsD a <*> dsD f)
dsM i (Macro1 m rs e) = unifyV i . Macro1 m rs <$> dsD e  -- XXX
dsM i Fail = pure $ unifyV i Fail
dsM i (Lam x e) = unifyV i . Lam x <$> dsD e
dsM i (Let e1 e2) = Let <$> dsD e1 <*> dsM i e2
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

dsScope :: Flags -> Expr -> Expr
dsScope flgs = eval flgs . (primops <=< addScope)

addScope :: Expr -> D Expr
addScope e = scope (S.fromList primOps) (Do e)

_knownEffects :: [Ident]
_knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts"
  ] ++ [invariantId]

_isLambdaEffect :: Ident -> Bool
_isLambdaEffect i = elem i [
  invariantId
  ]

invariantId :: Ident
invariantId = Ident noLoc "invariant"

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

scope :: S.Set Ident -> Expr -> D Expr
scope sc = expr
  where
    expr e@Lit{} = pure e
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
      let Exists is e1'' = e1'
      e2' <- scope sc' e2
      pure $ Exists is $ seqE [e1'', e2']
    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2
    expr (DefineV i) = pure $ Variable i
    expr (DefineE i e) = Unify (Variable i) <$> expr e
    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr (Macro1 m [] e1) = Macro1 m [] <$> exprD e1
    expr Macro1 {} = unimplemented "Macro1 with effects"
    expr (HasType e1 e2) = HasType <$> expr e1 <*> expr e2
    expr (TLam i r e1 e2 me3) = do
      (e1', sc') <- defs (S.insert i sc) e1
      TLam i r e1' <$> scopeD sc' e2 <*> traverse (scopeD sc') me3
    expr (Exists _ e) = expr e
    expr (Lam i e) = Lam i <$> scopeD (S.insert i sc) e
    expr Fail = pure Fail
    expr e = impossible e

    exprD e = fst <$> defs sc e
    scopeD s e = fst <$> defs s e

    defs :: S.Set Ident -> Expr -> D (Expr, S.Set Ident)
    defs as e = do
      let is = getVisible e
          errM = filter ((> 1) . length) $ group $ sort is
          errS = [ (i, j) | i <- is, i `S.member` sc, j <- filter (== i) (S.toList sc) ]
          s' = foldr S.insert as is
      e' <- scope s' e
      errMultiple errM
      errShadow errS
      pure (Exists is e', s')


-- Get all visible identifiers from i := e
getVisible :: HasCallStack => Expr -> [Ident]
getVisible Lit{} = []
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
simpValue = pure . f
  where f (Seq (Snoc es e)) = seqE $ map f (Snoc (filter (not . isValue) es) e)
        f e = composOp f e

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
    expr _ Fail = Fail
    expr s (Lam i e) = Lam i (expr s e)
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
    f (HasType e t) = do
      verif <- gets (fVerify . dflags)
      if verif then
        HasType <$> f e <*> f t
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
lower (ApplyEff _rs _e) = undefined
lower (Unify e1 e2) = Unify <$> lower e1 <*> lower e2
lower (Choice e1 e2) = Choice <$> lower e1 <*> lower e2
lower (For2 (Exists is e1) e2) = join $ lowerFor is <$> lower e1 <*> lower e2
lower (If3 (Exists is e1) e2 e3) = join $ lowerIf is <$> lower e1 <*> lower e2 <*> lower e3
lower (Macro1 (Ident _ "all") [] e) = lowerAll =<< lower e
lower (Macro1 (Ident _ "one") [] e) = lowerOne =<< lower e
lower (Succeeds e) = lowerSucceeds =<< lower e
lower (Macro1 (Ident _ "decides") [] e) = lowerDecides =<< lower e
lower (Macro1 (Ident _ "assume") [] e) = lowerAssume =<< lower e
lower (Macro1 (Ident _ "lowered") [] e) = pure e
lower (Exists is e) = lExists is <$> lower e
lower (TLam i rs (Exists is e1) e2 me3) = join $ lowerTLam i rs is <$> lower e1 <*> lower e2 <*> traverse lower me3
lower (HasType e t) = join $ lowerHasType <$> lower e <*> lower t
lower (Lam i e) = Lam i <$> lower e
lower Fail = pure Fail
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

lowerTLam :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLam i rs is e1 e2 me3 = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerTLamVerify i rs is e1 e2 me3
   else
    lowerTLamRun i rs is e1 e2 me3

-- XXX what about _rs
lowerTLamVerify :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLamVerify i rs is e1 e2 me3 = do
  (e2', e2'') <-
    case me3 of
      Nothing -> pure (e2, e2)
      Just t -> do
        x <- newIdent (getLoc t) "x"
        pure (ApplyD t e2, Exists [x] $ ApplyD t (Variable x))
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
lowerTLamRun :: Ident -> [Eff] -> [Ident] -> Expr -> Expr -> Maybe Expr -> D Expr
lowerTLamRun i rs is e1 e2 me3 = do
  -- XXX This inserts Succeeds late, and scope insertion has already happened.
  -- XXX This might be wrong.
  e2' <- maybe (pure e2) (\ t -> lowerSucceeds (ApplyD t e2)) me3
  let invariant = --invariantId `elem` rs  || True -- XXX
                  openId `notElem` rs
  if null is && e1 == Array [] then
    pure $ Lam i e2'   -- Simple special case
   else
    if invariant then
      pure $ Lam i $ lExists is (seqE [e1, e2])
    else
      Lam i <$> lowerIf is e1 e2 DomainFail

lowerHasType :: Expr -> Expr -> D Expr
lowerHasType e t = do
  verif <- gets (fVerify . dflags)
  if verif then
    lowerHasTypeVerify e t
   else
    undefined -- lowerSucceeds (ApplyD t e)

lowerHasTypeVerify :: Expr -> Expr -> D Expr
lowerHasTypeVerify e t = do
  x <- newIdent (getLoc t) "x"
  pure $ Seq [ eVerify $ eAssert $ ApplyD t e, eAssume $ Exists [x] $ ApplyD t (Variable x) ]

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
  if verif then
    pure $ eAssert e
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
  if verif then
    unimplemented "verify-decides"
   else if useSplit then
    lowerDecidesSplit e
   else
    pure $ Macro1 (Ident noLoc "decides") [] e

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
eDecide = Macro1 (Ident noLoc "decide") []

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
    fvs (Wrong _) = []
    fvs (Macro1 _ _ e) = fvs e
    fvs (Split e1 e2 e3) = fvs e1 ++ fvs e2 ++ fvs e3
    fvs (If3 (Exists is e1) e2 e3) = fvs (Exists is (Seq [e1, e2])) ++ fvs e3
    fvs Fail = []
    fvs DomainFail = []
    fvs e = impossible e

-- XXX binders
getAllVars :: Core -> [Ident]
getAllVars = Epic.List.nub . execWriter . vars
  where vars e@(Variable i) = do tell [i]; pure e
        vars e@(Lam i e') = do tell [i]; _ <- vars e'; pure e
        vars e@(Exists is e') = do tell is; _ <- vars e'; pure e
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
