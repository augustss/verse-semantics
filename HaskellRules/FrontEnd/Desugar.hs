{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module FrontEnd.Desugar(
  DHow(..), desugarSmall, desugar,
  primOps, covariantId, dsScope,
  simplify,
  ) where
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack
--import Epic.List
import Epic.Print
--import FrontEnd.Desugar
import FrontEnd.Error
import FrontEnd.Expr

desugarSmall :: DHow -> Expr -> Expr
desugarSmall h = eval h . (dsSmall <=< dropParens)

desugar :: DHow -> Expr -> Expr
desugar h = eval h .
            (simp <=<
             traceDS "addScope" <=< addScope <=<
             traceDS "dsD" <=< dsD <=<
             traceDS "addDeref" <=< addDeref <=<
             traceDS "dsSmall" <=< dsSmall <=<
             traceDS "dropParens" <=< dropParens)

traceDS :: String -> Expr -> D Expr
traceDS _msg e = --trace ("---- " ++ _msg ++ "\n" ++ prettyShow e) $
                pure e

------

type D = State DState

-- XXX Do we really need the distinction between abstracttion and evaluation
-- context?
-- Right now it is used to guide desugaring to avoid an uninstantiated exist
-- in the function desugaring.

data DState = DState { nextNo :: !Int, how :: !DHow, context :: DContext }
  deriving (Show)
data DHow = DRun | DVerify
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

eval :: DHow -> D Expr -> Expr
eval h = flip evalState DState{ nextNo = 1, how = h, context = DEval }

-- Desugar into Small Source Verse
dsSmall :: Expr -> D Expr
dsSmall = ds
  where
    ds :: Expr -> D Expr
    -- Application and unification
    ds (InfixOp e1 (Op "where") e2) = do
      x <- newIdent (getLoc e1) "x"
      ds $ seqE [Define x e1, e2, Variable x]
    ds (InfixOp e1@Variable{} (Op "=") e2) = ds $ Unify e1 e2
    ds (InfixOp e1            (Op "=") e2) = do x <- newIdent (getLoc e1) "x"; ds $ seqE [Define x e1, Unify (Variable x) e2]
    ds (ApplyD  e1 e2) = join (apply ApplyD <$> ds e1 <*> ds e2)
    ds (ApplyS  e1 e2) = join (apply applyS <$> ds e1 <*> ds e2)
      where applyS x y = Succeeds (ApplyD x y)

    -- Bindings
    ds (InfixOp e1 o@(Op ":")  e2) = ds =<< defn e1 (PrefixOp o e2)
    ds (InfixOp e1   (Op ":=") e2) = ds =<< defn e1 e2

    -- Function notation
    ds (Typedef e) = do y <- newIdent (getLoc e) "y"; ds $ Function [(Define y e, [])] (Variable y)
    ds (InfixOp e1 (Op "=>") e2) = ds $ Function [(e1, [])] e2
    ds (Function (a:as@(_:_)) b) = ds $ Function [a] $ Function as b
    -- ds Function [] ...
    -- XXX effects
    ds (If1 e) = ds $ If2E e eFalse
    ds (If2 e1 e2) = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2) = do x <- newIdent (getLoc e1) "x"; ds $ If3 (Define x e1) (Variable x) e2
    ds (For1 e) = do x <- newIdent (getLoc e) "x"; ds $ For2 (Define x e) (Variable x)

    -- Operators
    ds (PrefixOp (Op "not") e) = do e' <- ds e; pure $ If3 e' eFail eFalse
    ds (PrefixOp (Op ":") e) = Range <$> ds e
    ds (PrefixOp (Ident l op) e) = ds (call "pre" l op e)
    ds (PostfixOp e (Op "?")) = Range <$> ds e
    ds (PostfixOp e (Ident l op)) = ds (call "post" l op e)
    ds (InfixOp e1 (Op "|") e2) = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2) = ds $ Seq [e1, e2]                  -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 eFail) eFail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2) = ds $ If2E e1 $ If2E e2 eFail
    ds (InfixOp e1 (Ident l op) e2) = ds (call "in" l op (Array [e1, e2]))

    -- Let do case
    ds (Case1 b) = do
      let l = getLoc b
      x <- Variable <$> newIdent l "x"
      ds $ Function [(InfixOp x (Op ":") eAny, [])] $ Case2 x b
    ds (Case2 _ _) = undefined
    ds (Block es) = ds $ seqE es

    ds (Seq es) = seqE <$> mapM ds es
    ds (HasType e1 e2) = join (apply HasType <$> ds e1 <*> ds e2)

    -- Misc
    ds (Option Nothing) = pure eFalse
    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (Define t e) (Array [Variable t])

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
  pure $ seqE [Define f e1, r]

apply1 :: (Value -> Value -> Expr) -> Value -> Expr -> D Expr
-- val1[val2] 
apply1 con x1 e2 | isValue e2 = apply2 con x1 e2   -- Easy special case.  Not really needed
-- val1[e2]  -->  a:=e2; val1[a]
apply1 con x1 e2 = do
  a <- newIdent (getLoc e2) "a"
  r <- apply2 con x1 (Variable a)
  pure $ seqE [Define a e2, r]

-- val1[val2]  --> 
apply2 :: (Value -> Value -> Expr) -> Value -> Value -> D Expr
apply2 con x1 x2 = pure $ con x1 x2

defn :: Expr -> Expr -> D Expr
-- Rule: (i := e) -->  (i := e)
defn (Variable i) e = pure $ Define i e
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
defn (Array ps) e = do
  xs <- mapM (\ p -> newIdent (getLoc p) "x") ps
  bs <- zipWithM defn ps (map Variable xs)
  let es = map (\ x -> InfixOp (Variable x) (Op ":") eAny) xs
  pure $ Seq $ [InfixOp (Array es) (Op "=") e] ++ bs
-- Rule (p1 -> p2) := e  -->  p1 := x1; p2 := x2; (x1 -> x2) := e
defn (InfixOp (Variable x1) (Op "->") (Variable x2)) e = pure $ Define2 x1 x2 e
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

---------------------------------------------------------------------------------

dsD :: Expr -> D Expr
dsD e | isValue e = pure e
dsD e@(ApplyD _ _) = pure e
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
dsM i e@(ApplyD _ _) = pure $ unifyV i e
-- Rule:  i |> x = t   -->  x = (i |> t)
dsM i (Unify x t) | isValue x = Unify x <$> dsM i t
-- Rule:  i |> x := t  -->  x := (i |> t)
dsM i (Define x t) = Define x <$> dsM i t
-- Rule:  i |> (j->x) := t  -->  j := i; x := (i |> t)
dsM i (Define2 j x t) = do
  t' <- dsM i t
  pure $ seqE [Define j (Variable i), Define x t']
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
dsM i (If3 e1 e2 e3) = If3 <$> dsD e1 <*> dsM i e2 <*> dsM i e3
dsM i (For2 e1 e2) = unifyV i <$> (For2 <$> dsD e1 <*> dsD e2)
dsM i (Function [(t1, r)] t2) = do
  h <- gets how
  c <- gets context
  dsFunction h c i t1 r t2
dsM i af@(HasType a f) | isValue f && isValue a = pure $ unifyV i af
dsM i (Macro1 m rs e) = unifyV i . Macro1 m rs <$> dsD e  -- XXX
dsM _ e = impossible e

dsFunction :: DHow -> DContext -> Ident -> Expr -> [Eff] -> Expr -> D Expr
--dsFunction DVerify _ _ _ _ _ = error "function desugaring for verification no implemented"
dsFunction _ DEval i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  t1' <- withContext DAbstract $ dsM x t1
  t2' <- dsD t2
  pure $ unifyV i $  -- Do the unification?
         TLam x effs t1' t2'
dsFunction _ DAbstract i t1 effs t2 = do
  x <- newIdent (getLoc t1) "x"
  y <- newIdent (getLoc t1) "y"
  z <- newIdent (getLoc t1) "z"
  t1' <- dsM x t1
  t2' <- withContext DEval $ dsM y t2
  pure $ TLam x effs (Define z t1') (Seq [Define y (ApplyD (Variable i) (Variable z)), t2'])

{-
dsMatch (Block es) v = dsMatch (Seq es) v
-- Rule: function(e1)<rs>{e2} :- v  -->  lambda x rs (exists y . y = (e1 :- x)) (exists q . q = v[y]; (e2 :- q))
dsMatch (Function [(e1,rs)] e2) v = do
  x <- newIdent (getLoc e1) "x"
  y <- newIdent (getLoc e1) "y"
  q <- newIdent (getLoc e1) "q"
  d1 <- dsMatch e1 x
  d2 <- dsMatch e2 q
  pure $ Lambda x rs (Define y d1) $ Seq [Define q (ApplyD (Variable v) (Variable y)), d2]
dsMatch (Function (a:as) e) v = dsMatch (Function [a] (Function as e)) v
dsMatch (ApplyEff rs e) v = ApplyEff rs <$> dsMatch e v
dsMatch (Succeeds e) v = ApplyEff [Ident noLoc "succeeds"] <$> dsMatch e v
dsMatch e _ = error $ "dsMatch: " ++ prettyShow e
-}


{-
applyEff :: [Eff] -> Expr -> Expr
applyEff rs (ApplyEff rs' e) = ApplyEff (rs ++ rs') e
applyEff rs e = ApplyEff rs e
-}

unifyV :: Ident -> Expr -> Expr
unifyV i e = Unify (Variable i) e

existsV :: [Ident] -> Expr -> Expr
existsV is e = --seqE $ map (\ i -> Define i AnyT) is ++ [e]
               Exists is e

{-
unifyV :: Ident -> Expr -> Expr
unifyV = Define

existsV :: [Ident] -> Expr -> Expr
existsV _is e = e
-}

{-
-- Hackily move exists in a lambda domain to the top
existsHack :: Expr -> D Expr
existsHack = pure . f
  where f (Lambda i rs d r) = Lambda i rs (pullExists d) (f r)
        f e = composOp f e
        pullExists (Exists is e) = Exists (is ++ is') e' where (is', e') = pull e
        pullExists _ = undefined
        pull (Exists is e) = (is ++ is', e') where (is', e') = pull e
        pull (Seq []) = ([], Seq [])
        pull (Seq (e:es)) = (is ++ is', Seq (e' : es'))
          where (is, e') = pull e
                (is', Seq es') = pull (Seq es)
        pull (Unify v e) = (is, Unify v e') where (is, e') = pull e
        pull e = ([], e)
-}

-- Pick the appropriate form of apply for operators
call :: String -> Loc -> String -> Expr -> Expr
call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "pre'^'", "pre'[]'", "post'^'",  -- no need for succeeds
                             "pre'+'","pre'-'",  -- XXX not really right
                             "in'+'","in'-'","in'*'",  -- XXX not really right
                             "in'+='", "in'-='", "in'*='", "in'/='", "in'.='",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='",
                             "length","in'..'"] = ApplyD
                | otherwise = ApplyS
            s' = p ++ "'" ++ s ++ "'"

----------------------------------------------

dsScope :: Expr -> Expr
dsScope = eval DRun . addScope

addScope :: Expr -> D Expr
addScope e = scope (S.fromList $ prel ++ primOps) (Do e)
 where prel = if Ident noLoc "PRELUDE" `elem` getVisible e then [] else prelude

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
    expr (Define i e) = Unify (Variable i) <$> expr e
    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr (Macro1 m [] e1) = Macro1 m [] <$> exprD e1
    expr Macro1 {} = unimplemented "Macro1 with effects"
    expr (Lambda i r e1 e2) = do
      (e1', sc') <- defs (S.insert i sc) e1
      Lambda i r e1' <$> scope sc' (Do e2)
    expr e@EmptyT = pure e
    expr (HasType e1 e2) = HasType <$> expr e1 <*> expr e2
    expr (Lam i e) = Lam i <$> scopeD (S.insert i sc) e
    expr (TLam i r e1 e2) = do
      (e1', sc') <- defs (S.insert i sc) e1
      TLam i r e1' <$> scopeD sc' e2
    expr (Exists _ e) = expr e
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
getVisible (Where e1 e2) = getVisible e1 ++ getVisible e2
--getVisible (Typedef _) = []
getVisible Macro1 {} = []
getVisible (Define i e) = i : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible EmptyT = []
getVisible Function{} = []
getVisible Lambda{} = []
getVisible (Exists is e) = is ++ getVisible e
getVisible (HasType e1 e2) = getVisible e1 ++ getVisible e2
getVisible Lam{} = []
getVisible TLam{} = []
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
getVar (Where e1 e2) = getVar e1 ++ getVar e2
--getVar (Typedef _) = []
getVar Macro1 {} = []
getVar (Define _ e) = getVar e
getVar (Define2 _ _ e) = getVar e
getVar Choice{} = []
getVar (Set _ _ e) = getVar e
getVar (MVar i t e) = i : maybe [] getVar t ++ maybe [] getVar e
getVar (Range e) = getVar e
getVar EmptyT = []
getVar Function{} = []
getVar Lambda{} = []
getVar TLam{} = []
getVar (Exists _ e) = getVar e
getVar (HasType e t) = getVar e ++ getVar t
getVar e = impossible e

-- Definitions that should go in a Prelude
prelude :: [Ident]
prelude = map (Ident noLoc)
  [ "int", "float", "string", "any", "nat", "false", "rational"
  , "in'->'"
  , "in'<'", "in'<='", "in'>'", "in'>='"
  , "in'.='", "new"
  , "pre'?'", "pre'[]'", "post'^'"
  ]

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
--  , "intGT$", "intGE$", "intLT$", "intLE$"
  , "in'..'"
  , "in'+='", "in'-='", "in'*='", "in'/='"
  , "print$"
  ]

------------------------

simp :: Expr -> D Expr
simp = f
  where f (ApplyD (Variable (Ident _ "any")) e) = f e
        f e = compos f e

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
    expr s (Where e1 e2) = Where (expr s e1) (expr s e2)
    expr s (Define i e) = Define i (expr s e)
    expr s (Define2 i j e) = Define2 i j (expr s e)
    expr s (Choice e1 e2) = Choice (exprD s e1) (exprD s e2)
    expr s (Set e1 (Ident l sop) e2) = set s e1 op (expr s e2)
      where op = Ident l ("in'" ++ sop ++ "'")
    expr s (MVar i (Just t) (Just e)) = Define i $ ApplyD (applyPrimD "new" (expr s t)) (expr s e)
    expr s (Range e1) = Range (expr s e1)
--    expr s (Typedef e1) = Typedef (exprD s e1)
    expr s (Macro1 m rs e1) = Macro1 m rs (exprD s e1)
    expr s (Lambda i rs e1 e2) = Lambda i rs (expr s' e1) (expr s' e2)
      where s' = defs s e1
    expr s (TLam i rs e1 e2) = TLam i rs (expr s' e1) (expr s' e2)
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

