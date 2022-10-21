{-# LANGUAGE TupleSections #-}
module Desugar(desugar, simplify, primOps, getVisible, covariantId) where
--import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.List
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
desugar = eval . (anfS <=< dsDo <=< scopeCheck <=< addDeref <=< dsD <=< dropParens)
  where eval = flip evalState 1

type D = State Int

type SExpr = Expr   -- Simple Expr: only has some of the constructors

dropParens :: Expr -> D Expr
dropParens = f
  where f (Parens e) = f e
        f (InfixOp (InfixOp (Variable i1) (Ident l2 ":") e2) (Ident l3  "=") e3) =
          f $ InfixOp (InfixOp (Variable i1) (Ident l2 ":") e2) (Ident l3 ":=") e3
        f e = compos f e

-- This follows the D transformation in calculus.ltx
dsD :: Expr -> D SExpr
dsD = expr
  where
    expr :: HasCallStack => Expr -> D Expr
    expr e | doTrace && trace ("dsD " ++ prettyShow e) False = undefined

    -- Basic forms
    -- D[k] = k
    expr e | isLiteral e = pure e
    -- D[false] = ()   This isn't necessary, but makes simple examples nicer
    expr (Variable (Ident _ "false")) = pure $ Array []
    -- D[x] = x
    expr e@Variable{} = pure e
    -- D[e1,...,en] = ???
    -- FIX D: update for splices
    -- YYY needs work
    expr (Array es) = arrSplice $ exprElems es
    -- D[e1;...;en] = D[e1]; ...; D[en]
    expr (Seq es) = seqE <$> mapM expr es
    -- D[e1(e2)] = D[e1](D[e2])
    expr (ApplyS e1 e2) = ApplyS <$> expr e1 <*> expr e2
    -- D[e1[e2]] = D[e1][D[e2]]
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2
    -- D[eff(rs){e}] = eff(rs){D[e]}
    expr (ApplyEff rs e) = ApplyEff rs <$> expr e
    -- Attributes are only allow for function definitions
    expr e@(EffAttr _ (Ident l _)) = syntaxError l $ "attribute not allowed: " ++ prettyShow e
    -- D[e1 = e2] = D[e1] = D[e2], also change constructor
    expr (InfixOp e1 (Ident _ "=") e2) = unify <$> expr e1 <*> expr e2
    -- D[e1 | e2] = D[e1] | D[e2], also change constructor
    expr (InfixOp e1 (Ident _ "|") e2) = Choice <$> expr e1 <*> expr e2
    -- D[e1 where e2] = D[e1] where D[e2], also change constructor
    expr (InfixOp e1 (Ident _ "where") e2) = Where <$> expr e1 <*> expr e2
    -- D[e1 :- e2] = M[e2] D[e1]
    expr (InfixOp e1 (Ident l ":-") e2) = do
      e1' <- expr e1
      e2' <- expr e2
      y <- newIdent l "y"
      e2'' <- dsM e2' (Variable y)
      pure $ Seq [define l y e1', e2'']

    -- Bindings
    -- D [lhs : t] = L[lhs] D[t]
{-
    expr (InfixOp lhs (Ident l ":") t) = dsL l lhs =<< expr t
    expr (InfixOp lhs (Ident l ":=") e)
    -- D [lhs := e] = D[lhs := type{e}]     in an abstraction context
      | ctx == Abs = expr $ InfixOp lhs (Ident l ":") (Typedef e)
    -- D [lhs := e] = P[lhs] e
      | otherwise = dsP l lhs e
-}
    expr (InfixOp lhs (Ident l ":") t) = dsP l lhs' (Range t')
      where (lhs', t') = dsColon l lhs t
    expr (InfixOp lhs (Ident l ":=") e) = dsP l lhs e

    -- Functions
    -- D[e1 => e2] = D[fn(e1){e2}]
    expr (InfixOp e1 (Ident _ "=>") e2) = expr $ Function [(e1, [covariantId])] e2
    -- See below
    expr (Function [(e, rs)] b)
      | all isLambdaEffect rs = function e rs b
      | otherwise = expr $ Function [(e, rs')] $ ApplyEff rs'' b
      where (rs', rs'') = partition isLambdaEffect rs
    -- D[fn a1 a2 ... {b}] = D[fn a1 (fn a2 ... {e})]
    expr (Function (a:as) b) = expr $ Function [a] $ Function as b

    -- Types
    -- D[:t] = : D[t], also change constructor
    expr (PrefixOp (Ident _ ":") t) = Range <$> expr t
    -- D[type{e}] = type{D[e]}
    expr (Typedef e) = do
      y <- newIdent noLoc "y"
      r <- newIdent noLoc "r"
      e' <- expr e
      e'' <- define noLoc r <$> dsM e' (Variable y)
      pure $ -- primFcn y e'' --  Lambda y [] e'' (Variable y)
             -- Function [(tAny noLoc y, [])] e
             Lambda y [covariantId] e'' (Variable r)
    expr (Macro1 m rs e) = Macro1 m rs <$> expr e

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
    -- D[not e] = D[if(e)then :false else false]
    expr (PrefixOp (Ident _ "not") e) = If3 <$> expr e <*> pure Fail <*> pure Unit
    -- D[op e] = op D[e]
    expr (PrefixOp (Ident l op) e) = expr $ call "pre" l op e
    -- D[e op] = D[e] op
    expr (PostfixOp e (Ident l op)) = expr $ call "post" l op e
    expr (InfixOp e1 (Ident l op) e2) =
      case op of
        -- D[e1 and e2] = D[e1]; D[e2]
        "and" -> expr $ seqE [e1, e2]
        -- D[e1 or e2] = D[if(e1)else e2]
        "or" -> expr $ If2E e1 e2
        -- D[e1 op e2] = D[e1] op D[e2]
        _    -> expr $ call "in" l op $ Array [e1, e2]

    -- Let, do, case
    -- 'let' kept until after scope check
    expr (Let e1 e2) = Let <$> expr e1 <*> expr e2
    -- 'do' kept until after scope check
    expr (Do e) = Do <$> expr e  -- XXX scope check?
    -- D[case {b}] = D[(x:any) => case(x)of b]
    expr (Case1 b) =
      newIdent noLoc "d" >>= \ i -> expr $ InfixOp (tAny noLoc i) (Ident noLoc "=>") $ Case2 (Variable i) b
    -- D[case(e)of b] = ...
    expr (Case2 e1 e2) = dsCase e1 e2

    -- Misc
    expr (Block es) = expr $ seqE es

    -- FIX D: add
    -- D[option{}] = false
    expr (Option Nothing) = pure Unit
    -- D[option{e}] = D[if(x:=e)then truth(e)
    expr (Option (Just e)) = inVar e >>= \ (t, e') -> expr $ If2 e' --(ApplyD eTruth t)
                                                                    (Array [t])

{-
    expr (Set e1 (Ident l "=") e2) =
      dsD $ ApplyD (eAssign l) $ Array [e1, e2]
-}
    expr (Set e1 op e2) = Set <$> expr e1 <*> pure op <*> expr e2
    expr (MVar i t e) = MVar i <$> traverse expr t <*> traverse expr e

    -- Make it idempotent
    expr (Define i e) = Define i <$> expr e
    expr (Define2 i j e) = Define2 i j <$> expr e
    expr (Choice e1 e2) = Choice <$> expr e1 <*> expr e2
    expr (Unify e1 e2) = unify <$> expr e1 <*> expr e2
    expr (Where e1 e2) = Where <$> expr e1 <*> expr e2
    expr (Range e) = Range <$> expr e
    expr AnyT = pure AnyT

    expr e = impossible e

    -- Pick the appropriate form of apply for operators
    call p l s e = con (Variable (Ident l s')) e
      where con | s' `elem` ["in'/'","pre'!'","post'?'",
                             "pre'^'", "pre'[]'", "post'^'",  -- no need for succeeds
                             "pre'+'",  -- XXX not really right
                             "in'+='", "in'-='", "in'*='", "in'/='", "in'.='",
                             "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='",
                             "length","in'..'"] = ApplyD
                | otherwise = ApplyS
            s' = p ++ "'" ++ s ++ "'"

    -- Handle function(e){b}
    --function e b | trace ("function " ++ show (e, b)) False = undefined
    function (InfixOp (Variable y) (Ident _ ":") (Variable (Ident _ "any"))) [] b =
      Lambda y [] (Array []) <$> expr b
      --primFcn y <$> expr b
    function e rs b = do
      y <- newIdent noLoc "y"
      e' <- expr e
      e'' <- dsM e' (Variable y)
      b' <- expr b
      pure $ Lambda y rs e'' b'

    -- Splice together ArrayElems
    arrSplice :: [ArrayElem] -> D SExpr
    arrSplice [] = pure $ Array []
    arrSplice [EElems es] = Array <$> mapM expr es  -- no splices
    arrSplice as = applyPrim "concat$" <$> Array <$> mapM (expr . f) as
      where f (EElems es) = Array es
            f (ESplice e) = e

unify :: Expr -> Expr -> Expr
unify AnyT e = e
unify e AnyT = e
unify e1 e2 = Unify e1 e2

--primFcn :: Ident -> SExpr -> SExpr
--primFcn y e = Function [(tAny noLoc y, [])] e

dsM :: SExpr -> SExpr -> D SExpr
dsM expr y =
  case expr of
    e | isLiteral e -> dflt
    Variable{} -> dflt
    Array es -> do
      xs <- mapM (const $ newIdent noLoc "y") es
      es' <- zipWithM dsM es (map Variable xs)
      pure $ seqE [unify y (Array $ map (tAny noLoc) xs), Array es']
    Seq (Snoc es e) -> do
      e' <- dsM e y
      pure $ Seq (Snoc es e')
    ApplyS{} -> dflt
    ApplyD{} -> dflt
    ApplyEff{} -> dflt
    If3 e1 e2 e3 -> If3 e1 <$> dsM e2 y <*> dsM e3 y
    For2 e1 e2 -> do
      let l = noLoc
      e1' <- dsD e1
      a <- newIdent l "a"
      x <- newIdent l "x"
      e2' <- dsM e2 (Variable x)
      let body = Function [(tAny l x, [])] e2'
          adef = define l a $ For2 e1' body
          test = Unify (applyPrimD "length" (Variable a)) (applyPrimD "length" y)
      i <- newIdent l "i"
      f <- newIdent l "f"
      let res = For2 (define l f $ ApplyD (Variable a) (tAny l i)) $ ApplyD (Variable f) (ApplyD y (Variable i))
      pure $ Seq [adef, test, res]
    Let e b -> Let e <$> dsM b y
    Do e -> Do <$> dsM e y
    Where e1 e2 -> Where <$> dsM e1 y <*> pure e2
    Unify e1 e2 -> do
      e1' <- dsM e1 y
      e2' <- dsM e2 y
      pure $ unify e1' e2'
--    Typedef{} -> dflt
    Macro1{} -> dflt
    Define i e -> Define i <$> dsM e y
    Define2 j i e -> do
      e' <- dsM e y
      pure $ Seq [Define j y, Define i e']
    Choice e1 e2 -> Choice <$> dsM e1 y <*> dsM e2 y
    Range e -> pure $ ApplyD e y
    AnyT -> dflt
    Function [(fexpr, rs)] gexpr -> do
      known <- do
        let h = y
        vy <- newIdent noLoc "y"
        x <- newIdent noLoc "x"
        z <- newIdent noLoc "z"
        r <- newIdent noLoc "r"
        ex <- define noLoc x <$> dsM fexpr (Variable vy)
        let ez = define noLoc z $ ApplyD h (Variable x)
        er <- (define noLoc r . Do) <$> dsM gexpr (Variable z)
        pure $ -- Function [(tAny noLoc vy, rs)] $ seqE [ex, ez, er]
               Lambda vy rs (seqE [ex, ez, er]) (Variable r)
      unknown <-
        Unify y <$> dsD expr
      if useKnown then
        pure $ If3 (applyPrimD "known$" y) known unknown
       else
        pure known
    Lambda i rs e1 e2 -> dsM (Function [(Where (tAny noLoc i) e1, rs)] e2) y
    _ -> impossible expr
  where
    dflt = pure $ unify y expr

useKnown :: Bool
useKnown = False -- True

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

newInt :: D Int
newInt = do
  n <- get
  put $! n+1
  pure n

newIdent :: Loc -> String -> D Ident
newIdent l s = do
  n <- newInt
  pure $ Ident l $ "$" ++ s ++ show n

inVarM :: Expr -> D (Expr, Maybe Expr)
inVarM e@Variable{} = pure (e, Nothing)
inVarM e = do
  i <- newIdent noLoc "d"
  pure (Variable i, Just $ define noLoc i e)

inVar :: Expr -> D (Expr, Expr)
inVar e = (\ (e', me) -> (e', fromMaybe e' me)) <$> inVarM e

inVarC :: Expr -> (Expr -> D Expr) -> D Expr
inVarC e@Variable{} k = k e
inVarC e k = do
  i <- newIdent noLoc "d"
  ke <- k (Variable i)
  pure $ seqE [define noLoc i e, ke]

dsCase :: Expr -> Block -> D Expr
-- D[case x of e1; ... en] = e1[x] || ... || en[x]
dsCase e@Variable{} (Block es) = dsD $ foldr mkOr Fail es
  where mkOr a r = If2E (ApplyD a e) r
dsCase Variable{} _ = internalError
-- D[case e of b] = x=D[e]; D[case x of b]
dsCase e b = do
  e' <- dsD e
  inVarC e' $ \ x -> dsCase x b

--eAssign :: Loc -> Expr
--eAssign l = Variable (Ident l "write$")

--eTruth :: Expr
--eTruth = Variable (Ident noLoc "truth")

define :: Loc -> Ident -> SExpr -> SExpr
define _l i e = Define i e

tAny :: Loc -> Ident -> SExpr
tAny l i = define l i AnyT

-- Desugar a definition e1 : e2
dsColon :: Loc -> Expr -> SExpr -> (SExpr, SExpr)
-- L[x] t = P (x := :t)
dsColon _ x@Variable{} t =
  (x, t)
-- L[l(a) t = L[l] (type{function(a){:t}})
dsColon l f@(ApplyS _ _) t | Just (lhs, arg, effs) <- getFun f = do
  dsColon l lhs (Typedef (Function [(arg, effs)] (Range t)))
-- L[l^] t = L[l] new[t]
dsColon l (PostfixOp f (Ident _ "^")) t =
  dsColon l f (applyPrimD "new" t)
-- L[l?] t = L[l] (?t)
dsColon l (PostfixOp f op@(Ident _ "?")) t =
  dsColon l f (PrefixOp op t)
-- L[l[]] t = L[l] ([]t)
dsColon l (ApplyD f (Array [])) t =
  dsColon l f (PrefixOp (Ident l "[]") t)
-- L[p~>q] t = P[p~>q] (:t)
dsColon _ p@(InfixOp _ (Ident _ "->") _) t =
  (p, t)
dsColon l f _ = syntaxError l $ "bad LHS of :, " ++ prettyShow f

-- Return function, argument, and attributes
getFun :: Expr -> Maybe (Expr, Expr, [Ident])
getFun = gf []
  where
    gf rs (EffAttr e r) = gf (r:rs) e
    gf rs (ApplyS f a) = Just (f, a, reverse rs)
    gf _ _ = Nothing

-- Desugar a definition lhs := e
dsP :: Loc -> Expr -> Expr -> D SExpr
dsP _ e1 e2 | doTrace && trace ("dsP " ++ prettyShow (e1, e2)) False = undefined
-- P[f(a)<r>...] e = P[f] (function(a)<r>...{e2})
dsP l e1 e2 | Just (f, a, rs) <- getFun e1 = dsP l f $ Function [(a, rs)] e2
-- P[x] e = x := D[e]
dsP l (Variable x) e = define l x <$> dsD e
-- P[:t] e = P[x:t] e, x fresh
dsP l (PrefixOp colon@(Ident _ ":") t) e = do
  x <- newIdent l "x"
  dsP l (InfixOp (Variable x) colon t) e
-- P[x:t] e = P[x] t[e]
dsP l (InfixOp p@Variable{} (Ident _ ":") t) e = do
  dsP l p (ApplyD t e)
-- P[l:t] e = P[l] t[e]
dsP l (InfixOp p (Ident _ ":") t) e = do
  dsP l p (ApplyD t e)
{-
dsP l (InfixOp p (Ident _ ":") t) e = do
  let (p', t') = dsColon l p t
  dsP l p' (ApplyD t' e)
-}
{-
  x <- newIdent l "x"
  p' <- dsP l p =<< dsD (ApplyD t (define l x e))
  pure $ seqE [p', Variable x]
-}
-- P[l?] e = P[l] option{e}
dsP l (PostfixOp lhs (Ident _ "?")) e =
  dsP l lhs (Option $ Just e)
{-
-- P[e1^] e = D[assign(e1, e)]
dsP l (PostfixOp e1 (Ident _ "^")) e = do
  dsD $ ApplyD (eAssign l) $ Array [e1, e]
-}
-- FIX L: update for splices
-- P[lhs1, ... lhsn] = ...
dsP l (Array lhss) e = dsPArr l lhss e
dsP l (InfixOp i (Ident _ "->") x) e = do
  (i', di) <-
    case i of
      Variable i' -> pure (i', [])
      _ -> do i' <- newIdent l "i"; di <- dsP l i (Variable i'); pure (i', [di])
  (x', dx) <-
    case x of
      Variable x' -> pure (x', [])
      _ -> do x' <- newIdent l "x"; dx <- dsP l x (Variable x'); pure (x', [dx])
  e' <- dsD e
  pure $ seqE $ di ++ dx ++ [Define2 i' x' e']

-- What else is allowed?  LitInt and LitRat would be easy.
dsP l x y = syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow x ++ ", RHS=" ++ prettyShow y

-- Handle ..l, l
dsPArr :: Loc -> [Expr] -> SExpr -> D SExpr
dsPArr l lhss ea = do
  e <- dsD ea
  case exprElems lhss of
    -- P[lhs0,...,lhsn] e = P[lhs0]x0; ...; P[lhsn]xn; (x0:any,...,xn:any) = e
    [EElems ls] -> do
      xs <- mapM (const $ newIdent l "d") ls
      let eun = Unify (Array (map (tAny l) xs)) e
      els <- zipWithM (\ lhs x -> dsP l lhs (Variable x)) ls xs
      pure $ Seq $ els ++ [eun]

    [ESplice lhs] ->
      dsP l lhs ea

    [EElems ls1, ESplice lhs] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent l "d"; pure (Variable v, [define l v e])
      let
        len1 = toInteger $ length ls1
        a1  = applyPrimD "takeL$" $ Array [LitInt len1, v]
        e'  = applyPrimD "dropL$" $ Array [LitInt len1, v]
      v' <- newIdent l "d"
      let
        bv' = define l v' e'
      m1 <- dsP l (Array ls1) a1
      mm <- dsP l lhs e'
      pure $ Seq $ bv ++ [m1, bv', mm, v]

    [ESplice lhs, EElems ls2] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent l "d"; pure (Variable v, [define l v e])
      let
        len2 = toInteger $ length ls2
        a2  = applyPrimD "takeR$" $ Array [LitInt len2, v]
        e'' = applyPrimD "dropR$" $ Array [LitInt len2, v]
      mm <- dsP l lhs e''
      m2 <- dsP l (Array ls2) a2
      pure $ Seq $ bv ++ [mm, m2, v]

    [EElems ls1, ESplice lhs, EElems ls2] -> do
      (v, bv) <- case e of Variable{} -> pure (e, []); _ -> do v <- newIdent l "d"; pure (Variable v, [define l v e])
      let
        len1 = toInteger $ length ls1
        len2 = toInteger $ length ls2
        a1  = applyPrimD "takeL$" $ Array [LitInt len1, v]
        e'  = applyPrimD "dropL$" $ Array [LitInt len1, v]
      v' <- newIdent l "d"
      let
        bv' = define l v' e'
        a2  = applyPrimD "takeR$" $ Array [LitInt len2, Variable v']
        e'' = applyPrimD "dropR$" $ Array [LitInt len2, Variable v']
      m1 <- dsP l (Array ls1) a1
      mm <- dsP l lhs e''
      m2 <- dsP l (Array ls2) a2
      pure $ Seq $ bv ++ [m1, bv', mm, m2, v]

    _ -> syntaxError l $ "Illegal LHS of ':=' " ++ prettyShow (Array lhss) ++ ", should only have one ..e"

-- Definitions that should go in a Prelude
prelude :: [Ident]
prelude = map (Ident noLoc)
  [ "int", "float", "string", "any", "nat", "false"
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
  , "wrong"
  , "in'+='", "in'-='", "in'*='", "in'/='"
  , "print$"
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
--    f (Do b) = f b
    -- D[let(e)in b] = D[e]; D[b]
    f (Let e b) = f $ seqE [e, b]

    -- D[e1 where e2] = D[x:= e1; e2; x]
    f (Where e1 e2) = do
      x <- newIdent noLoc "w"
      pure $ Seq [define noLoc x e1, e2, Variable x]

    -- D[:()] = empty
    f (Range (Array [])) =
      pure EmptyT
    -- D[:t] = t[x:any]
    f (Range e) = do
      r <- newIdent noLoc "r"
      e' <- f e
      pure $ ApplyD e' (tAny noLoc r)

    f (Define2 i x e) = do
      e' <- dsM e (Variable i)
      e'' <- f e'
      pure $ Seq [tAny noLoc i, Define x e'']

    f e = compos f e

--------------------
-- Make all Array take value arguments, as well as ApplyS/ApplyD
anfS :: Expr -> D Expr
anfS = anf
  where
    anf e@Array{} = val e
    anf (ApplyS e1 e2) = app ApplyS e1 e2
    anf (ApplyD e1 e2) = app ApplyD e1 e2
    anf (Unify e1 e2) = do
      e1' <- anf e1
      e2' <- anf e2
      pure $ Unify e1' e2'
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
--    value e@Typedef{} = ([],) <$> anf e
    value e@AnyT{} = pure ([], e)
    value (Define i e) = do
      -- Special version of next case; no need for a new variable
      e' <- anf e
      pure ([Define i e'], Variable i)
    value e = do
      i <- newIdent noLoc "a"
      e' <- anf e
      pure ([Define i e'], Variable i)
    app con e1 e2 = do
      (es1, e1') <- value e1
      (es2, e2') <- value e2
      pure $ seqE $ es1 ++ es2 ++ [con e1' e2']

------------

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
getVisible (Define2 i j e) = i : j : getVisible e
getVisible Choice{} = []
getVisible (Range e) = getVisible e
getVisible AnyT = []
getVisible EmptyT = []
getVisible Function{} = []
getVisible Lambda{} = []
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
getVar AnyT = []
getVar EmptyT = []
getVar Function{} = []
getVar Lambda{} = []
getVar e = impossible e

{-
getFree :: Expr -> [Ident]
getFree LitInt{} = []
getFree LitRat{} = []
getFree (Variable i) = [i]
getFree (Array es) = foldr union [] $ map getFree es
getFree (Seq es) = foldr union [] $ map getFree es
getFree (ApplyS e1 e2) = getFree e1 `union` getFree e2
getFree (ApplyD e1 e2) = getFree e1 `union` getFree e2
getFree (If3 e1 e2 e3) = getFreeD e1 `union` (getFreeD e2 \\ getVisible e1) `union` getFreeD e3
getFree (For2 e1 e2) = getFreeD e1 `union` (getFreeD e2 \\ getVisible e1)
getFree (Let e1 e2) = getFreeD e1 `union` (getFree e2 \\ getVisible e1)
getFree (Do e) = getFreeD e
getFree (Unify e1 e2) = getFree e1 `union` getFree e2
getFree (Typedef e) = getFreeD e
getFree (Define i e) = i : getFree e
getFree (Choice e1 e2) = getFreeD e1 `union` getFreeD e2
getFree (Range e) = getFree e
getFree AnyT = []
getFree (Function [(e,_)] b) = getFreeD b \\ getVisible e
getFree e = impossible e

getFreeD :: Expr -> [Ident]
getFreeD e = getFree e \\ getVisible e
-}

--------------

data ScopeErr
  = ErrMultiple [Ident]
  | ErrUndefined Ident
  | ErrShadow Ident
--  deriving (Show)

scopeCheck :: Expr -> D Expr
scopeCheck e = do
  let errs = scopeErrs (S.fromList $ prel ++ primOps) (Do e)
      -- HACK: Recognize when we have loaded prelude.verse
      prel = if Ident noLoc "PRELUDE" `elem` getVisible e then [] else prelude
  case [ is | ErrMultiple is <- errs ] of
    [] -> pure ()
    is : _ -> error $ "scopeCheck: Multiply defined " ++ prettyShow (head is) ++
                      prettyShow [ l | Ident l _ <- is ]
  case [ i | ErrUndefined i <- errs ] of
    [] -> pure ()
    -- Make it a trace instead of an error for now
    is -> mapM_ undef is
      where undef i@(Ident l _) = traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i)
  case [ i | ErrShadow i <- errs ] of
    [] -> pure e
    -- Make it a trace instead of an error for now
    iis -> trace ("scopeCheck: warning shadowing " ++ show iis) $
           -- XXX Here we should patch up the shadowing problem
           pure e

knownEffects :: [Ident]
knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts", "covariant"
  ]

isLambdaEffect :: Ident -> Bool
isLambdaEffect i = elem i [
  covariantId
  ]

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
    expr (ApplyEff is e) =
      [ErrUndefined i | i <- is \\ knownEffects ] ++
      expr e
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
    expr (Where e1 e2) = expr e1 ++ expr e2
    expr (Define _ e) = expr e
    expr (Define2 _ _ e) = expr e
    expr (Choice e1 e2) = expr (Do e1) ++ expr (Do e2)
    expr (Range e1) = expr e1
--    expr (Typedef e1) = expr (Do e1)
    expr (Macro1 _ [] e1) = expr (Do e1)
    expr Macro1 {} = unimplemented "Macro1 with effects"
    expr (Lambda i _ e1 e2) = errs ++ scopeErrs s'' e1 ++ scopeErrs s'' (Do e2)
      where (errs, s') = defs e1
            s'' = S.insert i s'
    expr AnyT = []
    expr e = impossible e

    defs :: Expr -> ([ScopeErr], S.Set Ident)
    defs e =
      let is = getVisible e
          errM = map ErrMultiple $ filter ((> 1) . length) $ group $ sort is
          errS = [ ErrShadow i | i <- is, i `S.member` s ]
          s' = foldr S.insert s is
      in  (errM ++ errS, s')

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
    expr _ AnyT = AnyT
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

getLoc :: Expr -> Loc
getLoc _ = noLoc

------------

simplify :: Expr -> Expr
simplify = simp
  where
    --simp (Unify e AnyT) = simp e
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

covariantId :: Ident
covariantId = Ident noLoc "covariant"
