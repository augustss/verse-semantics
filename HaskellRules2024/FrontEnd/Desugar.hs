{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module FrontEnd.Desugar(
      desugar
    , D, runD, traceD, getDFlagsX
  ) where

import Prelude hiding (pi)

import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags
import Rules.Core( PrimOp, allPrimOps, primOpString )

-- Epic libraries
import Epic.Print

-- General libraries
import Data.Either
import Data.Maybe
import Data.IORef
import qualified Data.Set as S
import Control.Monad

--import qualified Data.Map as M

-- QUESTIONS:
--  x:int='a'   fail or wrong?, tests L93, L95

-- TODO:
--  Add Length
--  Add Err

-- TODO:
--  x:t=v is syntactic sugar for x:=(:t=v) and
--  :t=v is a special form meaning it's not the same as (:t)=v, which is just unification.
--  desugar function effects

-----------------------------------------------
--
--      The main desugaring pass: desugar
--
-----------------------------------------------


desugar :: Flags -> Bool -> SrcExpr -> IO SrcCore
desugar flgs add_verification
  = runD flgs .
     (-- Heavy lifting: Fig 9
         traceDS "Main desugaring"
     <=< dsDD_12 ds_model

     -- Side effects
--   <=<  traceDS "addDeref"
--   <=< addDeref

     -- Desugar into Small Source
     <=< traceDS "Desugar to Small Source"
     <=< dsSmall

     -- Prepends prelude from
     --    verifyprelude.verse, mediumprelude.verse
     <=< traceDS "Add prelude"
     <=< addPrelude

     -- Syntax fixes
     <=< traceDS "syntaxFixes"
     <=< syntaxFixes)
  where
    ds_model | add_verification = MV
             | otherwise        = MX

--------------------------------------------------------
--
--         Desugar into Small Source Verse
--
--------------------------------------------------------

dsSmall :: SrcExpr -> D SrcExpr
dsSmall = ds
  where
    ds :: SrcExpr -> D SrcExpr

    -- Application
    ds (ApplyD  e1 e2) = join (apply ApplyD <$> ds e1 <*> ds e2)
    ds (ApplyS  e1 e2) = join (apply applyS <$> ds e1 <*> ds e2)
      where
        -- This replaces f(e) with check<succeeds>{f[e]}
        applyS x y = eCheck [effSucceeds] (ApplyD x y)

    -- (e1 = e2)  --->  Unify
    ds (InfixOp e1 (Op "=")  e2) = Unify <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op ">>") e2) = Guard <$> ds e1 <*> ds e2

    -- Bindings
    ds (InfixOp e1 o@(Op ":")  e2) = ds =<< defn e1 (PrefixOp o e2)  -- PCOLONT
    ds (InfixOp e1   (Op ":=") e2) = ds =<< defn e1 e2

    -- Expand type{t} to Fun(x := t)<closed>{x}
    ds (Typedef e) = do { x <- newIdent (getLoc e) "x"
                        ; ds $ Function [(eDefine x e, [closedId])] (Variable x) }
    --  S more direct desugaring, which generates less verification clutter,
    --  is   type{t} --> \x. x=t
    -- But it is a wrong desugaring. e.g   type{_(:int):int}  test "HO15"
    --     ds (Typedef e) = do { x <- newIdent (getLoc e) "x"
    --                         ; Lam x <$> (Unify (Variable x) <$> ds e) }

    -- Function notation
    ds (InfixOp e1 (Op "=>") e2)  = ds $ Function [(e1, [closedId])] e2
    ds (Function (a:as@(_:_)) b)  = ds $ Function [a] $ Function as b
    ds (Function [(e1, effs)] e2) = do
           e1' <- ds e1
           e2' <- ds e2
           effs' <- checkEffs effs
           pure $ Function [(e1', effs')] e2'

    -- Conditional and for-loop notation
    ds (If1 e)        = ds $ If2E e eFalse
    ds (If2 e1 e2)    = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2)   = do x <- newIdent (getLoc e1) "x"; ds $ If3 (eDefine x e1) (Variable x) e2
    ds (If3 e1 e2 e3) = do { e1' <- ds e1; e2' <- ds e2; e3' <- ds e3
                           ; let the_fun = One (Choice (seqE [e1', Lam underscore e2'])
                                                       (Lam underscore e3'))
                           ; return (ApplyD the_fun (Array [])) }

    ds (For1 e) = do x <- newIdent (getLoc e) "x"; ds $ For2 (eDefine x e) (Variable x)

    -- Array
    ds (Array es) = arraySplice =<< mapM elm es
      where elm (PrefixOp (Ident l "..") e) = PrefixOp (Ident l "..") <$> ds e
            elm e = ds e

    -- Let and where
    --    (let e in b) --> e; b
    --    (e1 where e2)  -->   ( x ::= e1; e2; x)
    ds (Let e b) = do { e' <- ds e; b' <- ds b; pure (Seq [e',b']) }
    ds (InfixOp e1 (Op "where") e2) = do
      x <- newIdent (getLoc e1) "x"
      ds $ seqE [eDefine x e1, e2, Variable x]

    -- Do and case
    ds (Case1 b) = do
      let l = getLoc b
      x <- Variable <$> newIdent l "x"
      ds $ Function [(InfixOp x (Op ":") eAny, [])] $ Case2 x b
    ds (Case2 _ _) = undefined
    ds (Block b)   = ds b                                               -- do e --> e
    ds (Blk es)    = ds $ seqE es

    ds (Seq es) = seqE <$> mapM ds es
    ds (OfType e1 eff e2) = OfType <$> ds e1 <*> pure eff <*> ds e2

    -- Operators
    ds (PrefixOp (Op "not") e)   = do e' <- ds e; pure $ If3 e' Fail eFalse
    ds (PrefixOp (Op "-") e)     = do ds $ InfixOp (Lit (LInt 0)) (Op "-") e
    ds (PrefixOp (Op "+") e)     = ds e  -- Prefix "+"; maybe should have an isInt test?
    ds (PrefixOp (Op ":") e)     = Range [effSucceeds] <$> ds e
    ds (PrefixOp (Op "?") e)     = do x <- Variable <$> newIdent (getLoc e) "x"
                                      let ee = Let (InfixOp x (Op ":") e) (Truth x)
                                      ds $ Typedef $ InfixOp eFalse (Op "|") ee
    ds (PrefixOp (Ident l op) e) = ds =<< call "pre" l op e

    ds (PostfixOp e (Ident l "?"))  = ds $ ApplyD e (Variable (Ident l "_"))
    ds (PostfixOp e (Ident l op))   = ds =<< call "post" l op e

    ds (InfixOp e1 (Op "|") e2)     = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2)   = ds $ Seq [e1, e2]                  -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 Fail) Fail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2)    = ds $ If2E e1 $ If2E e2 Fail
    ds (InfixOp e1 (Ident l op) e2) = ds =<< call "in" l op (Array [e1, e2])

    -- Variables
    ds (Variable ident@(Ident l v))
      | v == "_"                    = DefineV <$> newIdent l "u"
      | Just op <- lookupPrimOp v   = return (EPrim op)
      | otherwise                   = return (Variable ident)

    -- Misc
    ds (Option Nothing) = pure eFalse

    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (eDefine t e) (Truth (Variable t))
    ds (Truth e) = ds $ Map [InfixOp e (Op "=>") e]

    ds (Macro1 (Ident _ "verify") _ e)  = Verify [] <$> ds e
    ds (Macro1 (Ident _ "first") [] e)  = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    ds (Exists xs b) = Exists xs <$> ds b

    ds (Map es) | Just kvs <- mapM simpleMapEntry es =
      ds $ ApplyD (eMkMap noLoc) $ Array [ Array [k, v] | (k, v) <- kvs ]

    ds emap@(Map es) = do
      let loc = getLoc emap
      f <- Variable <$> newIdent loc "f"
      i <- Variable <$> newIdent loc "i"
      a <- Variable <$> newIdent loc "a"
      ds $ ApplyD (eMkMap loc) $
                  For2 (Seq [InfixOp f (Ident loc ":") (Array es),
                             InfixOp (InfixOp i (Ident loc "->") a) (Ident loc ":") f])
                       (Array [i, a])

    ds x = compos ds x

checkEffs :: [Eff] -> D [Eff]
checkEffs = mapM checkEff
  where
    checkEff (Ident _ "invariant") = pure closedId
    checkEff e | e `elem` knownEffects = pure e
               | otherwise = errorMessage $ "unknown effect: " ++ show e


knownEffects :: [Ident]
knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts", "transacts", "open"
  ] ++ [closedId]

closedId :: Ident
closedId = Ident noLoc "closed"

--------------------------------------
-- Application
--------------------------------------

apply :: (SrcValue -> SrcValue -> SrcExpr) -> SrcExpr -> SrcExpr -> D SrcExpr
-- val1[e2]  -->
apply con e1 e2 | isValue e1 = apply1 con e1 e2   -- Easy special case.  Not really needed
-- e1[e2]  -->  f:=e1; f[e2]
apply con e1 e2 = do
  f <- newIdent (getLoc e1) "f"
  r <- apply1 con (Variable f) e2
  pure $ seqE [eDefine f e1, r]

apply1 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcExpr -> D SrcExpr
-- val1[val2]
apply1 con x1 e2 | isValue e2 = apply2 con x1 e2   -- Easy special case.  Not really needed
-- val1[e2]  -->  a:=e2; val1[a]
apply1 con x1 e2 = do
  a <- newIdent (getLoc e2) "a"
  r <- apply2 con x1 (Variable a)
  pure $ seqE [eDefine a e2, r]

-- val1[val2]  -->
apply2 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcValue -> D SrcExpr
apply2 con x1 x2 = pure $ con x1 x2

--------------------------------------
-- Patterns
--------------------------------------

defn :: SrcExpr -> SrcExpr -> D SrcExpr
-- Desugars (p := e) into an expression; see Fig 3, top group

-- Rule: (i := e) -->  (i := e)
defn (Variable (Ident _ "_")) e = do
  x <- newIdent (getLoc e) "u"
  pure $ eDefine x e

defn (Variable i) e = pure $ eDefine i e

-- Rule:   p(a)<fxs> := rhs   -->  p := fun(a){check<fxs>{rhs}}
--         Adding <succeeds> if not present
defn p e
  | (p1, fxs) <- getEffs p
  , Just (f, a, rs) <- getFun p1
  = defn f (Function [(a, rs)] (eCheck (addSucceeds fxs) e))

-- Rule: (e1<fx>:e2 := e)    -->  (e1 :=        e |>{fx} e2)
-- Rule: (f(a)<fx>:e2 := e)  -->  (e1 := fun(a){e |>{fx} e2})
-- but adding <succeeds> if omittec leaving behind <open/closed>
defn (InfixOp e1 (Op ":") e2) e
   = case getFun e1' of
       Just (f, a, rs) -> defn f (Function [(a,rs)] (OfType e fxs' e2))
       Nothing         -> defn e1'                  (OfType e fxs' e2)
  where
    (e1', fxs) = getEffs e1
    fxs' = addSucceeds fxs


-- Rule: (:e2) := e  -->  (x:e2) := e, x fresh
defn (PrefixOp op@(Op ":") e2) e
  = do { u <- newIdent (getLoc e2) "u"
       ; defn (InfixOp (Variable u) op e2) e }

-- Rule: (f(a) := e)  -->  (f := function(a){e})
-- Rule: (p<a> := e)  -->  ...

-- Rule: (p<fx> := e) -->  (p := check<fx>{e})
defn p@(EffAttr {}) e
  | not (null fxs) = defn p' (eCheck fxs e)
  where
    (p', fxs) = getEffs p

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


addSucceeds :: [Eff] -> [Eff]
addSucceeds [] = [effSucceeds]   -- Default is <succeeds>
addSucceeds fx = fx

--------------------------------------------------------
--         Functions to take apart SrcExpr
--------------------------------------------------------

-- Return function, argument, and attributes
getFun :: SrcExpr -> Maybe (SrcExpr, SrcExpr, [Ident])
getFun = gf []
  where
    gf rs (EffAttr e r) = gf (r:rs) e
    gf rs (ApplyS f a) = Just (f, a, reverse rs)
    gf _ _ = Nothing

getEffs :: SrcExpr -> (SrcExpr, [Eff])  -- e<fx1><fx2> --> (e, [fx1,fx2])
getEffs orig_e = go orig_e []
  where
    go (EffAttr e fx) fxs
     | isOpenClosed fx    = wrap fx (go e fxs)
     | otherwise          = go e (fx:fxs)
    go e              fxs = (e, fxs)

    wrap fx (e, fxs) = (EffAttr e fx, fxs)

--------------------------------------
-- Maps
--------------------------------------

simpleMapEntry :: SrcExpr -> Maybe (SrcExpr, SrcExpr)
simpleMapEntry (InfixOp k (Op "=>") v) = Just (k, v)
simpleMapEntry _ = Nothing

--------------------------------------
-- Arrays
--------------------------------------

defnArray :: [SrcExpr] -> SrcExpr -> D SrcExpr
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

arraySplice :: [SrcExpr] -> D SrcExpr
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

eAppend :: SrcExpr -> SrcExpr -> Ident -> SrcExpr
eAppend (Array xs) (Array ys) z = eDefine z (Array (xs ++ ys))
eAppend x y z = Seq [DefineV z, ApplyD (Variable (Ident noLoc "append$")) (Array [x, y, Variable z])]

data ArrayElem = EElems [SrcExpr] | ESplice SrcExpr
  deriving (Show)

-- Handle an array element, it can be ..e or e
arrayElems :: [SrcExpr] -> [ArrayElem]
arrayElems = grp . map cls
  where cls (PrefixOp (Ident _ "..") e) = Left e
        cls e = Right e
        grp [] = []
        grp (Left e : as) = ESplice e : grp as
        grp as =
          let (rs, bs) = span isRight as
          in  EElems [ e | Right e <- rs ] : grp bs

--------------------------------------
-- Calls
--------------------------------------

call :: String         -- "pre", "post", or "in" depending on prefix, postfix or infix
     -> Loc -> String  -- Function
     -> SrcExpr        -- Argumemt
     -> D SrcExpr
-- Pick the appropriate form of apply for operators
-- SLPJ don't understand
call _ loc op arg = return (ApplyD (Variable (Ident loc op)) arg)

{-
call p l s e = do
  ver <- getDFlagsX (not . fAssumeVerified)
  let
    -- For verification, use ApplyS.  At runtime, skip the test.
    con | ver && s' `elem` [
                     "pre'+'","pre'-'",
                     "in'+'","in'-'","in'*'"] = ApplyS
        | s' `elem` ["in'/'","pre'!'",
                     "pre'^'", "pre'[]'", "post'^'",  -- no need for succeeds
                     "pre'+'","pre'-'",  -- XXX not really right
                     "in'+'","in'-'","in'*'",  -- XXX not really right
                     "in'+='", "in'-='", "in'*='", "in'/='", "in'.='",
                     "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='",
                     "length","in'..'"] = ApplyD
        | otherwise = ApplyS
    s' = p ++ "'" ++ s ++ "'"
  pure $ con (Variable (Ident l s')) e
-}

--------------------------------------------------------
--
--         Adding dereferencing
--
--------------------------------------------------------

_addDeref :: SrcExpr -> D SrcExpr
_addDeref = pure . exprD S.empty
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
    expr s (Range fx e1) = Range fx (expr s e1)
--    expr s (Typedef e1) = Typedef (exprD s e1)
    expr s (Macro1 m rs e1) = Macro1 m rs (exprD s e1)
    expr s (Exists is e) = Exists is (expr s e)
    expr s (OfType e fx t) = OfType (expr s e) fx (expr s t)
    expr _ Fail = Fail
    expr s (Lam i e) = Lam i (expr s e)
    expr _ e@EPrim{} = e
--    expr s (Map es) = Map $ map (expr s) es
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

    defs :: S.Set Ident -> SrcExpr -> S.Set Ident
    defs s e = S.union s (S.fromList (getVar e))

    applyPrimD s e = ApplyD (Variable (Ident noLoc s)) e


--------------------------------------------------------
--
--         dsDx: Desugaring Small Source -> Big Core
--
--------------------------------------------------------

data Pi
  = P SrcExpr -- ^ P(x)    The SrcExpr is always small and freely duplicable
              --           Typically just (Variable x)
  | E         -- ^ E
  deriving (Eq, Ord, Show)

data DsMode12
  = MX -- ^ x "execution"
  | MV -- ^ + "verification"
  | MI -- ^ - "checking" ("implementation")
  deriving (Eq, Ord, Show)


dsD_12 :: SrcExpr -> D SrcExpr
dsD_12 = dsDD_12 MV

dsDD_12 :: DsMode12 -> SrcExpr -> D SrcExpr
dsDD_12 s t = dsM_12 s t E


dsB_12 :: DsMode12 -> SrcExpr -> Pi -> SrcExpr -> D SrcExpr
dsB_12 s t E     _
  = dsM_12 s t E
dsB_12 s t (P f) j
  = do z <- newIdent (getLoc t) "z";
       seqDE [ pure $ eDefine z (ApplyD f j)
             , dsM_12 s t (P (Variable z))]

seqDE :: [D SrcExpr] -> D SrcExpr
seqDE ds = seqE <$> sequence ds

defineDE :: String -> D SrcExpr
         -> D (SrcExpr,   -- The defn
               SrcExpr)   -- The use
-- define "x" de   returns   x := e, along with x itself
-- But (just to save clutter) if the rhs turns out to be tiny,
--   just return it alone
defineDE nm ds_rhs
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then pure (seqE [], rhs')
         else do { x <- newIdent (getLoc rhs') nm
                 ; pure (eDefine x rhs', Variable x) } }

defineDE2 :: String
          -> D SrcExpr               -- The RHS
          -> (SrcExpr -> D SrcExpr)  -- The body
          -> D SrcExpr               -- z := rhs; body
defineDE2 nm ds_rhs ds_body
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then ds_body rhs'
         else do { x <- newIdent (getLoc rhs') nm
                 ; body' <- ds_body (Variable x)
                 ; if x `elem` getAllIdents body'  -- See Note [Avoiding clutter]
                   then pure (seqE [eDefine x rhs', body'])
                   else pure (seqE [rhs',           body']) } }

{- Note [Avoiding clutter]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In defineDE2 we are building a term looking like:

   x := rhs
   ...body...

where `x` is complely fresh.   We can abbreviate this if

* `rhs` is atomic; then just use `rhs` rather than `x`

* `x` is not used in rhs.  Since `rhs` is a SrcExpr, its binding structure
  is not obvious, and it's awkward to get its true free variables. But since
  `x` is fresh anyway it suffices to look for /any/ occurrence of `x`. At worst
  we'll create a binding we don't really need.

  Hence the user of `getAllIdents`.
-}

dsM_12 :: DsMode12 -> SrcExpr -> Pi -> D SrcCore

-------------------- Functions -----------------------
dsM_12 MV t@(Function [(t1, _fx)] t2) pi        -- MCFUN+
  = do r <- newIdent (getLoc t) "r"
       body <- defineDE2 "j" (dsM_12 MI t1 (P (Variable r)))
                             (dsB_12 MV t2 pi)
       seqDE [ pure (eVerify [r] body)
             , dsM_12 MI t pi ]

dsM_12 MI (Function [(t1, _fx)] t2) pi        -- MCFUN-
  = do i   <- newIdent (getLoc t1) "i"
       body <- defineDE2 "j" (dsM_12 MV t1 (P (Variable i)))
                             (dsB_12 MI t2 pi)
       pure $ {- TODO: ISFUN -} Lam i body

dsM_12 MX (Function [(t1, _fx)] t2) pi        -- MCFUNX
  = do i   <- newIdent (getLoc t1) "i"
       body <- defineDE2 "j" (dsM_12 MX t1 (P (Variable i)))
                             (dsB_12 MX t2 pi)
       pure $ Lam i body

-------------------- e |>{fx} t -----------------------
dsM_12 MV (OfType t1 fx t2) pi      -- MOFTYPE+
   = eCheck fx <$> verify_body
-- SLPJ: check this; not what is in the doc
--            , dsM_12 MI t pi ]
--    = seqDE [ eCheck fx <$> verify_body
--            , dsM_12 MI t pi ]
--  = seqDE [ eVerify [] <$> (eCheck fx <$> verify_body)
--          , dsM_12 MI t pi ]
  where
    verify_body = do { (e1, x) <- defineDE "x" (dsM_12 MV t1 pi)
                     ; (e2, z) <- defineDE "z" (dsDD_12 MV t2)
                     ; pure (seqE [e1, e2, eApplyD z x]) }

dsM_12 MI (OfType t1 fx t2) _pi      -- MOFTYPE-
    -- SLPJ: pi is unused, which seems suspicious
    -- But I think that's a correct reflection of opacity
  = do { (e2, z) <- defineDE "z" (dsDD_12 MI t2)
       ; pure (seqE [ e2
                    , eGuard (getFree t1) (seqE [eHavoc fx, eSome z]) ]) }

dsM_12 s (OfType t1 fx t2) pi      -- MOFTYPE2
  = do { (e1, x) <- defineDE "x" (dsM_12 s t1 pi)
       ; e2 <- dsM_12 s (Range fx t2) (P x)
       ; pure (seqE [e1,e2]) }

-------------------- :{fx} t -----------------
-- SLPJ: I don't think we want fx on Range at all

dsM_12 MI (Range _fx t) (P i)                 -- MTYPE1
  = do { (e, z) <- defineDE "z" (dsDD_12 MI t)
       ; pure (seqE [e, eGuard (getFree i) (eSome z) ]) }
-- SLPJ: check this... it's not what is in the doc yet
--       ; pure (seqE [e, eHavoc fx, eApplyD z i ]) }

dsM_12 s (Range _fx t) (P i)                  -- MTYPEP
  = do { (e, z) <- defineDE "z" (dsDD_12 s t)
       ; pure (seqE [e, eApplyD z i]) }   -- z[x]

dsM_12 s (Range _fx t) E                      -- MTYPEE
  = do { x <- newIdent (getLoc t) "x"
       ; (e, z) <- defineDE "z" (dsDD_12 s t)
       ; let z_app = eApplyD z (Variable x)   -- z[x]
       ; pure (Exists [x] (seqE [e, z_app])) }

-------------------- check<fx>{t} -----------------
dsM_12 MI (Check _fx t) pi                  -- MCHECK-
  = dsM_12 MI t pi

dsM_12 s (Check fx t) pi                   -- MCHECK+X
  = Check fx <$> dsM_12 s t pi

-------------------- (x~>y) := t -----------------
dsM_12 s (DefineIE x y t) E                -- MSQUIGE
  = do i <- newIdent (getLoc t) "i"
       eExists [i] <$> dsM_12 s (DefineIE x y t) (P (Variable i))

dsM_12 s (DefineIE x y t) (P i)             -- MSQUIGP
  = seqDE [ pure $ eDefine x i
          ,        eDefine y <$> dsM_12 s t (P i)
          ]

-------------------- array{t1,...tn} -----------------
dsM_12 s (Array ts) E                       -- MARRAYE
   = Array <$> mapM (\t -> dsM_12 s t E) ts

dsM_12 s (Array ts) (P i)                   -- MARRAYP
   | not (null ts)  -- Shortcut for empty ts, via MEQ
                    -- M_s[ <> ]P(i) --> i = <>
   = do { js <- mapM (\t -> newIdent (getLoc t) "j") ts
        ; es <- zipWithM do_one ts js
        ; pure (eExists js (seqE [ Unify i (Array (Variable <$> js))
                                 , Array es ])) }
   where
     do_one :: SrcExpr -> Ident -> D SrcExpr
     do_one t j = dsM_12 s t (P (Variable j))

-------------------- x := t -----------------
dsM_12 s (DefineE x t) pi                   -- MBIND
  = eDefine x <$> dsM_12 s t pi

-------------------- v >> t -----------------
dsM_12 s (Guard t1 t2) pi                   -- MGUARD
  = Guard <$> dsD_12 t1 <*> dsM_12 s t2 pi

-------------------- exi x. t -----------------
dsM_12 s (Exists is t) pi                   -- MEXISTS
  = Exists is <$> dsM_12 s t pi

-------------------- t1 = t2 -----------------
dsM_12 s (Unify t1 t2) pi                   -- MEQ
  = Unify <$> dsM_12 s t1 pi <*> dsM_12 s t2 pi

-------------------- t1 ; t2 -----------------
dsM_12 MX (Seq ts) pi                      -- MSEMIX
  = do let (ts', t) = unSeq ts
       es' <- mapM (dsDD_12 MX) ts'
       e'  <- dsM_12 MX t pi
       pure $ seqE (es' ++ [e'])

dsM_12 s  (Seq ts) pi                      -- MSEMI
  = do let (ts', t) = unSeq ts
       es' <- mapM (dsDD_12 MV) ts'
       e'  <- dsM_12 s t pi
       pure $ seqE (es' ++ [e'])

-------------------- (t1 | t2) and fail -----------------
dsM_12 s (Choice t1 t2) pi                 -- MCHOICE
  = Choice <$> dsM_12 s t1 pi <*> dsM_12 s t2 pi

dsM_12 _ Fail _
   = pure Fail

-------------------- k, op, x -----------------
dsM_12 _ t@(Lit{}) E                       -- MCONST
   = pure t

dsM_12 _ t@(EPrim{}) E                     -- MPrim
   = pure t

dsM_12 _ t@(Variable {}) E                 -- MVAR
   = pure t

-------------------- t1[t2] -----------------
dsM_12 s (ApplyD t1 t2) E                  -- MVAR
   = eApplyD <$> dsDD_12 s t1 <*> dsDD_12 s t2

-- dsM_12 s (If3 t1 t2 t3) pi                 -- MIF
--   = If3 <$> dsDD_12 s t1 <*> dsM_12 s t2 pi <*> dsM_12 s t3 pi

dsM_12 s (Macro1 m rs t) pi
   = Macro1 m rs <$> dsM_12 s t pi

dsM_12 s (Lam x t) _pi
   = Lam x <$> dsM_12 s t E

dsM_12 _ e@(DefineV _) _
   = pure e

---------- Other terms with P(i) ---------------
dsM_12 s t (P i)                           -- MEQ
   = Unify i <$> dsM_12 s t E


dsM_12 s t pi
   = error $ "TODO: dsM_12 " ++ show (s, t, pi)


-----------------------------------------------
--
--    addPrelude: a small pass to add the prelude
--
-----------------------------------------------


addPrelude :: SrcExpr -> D SrcExpr
addPrelude orig_e
  = do { prel  <- getDFlagsX (snd . fPrelude)
       ; prel1 <- syntaxFixes prel
       ; return (addUsed (spl prel1) orig_e) }
  where
    -- Split the prelude into an association list
    spl (Array ds) = map (\ e -> (nameOf e, e)) ds
    spl e = impossible e

    -- Find the name of a definition
    nameOf (InfixOp lhs (Ident _ ":=") _) = lhsName lhs
    nameOf e = impossible e

    lhsName :: SrcExpr -> Ident
    lhsName (EffAttr e _) = lhsName e
    lhsName (ApplyS e _) = lhsName e
    lhsName (Variable i) = i
    lhsName e = impossible e

-- Hackily add prelude identifiers used in e
addUsed :: [(Ident, SrcExpr)] -> SrcExpr -> SrcExpr
addUsed prel = loop []
  where
    loop vs e =
      let is = getAllIdents e
          ps = filter (\ (i, _) -> i `elem` is && i `notElem` vs) prel
      in  case ps of
            [] -> e
            ies -> loop (map fst ies ++ vs) $ seqE (map snd ies ++ [e])


--------------------------------------------------------
--
--    primops: a little pass to replace Ident "op" with EPrim "op"
--
--------------------------------------------------------

lookupPrimOp :: String -> Maybe PrimOp
lookupPrimOp s = lookup s prs
  where
    prs :: [(String,PrimOp)]
    prs = [(primOpString op, op) | op <- allPrimOps]

--------------------------------------------------------
--
--         syntaxFixes
--
--------------------------------------------------------

-- Do various early changes:
--  * (e)       -->  e             parens are there to stop the next from possibly firing
--  * e1:e2=e3  -->  e1:e2 := e3   XXX should we do this?
--  * (e1,...)  -->  array{e1,...} no need to distingush them anymore
--  * x&y:e     -->  array{x&y:e}  if outside an array
--                   x:e; y:e      if inside an array
syntaxFixes :: SrcExpr -> D SrcExpr
syntaxFixes = pure . f
  where f :: SrcExpr -> SrcExpr
        f (Parens e) = f e
        f (InfixOp (InfixOp (Variable i1) o@(Op ":") e2) (Ident l3  "=") e3) =
          f $ InfixOp (InfixOp (Variable i1) o e2) (Ident l3 ":=") e3
        f (Tuple es) = f (Array es)
        f (Array es) = Array $ concatMap g es
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":" ) _) = f (Array [e])  -- PAMP1
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":=") _) = f (Array [e])  -- PAMP1
        f e = composOp f e

        -- PAMP2
        g :: SrcExpr -> [SrcExpr]
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":" ) rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":=") rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g e = [f e]



-----------------------------------------------
--      The desugaring monad: D
--
--   It is an IO monad (for tracing), with an env that carries
--       a mutable fresh-variable supply
--       the Flags
--
-----------------------------------------------

newtype D a = MkD (DEnv -> IO a)

data DEnv = DEnv { nextNo :: !(IORef Int), dflags :: !Flags }

instance Monad D where
  MkD m1 >>= k = MkD (\env -> do { r <- m1 env
                                 ; let MkD m2 = k r
                                 ; m2 env })
instance Applicative D where
  pure x = MkD (\_ -> return x)
  (<*>) = ap

instance Functor D where
  fmap f (MkD m) = MkD (\env -> f <$> m env)

runD :: Flags -> D a -> IO a
-- Runs the D monad
runD flags (MkD thing_inside)
  = do { nextref <- newIORef 1
       ; let env = DEnv { nextNo = nextref, dflags = flags }
       ; thing_inside env }

traceDS :: String -> SrcExpr -> D SrcExpr
traceDS msg e = do { traceD msg (pPrint e)
                   ; pure e }

traceD :: String -> Doc -> D ()
traceD msg doc
  = do { do_trace <- getDFlagsX fTraceDesugar
       ; if do_trace
         then doIO_D (putStrLn ("\n------- " ++ msg ++ "---------\n" ++ render doc))
         else return () }

doIO_D :: IO a -> D a
doIO_D io = MkD (\_ -> io)

getDFlags :: D Flags
getDFlags = MkD (\(DEnv { dflags = flags }) -> return flags)

getDFlagsX :: (Flags -> a) -> D a
getDFlagsX f = f <$> getDFlags

newInt :: D Int
newInt = MkD $ \(DEnv { nextNo = ref }) ->
  do { n <- readIORef ref
     ; writeIORef ref (n+1)
     ; return n }

newIdent :: Loc -> String -> D Ident
newIdent l s = do
  n <- newInt
  pure $ Ident l $ "$" ++ s ++ show n
