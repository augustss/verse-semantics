{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DeriveFunctor, PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module FrontEnd.Desugar(
    DsM, runD, doIO_D, -- DError, traceD, getDFlagsX, traceDS, putScopeErr, 
    sDesugarExpr,
  ) where

import Prelude hiding (pi)

import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags
-- import FrontEnd.ENVDesugar( envDesugar )
import Core.Expr  ( allPrimOps, primOpString )

-- Epic libraries
import Epic.Print

-- General libraries
import Data.Maybe( catMaybes, fromMaybe )
import Data.IORef
import Data.List
import qualified Data.Set as S
import Control.Monad

import GHC.Stack

-- QUESTIONS:
--  x:int='a'   fail or wrong?, tests L93, L95

-- TODO:
--  Add Err

-- TODO:
--  x:t=v is syntactic sugar for x:=(:t=v) and
--  :t=v is a special form meaning it's not the same as (:t)=v, which is just unification.
--  desugar function effects


--------------------------------------------------------
--
--           The S-desugaring
--     Desugar into Essential Verse
--     Figs 4 and 5 of desugaring.pdf
--
--------------------------------------------------------

sDesugarExpr :: SrcExpr -> DsM SrcEssential
-- Source Verse to Essential Verse
-- Figs 4 and 5 in verse-spec.
sDesugarExpr = ds
  where
    ds :: SrcExpr -> DsM SrcEssential

    -- These can happen when going via Andy's stuff
{-
    ds (ApplyD (Variable (Ident l s)) (Array [e1,e2])) | Just r <- stripPrefix "operator'" s =
      ds (InfixOp e1 (Ident l (init r)) e2)
    ds (ApplyD (Variable (Ident l s)) e) | Just r <- stripPrefix "prefix'" s =
      ds (PrefixOp (Ident l (init r)) e)
-}
    --  Currently not done:
    --     e1:e2=e3  -->  e1:e2 := e3   XXX should we do this?

    ds (Parens e) = ds e
    ds (Tuple es) = ds (Array es)

    ds (ApplyD (Variable (Ident l s)) e) | Just r <- stripPrefix "postfix'" s, r `elem` ["?'"] =
      ds (PostfixOp e (Ident l (init r)))


    -- Application
    ds (ApplyD  e1 e2) = ApplyD <$> ds e1 <*> ds e2
    ds (ApplyS  e1 e2) = do e1' <- ds e1; e2' <- ds e2; applyS e1' e2'
      where
        -- This replaces f(a) with check<succeeds>{f[a]},
        -- but pulls out expressions, so e1(e2) turns into f:=e1; a:=e2; check<succeed>{f[a])
        applyS f a | not (isValue f) = do fi <- newIdent (getLoc f) "f"; fa <- applyS (Variable fi) a; return $ DefineE fi f `Seq` fa
        applyS f a | not (isValue a) = do ai <- newIdent (getLoc a) "a"; fa <- applyS f (Variable ai); return $ DefineE ai a `Seq` fa
        applyS f a = return $ eCheck effSucceeds (ApplyD f a)

    -- (e1 = e2)  --->  Unify
    ds (InfixOp e1 (Op "=")  e2)
      | PrefixOp  (Op ":") _ <- e1 = defn e1 e2
      | InfixOp _ (Op ":") _ <- e1 = defn e1 e2
      | otherwise                  = Unify <$> ds e1 <*> ds e2

    -- Guard
    ds (InfixOp e1 (Op ">>") e2) = Guard <$> ds e1 <*> ds e2

    -- Bindings
    ds (InfixOp e1 (Op ":=") e2) = defn e1 e2 >>= ds
    ds (InfixOp e1 (Op ":")  e2) = defn e1 (Range e2) >>= ds

    -- Function notation
    ds (InfixOp e1 (Op "=>") e2)  = ds $ Function Closed e1 effSucceeds e2
       -- The e1=>e2 notation has an implicit <succeeds>

    ds (Function q e1 effs e2) = Function q <$> ds e1 <*> pure effs <*> ds e2

    -- Conditionals
    -- We must retain IF3 (i.e `if e1 then e2 else e3`) because
    -- the main dsM_12 desugaring needs to push `pi` into the branches.
    ds (If1 e)        = ds $ If2E e eFalse
    ds (If2 e1 e2)    = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2)   = do x <- newIdent (getLoc e1) "x"; ds $ If3 (eDefine x e1) (Variable x) e2

    -- For-loops
    -- for(e1){e2} = arrMap$[ \t. t[], all{ e1; \_.e2 } ]
    ds (For1 e)     = do { x <- newIdent (getLoc e) "x"
                         ; ds $ For2 (eDefine x e) (Variable x) }
    ds (For2 e1 e2) = For2 <$> ds e1 <*> ds e2

    -- Array
    ds (Array es) = Array <$> mapM ds es

    -- Let
    --    (let e in b)  --> e; b          Assumes binders in b are not free in e
    ds (Let e b)                    = do { e' <- ds e; b' <- ds b; return (eSeq [e',b']) }

    -- Where
    --    (e1 where e2) --> e1 where e2   Need to keep this for M-desugaring!
    --                                    Can't swap to (e2;e1) because that switches
    --                                       the order of choices
    ds (InfixOp e1 (Op "where") e2) = Where <$> ds e1 <*> ds e2

    -- Do and case
    ds (Case1 b)     = do { let l = getLoc b
                          ; x <- Variable <$> newIdent l "x"
                          ; ds $ Function Closed (InfixOp x (Op ":") eAny) effTop (Case2 x b) }
    ds (Case2 _ _)    = undefined
    ds (Block b)      = ds b                              -- do e --> e
    ds (Blk es)       = ds $ eSeq es
    ds e@(DefineV {}) = pure e

    ds (Seq e1 e2)        = mkSeq <$> ds e1 <*> ds e2
    ds (OfType e1 eff e2) = OfType <$> ds e1 <*> pure eff <*> ds e2

    -- Operators
    -- NB: Prefix '?' is just a function now; see note [Truth values] in Rules.Core
    ds (PrefixOp (Op "not") e)   = do e' <- ds e; pure $ If3 e' Fail eFalse
    ds (PrefixOp (Op ":") e)     = Range <$> ds e
    ds (PrefixOp (Op "..") e)    = Splice   <$> ds e  -- See Note [Desugaring array splices]
    ds (PrefixOp (Ident l op) e) = ds =<< call Pre l op e

    -- e?  means simply  e[_]  or equivalently   exists x. e[x]
    ds (PostfixOp e (Ident l "?"))  = ds $ ApplyD e (Variable (Ident l "_"))
    -- All other postfix ops
    ds (PostfixOp e (Ident l op))   = ds =<< call Post l op e

    -- Infix ops
    ds (InfixOp e1 (Op "|") e2)     = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2)   = ds $ mkSeq e1 e2                   -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 Fail) Fail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2)    = ds $ If2E e1 $ If2E e2 Fail
    ds (InfixOp e1 (Ident l op) e2) = ds =<< call In l op (Array [e1, e2])

    -- Variables
    ds (Variable ident@(Ident _ v))
      | v == "fail"                 = return Fail
      | Just op <- lookupPrimOp v   = return (EPrim op)
      | otherwise                   = return (Variable ident)

    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option Nothing) = pure eFalse
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (eDefine t e) (Truth (Variable t))
    ds (Truth e) = Truth <$> ds e

    ds (Macro1 (Ident _ "one")   _ e)    = One <$> ds e
    ds (Macro1 (Ident _ "all")   _ e)    = All <$> ds e
    ds (Macro1 (Ident _ "verify") _ e)   = Verify [] <$> ds e
    ds (Macro1 (Ident _ "some") _ e)     = Some <$> ds e
    ds (Macro1 (Ident _ "check") fx e)   = Check (toEff effTop fx) <$> ds e
    ds (Macro1 (Ident _ "expect") fx e)  = Check (toEff effTop fx) <$> ds e
                                           -- I think "expect" is Tim's notation for "check"
    ds (Macro1 (Ident _ "succeeds") _ e) = eCheck effSucceeds <$> ds e
    ds (Macro1 (Ident _ "decides")  _ e) = eCheck effDecides  <$> ds e
    ds (Macro1 (Ident _ "fails")    _ e) = eCheck effFails    <$> ds e

    -- assume<fx>{e}  ==   havoc<fx>; some(\x. x=e; x)
    ds (Macro1 (Ident _ "assume") fx e)  = do { x <- newIdent (getLoc e) "x"
                                              ; e' <- ds e
                                              ; pure (eSeq [ eHavoc (toEff effSucceeds fx)
                                                           , Some (Lam x (eSeq [Unify (Variable x) e'
                                                                               , Variable x]))]) }

    -- first{e}        ==  if (x:=e) then x  else fail
    -- first(e1){e2}   ==  if e1     then e2 else fail
    ds (Macro1 (Ident _ "first") _ e)   = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    -- type{t}  ==  Fun(x := t)<closed>{x}
    ds (Macro1 (Ident _ "type") _ e) = do { e' <- ds e; encodeType e' }
    ds (Macro1 (Ident _ "rtype") _ e) = do { e' <- ds e; encodeRType e' }

    -- I want to desugar Exists to DefineV; but to do that I need to make up
    -- fresh identifiers (easy) and substitute the fresh one for the old one
    -- (not so easy).  So for now I am just keeping Exists.
    ds (Exists xs b) = Exists xs <$> ds b

    ds (Map es) | Just kvs <- mapM simpleMapEntry es =
      ds $ ApplyD (eMkMap noLoc) $ Array [ Array [k, v] | (k, v) <- kvs ]

    ds emap@(Map es) = do
      let loc = getLoc emap
      f <- Variable <$> newIdent loc "f"
      i <- Variable <$> newIdent loc "i"
      a <- Variable <$> newIdent loc "a"
      ds $ ApplyD (eMkMap loc) $
                  For2 (mkSeq (InfixOp f (Ident loc ":") (Array es))
                              (InfixOp (InfixOp i (Ident loc "->") a) (Ident loc ":") f))
                       (Array [i, a])

    ds e@(EffAttr {}) = errorMessage (showWithHerald "Unexpected effects" (pPrint e))

    ds e = compos ds e    -- Core expressions like Lit, Variable,
                          -- DefineV, DefineE, Unify, EPrim, etc

encodeType :: SrcEssential -> DsM SrcEssential
  -- Encodes type{e}, returning  fun(x:=e){x}
  -- You might think that a more direct desugaring would be
  --      type{t} --> \x. x=t
  -- using a ICFP lambda.
  -- But it is a wrong desugaring. e.g   type{_(:int):int}  test "HO15"
encodeType e = do { x <- newIdent noLoc "x"
                  ; return (Function Closed (eDefine x e) effSucceeds (Variable x)) }

encodeRType :: SrcEssential -> DsM SrcEssential
  -- Encodes type{e}, returning  relation(x:=e){x}
encodeRType e = do { x <- newIdent noLoc "x"
                   ; return (Relation (eDefine x e) effSucceeds (Variable x)) }

--------------------------------------
-- Patterns
--------------------------------------

defn :: (HasCallStack) => SrcPat -> SrcExpr -> DsM SrcExpr
-- Desugars (p := e) into an expression; see Fig 3, top group
-- Neither input is desugared; the result
--   should have sDesugarExpr applied to it.

defn (Parens p) e = defn p e
defn (Tuple es) e = defn (Array es) e

-- DSWILD1:   _ := e  -->  e
defn (Variable i) e
  | isSrcUnderscore i = pure e
  | otherwise         = pure $ eDefine i e

-- DSWILD2:   (:e2) := e  -->  (_:e2) := e
defn (PrefixOp op@(Op ":") e2) e
  = defn (InfixOp (Variable srcUnderscore) op e2) e

-- DSFUN1/2: (f(a)<fxs> := e)  -->  (f := function(a)<fxs>{e})
-- DSTY1/2:  (p:ty<fxs> := e)  -->  e |><fxs> ty
defn p rhs | ApplyS p2 a            <- p1 = defn_fun p2 a  fxs rhs   -- DSFUN1, DSFUN2
           | InfixOp p2 (Op ":") e2 <- p1 = defn_ty  p2 e2 fxs rhs   -- DSTY1, DSTY2
  where
    (p1, fxs) = getEffs p

--defn (EffAttr e1 r) e v = defn e1 (applyEff [r] e) v
-- Rule: (p?) := e  -->  p := option{e}
--defn (PostfixOp p (Ident _ "?")) e = defn p (Option $ Just e)
-- Rule: (p1,...) := e  -->  (x1:any,...) = e; p1 := x1; ...

defn (Array ps) e = defnArray ps e

-- Rule (x ~> p) := e  -->  p := (x -> e)
defn (InfixOp (Variable x) (Op "->") p) e = do
  r <- defn p (DefineIE x e)
  pure $ eSeq [DefineV x, r]
-- Rule (p1 ~> p2) := e  -->  p1 := (exists x); p2 := (x -> e)
defn (InfixOp p1 (Op "->") p2) e = do
  x <- newIdent (getLoc p2) "x"
  r1 <- defn p1 (Variable x)
  r2 <- defn p2 (DefineIE x e)
  pure $ eSeq [DefineV x, r1, r2]

defn xs@(InfixOp _ (Op "&") _) e
  = -- See Note [Desugaring ampersand]
    do { es <- mapM (\p -> defn p e) (get xs)
       ; pure $ Splice $ Array es }
  where
    get (InfixOp p1 (Op "&") p2) = get p1 ++ get p2
    get p                        = [p]

defn p _ = errorMessage $ "Bad LHS to := " ++ prettyShow p

pattern OfTypeX :: SrcExpr -> Eff -> SrcExpr -> SrcExpr
pattern OfTypeX e1 eff e2 <- OfType e1 (maybeToEff -> eff) e2
  where OfTypeX e1 eff e2 = OfType e1 (Just eff) e2
maybeToEff :: Maybe Eff -> Eff
maybeToEff = fromMaybe effTop

defn_ty :: SrcPat -> SrcExpr -> [EffString] -> SrcExpr -> DsM SrcExpr
defn_ty p t fxs1 rhs
  = defn p (OfTypeX rhs (toEff effTop (fxs1 ++ fxs2)) t')
     -- effSucceeds:   x:type := rhs  -->   x := rhs |><decides> ty
     --                See DSTY2 Fig 4
  where
    (t', fxs2) = getEffs t
       -- Currently                   x : int<succeeds> := t
       -- parses as                   x : (int <succeeds>) := t
       -- but we want to treat it as  (x : int) <succeeds> := t
       -- This getEffs call smooths over the discrepancy.  Yuk.

defn_fun :: SrcPat -> SrcExpr -> [EffString] -> SrcExpr -> DsM SrcExpr
-- f(x)<fxs> := rhs
defn_fun f a fxs rhs = defn f (eFunction a fxs rhs)

eDefine :: HasCallStack => Ident -> SrcEssential -> SrcEssential
-- Generates (x:=e) in Essential Verse
eDefine x _ | isSrcUnderscore x = error "eDefine got '_'"
-- x := (e1; ...; en)   generates   e1; ... e(n-1); x:=en
-- Smart contructor, floats out nested defines
eDefine x (Seq e1 e2) = mkSeq e1 (eDefine x e2)
eDefine x rhs = DefineE x rhs


--------------------------------------------------------
--         Functions to take apart SrcExpr
--------------------------------------------------------

getEffs :: SrcExpr -> (SrcExpr, [EffString])
-- e<fx1><fx2> --> (e, [fx1,fx2])
getEffs orig_e = go orig_e []
  where
    go (EffAttr e fx) fxs = go e (fx:fxs)
    go e              fxs = (e, fxs)

--------------------------------------
-- Maps
--------------------------------------

simpleMapEntry :: SrcExpr -> Maybe (SrcExpr, SrcExpr)
simpleMapEntry (InfixOp k (Op "=>") v) = Just (k, v)
simpleMapEntry _ = Nothing

--------------------------------------
-- Arrays
--------------------------------------

defnArray :: [SrcPat] -> SrcExpr -> DsM SrcExpr
-- Dealing with an array on the LHS of a ":=", i.e.  an "array pattern"
-- For example
--     (p1, p2, ..p3, p4) := e
-- --->
--       p1 := x1; p2 := x2; p3 :=  x3; p4 := x4
--       array{ exists x1, exists x2, ..exists x3, exists x4 } = e
--  NB: the ".." part is dealt with in mDesugaring
--
-- Short cut if pi is a variable.  E.g. if p1 is a variable a, we et
--       p2 := x2; p3 := x3; p4 := x4
--       arraySplice{ exists a, exists x2, ..exists x3, exists x4 }

defnArray ps rhs
  = do { (ds, es) <- unzip <$> mapM do_one_elem ps
       ; pure $ eSeq $ catMaybes ds ++ [Unify (Array es) rhs] }
  where
    do_one_elem :: SrcPat -> DsM (Maybe SrcEssential, SrcEssential)
    do_one_elem p
      | PrefixOp (Op "..") p' <- p
      = -- See Note [Desugaring array splices]
        do { (md, e) <- do_one p'; pure (md, Splice e) }
      | otherwise
      = do_one p

    do_one :: SrcPat -> DsM (Maybe SrcEssential, SrcEssential)
    do_one pat
      | Variable v <- pat
      = -- Short cut for a common case: (a,b):=e
        pure (Nothing, DefineV v)
      | otherwise
      = do { x <- newIdent (getLoc pat) "x"
           ; d <- defn pat (Variable x)
           ; pure (Just d, DefineV x) }


--------------------------------------
-- Calls
--------------------------------------

data CallFixity = Pre | Post | In
  deriving(Show)

-- Use of an pre-/post-/in-fix operator
call :: CallFixity     -- fixity of calls
     -> Loc -> String  -- Function
     -> SrcExpr        -- Argument
     -> DsM SrcExpr
call fix loc op arg = return (ApplyD op_e arg)
  where
    op_e = case lookupPrimOp op of
              Just prim -> EPrim prim
              Nothing   -> Variable (Ident loc op')
    op' = case fix of
            Pre  -> "prefix'"   ++ op ++ "'"
            Post -> "postfix'"  ++ op ++ "'"
            In   -> "operator'" ++ op ++ "'"

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

_addDeref :: SrcExpr -> DsM SrcExpr
_addDeref = pure . exprD S.empty
  where
    expr _ e@Lit{} = e
    expr s e@(Variable i) | i `S.member` s = applyPrimD "read$" e
                          | otherwise = e
    expr s (Array es) = Array $ map (expr s) es
    expr s (Seq    e1 e2) = Seq    (expr s e1) (expr s e2)
    expr s (ApplyS e1 e2) = ApplyS (expr s e1) (expr s e2)
    expr s (ApplyD e1 e2) = ApplyD (expr s e1) (expr s e2)
    expr s (If3 e1 e2 e3) = If3 (expr s' e1) (expr s' e2) (exprD s e3)
      where s' = defs s e1
    expr s (For2 e1 e2) = For2 (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Let e1 e2) = Let (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Block e) = Block (exprD s e)
    expr s (Function q a rs e2) = Function q a rs (exprD s' e2)
      where s' = defs s a
    expr s (Unify e1 e2)  = Unify (expr s e1) (expr s e2)
    expr _ (DefineV i)    = DefineV i
    expr s (DefineE i e)  = DefineE i (expr s e)
    expr s (DefineIE i e) = DefineIE i (expr s e)
    expr s (Choice e1 e2) = Choice (exprD s e1) (exprD s e2)
    expr s (Set e1 (Ident l sop) e2) = set s e1 op (expr s e2)
      where op = Ident l ("in'" ++ sop ++ "'")
    expr s (MVar i (Just t) (Just e)) = DefineE i $ ApplyD (applyPrimD "new" (expr s t)) (expr s e)
    expr s (Range e1) = Range (expr s e1)
    expr s (Macro1 m rs e1) = Macro1 m rs (exprD s e1)
    expr s (Exists is e) = Exists is (expr s e)
    expr s (OfType e fx t) = OfType (expr s e) fx (expr s t)
    expr _ Fail = Fail
    expr s (Lam i e) = Lam i (expr s e)
    expr _ e@EPrim{} = e
--    expr s (Map es) = Map $ map (expr s) es
    expr _ e = impossible "addDeref" e

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
--           Array splicing: ..e
--
--------------------------------------------------------

{- Note [Desugaring array splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes how the "array splicing" form ..e is handled.
For example:

* Expressions
     (1, ..(3,4), 5,6)   =    (1,3,4,5,6)
  The array (3,4) is "spliced" into the final array

* Pattern bindings
     (x, ..y) := (1,2,3,4)
  Here x is bound to 1, and y is bound to (2,3,4)

* f( x:int, ..y:[]int ){ x+Length[y] }; f[1,2,3]
  Here x gets bound to 1, and y gets bound to (2,3). result is 1+2.

Here is how "..e" is handled:

Parsing: PrefixOp ".." e

S-desugaring: (PrefixOp ".." e) to (Splice e)
   This happens in two places
    - sDesugarExpr on SrcExpr (PrefixOp ".." e)
    - defnArray with a SrcPat (PrefixOp ".." e)

In M-desugaring (wrapping):
  - Expression case: M[Array [t1, Splice t2] _
      call mkArray [ M[t1]_, Splice (M[t2]_) ]

  - Pattern case: M[Array [t1, Splice t2] P(i)
      exists x1,x2
      i = mkArray [ x1, Splice x2 ]
      mkArray [ M[t1]P(x1), Splice (M[t2]P(x2)) ]

    Crucially, note that we desugar the original expressions ti
                                    with M[..]P(xi)

  - The function mkArray builds an array, using arrApp$ to take
    account of arguments that are wrapped in Splice


How to typeset in Fig 6:
   Idea:  mkArray <t1,Splice t2, t3, t4> <e1, e2, e3, e4>  = <e1> ++ e2 ++<e3,e4>

   auxfun (Splice t) e = e
   auxfun (t) e        = <e>
      where (ts', splices) = split ts


Note [Desugaring ampersand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes how the "ampersand" form (x&y) is handled.
First some premable:

* Note that x&y can only occur in /patterns/, SrcPat, which themselves
  appear only in
     p : t
     p := t

* Note also that the "&" notation has flattening behavior that is a bit
  like "..e" notation.  E.g.
        f( x&y:int, z:int ) := ...
     means
        f( x:int, y:int, z:int ) := ...

  and
      f( ..(1,2), 3 ) := ...
   means
      f( 1,2,3 ) := ...

  This Note explains how we leverage Splice (from Note [Desugaring array splices]
  to desugar "&" notation.

Now to the payload.

Parsing: (InfixOp e1 "&" e2)

S-Desugaring:
  Patterns, SrcPat, are desugared by `defn`. So we just need to say
  how `defn` desugars
     (x&y) := e
  We turn it into
     Splice (Array [defn x e, defn y e])

M-Desugaring:
  Given f( x&y:int, z:int ) := rhs
  M-desugaring will see
     f := fun( Array [Splice (Array [ x := :int, y := :int ])
                     , x := :int ]
             ){rhs}

  If we had written
     f( ..(x:int, y:int), z:int )
  we'd get just the same output of S-desugaring.

Wrinkles:

(AMP1) What about (x&y):int?   S-Desugaring `defn` turns this into
        (x&y):int
   -->  (x&y) := :int
   -->  Splice (Array [x:= :int, y:= :int])

   Now M-Desugaring sees a "naked" splice.  Just drop the Splice, to get
   an array, the same result as if you'd written
      (x:int, y:int)
   which is what we want.

Tricky stuff!
-}

--------------------------------------------------------
--
--     Desugaring Essential Verse to Mini Verse
--        The W (wrapper introduction) transformation
--        Fig 6 in verse-spec.pdf
--
--------------------------------------------------------

data WContext   -- The context for the W transformation
                -- Written \kappa in Fig 6
  = WC { wc_inp :: Input
       , wc_fxs :: EffContext
       }
  deriving( Show )

data Input
  = NoInput       -- ^ Typeset as bullet, circle, or underscore
  | PI Ident      -- ^ An input variable x
  deriving( Show )

data EffContext
  = DomCtxt       -- In the domain (argument) of a function, nothing to push down
  | RngCtxt Eff   -- In the range (body) of a function with effects Eff
  deriving( Show )


--------------------------------------------------------
--
--     Desugaring Mini Verse to Big Core
--        Fig 9 in verse-spec.pdf
--
--------------------------------------------------------

data Pi
  = P SrcCore  -- ^ P(x)    The SrcCore is always small and freely duplicable
               --           Typically just (Variable x)
  | E          -- ^ E
  deriving (Eq, Ord, Show)

data DsMode
  = MX       -- ^ x "execution"
  | MV Bool  -- ^ + "verification"; True <=> immediate consumer is a verify{}
             -- The Bool is just an optimisation, allows us to omit useless code
  | MI       -- ^ - "client" ("implementation")
  deriving (Eq, Ord, Show)


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


-----------------------------------------------
--      The desugaring monad: DsM
--
--   It is an IO monad (for tracing), with an env that carries
--       * a mutable fresh-variable supply
--       * a way to report errors
--       * the Flags
--
-----------------------------------------------

newtype DsM a = MkD (DEnv -> IO a)

newtype DError = MkDError Ident deriving (Eq, Ord, Show)

data DEnv = DEnv { nextNo :: !(IORef Int), scopeErr :: !(IORef [DError]), dflags :: !Flags }

instance Monad DsM where
  MkD m1 >>= k = MkD (\env -> do { r <- m1 env
                                 ; let MkD m2 = k r
                                 ; m2 env })
instance Applicative DsM where
  pure x = MkD (\_ -> return x)
  (<*>) = ap

instance Functor DsM where
  fmap f (MkD m) = MkD (\env -> f <$> m env)

runD :: Flags -> a -> DsM a -> IO a
-- Runs the DsM monad
-- May throw an exception in case of errors
runD flags err_result (MkD thing_inside)
  = do { nextref <- newIORef 1
       ; scopeErrRef <- newIORef []
       ; let env = DEnv { nextNo = nextref, scopeErr = scopeErrRef, dflags = flags }
       ; res  <- thing_inside env
       ; errs <- readIORef scopeErrRef
       ; case errs of
           [] -> return res
           _  -> do { displayDoc (text "ERROR(s):" <+> vcat (map pPrint errs))
                    ; return err_result }
       }

doIO_D :: IO a -> DsM a
doIO_D io = MkD (\_ -> io)

newInt :: DsM Int
newInt = MkD $ \(DEnv { nextNo = ref }) ->
  do { n <- readIORef ref
     ; writeIORef ref (n+1)
     ; return n }

newIdent :: Loc -> String -> DsM Ident
newIdent l s = do
  n <- newInt
  pure $ Ident l $ "$" ++ s ++ show n


instance Pretty DError where
  pPrintPrec l p (MkDError i) = text "Unbound identifer:" <+> pPrintPrec l p i
