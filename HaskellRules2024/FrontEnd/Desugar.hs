{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module FrontEnd.Desugar(
    desugar
    , DsM, DError, runD, traceD, getDFlagsX, traceDS, putScopeErr, doIO_D
    , addPrelude, sDesugarExpr, essToMini
    , miniToCore
  ) where

import Prelude hiding (pi)

import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags
import Core.Expr( PrimOp(..), allPrimOps, primOpString )

-- Epic libraries
import Epic.Print

-- General libraries
import Data.Maybe( catMaybes )
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


-----------------------------------------------
--
--      The main desugaring pass: desugar
--
-----------------------------------------------


desugar :: Flags -> Bool -> SrcExpr -> IO SrcCore
desugar flgs add_verification e_parsed
  = runD flgs $
    do { _ <- traceDS "parsed" e_parsed

       -- Prepend prelude from
       --    verifyprelude.verse, mediumprelude.verse
       ; e_prel <- addPrelude e_parsed
       ; _ <- traceDS "Add prelude" e_prel

       -- Desugar into Essential Verse by doing superficial desugaring
       ; e_essential <- sDesugarExpr e_prel
       ; _ <- traceDS "Superficial desugaring into Essential Verse" e_essential

       ; e_mini <- essToMini e_essential
       ; _ <- traceDS "Desugar Essential Verse into Mini Verse" e_mini

       ; e_ds <- miniToCore add_verification e_mini

       ; _ <- traceDS "Desugar Mini Verse into Core Verse" e_ds

       ; return e_ds
       }

  where

--------------------------------------------------------
--
--           The S-desugaring
--     Desugar into Small Source Verse
--     Figs 3 and 4 of desugaring.pdf
--
--------------------------------------------------------

sDesugarExpr :: SrcExpr -> DsM SrcEssential
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
    ds (ApplyS  e1 e2) = applyS <$> ds e1 <*> ds e2
      where
        -- This replaces f(e) with check<succeeds>{f[e]}
        applyS x y = eCheck effSucceeds (ApplyD x y)

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
                          -- DefineE, Unify, EPrim, etc

encodeType :: SrcEssential -> DsM SrcEssential
 -- Encodes type{e}, returning  fun(x:=e}{x}
 -- You might think that a more direct desugaring would be
--      type{t} --> \x. x=t
-- using a ICFP lambda.
-- But it is a wrong desugaring. e.g   type{_(:int):int}  test "HO15"
encodeType e = do { x <- newIdent noLoc "x"
                  ; return (Function Closed (eDefine x e) effSucceeds (Variable x)) }

--------------------------------------
-- Patterns
--------------------------------------

defn :: (HasCallStack) => SrcPat -> SrcExpr -> DsM SrcExpr
-- Desugars (p := e) into an expression; see Fig 3, top group
-- Neither input is desugared; the result
--   should have sDesugarExpr applied to it.

defn (Parens p) e = defn p e
defn (Tuple es) e = defn (Array es) e

defn (Variable i) e
  | isSrcUnderscore i = pure e
  | otherwise         = pure $ eDefine i e

defn xs@(InfixOp _ (Op "&") _) e
  = -- See Note [Desugaring ampersand]
    do { es <- mapM (\p -> defn p e) (get xs)
       ; pure $ Splice $ Array es }
  where
    get (InfixOp p1 (Op "&") p2) = get p1 ++ get p2
    get p                        = [p]

-- DSWILD1:   (:e2) := e  -->  (_:e2) := e
defn (PrefixOp op@(Op ":") e2) e
  = defn (InfixOp (Variable srcUnderscore) op e2) e

-- Rule: (f(a) := e)  -->  (f := function(a){e})
-- Rule: (p:ty := e)  -->  e |> ty
defn (ApplyS p a)            e = defn_fun p a [] e      -- DSFUN2
defn (InfixOp p (Op ":") e2) e = defn_ty p e2 [] e      -- DSTY2

-- Rule: (f(a)<fxs> := e)  -->  (f := function(a)<fxs>{e})
-- Rule: (p:ty<fxs> := e)  -->  e |><fxs> ty
defn p@(EffAttr {}) e
  | ApplyS p2 a <- p1            = defn_fun p2 a fxs e    -- DSFUN1
  | InfixOp p2 (Op ":") e2 <- p1 = defn_ty  p2 e2 fxs e   -- DSTY1
  where
    (p1, fxs) = getEffs p

--defn (EffAttr e1 r) e v = defn e1 (applyEff [r] e) v
-- Rule: (p?) := e  -->  p := option{e}
--defn (PostfixOp p (Ident _ "?")) e = defn p (Option $ Just e)
-- Rule: (p1,...) := e  -->  (x1:any,...) = e; p1 := x1; ...

defn (Array ps) e = defnArray ps e

-- Rule (p1 ~> p2) := e  -->  p1 := (exists x); p2 := (x -> e)
defn (InfixOp p1 (Op "->") p2) e = do
  x <- newIdent (getLoc p2) "x"
  r1 <- defn p1 (Variable x)
  r2 <- defn p2 (DefineIE x e)
  pure $ eSeq [r1, r2]

defn p _ = errorMessage $ "Bad LHS to := " ++ prettyShow p

defn_ty :: SrcPat -> SrcExpr -> [EffString] -> SrcExpr -> DsM SrcExpr
defn_ty p t fxs1 rhs
  = defn p (OfType rhs (toEff effTop (fxs1 ++ fxs2)) t')
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

mkArray :: [SrcEssential] -> DsM SrcEssential
-- mkArray [t1, t2, ..t3, t4]
--   = exists r1 r2.
--     arrApp$[ <t1,t2>, t3, r1 ];
--     arrApp$[ r1, <t4>, r2 ];
--     r2
mkArray es
  = case grabFirst es of
      (e', [])  -> pure e'
      (e', es') -> do { rest <- mkArray es'; mkAppend e' rest }
  where
    grabFirst :: [SrcEssential] -> (SrcEssential, [SrcEssential])
    grabFirst []                 = (Array [], [])
    grabFirst (Splice e : es')   = (e,es')
    grabFirst (e        :   es') = go [e] es'
      where
        go elts []                   = (Array (reverse elts), [])
        go elts es''@(Splice {} : _) = (Array (reverse elts), es'')
        go elts (elt : es'')         = go (elt:elts) es''

mkAppend :: SrcExpr -> SrcExpr -> DsM SrcExpr
-- eAppend e1 e2  =   e1 ++ e2
mkAppend (Array xs) (Array ys) = pure (Array (xs ++ ys))
mkAppend x          y          = do { r    <- newIdent noLoc "r"
                                    ; pure $ eSeq [ ApplyD (EPrim ArrApp) (Array [x, y, DefineV r])
                                                  , Variable r ] }

--------------------------------------------------------
--
--     Desugaring Essential Verse to Mini Verse
--        The W (wrapper introduction) transformation
--        Fig 6 in verse-spec.pdf
--
--------------------------------------------------------

data WContext   -- The context for the W transformation
  = WC { wc_inp :: Input
       , wc_fxs :: EffContext
       }
  deriving( Show )

data Input
  = NoInput      -- ^ Typeset as bullet, circle, or underscore
  | PI SrcMini    -- ^ An input variable x
  deriving( Show )

data EffContext
  = DomCtxt       -- In the domain (argument) of a function, nothing to push down
  | RngCtxt Eff   -- In the range (body) of a function with effects Eff
  deriving( Show )

{- Note [Pushing down effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this partially opaque function
  f(x:int)<decides> := ( e |> t, 7 )
When verifying, a client should see
  f = \i. x:=int[i]; ( (havoc<decides>; some(t)), 7 )
The opaque bits, under the (e |> t) should inherit the <decides> from
the function definition.  This is the "pushing down" in `essToMini`:

* The EffContext pushes into the body (range) of the function
* It gets intersected into any |> opacity constructs

The EffContext is also used in essToMini 
-}

essToMini :: SrcEssential -> DsM SrcMini
-- Essential Verse --> Mini Verse
essToMini orig_e = go_expr orig_e
  where
    kap_init = WC { wc_inp = NoInput, wc_fxs = DomCtxt }

    go_expr = go kap_init

    go :: WContext -> SrcEssential -> DsM SrcMini
    -- Typically (go kap t) = e, where `kap::(Input,[Eff])` is short for `kappa`

    -- Simple cases: WCONST, WVAR, WPRIM, WFAIL, WSEQ, WSUNIFY, WCHOICE
    go kap e@(Lit {})      = kap `ueq` return e
    go kap e@(Variable v)
       | isSrcUnderscore v = case wc_inp kap of
                               NoInput -> return existsXX
                               PI i    -> return i
       | otherwise         = kap `ueq` return e
    go kap e@(EPrim {})    = kap `ueq` return e
    go _kp Fail            = return Fail
    go kap (Unify  e1 e2)  = eUnify  <$> go kap e1 <*> go kap e2
    go kap (Choice e1 e2)  = Choice  <$> go kap e1 <*> go kap e2
    go kap (Where t1 t2)   = do { (dr, r) <- defineDE "r" (go kap t1)
                                ; e2 <- go_expr t2
                                ; return (eSeq [dr, e2, r]) }

    -- Sequential composition: WSEMI
    go kap (Seq t1 t2)     = do { e1 <- go_expr t1 -- NB: t1 gets no effects at all
                                ; e2 <- go kap t2
                                ; return (mkSeq e1 e2) }

    -- if, for, all one: WIF, WFOR, WALL, WONE
    go kap (If3 t1 t2 t3) = If3  <$> go_expr t1 <*> go kap t2 <*> go kap t3
    go kap (For2 t1 t2)   = kap `ueq` (For2   <$> go_expr t1 <*> go kap t2)
    go kap (All t)        = kap `ueq` (All    <$> go_expr t)
    go kap (One t)        = kap `ueq` (One    <$> go_expr t)

    -- Application: WAPP
    go kap (ApplyD t1 t2) = kap `ueq` (ApplyD <$> go_expr t1 <*> go_expr t2)

    -- WEXISTS (exists x);  WDEF (x:=t)
    go kap (Exists xs e) = Exists xs <$> go kap e   -- Just propagate Exists
    go _kp (DefineV x)   = return (DefineV x)
    go kap (DefineE x t) = do { e <- go kap t
                              ; return (eSeq [DefineV x, eUnify (Variable x) e]) }

    -- WSQUIG (x~>y:=t)
    go kap (DefineIE x t) = do { capture <- kap `ueq` pure (Variable x)
                               ; e <- go (kap { wc_inp = PI (Variable x) }) t
                               ; return (eSeq [DefineV x, capture, e]) }

    -- WCHK: check<fx>{t}
    go kap (Check fx t) = Check fx <$> go kap t

    -- WCOL1, WCOL2, WCOL3: (:t)
    go (WC { wc_inp = inp, wc_fxs = cfxs }) (Range t)
      = case inp of
          NoInput -> do { e <- go_expr t
                        ; x <- newIdent (getLoc t) "x"
                        ; return (eSeq [DefineV x, ApplyD e (Variable x)]) }
          PI i -> case cfxs of
                     DomCtxt     -> ApplyD <$> go_expr t <*> pure i
                     RngCtxt fxs -> OfType i fxs <$> go_expr t
                       -- See Note [Pushing down the effects]

    -- WOFTYPE: t1 |> t2
    go kap@(WC { wc_fxs = cfxs }) (OfType t1 fxs2 t2)
      = OfType <$> go (kap { wc_fxs = DomCtxt }) t1
              -- Push the input into t1, but OfType deals with effects,
              -- so don't push RngCtxt into t1
               <*> pure fxs'
               <*> go_expr t2
      where
        fxs' = case cfxs of
                 DomCtxt      -> effSucceeds -- ToDo: ignore fxs2?
                 RngCtxt fxs1 -> fxs1 `intersectEffects` fxs2

    -- Arrays: <t1, .., tn>.  Need to take care of splices
    go kap (Splice t) = go kap t

    -- Arrays: WTUP1 and WTUP2
    go kap@(WC { wc_inp = inp }) (Array ts)
      = case inp of
          NoInput -> -- WTUP1
                     do { es <- mapM do_one ts; mkArray es }
                   where
                     do_one (Splice t) = Splice <$> go kap t
                     do_one t          =            go kap t

          PI i | null ts   -- Optimisation: instead of (i=<>; <>), just generate (i=<>)
               -> pure (eUnify i (Array []))
               | otherwise     -- WTUP2
               -> do { prs <- mapM do_one ts
                     ; let (exi_js, es) = unzip prs
                     ; exi_js_arr <- mkArray exi_js
                     ; res_arr    <- mkArray es
                     ; pure (eSeq [ eUnify i exi_js_arr, res_arr ]) }
              where
                do_one :: SrcExpr -> DsM (SrcExpr, SrcExpr)
                -- Returns the pattern-match decl, and the thing to put in the tuple
                do_one (Splice t) = do { (d, e) <- do_one t
                                       ; pure (Splice d, Splice e) }
                do_one         t  = do { j <- newIdent (getLoc t) "j"
                                       ; e <- go (kap { wc_inp = PI (Variable j) }) t
                                       ; pure (DefineV j, e) }

    -- truth{t}: WTRU1 and WTRU2
    go kap@(WC { wc_inp = inp }) (Truth t)
      = case inp of
          NoInput -> Truth <$> go (kap { wc_inp = NoInput }) t
          PI i -> do { j <- newIdent (getLoc t) "j"
                     ; e <- go (kap { wc_inp = PI (Variable j) }) t
                     ; return (eSeq [ DefineV j, eUnify i (Truth (Variable j)), Truth e ]) }

    -- Functions: WFUN1, WFUN2
    go (WC { wc_inp = inp }) (Function aperture t1 fxs1 t2)
      = case inp of
          NoInput -> do { i <- newIdent (getLoc t1) "i"
                        ; let kap_arg  = WC { wc_inp = PI (Variable i), wc_fxs = DomCtxt }
                              kap_body = WC { wc_inp = NoInput,         wc_fxs = RngCtxt fxs1 }
                        ; XDLam Closed i <$> go kap_arg t1 <*> fmap (eCheck fxs1) (go kap_body t2) }
          PI f -> do { i <- newIdent (getLoc t1) "i"
                     ; x <- newIdent (getLoc t2) "x"
                     ; let kap_arg  = WC { wc_inp = PI (Variable i),            wc_fxs = DomCtxt}
                           kap_body = WC { wc_inp = PI (ApplyD f (Variable x)), wc_fxs = RngCtxt fxs1 }
                     ; e1 <- go kap_arg  t1
                     ; e2 <- go kap_body t2
                     ; return (eSeq [ ApplyD (EPrim IsFun) f
                                    , XDLam aperture i (eSeq [DefineV x, eUnify (Variable x) e1])
                                                       (eCheck fxs1 e2) ]) }
                                      -- IsFun[f]: see test M26Mar25-5

    -- Core constructs used (only) in the Prelude
    -- Only used in the NoInput case
    go (WC { wc_inp = NoInput }) (Lam x t)    = Lam x <$> go_expr t
    go (WC { wc_inp = NoInput }) (Some t)     = Some  <$> go_expr t
    go (WC { wc_inp = NoInput }) (Guard xs t) = Guard <$> go_expr xs <*> go_expr t

    -- Report any un-handled cases
    go kap t = error $ "TODO: essToMini " ++ show (kap, t)

    ueq :: WContext -> DsM SrcMini -> DsM SrcMini
    ueq (WC { wc_inp = inp, wc_fxs = cfxs }) ds_e
      = case inp of
           NoInput -> ds_e
           PI i -> case cfxs of
                    DomCtxt -> Unify i <$> ds_e  -- See M20Dec24-3 for a simple example
                    RngCtxt fxs -> do { e <- ds_e    -- See test `blame0` for a simple example
                                      ; v <- newIdent noLoc "i"
                                      ; pure (OfType i fxs (Lam v (Unify (Variable v) e))) }

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

miniToCore :: Bool -> SrcMini -> DsM SrcCore
-- The V transformation; Fig 9 in verse-spec.pdf
miniToCore add_verification e_top
  | add_verification
  = do { e' <- go (MV True, []) e_top
       ; return (Verify [] (Check effSucceeds e')) }
  | otherwise
  = go (MX, []) e_top

  where
    go_nt :: (DsMode,[Ident]) -> SrcMini -> DsM SrcCore
    -- go_nt is just `go` in a non-tail context
    go_nt (MV True, xs) = go (MV False, xs)
    go_nt md            = go md

    go :: (DsMode, [Ident]) -> SrcMini -> DsM SrcCore
    go _md Fail            = return Fail  -- MFAIL
    go _md e@(Lit {})      = return e     -- MCONST
    go _md e@(Variable {}) = return e     -- MVAR
    go _md e@(EPrim {})    = return e     -- MOP
    go _md e@(DefineV {})  = return e     -- MBIND

    -- MTUP, MTRUTH, MSEMI, MEQ, MCHOICE, MAPP
    go md (Array es)     = Array <$> mapM (go_nt md) es
    go md (Truth e)      = Truth <$> go md e
    go md (Seq e1 e2)    = mkSeq  <$> go_nt md e1 <*> go md e2
    go md (Unify e1 e2)  = eUnify <$> go_nt md e1 <*> go_nt md e2
    go md (ApplyD e1 e2) = ApplyD <$> go_nt md e1 <*> go_nt md e2
    go md (Choice e1 e2) = Choice <$> go md    e1 <*> go md    e2
    go md (Exists xs e)  = Exists xs <$> go md e

    -- MFOR, MIF
    go md (For2 e1 e2)   = For2 <$> go_nt md e1 <*> go_nt md e2
    go md (If3 e1 e2 e3) = If3 <$> go_nt md e1 <*> go_nt md e2 <*> go_nt md e3
    go md (One e)        = One <$> go_nt md e
    go md (All e)        = All <$> go_nt md e

    -- MOFTYPE-, MOFTYPE+X: (e1 |> e2)
    go md@(sig,xs) (OfType e1 fx e2)
      = case sig of
          MX       -> do_mvmx
          MI       -> do_mi
          MV False -> do { body <- do_mvmx
                         ; rest <- do_mi
                         ; return (eSeq [eVerify [] body, rest]) }
          MV True -> do { body <- do_mvmx
                        ; return (eVerify [] body) }
      where
        do_mi = do { (dz, z) <- defineDE "z" (go (MI,xs) e2)
                         -- Very important: use MX here because we don't want
                         -- to generate verify's inside the Some.
                         -- Small example: M20Jan25-1
                   ; let gds = nub (xs ++ getFree e1)
                         -- Guard on both free vars of e1 and lambda-bound vars
                         -- Example Fin4: y:int := y

                   ; return (eGuard gds (eSeq [ eHavoc fx, dz, eSome z])) }

        do_mvmx = do { e1' <- go_nt md e1   -- Notice md; may be MV or MX
                     ; e2' <- go_nt md e2
                     ; if isAtomic e1'  -- Just an optimisation
                       then return (Check effSucceeds (ApplyD e2' e1'))
                       else do { r <- newIdent (getLoc e1) "r"
                              ; return (eSeq [ DefineV r, eUnify (Variable r) (eCheck fx e1')
                                             , Check effSucceeds (ApplyD e2' (Variable r)) ]) } }

    -- MCHECK-, MCHECK+X:  check<fx>{e}
    go md@(m,_) (Check fx e)
      = case m of
           MV {} -> Check fx <$> go_nt md e   -- ToDo: check that go_mt

           MI    -> go md e  -- No Check here: needed for tests
                             -- M19Mar25-2, M20Dec24-1, S1Aug24-3, T29Jul24-3
                             -- Transparent functions act like "macros"

           MX    -> Check fx <$> go md e
                    -- For consistency, we want no Check here either
                    -- Again, transparent functions act like "macros"
                    -- But if we remove the Check:
                    --    S1Aug24-4, Ev23, EV23a start passing
                    --    For6, check2, check7 start failing

    -- Functions proper: MCFUN+, MCFUN-, MCFUNX
    go (MV omit_client, xs) e@(XDLam Closed x e1 e2)
      | omit_verify, omit_client = return (Array [])
      | omit_verify              = go (MI,xs) e
      | omit_client              = do_verify
      | otherwise
      = do { ever <- do_verify
           ; efun <- go (MI, xs) e
           ; return (eSeq [ever, efun]) }
      where
        omit_verify = shortCutDefnVerify e1 e2
        do_verify = do { e1' <- go (MI,      x:xs) e1
                       ; e2' <- go (MV True, x:xs) e2
                       ; return (eVerify [x] (eSeq [e1',e2'])) }

    go (MI,xs) (XDLam Closed x e1 e2)
      = do { e1' <- go (MV False, x:xs) e1
           ; e2' <- go (MI, x:xs) e2
           ; return (Lam x (eSeq [ e1', e2' ])) }
    go (MX,xs) (XDLam Closed x e1 e2)
      = do { e1' <- go (MX, x:xs) e1
           ; e2' <- go (MX, x:xs) e2
           ; return (Lam x (eSeq [ e1', e2' ])) }

    -- Pass through Core constructs, used in Prelude
    go md (Lam x t)    = Lam x <$> go md t
    go md (Guard xs t) = Guard <$> go md xs <*> go md t
    go md (Some t)     = Some  <$> go md t

    go md e = error $ "TODO: miniToCore " ++ show (md, e)

shortCutDefnVerify :: SrcMini -> SrcMini -> Bool
-- True if the expression definitely verifies
-- Used to reduce clutter, esp for type{e} = fun(x:=e){x}
shortCutDefnVerify e1 e2
  | isConst e2         = True
  | Variable {} <- e2  = True
  | Check eff body <- e2
  , eff == effSucceeds
  = case body of
      Variable x       -> x `elem` pat_binders
      _ | isConst body -> True
        | otherwise    -> False
  | otherwise
  = False
  where
    pat_binders = getVisibleBinders e1

------- Encodings with iter ---------------------------

defineDE :: String -> DsM SrcExpr
         -> DsM (SrcExpr,   -- The defn
                 SrcExpr)   -- The use
-- define "x" de   returns   x := e, along with x itself
-- But (just to save clutter) if the rhs turns out to be tiny,
--   just return it alone
defineDE nm ds_rhs
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then pure (eSeq [], rhs')
         else do { x <- newIdent (getLoc rhs') nm
                 ; pure (coreDefine x rhs', Variable x) } }

coreDefine :: Ident -> SrcCore -> SrcCore
coreDefine x (Seq e1 e2) = mkSeq e1 (coreDefine x e2)
coreDefine x rhs         = mkSeq (DefineV x) (Unify (Variable x) rhs)

-----------------------------------------------
--
--    addPrelude: a small pass to add the prelude
--
-----------------------------------------------


addPrelude :: SrcExpr -> DsM SrcExpr
addPrelude orig_e
  = do { prel  <- getDFlagsX (snd . fPrelude)
       ; return (addUsed (spl prel) orig_e) }
  where
    -- Split the prelude into an association list
    spl (Array ds) = map (\ e -> (nameOf e, e)) ds
    spl e = impossible "addPrelude1" e

    -- Find the name of a definition
    nameOf (InfixOp lhs (Ident _ ":=") _) = lhsName lhs
    nameOf e = impossible "addPrelude2" e

    lhsName :: SrcExpr -> Ident
    lhsName (EffAttr e _) = lhsName e
    lhsName (ApplyS e _) = lhsName e
    lhsName (Variable i) = i
    lhsName e = impossible "addPrelude3" e

-- Hackily add prelude identifiers used in e
addUsed :: [(Ident, SrcExpr)] -> SrcExpr -> SrcExpr
addUsed prel = loop []
  where
    loop vs e =
      let is = getAllIdents e
          ps = filter (\ (i, _) -> i `elem` is && i `notElem` vs) prel
      in  case ps of
            [] -> e
            ies -> loop (map fst ies ++ vs) $ eSeq (map snd ies ++ [e])


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

runD :: Flags -> DsM a -> IO a
-- Runs the DsM monad
-- May throw an exception in case of errors
runD flags (MkD thing_inside)
  = do { nextref <- newIORef 1
       ; scopeErrRef <- newIORef []
       ; let env = DEnv { nextNo = nextref, scopeErr = scopeErrRef, dflags = flags }
       ; res  <- thing_inside env
       ; errs <- readIORef scopeErrRef
       ; case errs of
           [] -> return res
           _  -> error ("Errors: " ++ show (nub errs))
       }

traceDS :: String -> SrcExpr -> DsM SrcExpr
traceDS msg e = do { traceD msg (pPrint e)
                   ; pure e }

traceD :: String -> Doc -> DsM ()
traceD msg doc
  = do { do_trace <- getDFlagsX fTraceDesugar
       ; when do_trace $ doIO_D $
         do { putStrLn ("\n------- " ++ msg ++ "---------")
            ; putStrLn (render (indent doc)) }
       }

doIO_D :: IO a -> DsM a
doIO_D io = MkD (\_ -> io)

getDFlags :: DsM Flags
getDFlags = MkD (\(DEnv { dflags = flags }) -> return flags)

putScopeErr :: Ident -> DsM ()
putScopeErr i = MkD (\(DEnv { scopeErr = ref }) -> modifyIORef ref (MkDError i:))

getDFlagsX :: (Flags -> a) -> DsM a
getDFlagsX f = f <$> getDFlags

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
  pPrintPrec l p (MkDError i) = text "unbound identifer" <+> pPrintPrec l p i
