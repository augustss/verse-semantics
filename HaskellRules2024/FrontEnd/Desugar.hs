{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DeriveFunctor #-}
module FrontEnd.Desugar(
      desugar
    , D, runD, traceD, getDFlagsX, traceDS
  ) where

import Prelude hiding (pi)

import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags
import Rules.Core( PrimOp(..), allPrimOps, primOpString )

-- Epic libraries
import Epic.Print

-- General libraries
import Data.Maybe( catMaybes )
import Data.IORef
import Data.List
import qualified Data.Set as S
import Control.Monad

import GHC.Stack
--import Debug.Trace

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
     <=< mDesugarExpr ds_model

     -- Side effects
--   <=<  traceDS "addDeref"
--   <=< addDeref

     -- Desugar into Small Source
     <=< traceDS "Desugar to Small Source"
     <=< sDesugarExpr

     -- Prepends prelude from
     --    verifyprelude.verse, mediumprelude.verse
     <=< traceDS "Add prelude"
     <=< addPrelude

     -- Syntax fixes
     <=< traceDS "syntaxFixes"
     <=< syntaxFixes
     <=< traceDS "parsed" )
  where
    ds_model | add_verification = MV
             | otherwise        = MX

--------------------------------------------------------
--
--           The S-desugaring
--     Desugar into Small Source Verse
--     Figs 3 and 4 of desugaring.pdf
--
--------------------------------------------------------

sDesugarExpr :: SrcExpr -> D SrcSmall
sDesugarExpr = ds
  where
    ds :: SrcExpr -> D SrcSmall


    -- These can happen when going via Andy's stuff
{-
    ds (ApplyD (Variable (Ident l s)) (Array [e1,e2])) | Just r <- stripPrefix "operator'" s =
      ds (InfixOp e1 (Ident l (init r)) e2)
    ds (ApplyD (Variable (Ident l s)) e) | Just r <- stripPrefix "prefix'" s =
      ds (PrefixOp (Ident l (init r)) e)
-}
    ds (ApplyD (Variable (Ident l s)) e) | Just r <- stripPrefix "postfix'" s, r `elem` ["?'"] =
      ds (PostfixOp e (Ident l (init r)))


    -- Application
    ds (ApplyD  e1 e2) = ApplyD <$> ds e1 <*> ds e2
    ds (ApplyS  e1 e2) = applyS <$> ds e1 <*> ds e2
      where
        -- This replaces f(e) with check<succeeds>{f[e]}
        applyS x y = eCheck [effSucceeds] (ApplyD x y)

    -- (e1 = e2)  --->  Unify
    ds (InfixOp e1 (Op "=")  e2) = Unify <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op ">>") e2) = Guard <$> ds e1 <*> ds e2

    -- Bindings
    ds (InfixOp e1 (Op ":=") e2) = defn e1 e2 >>= ds

    ds (InfixOp e1 (Op ":")  e2)
      | Just es <- getAmpersands e1
        -- f&g : ty       -->  (f:ty, g:ty)
      = ds (Array [ InfixOp e (Op ":") e2 | e <- es ])

      | Just (f, a, fxs) <- getFun e1
        -- f(a)<fx> : ty  -->  f := fun(a){ :ty<fx> }
        --        e : ty  -->  e := :ty<>
        -- c.f. the ":" case of `defn`
      = defn f (Function [a] (Range fxs e2)) >>= ds

      | otherwise
      = defn e1 (Range [] e2) >>= ds

    -- Function notation
    ds (InfixOp e1 (Op "=>") e2)  = ds $ Function [(e1, [closedId])] (eCheck [effSucceeds] e2)
       -- The e1=>e2 notation has an implicit check<succeeds>
    ds (Function (a:as@(_:_)) b)  = ds $ Function [a] $ Function as b
    ds (Function [(e1, effs)] e2) = do
           e1' <- ds e1
           e2' <- ds e2
           effs' <- checkEffs effs
           pure $ Function [(e1', effs')] e2'

    -- Conditionals
    -- We must retain IF3 (i.e `if e1 then e2 else e3`) because
    -- the main dsM_12 desugaring needs to push `pi` into the branches.
    ds (If1 e)        = ds $ If2E e eFalse
    ds (If2 e1 e2)    = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2)   = do x <- newIdent (getLoc e1) "x"; ds $ If3 (eDefine x e1) (Variable x) e2

    -- For-loops
    -- for(e1){e2} = arrMap$[ \t. t[], all{ e1; \_.e2 } ]
    ds (For1 e)     = do x <- newIdent (getLoc e) "x"
                         ds $ For2 (eDefine x e) (Variable x)
    ds (For2 e1 e2) = do e1' <- ds e1
                         e2' <- ds e2
                         pure (ApplyD (EPrim ArrMap)
                                      (Array [eForceLam
                                             , All (eSeq [e1', eThunk e2'])]))

    -- Array
    ds (Array es) = Array <$> mapM ds (concatMap flattenAmpersandElt es)

    -- Let and where
    --    (let e in b)  --> e; b
    --    (e1 where e2) --> e1 where e2   Need to keep this for M-desugaring!
    ds (Let e b) = do { e' <- ds e; b' <- ds b; pure (Seq [e',b']) }
    ds (InfixOp e1 (Op "where") e2) = Where <$> ds e1 <*> ds e2

    -- Do and case
    ds (Case1 b)     = do { let l = getLoc b
                          ; x <- Variable <$> newIdent l "x"
                          ; ds $ Function [(InfixOp x (Op ":") eAny, [])] $
                                 Case2 x b }
    ds (Case2 _ _)    = undefined
    ds (Block b)      = ds b                              -- do e --> e
    ds (Blk es)       = ds $ eSeq es
    ds e@(DefineV {}) = pure e
--      | isSrcUnderscore i = DefineV <$> newIdent (getLoc e) "x"

    ds (Seq es) = eSeq <$> mapM ds es
    ds (OfType e1 eff e2) = OfType <$> ds e1 <*> pure eff <*> ds e2

    -- Operators
    -- NB: Prefix '?' is just a function now; see note [Truth values] in Rules.Core
    ds (PrefixOp (Op "not") e)   = do e' <- ds e; pure $ If3 e' Fail eFalse
    ds (PrefixOp (Op ":") e)     = Range [] <$> ds e
    ds (PrefixOp (Op "..") e)    = Splice   <$> ds e
    ds (PrefixOp (Ident l op) e) = ds =<< call Pre l op e

    -- e?  means simply  e[_]  or equivalently   exists x. e[x]
    ds (PostfixOp e (Ident l "?"))  = ds $ ApplyD e (Variable (Ident l "_"))
    -- All other postfix ops
    ds (PostfixOp e (Ident l op))   = ds =<< call Post l op e

    -- Infix ops
    ds (InfixOp e1 (Op "|") e2)     = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2)   = ds $ Seq [e1, e2]                  -- XXX multiplicity?
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

    ds (Macro1 (Ident _ "one")   _ e)   = One <$> ds e
    ds (Macro1 (Ident _ "all")   _ e)   = All <$> ds e
    ds (Macro1 (Ident _ "verify") _ e)  = Verify [] <$> ds e
    ds (Macro1 (Ident _ "some") _ e)    = Some <$> ds e
    ds (Macro1 (Ident _ "check") fx e)  = Check fx <$> ds e
    ds (Macro1 (Ident _ "succeeds") _ e)= eCheck [effSucceeds] <$> ds e
    ds (Macro1 (Ident _ "decides") _ e) = eCheck [effDecides] <$> ds e

    -- assume<fx>{e}  ==   havoc<fx>; some(\x. x=e; x)
    ds (Macro1 (Ident _ "assume") _ e)  = do { x <- newIdent (getLoc e) "x"
                                             ; e' <- ds e
                                             ; pure (Some (Lam x (Seq [e', Variable x]))) }

    -- first{e}        ==  if (x:=e) then x  else fail
    -- first(e1){e2}   ==  if e1     then e2 else fail
    ds (Macro1 (Ident _ "first") [] e)  = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    -- type{t}  ==  Fun(x := t)<closed>{x}
    ds (Macro1 (Ident _ "type") _ e)
      = do { x <- newIdent (getLoc e) "x"
           ; ds $ Function [(eDefine x e, [closedId])] (Variable x) }
           --  A more direct desugaring, which generates less verification clutter,
           --  is   type{t} --> \x. x=t
           -- But it is a wrong desugaring. e.g   type{_(:int):int}  test "HO15"

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

flattenAmpersandElt :: SrcExpr -> [SrcExpr]
flattenAmpersandElt e = case getAmpersands e of
                          Just es -> es
                          Nothing -> [e]

getAmpersands :: SrcExpr -> Maybe [SrcExpr]
-- `getAmpersands` flattens out all the nested `&` into a list
-- (e1 & e2 & 3)  -->   Just [e1,e2,e3]
-- e              -->   Nothing         # if e is not (e1&e2)
getAmpersands e
  | InfixOp _ (Op "&") _ <- e  = Just (get e)
  | otherwise                  = Nothing
  where
    get (InfixOp p1 (Op "&") p2) = get p1 ++ get p2
    get p                        = [p]

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

{-
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
  pure $ eSeq [eDefine f e1, r]

apply1 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcExpr -> D SrcExpr
-- val1[val2]
apply1 con x1 e2 | isValue e2 = apply2 con x1 e2   -- Easy special case.  Not really needed
-- val1[e2]  -->  a:=e2; val1[a]
apply1 con x1 e2 = do
  a <- newIdent (getLoc e2) "a"
  r <- apply2 con x1 (Variable a)
  pure $ eSeq [eDefine a e2, r]

-- val1[val2]  -->
apply2 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcValue -> D SrcExpr
apply2 con x1 x2 = pure $ con x1 x2
-}

--------------------------------------
-- Patterns
--------------------------------------

defn :: SrcPat -> SrcExpr -> D SrcExpr
-- Desugars (p := e) into an expression; see Fig 3, top group
-- Neither input is desugared; the result
--   should have sDesugarExpr applied to it.

-- Rule: (i := e) -->  (i := e)
defn (Variable (Ident _ "_")) e = do
  x <- newIdent (getLoc e) "u"
  pure $ eDefine x e

defn (Variable i) e = pure $ eDefine i e

-- Rule:   p(a)<fxs> := rhs   -->  p := fun(a){check<fxs>{rhs}}
--         Adding <succeeds> if not present
-- E.g. f(x)<decides> := 3    # NB: has no effect on caller, which still sees 3
defn p e
  | Just (f, a, fx) <- getFun p
  = defn f (Function [a] (eCheck fx e))

-- Rule: (f(a)<fx>:e2 := e)  -->  (e1 := fun(a){e |><fx> e2})
-- Rule:       (e1:e2 := e)  -->  (e1 :=        e |><>   e2)
-- but adding <succeeds> if omitted, leaving behind <open/closed>
defn (InfixOp e1 (Op ":") e2) e
   | Just (f, a, fxs) <- getFun e1
   = defn f (Function [a] (OfType e fxs e2))
   | otherwise
   = defn e1              (OfType e []  e2)

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

defn (Array ps) e = defnArray (concatMap flattenAmpersandElt ps) e

-- Rule (p1 -> p2) := e  -->  p1 := x1; p2 := x2; (x1 -> x2) := e
defn (InfixOp (Variable x1) (Op "->") (Variable x2)) e = pure $ DefineIE x1 x2 e
defn (InfixOp x1@Variable{} op@(Op "->") p2) e = do
  x2 <- Variable <$> newIdent (getLoc p2) "x"
  r2 <- defn p2 x2
  r  <- defn (InfixOp x1 op x2) e
  pure $ eSeq [r2, r]
defn (InfixOp p1 op@(Op "->") p2) e = do
  x1 <- Variable <$> newIdent (getLoc p2) "x"
  r1 <- defn p1 x1
  r  <- defn (InfixOp x1 op p2) e
  pure $ eSeq [r1, r]

defn p _ = errorMessage $ "Bad LHS to := " ++ prettyShow p


addSucceeds :: [Eff] -> [Eff]
addSucceeds [] = [effSucceeds]   -- Default is <succeeds>
addSucceeds fx = fx

--------------------------------------------------------
--         Functions to take apart SrcExpr
--------------------------------------------------------

getFun :: SrcExpr -> Maybe (SrcExpr, (SrcExpr,[Eff]), [Eff])
-- f(a)<decides><closed>  -->  Just (f, (a,<closed), <decides>)
-- The (SrcExpr,[Eff]) ends up on the Function;
-- while the final [Eff] refers to the body of the function
getFun = gf []
  where
    gf rs (ApplyS f a) = Just (f, (a,fun_effs), addSucceeds body_effs)
      where
        (fun_effs, body_effs) = partition isOpenClosed rs
    gf rs (EffAttr e r)       = gf (r:rs) e
    gf _ _                    = Nothing

getEffs :: SrcExpr -> (SrcExpr, [Eff])
-- e<fx1><fx2> --> (e, [fx1,fx2])
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

defnArray :: [SrcPat] -> SrcSmall -> D SrcSmall
-- Dealing with an array on the LHS of a ":=", i.e.  an "array pattern"
-- For example
--     (p1, p2, ..p3, p4) := e
-- --->
--       p1 := x1; p2 := x2; p3 :=  x3; p4 := x4
--       arraySplice{ exists x1, exists x2, ..exists x3, exists x4 }
--  -->
--       p1 := x1; p2 := x2; p3 := x3; p4 := x4
--       arrApp$[ ar{exists x1,exists x2}, exists x3, exists t1 ]
--       arrApp$[ t1, exists x4, exists t2 ]
--       t2 = e
--
-- Short cut if pi is a variable.  E.g. if p1 is a variable a, we et
--       p2 := x2; p3 := x3; p4 := x4
--       arraySplice{ exists a, exists x2, ..exists x3, exists x4 }

defnArray ps rhs
  = do { (ds, es) <- unzip <$> mapM do_one_elem ps
       ; arr      <- mkArray es
       ; pure $ eSeq $ catMaybes ds ++ [Unify arr rhs] }
  where
    do_one_elem :: SrcPat -> D (Maybe SrcSmall, SrcSmall)
    do_one_elem (PrefixOp (Op "..") p) = do { (md, e) <- do_one p
                                            ; pure (md, Splice e) }
    do_one_elem p                      = do_one p

    do_one :: SrcPat -> D (Maybe SrcSmall, SrcSmall)
    do_one pat
      | Variable v <- pat
      = -- Short cut for a common case: (a,b):=e
        pure (Nothing, DefineV v)
      | otherwise
      = do { x <- newIdent (getLoc pat) "x"
           ; d <- defn pat (Variable x)
           ; pure (Just d, DefineV x) }

mkArray :: [SrcSmall] -> D SrcSmall
mkArray es
  = case grabFirst es of
      (e', [])  -> pure e'
      (e', es') -> do { rest <- mkArray es'; mkAppend e' rest }
  where
    grabFirst :: [SrcSmall] -> (SrcSmall, [SrcSmall])
    grabFirst []                 = (Array [], [])
    grabFirst (Splice e : es')   = (e,es')
    grabFirst (e        :   es') = go [e] es'
      where
        go elts []                   = (Array (reverse elts), [])
        go elts es''@(Splice {} : _) = (Array (reverse elts), es'')
        go elts (elt : es'')         = go (elt:elts) es''

mkAppend :: SrcExpr -> SrcExpr -> D SrcExpr
-- eAppend e1 e2  =   e1 ++ e2
mkAppend (Array xs) (Array ys) = pure (Array (xs ++ ys))
mkAppend x          y          = do { r    <- newIdent noLoc "r"
                                    ; pure $ eSeq [ ApplyD (EPrim ArrApp) (Array [x, y, DefineV r])
                                                  , Variable r ] }

{-
data ArrayElem e = EElem e | ESplice e
  deriving (Show, Functor)

arraySplice :: [SrcSmall] -> D SrcSmall
-- arraySplice [es1, ..e, es2]
--   -->  e1 = arraySplice e1 ++ (e ++ arraySplice es2)
arraySplice as
  = case arrayElems as of
      []          -> pure $ Array []
      e:es        -> app (arr e) es
  where
    arr (EElems es) = Array es
    arr (ESplice e) = e

    app r [] = pure r
    app r (e : es) = do
      t <- newIdent noLoc "t"
      rest <- app (DefineV t) es
      pure $ eSeq [eAppend r (arr e) (Variable t), rest]

   -- app e1 [e2,e3]
   --  =  append[e1,e2,t1]; app (exists t1) [e3]
   --  =  append[e1,e2,t1]; append[exists t1,e3,t2]; app (exists t2) []
   --  =  append[e1,e2,t1]; append[exists t1,e3,t2]; exists t2

arrayElems :: [SrcSmall] -> [ArrayElem]
-- Handle an array element, it can be ..e or e
-- Returns a list of [EElems es, ESplice e, ESplice e, .. ]
-- Where we maximally group the EElems, for efficiency
arrayElems = grp . map classify
  where
    classify :: SrcSmall -> ArrayElem
    classify (PrefixOp (Ident _ "..") e) = ESplice e
    classify e                           = EElems [e]

    grp :: [ArrayElem] -> [ArrayElem]
    -- Group adjacent EElems together
    grp []                = []
    grp (ESplice e : as)  = ESplice e : grp as
    grp (EElems es1 : as) = case grp as of
                             EElems es2 : as' -> EElems (es1++es2) : as'
                             as'              -> EElems es1        : as'
-}



--------------------------------------
-- Calls
--------------------------------------

data CallFixity = Pre | Post | In
  deriving(Show)

-- Use of an pre-/post-/in-fix operator
call :: CallFixity     -- fixity of calls
     -> Loc -> String  -- Function
     -> SrcExpr        -- Argument
     -> D SrcExpr
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
    expr _ (DefineV i)      = DefineV i
    expr s (DefineE i e)    = DefineE i (expr s e)
    expr s (DefineIE i j e) = DefineIE i j (expr s e)
    expr s (Choice e1 e2) = Choice (exprD s e1) (exprD s e2)
    expr s (Set e1 (Ident l sop) e2) = set s e1 op (expr s e2)
      where op = Ident l ("in'" ++ sop ++ "'")
    expr s (MVar i (Just t) (Just e)) = DefineE i $ ApplyD (applyPrimD "new" (expr s t)) (expr s e)
    expr s (Range fx e1) = Range fx (expr s e1)
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
--           The M-desugaring
--     Desugaring Small Source -> Big Core
--        Fig 9 in desugaring.pdf
--
--------------------------------------------------------

data Pi
  = P SrcCore  -- ^ P(x)    The SrcCore is always small and freely duplicable
               --           Typically just (Variable x)
  | E          -- ^ E
  deriving (Eq, Ord, Show)

data DsMode12
  = MX -- ^ x "execution"
  | MV -- ^ + "verification"
  | MI -- ^ - "checking" ("implementation")
  deriving (Eq, Ord, Show)

mDesugarExpr :: DsMode12 -> SrcSmall -> D SrcCore
mDesugarExpr s t = dsM_12 s t E

dsB_12 :: DsMode12 -> SrcSmall -> Pi -> SrcCore -> D SrcExpr
dsB_12 s t E     _  = dsM_12 s t E
dsB_12 s t (P f) j  = dsM_12 s t (P (ApplyD f j))
-- SLPJ: Crucial chnage; this makes HO13 work
--  = do z <- newIdent (getLoc t) "z";
--       seqDE [ pure $ eDefine z (ApplyD f j)
--             , dsM_12 s t (P (Variable z))]

seqDE :: [D SrcCore] -> D SrcCore
seqDE ds = eSeq <$> sequence ds

defineDE :: String -> D SrcExpr
         -> D (SrcExpr,   -- The defn
               SrcExpr)   -- The use
-- define "x" de   returns   x := e, along with x itself
-- But (just to save clutter) if the rhs turns out to be tiny,
--   just return it alone
defineDE nm ds_rhs
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then pure (eSeq [], rhs')
         else do { x <- newIdent (getLoc rhs') nm
                 ; pure (eDefine x rhs', Variable x) } }

defineDE2 :: String
          -> D SrcSmall              -- The RHS
          -> (SrcCore -> D SrcCore)  -- The body
          -> D SrcCore               -- z := rhs; body
defineDE2 nm ds_rhs ds_body
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then ds_body rhs'
         else do { x <- newIdent (getLoc rhs') nm
                 ; body' <- ds_body (Variable x)
                 ; if x `elem` getAllIdents body'  -- See Note [Avoiding clutter]
                   then pure (eSeq [eDefine x rhs', body'])
                   else pure (eSeq [rhs',           body']) } }

-- This avoids constructing '_ := e', which can happen with inputs like 'function(exists _){e}\
eDefine' :: Ident -> SrcExpr -> SrcExpr
eDefine' i e | isSrcUnderscore i = e
             | otherwise         = eDefine i e

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

dsM_12 :: HasCallStack => DsMode12 -> SrcSmall -> Pi -> D SrcCore
-- This one does the heavy lifting

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
-- M+[ t1 |>fx t2 ]pi = x := check<fx>{ M[t1]pi }
--                      check<succeeds>{ z := M+[t2]E; z[x] }
dsM_12 MI (OfType t1 fx t2) _pi      -- MOFTYPE-
    -- SLPJ: pi is unused, which seems suspicious
    -- But I think that's a correct reflection of opacity
  = do { (dz, z) <- defineDE "z" (mDesugarExpr MI t2)
       ; pure (eSeq [ dz, eGuard (getFree t1) (eSeq [eHavoc fx, eSome z]) ]) }

dsM_12 MV (OfType t1 fx t2) pi      -- MOFTYPE+X
   = do { (dx, x) <- defineDE "x" (eCheck fx <$> dsM_12 MV t1 pi)
        ; (dz, z) <- defineDE "z" (mDesugarExpr MV t2)
        ; pure (eSeq [dx, eCheck [effSucceeds] (eSeq [dz, eApplyD z x])]) }

dsM_12 s (OfType t1 fx t2) pi      -- MOFTYPEX
  = do { (e1, x) <- defineDE "x" (eCheck fx <$> dsM_12 s t1 pi)
       ; e2 <- dsM_12 s (Range fx t2) (P x)
       ; pure (eSeq [e1,e2]) }

-------------------- :{fx} t -----------------
-- Roughly:  M_s[ :<fx> t ] (P e)  =  M_s[ e |><fx> t ] E

dsM_12 MI (Range fx t) (P e)                 -- MTYPE-
  = do { (dz, z) <- defineDE "z" (mDesugarExpr MI t)
       ; pure (eSeq [dz, eGuard (getFree e) (eSeq [eHavoc fx, eSome z]) ]) }

dsM_12 s (Range fx t) (P e)                  -- MTYPE+X
  | null fx
  = do { (dz, z) <- defineDE "z" (mDesugarExpr s t)
       ; pure (eSeq [ dz, eApplyD z e]) }

  | otherwise
  = do { y <- newIdent (getLoc t) "y"
       ; (dz, z) <- defineDE "z" (mDesugarExpr s t)
       ; pure (eSeq [ eDefine y (eCheck fx e)
                    , eCheck [effSucceeds] (eSeq [dz, eApplyD z (Variable y)])]) }

dsM_12 s (Range _fx t) E                      -- MTYPEE
  = do { x <- newIdent (getLoc t) "x"
       ; (e, z) <- defineDE "z" (mDesugarExpr s t)
       ; let z_app = eApplyD z (Variable x)   -- z[x]
       ; pure (Exists [x] (eSeq [e, z_app])) }

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
  = do { body <- dsM_12 s t (P i)
       ; pure (eSeq [eDefine x i, eDefine y body]) }

-------------------- x := t -----------------
dsM_12 s (DefineE x t) pi                   -- MBIND
  = eDefine x <$> dsM_12 s t pi

-------------------- exists x -----------------
-- Equivalent to to (y:any) provided any = \x.x
--   M_sig[ exists y ]E    = exists y
--   M_sig[ exists y ]P(i) = y := i
dsM_12 _ (DefineV y) E     = pure $ DefineV y
dsM_12 _ (DefineV y) (P i) = pure $ eDefine' y i

-------------------- v >> t -----------------
dsM_12 s (Guard t1 t2) pi                   -- MGUARD
  = Guard <$> dsM_12 s t1 E <*> dsM_12 s t2 pi

-------------------- exi x. t -----------------
-- dsM_12 s (Exists is t) pi@(P {})
--  = do { let us = [ eDefine i eSomeAny | i <- is ]
--       ; e' <- dsM_12 s t pi
--       ; pure (eSeq (us ++ [e'])) }
--
dsM_12 s (Exists is t) E      = Exists is <$> dsM_12 s t E
dsM_12 _ t@(Exists {}) (P {}) = impossible "Exists in pattern" t

dsM_12 s (Lam x t) E          = Lam x <$> dsM_12 s t E
dsM_12 _ t@(Lam {}) (P {})    = impossible "Exists in pattern" t

dsM_12 s (Some t) E           = Some <$> dsM_12 s t E
dsM_12 _ t@(Some {}) (P {})   = impossible "Some in pattern" t

-------------------- array{t1,...tn} -----------------
dsM_12 s (Splice t) pi                       -- MARRAYE
   = Splice <$> dsM_12 s t pi

dsM_12 s (Array ts) E                       -- MARRAYE
   = do { elts <- mapM (\t -> dsM_12 s t E) ts; mkArray elts }

--      arraySplice =<< mapM elm es
--      where
--        -- SLPJ why not just to (mapM ds es)?
--        elm (PrefixOp dd@(Ident _ "..") e) = PrefixOp dd <$> ds e
--        elm e                              = ds e

dsM_12 s (Array ts) (P i)                   -- MARRAYP
   = do { prs <- mapM do_one ts
        ; let (ds, es) = unzip prs
        ; arr <- mkArray es
        ; res <- mkArray ds
        ; pure (eSeq [ Unify i arr, res ]) }
   where
     do_one :: SrcExpr -> D (SrcExpr, SrcExpr)
     -- Returns the pattern-match decl, and the thing to put in the tuple
     do_one (Splice e) = do { (d, e') <- do_one e
                            ; pure (Splice d, Splice e') }
     do_one         e  = do { j <- newIdent (getLoc e) "j"
                            ; e' <- dsM_12 s e (P (Variable j))
                            ; pure (DefineV j, e') }

-------------------- truth{t1} -----------------
dsM_12 s (Truth t) E                       -- MTRUTHE
   = Truth <$> dsM_12 s t E

dsM_12 s (Truth t) (P i)                   -- MTRUTHP
   = do { j <- newIdent (getLoc t) "j"
        ; e <- dsM_12 s t (P (Variable j))
        ; pure (eExists [j] (eSeq [ Unify i (Truth (Variable j))
                                  , Truth e ])) }

-------------------- t1 = t2 -----------------
dsM_12 s (Unify t1 t2) pi                   -- MEQ
  = Unify <$> dsM_12 s t1 pi <*> dsM_12 s t2 pi

-------------------- t1  where t2 -----------------
dsM_12 s (Where t1 t2) pi                   -- MWERE
  = do { (e1,z) <- defineDE "z" (dsM_12 s t1 pi)
       ; e2 <- mDesugarExpr s t2
       ; pure (eSeq [e1, e2, z]) }

-------------------- t1 ; t2 -----------------
dsM_12 MX (Seq ts) pi                      -- MSEMIX
  = do let (ts', t) = unSeq ts
       es' <- mapM (mDesugarExpr MX) ts'
       e'  <- dsM_12 MX t pi
       pure $ eSeq (es' ++ [e'])

dsM_12 s  (Seq ts) pi                      -- MSEMI
  = do let (ts', t) = unSeq ts
       es' <- mapM (mDesugarExpr MV) ts'
       e'  <- dsM_12 s t pi
       pure $ eSeq (es' ++ [e'])

-------------------- (t1 | t2) and fail -----------------
dsM_12 s t@(Choice {}) (P i)                 -- MCHOICE
  = flipToE s t i
dsM_12 s (Choice t1 t2) E                    -- MCHOICE
  = Choice <$> dsM_12 s t1 E <*> dsM_12 s t2 E

dsM_12 _ Fail _
   = pure Fail

-------------------- k, op, x -----------------
dsM_12 s t@(Lit{}) (P i)                   -- MCONST
  = flipToE s t i

dsM_12 _ t@(Lit{}) E                       -- MCONST
   = pure t

dsM_12 _ t@(EPrim{}) E                     -- MPrim
   = pure t

-------------------- Unerscore "_" -------------
-- M_sigma[ _ ] E     = exists x.x
-- M_sigma[ _ ] P(i) = i
dsM_12 _ (Variable v) pi             -- MUNDER
   | isSrcUnderscore v
   = case pi of
       E   -> pure existsXX
       P i -> pure i

dsM_12 _ t@(Variable {}) E     = pure t
dsM_12 s t@(Variable {}) (P i) = flipToE s t i

-------------------- t1[t2] -----------------
-- Rule MVAR
dsM_12 s t@(ApplyD {})  (P i) = flipToE s t i
dsM_12 s (ApplyD t1 t2) E     = eApplyD <$> mDesugarExpr s t1 <*> mDesugarExpr s t2

-------------------- if t1 then t2 else t3 ----
-- Push `pi` into `t2` and `t3`
-- and desugar (if e1 then e2 else e3) --> one{ (e1; \_.e2) | (\_.e3) }[]
-- The key point is that existentials bound in e1 scope over e2
dsM_12 s (If3 t1 t2 t3) pi                 -- MIF
   = do { e1 <- mDesugarExpr s t1
        ; e2 <- dsM_12 s t2 pi
        ; e3 <- dsM_12 s t3 pi
        ; pure (eForce (One (Choice (eSeq [e1, eThunk e2])
                                    (eThunk e3)))) }

---------- Other terms with P(i) ---------------

-- We allow all{e}, one{e} in patterns using flipToE
--    (not very important)

dsM_12 s (All t) E       = All <$> dsM_12 s t E
dsM_12 s t@(All{}) (P i) = flipToE s t i

dsM_12 s (One t) E       = One <$> dsM_12 s t E
dsM_12 s t@(One{}) (P i) = flipToE s t i

dsM_12 s t pi = error $ "TODO: dsM_12 " ++ show (s, pi, t)


----------------------------------
flipToE :: DsMode12 -> SrcSmall -> SrcCore -> D SrcCore

flipToE MI t i                           -- MEQG
  = do { e <- mDesugarExpr MI t
       ; v <- newIdent (getLoc t) "v"
       ; pure (eGuard (getFree i) $
               eSome (Lam v (Variable v `Unify` e))) }

-- Deals with M_sigma[ t ] P(i)
-- when we don't want to push the P(i) further into t
-- NEW version   M-[t]P(i) = z := D-[t]; i ;; some( \v. v=z )     <--- correct
-- OLD version   M-[t]P(i) = i = D-[t]
--
--     f( x := 3 ) := ..
--     f( x := :type{3} ) : = ...
--     f( x := (3|4) ) := ..

--  = M-[ :type{t} ]P(i)
--  = z:= type{t}; i ;; some{z}
--  = z := \x. x=t; i ;; some{\}

--flipToE MI t (Variable r)  -- Short cut
--  = Unify <$> mDesugarExpr MI t <*> pure (Variable r)



flipToE s t i
   = Unify i <$> dsM_12 s t E

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
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":" ) rhs)
          = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":=") rhs)
          = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
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
