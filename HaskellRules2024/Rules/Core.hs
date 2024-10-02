{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use :" #-}

module Rules.Core
  ( -- The data type itself
    Expr(..), Val, pattern LitInt
  , Ident(..)
  , Lit(..), Ptr, Path(..)
  , isVal, isHNF, isComparable
  , valid, prep, norm
  , pPrintSmallExpr
  , unIter

    -- Assupmtions
  , Assump(..), FailableAssump(..), AssumpOp(..), GroundVal(..), isPosAssump

    -- Rewriting
  , Rule, Context, isContext, (<@)
  , stepRule, everywhere, tryBefore
  , NormResult(..), normalize, showNormResult
  , Fuel, lotsOfSteps
  , RuleEnv(..), extendRuleEnv

    -- Binding and substitution
  , subst, bvs
  , unbindAs, unExis
  , alphaRename, matchExi_alphaRename, matchEq
  , alphaRenameVerify

    -- Effects
  , Effect(..), canSucceed, canFail

  -- Primops
  , PrimOp(..), allPrimOps, primOpString, primOpCanFail
  ) where

import Prelude hiding( (<>) )
import Epic.Print

import Data.Data(Data)
import Data.List( union, delete )
import TRS.Bind
import TRS.Traced
import Test.QuickCheck

import Control.Monad( liftM2 )
import Data.Scientific(Scientific)

--------------------------------------------------------------------------------
--
--            The main expression datatype
--
--------------------------------------------------------------------------------

type Val = Expr

data Expr
  -- Values
  = Var Ident
  | Lit Lit
  | Arr [Val]
  | Tru Val          -- truth{v}; see Note [Truth values]
  | Lam (Bind Expr)
  | Op PrimOp

  -- Programs
  | Expr :=: Expr    -- unification      "="
  | Expr :>: Expr    -- seq. composition ";"
  | Expr :|: Expr    -- choice           "|"
  | Val  :@: Val     -- application      v1[v2]
  | Exi (Bind Expr)
  | Fail

  -- Iterator over choices
  | Iter Expr Expr Expr Expr -- choice iteration; see Note [iter]

  -- Verifier
  | Some Val
  | Val :>>: Expr    -- guard |>   <-- black triangle
  | Check Effect Expr
  | Verify (BindList ([Assump],Expr))

  -- HOLE, only for contexts
  | HOLE
 deriving ( Eq, Ord, Show )

{- Note [iter]
The iter construct is a (left) fold over choices.

In the expression iter(e){u;f;g}
  * e is the choices we are iterating over
  * u is the "accumulator", i.e., the result we are building up
  * f is what to do if e does not fail
  * g is what to do if e fails

Both f and g are always explicit lambdas.  Anything else is invalid.
The desugaring ensures this invariant, and the rules maintain it.
The function f will be called with
  * current accumulator
  * value from the first argument to iter
  * continuation
If f wants iteration to terminate, it simply returns a value.
If f wants iteration to continue, it calls the continuation with a new accumulator.

Iter has the following reduction rules:
(ITER-FAIL)    iter(fail,  ){u; f; g}  -->  g u
(ITER-VALUE)   iter(v,     ){u; f; g}  -->  f u v g
(ITER-CHOICE)  iter(e1 | e2){u; f; g}  -->  iter(e1){u; f; \ x . iter(e2){x; f; g} }

Note that ITER-CHOICE has no requirement on e1 being a value.

If choices are always under if/one/all/for then the rules
CHOICE-ASSOC, CHOICE-FAIL-L, and CHOICE-FAIL-R are not needed.

Here's how if/one/all/for are encoded using iter.

  * if(e1) e2 else e3  -->  iter(e1; <vs>){ <>; (\ _ a _ . exi vs . a=<vs>; e2); (\_ . e3) }
      Where vs are the bound variables of e1 also used in e2.
      If vs is empty or a singleton, we can simplify this.
      The accumulator plays no role here.

  * one{e}  -->  iter(e){ <>; (\ _ a _ . a); (\ _ . fail) }
      The accumulator plays no role here.

  * all{e}  -->  iter(e){ <>; (\ a v c . c (snoc(a, v))); (\ a . a) }
      The array is built in the accumulator; built up by snocing
      new elements to the accumulator.

  * for(e1){e2}  -->  iter(e1; <vs>){ <>; step; (\ a . a) }
      where
        step = \ a x c . exi vs . x = <vs>; c (snoc(a, e2))
      Where vs are the bound variables of e1 also used in e2.
      If vs is empty or a singleton, we can simplify this.
      The array is built in the accumulator; built up by snocing
      new elements to the accumulator.

where snoc xs x = arrApp$[xs, <x>, _]

-}

{- Note [Truth values]
~~~~~~~~~~~~~~~~~~~~~~
As well as literals, arrays, lambdas, the language has a primitive value
    Tru v
called a "truth-value".  It behaves very like a 1-tuple, or like a
singleton finite map [v => v]

* Source syntax:  truth{v}

* Rewrite rules
    Primops:     isTru$[ truth{v} ]  --> truth{v}, otherwise fail
                 isComp$[truth{v}]   --> truth{isComp${v}]

    APP-TRU:     truth{v1}{v2}       --> v1=v2;v1

    U-TRU:       truth{v1}=truth{v2};e --> v1=v2;e
                 truth{v1}=v2;e        --> fail,  if v2 is a non-truth HNF


* Verification:
    SPLIT-TRU    verify(R;A){P[r=truth{v}]
                     --> verify(R,r1; A,r=truth{r1}){P[r1=v]}
                         verify(R;    A,r/=truth{_}){P[fail]}

* Prelude
    ?t = \x. if (truth{y:any} = x)
             then truth{t[y]}
             else x = ()
       That is, given a type `t`, ?t is a type that checks if its
       argument is a truth-value, and if so applies `t` to the payload

Note [Treatment of underscore in Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Core we allow "_" in exactly two places:
  * On LHS of ";", thus           _=e1; e2
  * Binder of lambda, thus        \_.e

Currently "_" is represented as a TRS.Bind.Ident, with string "_".
See TRS.Bind
      underscore   :: Ident
      isUnderscore :: Ident -> Bool

However underscore is not a "real" Ident; really it's a separate construct.
So TRS.Bind.Variables ignores it, and it never shows up as a free variable.

Underscore is treated specially in two rules
  * UNDERSCORE-ELIM   _=v; e    --> e
  * APP-LAM           (\_.e)[v] --> e

-}

--------------------------------------------------------------------------------
--
--                 PrimOps
--
--------------------------------------------------------------------------------

data PrimOp
 = -- Operations on integers
   Add | Sub | Mul | Div | Neg

   -- Operations on arrays
 | ArrLen
 | DotDot     -- dotDot$[m,n] = <m, m+1, .., n>
 | ArrApp     -- arrApp$[ Arr as, Arr bs, r ] =  r=(as++bs); r

   -- Relational
 | Gt | Lt | NEq | GEq | LEq

   -- Type tests
 | IsInt | IsStr | IsChar | IsArr | IsComp | IsTru

 deriving
   ( Eq, Ord, Bounded, Enum, Show, Data )

allPrimOps :: [PrimOp]
allPrimOps = [minBound .. maxBound]

primOpString :: PrimOp -> String
primOpString Add = "intAdd$"
primOpString Sub = "intSub$"
primOpString Mul = "intMul$"
primOpString Div = "intDiv$"
primOpString Neg = "intNeg$"

primOpString ArrLen   = "arrLen$"
primOpString DotDot   = "dotDot$"
primOpString ArrApp   = "arrApp$"

primOpString Gt  = "intGT$"
primOpString GEq = "intGE$"
primOpString Lt  = "intLT$"
primOpString LEq = "intLE$"
primOpString NEq = "intNE$"

primOpString IsInt  = "isInt$"
primOpString IsStr  = "isStr$"
primOpString IsChar = "isChar$"
primOpString IsArr  = "isArr$"
primOpString IsComp = "isComp$"
primOpString IsTru  = "isTru$"

primOpCanFail :: PrimOp -> Bool

-- These operations /can/ fail, and /don't/ produce a value
primOpCanFail Gt     = True
primOpCanFail Lt     = True
primOpCanFail NEq    = True
primOpCanFail GEq    = True
primOpCanFail LEq    = True
primOpCanFail IsInt  = True
primOpCanFail IsStr  = True
primOpCanFail IsChar = True
primOpCanFail IsArr  = True
primOpCanFail IsComp = True
primOpCanFail IsTru  = True
primOpCanFail ArrApp = True
primOpCanFail DotDot = True  -- can fail when the interval is empty

-- These operations /can't/ fail, and /do/ produce a value
primOpCanFail Add      = False
primOpCanFail Sub      = False
primOpCanFail Mul      = False
primOpCanFail Div      = False
primOpCanFail Neg      = False
primOpCanFail ArrLen   = False

--------------------------------------------------------------------------------
--
--                 Literals
--
--------------------------------------------------------------------------------

data Lit
  = LInt Integer            -- d
  | LRat Scientific String  -- d.d
  | LChar Char              -- 'c'
  | LStr String             -- "str"
  | LPath Path              -- /path/to/something
  | LPtr Ptr                -- not a textual literal, just used when translating back.
  deriving (Eq, Ord, Data)

pattern LitInt :: Integer -> Expr
pattern LitInt i = Lit (LInt i)

instance Pretty Lit where
  pPrintPrec l p lit =
    case lit of
      LInt i
        | i >= 0 -> text $ show i
        | otherwise -> maybeParens (p >= 10) $ text $ show i
      LRat r s -> text (show r ++ s)
      LChar c  -> text (show c)
      LStr s   -> text (show s)
      LPath s  -> pPrintPrec l p s
      LPtr ptr -> text ("R#" ++ show ptr)

instance Show Lit where
  show = prettyShow


--------------------------------------------------------
--               Pointers (= refs)
--------------------------------------------------------

type Ptr = Int    -- ToDo: newtype

--------------------------------------------------------
--               Path
--------------------------------------------------------

newtype Path = Path String
  deriving (Eq, Ord, Show, Data)

instance Pretty Path where
  pPrintPrec _ _ (Path s) = text s


--------------------------------------------------------------------------------
--
--                 Assumptions
--
--------------------------------------------------------------------------------

data GroundVal
  = GVVar {gv_var :: Ident}
  | GVLit Lit
  | GVArr [GroundVal]
  | GVTru GroundVal
  deriving( Eq, Ord, Show )

data Assump
  = A_Pos FailableAssump                 -- e.g r = <s,t>         or   r>s
  | A_Neg FailableAssump                 -- e.g. not (r = <s,t>)  or   not (r>s)
  | A_PrimOp Ident AssumpOp GroundVal    -- e.g. r = op[v],  (primOpCanFail op) is False
  deriving( Eq, Ord, Show )

data FailableAssump
  = A_GVEq  Ident  GroundVal
  | A_RelOp PrimOp GroundVal             -- (primOpCanFail op) is True
  deriving ( Eq, Ord, Show )

data AssumpOp
  = AO_Apply                            -- AO_apply [r,a]    means  r[a], r applied to a
  | AO_Prim PrimOp                      -- (primOpCanFail op) is False
  deriving( Eq, Ord, Show )

instance Pretty AssumpOp where
  pPrint AO_Apply     = text "Apply"
  pPrint (AO_Prim op) = pPrint op

instance Pretty Assump where
  pPrint (A_Pos a)          = pPrint a
  pPrint (A_Neg a)          = text "not" <> parens (pPrint a)
  pPrint (A_PrimOp i AO_Apply (GVArr [fun,arg]))
                            = pPrint i <+> text "=" <+> pPrint fun <> brackets (pPrint arg)
  pPrint (A_PrimOp i op gv) = pPrint i <+> text "=" <+> pPrint op <> brackets (pPrint gv)

instance Pretty FailableAssump where
  pPrint (A_GVEq i gv)      = pPrint i <+> text "="  <+> pPrint gv
  pPrint (A_RelOp op gv)    = pPrint op <> brackets (pPrint gv)

instance Pretty GroundVal where
  pPrint (GVVar i)   = pPrint i
  pPrint (GVLit l)   = pPrint l
  pPrint (GVArr gvs) = char '<' <> fsep (punctuate comma $ map pPrint gvs) <> char '>'
  pPrint (GVTru gv)  = text "truth" <> braces (pPrint gv)

isPosAssump :: Assump -> Bool
isPosAssump (A_Pos {})    = True
isPosAssump (A_PrimOp {}) = True
isPosAssump (A_Neg {})    = False


--------------------------------------------------------------------------------
--
--                 Effects
--
--------------------------------------------------------------------------------

data Effect
  = Fails
  | Succeeds
  | Decides
 deriving ( Eq, Ord )

instance Show Effect where
  show Fails    = "fails"
  show Succeeds = "succeeds"
  show Decides  = "decides"

instance Pretty Effect where
  pPrint eff = text (show eff)

canSucceed :: Effect -> Bool
-- True if one result is acceptable
canSucceed Succeeds = True
canSucceed Decides  = True
canSucceed Fails    = False

canFail :: Effect -> Bool
-- True if no results is acceptable
canFail Succeeds = False
canFail Decides  = True
canFail Fails    = True

--------------------------------------------------------------------------------
--
--                 Pretty-printing
--
--------------------------------------------------------------------------------

instance Pretty Expr where
  pPrintPrec = pPrintPrecE

instance Pretty PrimOp where
  pPrint op = text (primOpString op)

pPrintPrecE :: PrettyLevel -> Rational -> Expr -> Doc
pPrintPrecE lvl prec the_expr
  = case the_expr of
       HOLE       -> text "HOLE"
       Fail       -> text "fail"
       Var x      -> pPrint x
       Lit i      -> pPrint i
       Op op      -> pPrint op

       e1 :=: e2   -> mbPar0 $ ppr1 e1 <+> char '=' <+> ppr1 e2
       e1 :|: e2   -> sep [ ppr1 e1, char '|' <+> ppr1 e2 ]
       e1 :@: e2   -> ppr1 e1 <> brackets (pp_call_arg e2)
       e@(_ :>: _) -> sep (punctuate semi $ map ppr1 (gatherSeqs e))
       e1 :>>: e2  -> mbPar0 $ ppr1 e1 <+> text ">>" <+> ppr1 e2

       Arr as  -> char '<' <> fsep (punctuate comma $ map ppr0 as) <> char '>'
       Tru a   -> text "truth" <> braces (ppr0 a)
       Iter e1 e2 e3 e4 -> text "iter"  <> parens (ppr0 e1) <> braces (sep (punctuate semi $ map ppr1 [e2,e3,e4]))
       Lam bnd -> mbPar0 $ char '\\' <> pprBind bnd
       Exi {}  -> mbPar0 $ sep [ text "exi" <+> fsep (map pPrint bndrs) <> char '.'
                               , indent (ppr0 body) ]
               where
                  (bndrs, body) = unpackExis the_expr

       Some e  -> text "some" <> parens (ppr0 e)
       Check fx e -> cat [ text "check" <> char '<' <> pPrint fx <> char '>'
                         , indent (braces (ppr0 e)) ]

       Verify bl -> cat [ text "verify" <> parens (sep [ fsep (punctuate comma (map pPrint ids)) <> char ';'
                                                       , fsep (punctuate comma (map pPrint as)) ])
                        , indent (braces (ppr0 body)) ]
           where
             (ids, (as, body)) = alphaRenameVerify (free bl) bl
  where
    ppr0 = pPrintPrecE lvl 0
    ppr1 = pPrintPrecE lvl 1

    mbPar0 = maybeParens (prec > 0)

    -- Reduce clutter: omit the angle brackets for a multi-arg call.
    -- That is, print f[3,2] rather than f[<3,2>]
    pp_call_arg (Arr es) = fsep (punctuate comma $ map ppr0 es)
    pp_call_arg e2       = ppr0 e2

pPrintSmallExpr :: Expr -> Doc
-- Show only a small expression; otherwise return "<big>"
pPrintSmallExpr e
  | exprSize e < 10 = pPrint e
  | otherwise       = text "<big>"

gatherSeqs :: Expr -> [Expr]
gatherSeqs (e1 :>: e2) = e1 : gatherSeqs e2
gatherSeqs e           = [e]

unpackExis :: Expr -> ([Ident], Expr)
-- Gather up a contiguous block of Exis, alpha-renaming
unpackExis orig_e
  = go (free orig_e) orig_e
  where
    go forb e
      | Exi bnd <- e
      , (bndr, body)   <- alphaRename (free bnd) bnd
      , (bndrs, inner) <- go (bndr:forb) body
      = (bndr:bndrs, inner)
    go _ other = ([], other)


pprBind :: Bind Expr -> Doc
pprBind bnd
  = pPrint bndr <> char '.' <+> pPrint body
  where
    (bndr, body) = alphaRename (free bnd) bnd


--------------------------------------------------------------------------------
--
--                 Size
--
--------------------------------------------------------------------------------

exprSize :: Expr -> Int
exprSize (Var {})      = 1
exprSize (Lit {})      = 1
exprSize (Op {})       = 1
exprSize Fail          = 1
exprSize HOLE          = 1
exprSize (Arr as)      = 1 + sum (map exprSize as)
exprSize (Tru a)       = 1 + exprSize a
exprSize (Lam bnd)     = 1 + bindSize bnd
exprSize (e1 :>: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :=: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :|: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :@: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :>>: e2)  = 1 + exprSize e1 + exprSize e2
exprSize (Exi bnd)     = 1 + bindSize bnd
exprSize (Iter e1 e2 e3 e4) = 1 + exprSize e1 + exprSize e2 + exprSize e3 + exprSize e4
exprSize (Some a)      = 1 + exprSize a
exprSize (Check _ e)  = 1 + exprSize e
exprSize (Verify bl)   = 10 + exprSize e
                       where
                         (_rs,(_as,e)) = unsafeUnbindList bl

bindSize :: Bind Expr -> Int
bindSize bnd = exprSize (snd (unsafeUnbind bnd))

--------------------------------------------------------------------------------
--
--                 Values
--
--------------------------------------------------------------------------------

isVal :: Expr -> Bool
isVal (Var v) = not (isUnderscore v)  -- "_" is not a valid identifier
isVal e       = isHNF e

isHNF :: Expr -> Bool
isHNF (Lit {}) = True
isHNF (Op {})  = True
isHNF (Arr es) = all isVal es   -- SLPJ: This had 'valid' stuff too, strangely
isHNF (Tru e)  = isVal e
isHNF (Lam {}) = True           -- valid e where (_,e) = unsafeUnbind bnd
                                -- SLPJ: why valid????
isHNF _        = False


isComparable :: Expr -> Bool
isComparable (Lit (LChar {})) = True
isComparable (Lit (LInt  {})) = True
isComparable (Lit (LStr  {})) = True
isComparable (Arr es)         = all isComparable es
isComparable (Tru e)          = isComparable e
isComparable _                = False -- ToDo: what about Path, Ptr, Rational?

--------------------------------------------------------------------------------
--
--                 Valid expressions
--
--------------------------------------------------------------------------------

valid :: Expr -> Bool
-- Checks if an expression is syntactically valid,
-- according to the syntax of desugaring.pdf
valid ((a :=: e1) :>: e2) = validL a && valid e1 && valid e2
valid (e1 :|: e2)         = valid e1 && valid e2
valid (a1 :@: a2)         = isVal a1 && isVal a2
valid (Exi bnd)           = valid e where (_,e) = unsafeUnbind bnd
  -- SLPJ: todo: check binder is not _
valid (Lam bnd)           = valid e where (_,e) = unsafeUnbind bnd
valid Fail                = True
valid (Some a)            = isVal a
valid (a :>>: e)          = isVal a && valid e  -- Guard
valid e@(Iter _ _ _ _)
  | Just (e1, e2, (_, _, _, e3), (_, e4)) <- unIter e
  = valid e1 && valid e2 && valid e3 && valid e4
valid (Iter _ _ _ _)      = False
valid (Check _ e)         = valid e
valid (Verify bl)         = valid e where (_, (_as,e)) = unsafeUnbindList bl
valid e                   = isVal e
  -- SLPJ: todo: check variable is not _

validL :: Expr -> Bool
-- Valid on the LHS of :=:
validL (Var _v) = True     -- Includes underscore "_"
validL e        = isVal e

prep :: Expr -> Expr
-- Convert an Expr into an Expr in the sub-language of desugaring.pdf
-- Hence: valid (prep e) == True
-- In particular:
--   * A-normal form; e.g. args of `:@:` are values
--   * (v=e) only to the left of `:>:`
prep (Var x)       = Var x
prep (Lit k)       = Lit k
prep (Arr as)      = prepVals as (\vs -> Arr vs)
prep (Tru a)       = prepVal a Tru
prep (Lam bnd)     = Lam (bind x (prep e)) where (x,e) = unsafeUnbind bnd
prep (Op op)       = Op op
prep (e1 :>: e2)   = prepSeq e1 e2
prep (a  :=: e)    = prepVal a (\v -> (v :=: prep e) :>: v)
prep (e1 :|: e2)   = prep e1 :|: prep e2
prep (a1 :@: a2)   = prepVal a1 (\v1 -> prepVal a2 (\v2 -> v1 :@: v2))
prep (Exi bnd)     = Exi (bind x (prep e)) where (x,e) = unsafeUnbind bnd
prep Fail          = Fail
prep (Some a)      = prepVal a (\v -> Some v)
prep (a :>>: e)    = prepVal a (\v -> v :>>: prep e)
prep (Check fx e)  = Check fx (prep e)
prep (Verify bl)   = Verify (bindList xs (as, prep e))
                     where (xs,(as,e)) = unsafeUnbindList bl
prep (Iter e1 e2 e3 e4) = prepVal e2 $ \ v2 -> Iter (prep e1) v2 (prep e3) (prep e4)
prep HOLE          = error "prep HOLE undefined"

prepSeq :: Expr -> Expr -> Expr
prepSeq (a :=: e1) e2 = prepVal a (\v -> (v :=: prep e1) :>: prep e2)
prepSeq e1         e2 = (Var underscore :=: prep e1) :>: prep e2

prepVal :: Expr -> (Val -> Expr) -> Expr
-- (prepVal e K) makes applies K to the value of e,
-- perhaps by adding an existential, thus (exi x. x = e; K[x])
prepVal a k
  | isVal pa  = k pa
  | otherwise = Exi (bind x ((Var x :=: pa) :>: k (Var x)))
 where
  pa = prep a
  x  = identNotIn (free (k pa))  -- UGH!  SLPJ: quadratic in prepVals

prepVals :: [Expr] -> ([Val] -> Expr) -> Expr
prepVals []     f = f []
prepVals (a:as) f = prepVal a (\v -> prepVals as (f . (v:)))

--------------------------------------------------------------------------------
--
--                 Variables
--
--------------------------------------------------------------------------------

instance Variables Expr where
  variables f (Var x)      = variables f x
  variables f (Arr es)     = variables f es
  variables f (Tru e)      = variables f e
  variables f (Lam bnd)    = variables f bnd
  variables f (e1 :=: e2)  = variables f (e1,e2)
  variables f (e1 :>: e2)  = variables f (e1,e2)
  variables f (e1 :|: e2)  = variables f (e1,e2)
  variables f (e1 :@: e2)  = variables f (e1,e2)
  variables f (Some e)     = variables f e
  variables f (e1 :>>: e2) = variables f (e1,e2)
  variables f (Check _ e)  = variables f e
  variables f (Exi bnd)    = variables f bnd
  variables f (Verify bnd) = variables f bnd
  variables f (Iter e1 e2 e3 e4) = variables f (e1, e2, e3, e4)
  variables _ _            = []

instance Variables FailableAssump where
  variables f (A_GVEq i gv)  = [i] `union` variables f gv
  variables f (A_RelOp _ gv) =             variables f gv

instance Variables Assump where
  variables f (A_Pos a)           = variables f a
  variables f (A_Neg a)           = variables f a
  variables f (A_PrimOp i _ gv)   = [i] `union` variables f gv


instance Variables GroundVal where
  variables _f (GVVar i)   = [i]
  variables _f (GVLit {})  = []
  variables f  (GVArr gvs) = variables f gvs
  variables f  (GVTru gv)  = variables f gv


--------------------------------------------------------------------------------
--
--                 Binders
--
--------------------------------------------------------------------------------

unbindAs :: Ident -> Bind Expr -> Expr
unbindAs x bnd = subst [(y,Var x)] e where (y,e) = unsafeUnbind bnd

alphaRename :: [Ident] -> Bind Expr -> (Ident,Expr)
-- Open up the binding, but avoiding any of the binders
--    * in `forb` or
--    * free in the binding
alphaRename forb top_t
  = alphaRenameBindWith freshen top_t
  where
    full_forb = forb ++ free top_t
    freshen x t
      | x `elem` full_forb = (x', subst [(x,Var x')] t)
      | otherwise          = (x, t)
      where
        x' = identNotIn full_forb

alphaRenameVerify :: [Ident] -> BindList ([Assump], Expr) -> ([Ident], ([Assump], Expr))
-- Open up a Verify block, avoiding any skolems in `forb`
-- (Unlike alphaRename we expect these to include all the in-scope
--  skolems, so we don't need to take the free vars of the BindList.)
alphaRenameVerify forb bl
  = alphaRenameBindListWith freshen bl
  where
     freshen rs (as,e) = (rs', (map (substAssump prs) as, substSkol prs e))
       where
         (rs', prs) = freshenSkolVars forb rs

freshenSkolVars :: [Ident]           -- Forbidden
                -> [Ident]           -- Freshen these
                -> ( [Ident]         -- Fresh
                   , Subst Ident )   -- Old-to-new mapping, maybe empty

freshenSkolVars top_forb top_rs
  = go top_forb [] [] top_rs
  where
    go _ rs' prs []
      = (reverse rs', reverse prs)
    go forb rs' prs (r:rs)
      | r `elem` forb = go (r':forb) (r':rs') ((r,r'):prs) rs
      | otherwise     = go (r:forb)  (r:rs')  prs          rs
      where
        r' = skolNotIn forb

-- Sorts binders and renames variables
-- TODO: new normalization for x=y
norm :: Expr -> Expr
norm orig_e = alpha 0 orig_e
 where
  var i = ident ("_" ++ show i)
  skvar i = ident ("_r" ++ show i)

  alpha k (Arr es)     = Arr (map (alpha k) es)
  alpha k (Tru e)      = Tru ((alpha k) e)
  alpha k (Lam bnd)    = Lam (bind x (alpha (k+1) e))
                       where x = var k; e = unbindAs x bnd
  alpha k (e1 :=: e2)  = alpha k e1 :=: alpha k e2
  alpha k (e1 :>: e2)  = alpha k e1 :>: alpha k e2
  alpha k (e1 :|: e2)  = alpha k e1 :|: alpha k e2
  alpha k (e1 :@: e2)  = alpha k e1 :@: alpha k e2
  alpha k (Some e)     = Some (alpha k e)
  alpha k (e1 :>>: e2) = alpha k e1 :>>: alpha k e2
  alpha k (Check fx e) = Check fx (alpha k e)
  alpha k e@(Exi _)    = alphaExi k [] e
  alpha k (Verify bl)  = let (rs, (as,e)) = unsafeUnbindList bl
                             rs' = map skvar [k+1..k+n]
                             n   = length rs
                             sub = rs `zip` rs'
                             e'  = alpha (k+n) e
                         in Verify (bindList rs' (map (substAssump sub) as, substSkol sub e'))
  alpha k (Iter e1 e2 e3 e4) = Iter (alpha k e1) (alpha k e2) (alpha k e3) (alpha k e4)
  alpha _ e            = e

  alphaExi k xs (Exi bnd) = alphaExi k (x:xs) e
   where
    (x,e) = unsafeUnbind bnd

  alphaExi k xs e = exis (map snd tab) (subst [(x,Var y)|(x,y)<-tab] e')
   where
    n   = length xs
    e'  = alpha (k+n) e
    ys  = free e'
    tab = filter (`elem` xs) ys `zip` [ var i | i <- [k..] ]

    exis []     e2 = e2
    exis (z:zs) e2 = Exi (bind z (exis zs e2))

--------------------------------------------------------------------------------
--
--            Substitution
--
--------------------------------------------------------------------------------

subst :: Subst Expr -> Expr -> Expr
-- Domain of substitution does not include skolems
subst sub orig_e
  | null sub  = orig_e      -- Short cut
  | otherwise = go orig_e
  where
    go (Var x)      = head $ [e | (y,e) <- sub, y == x] ++ [Var x]
    go (Arr es)     = Arr (map go es)
    go (Tru e)      = Tru (go e)
    go (Lam bnd)    = Lam (substBind subst_e_ops sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  subst_e_ops sub bnd)
    go (Verify bl)  = Verify (substBinds subst_verify_ops sub bl)
    go (Iter e1 e2 e3 e4) = Iter (go e1) (go e2) (go e3) (go e4)
    go e            = e

    subst_e_ops :: SubstOps Expr Expr
    subst_e_ops = SubstOps { so_subst = subst
                           , so_fresh = identNotIn
                           , so_var = Var }

    subst_verify_ops :: SubstOps Expr ([Assump],Expr)
    subst_verify_ops = SubstOps { so_subst = subst_v
                                , so_fresh = skolNotIn  -- NB: skolNotIn here
                                , so_var = Var }
      where
        subst_v :: Subst Expr -> ([Assump],Expr) -> ([Assump],Expr)
        subst_v sub' (as,e) = (as, subst sub' e)

substSkol :: Subst SkolIdent -> Expr -> Expr
-- Domain of substitution is skolem variables; range is just an identifier
substSkol sub orig_e
  | null sub  = orig_e    -- Short cut
  | otherwise = go orig_e
  where
    go (Var x)      = Var (head $ [e | (y,e) <- sub, y == x] ++ [x])
    go (Arr es)     = Arr (map go es)
    go (Tru e)      = Tru (go e)
    go (Lam bnd)    = Lam (substBind subst_e_ops sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  subst_e_ops sub bnd)
    go (Verify bl)  = Verify (substBinds subst_verify_ops sub bl)
    go (Iter e1 e2 e3 e4) = Iter (go e1) (go e2) (go e3) (go e4)
    go e            = e

    subst_e_ops :: SubstOps SkolIdent Expr
    subst_e_ops = SubstOps { so_subst = substSkol
                           , so_fresh = identNotIn
                           , so_var = id }

    subst_verify_ops :: SubstOps SkolIdent ([Assump],Expr)
    subst_verify_ops = SubstOps { so_subst = subst_v
                                , so_fresh = skolNotIn  -- NB: skolNotIn here
                                , so_var = id }
      where
        subst_v :: Subst SkolIdent -> ([Assump],Expr) -> ([Assump],Expr)
        subst_v sub' (as,e) = (map (substAssump sub') as, substSkol sub' e)

substAssump :: Subst Ident -> Assump -> Assump
substAssump sub top_asm
  | null sub  = top_asm      -- Short cut
  | otherwise = go  top_asm
  where
    go (A_Pos a)          = A_Pos (goF a)
    go (A_Neg a)          = A_Neg (goF a)
    go (A_PrimOp x op gv) = A_PrimOp (lookupIdSubst sub x) op (substGV sub gv)
    goF (A_GVEq x gv)     = A_GVEq   (lookupIdSubst sub x)    (substGV sub gv)
    goF (A_RelOp op gv)   = A_RelOp                        op (substGV sub gv)

substGV :: Subst Ident -> GroundVal -> GroundVal
substGV sub (GVVar x)  = GVVar (lookupIdSubst sub x)
substGV _   (GVLit l)  = GVLit l
substGV sub (GVArr vs) = GVArr (map (substGV sub) vs)
substGV sub (GVTru v)  = GVTru (substGV sub v)

lookupIdSubst :: Subst Ident -> Ident -> Ident
lookupIdSubst sub x
  = case [y | (x',y) <- sub, x==x'] of
      (y:_) -> y
      []    -> x

--------------------------------------------------------------------------------
--
--            Rewriting
--
--------------------------------------------------------------------------------

type Rule = RuleEnv -> Expr -> [(String,Expr)]

data RuleEnv = RE { skolVars :: [Ident], assumps :: [Assump] }

emptyRuleEnv :: RuleEnv
emptyRuleEnv = RE { skolVars = [], assumps = [] }

extendRuleEnv :: RuleEnv -> [Ident] -> [Assump] -> RuleEnv
extendRuleEnv rule_env@(RE { skolVars = skols, assumps = asms }) new_skols new_asms
  = rule_env { skolVars = new_skols ++ skols, assumps = new_asms ++ asms }

stepRule :: Rule -> Expr -> [(String,Expr)]
stepRule rule expr = rule emptyRuleEnv     -- Empty set of skolems
                          expr

delSkol :: RuleEnv -> Ident -> RuleEnv
delSkol env@(RE { skolVars=skols }) x = env { skolVars = delete x skols }

tryBefore :: Rule -> Rule -> Rule
-- Run rule1, and only if it can do nothing try rule2
tryBefore rule1 rule2 env e
  | null rule1_results = rule2 env e
  | otherwise          = rule1_results
  where
    rule1_results = rule1 env e

-- apply a rule everywhere (recursively) in the expression
everywhere :: Rule -> Rule
everywhere step env orig_e
  = step env orig_e ++ recurse orig_e
 where
  recurse (Arr es)     = [ (s, Arr (take i es ++ [e'] ++ drop (i+1) es))
                         | i <- [0..length es-1]
                         , (s,e') <- everywhere step env (es!!i)
                         ]
  recurse (Tru e)      = [ (s, Tru e')  | (s,e') <- everywhere step env e ]
  recurse (Lam bnd)    = [ (s, Lam (bind x e')) | (s,e') <- everywhere step env' e ]
                       where
                         (env',x,e) = walkInsideBind env bnd
  recurse (Exi bnd)    = [ (s, Exi (bind x e')) | (s,e') <- everywhere step env' e ]
                       where
                         (env',x,e) = walkInsideBind env bnd
  recurse (e1 :=: e2)  = [ (s, e1' :=: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :=: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :>: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :|: e2)  = [ (s, e1' :|: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :|: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :@: e2)  = [ (s, e1' :@: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :@: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (Some e)     = [ (s, Some e') | (s,e') <- everywhere step env e ]
  recurse (e1 :>>: e2) = [ (s, e1' :>>: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :>>: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (Check fx e) = [ (s, Check fx e') | (s,e') <- everywhere step env e ]
  recurse (Verify bl)  = [ (s, Verify (bindList rs (as,e')))
                         | (s,e') <- everywhere step env' e ]
                       where
                         env' = extendRuleEnv env rs as
                         (rs,(as,e)) = alphaRenameVerify (skolVars env) bl
  recurse (Iter e1 e2 e3 e4) =
                         [ (s, Iter e1' e2 e3 e4) | (s,e1') <- everywhere step env e1 ] ++
                         [ (s, Iter e1 e2' e3 e4) | (s,e2') <- everywhere step env e2 ] ++
                         [ (s, Iter e1 e2 e3' e4) | (s,e3') <- everywhere step env e3 ] ++
                         [ (s, Iter e1 e2 e3 e4') | (s,e4') <- everywhere step env e4 ]

  recurse _            = []

walkInsideBind :: RuleEnv -> Bind a -> (RuleEnv, Ident, a)
-- When we walk inside an (Exi x e) or (Lam x e) we need to delete
-- `x` from the skolems in the RuleEnv. This would not be necessary if
-- skolems and ordinary identifiers came from different name spaces, but
-- currently they are the same, so we need to take care
walkInsideBind env bnd
  = (delSkol env x, x, e)
  where
    (x,e) = unsafeUnbind bnd

-- treat "exi x1 .. exi xn" as one block when matching
unExis :: Expr -> (Context, Expr)
unExis (Exi bnd) = (Exi (bind x exis), body)
 where
  (x,e)       = unsafeUnbind bnd
  (exis,body) = unExis e
unExis e         = (HOLE, e)

-- structural rules matching
matchExi_alphaRename :: [Ident] -> Expr -> [(Context, Ident, Expr)]
matchExi_alphaRename zs e =
  [ cxe
  | Exi bnd <- [e]
  , let (x,ex) = alphaRename zs bnd
        cxes   = matchExi_alphaRename (x:zs) ex
  , cxe <- -- just add "bind x" to the exis
           [ (Exi (bind x ctx),y,ey)
           | (ctx,y,ey) <- cxes
           ]
           -- add a case where "bind x" is the variable we're matching on
        ++ case cxes of
             [] -> [ (HOLE,x,ex) ]
             _  -> [ (Exi (bind y ctx),x,ey)
                   | (ctx,y,ey) <- [head cxes]
                   ]
  ]

matchEq :: Expr -> [(Expr,Expr)]
-- Matches (v = e), and also (v1 = v2) returning (v2 = v1)
matchEq e =
  [ (lhs, rhs)
  | e1 :=: e2 <- [e]
  , (lhs,rhs) <- (e1,e2) : [ (Var y, Var x)
                           | (Var x, Var y) <- [(e1,e2)]
                           ]
  ]

type Fuel = Int

lotsOfSteps :: Fuel
lotsOfSteps = 1000

data NormResult
  = NormOK        -- No rewrites apply
  | NormExpired   -- We ran out of fuel
  | NormInvalid   -- A rewrite produced an invalid output
                  -- according to the `valid` predicate
  deriving( Eq )

instance Show NormResult where
   show = showNormResult

showNormResult :: NormResult -> String
showNormResult NormOK      = "reached a normal form"
showNormResult NormExpired = "ran out of fuel (Unexpected)"
showNormResult NormInvalid = "reached an invalid expression -- yikes!"

normalize :: Fuel    -- Maximum number of steps
          -> Rule -> Expr
          -> (NormResult, Traced Expr)
-- Repeatedly apply the first in the
-- list of possiblities returned by the rule
normalize fuel rule orig_e = go fuel [] orig_e
 where
  go :: Int
     -> [(String,Expr)]   -- Accumulating trace
     -> Expr
     -> (NormResult, Traced Expr)
  go fuel_left tr e =
    case stepRule rule e of
      []                        -> (NormOK,      e  :<-- tr)
      (s,e'):_ | fuel_left==0   -> (NormExpired, e  :<-- tr)
               | not (valid e') -> (NormInvalid, e' :<-- tr')
               | otherwise      -> go (fuel_left-1) tr' e'
              where
               tr' = (s,e):tr

--------------------------------------------------------------------------------
--
--            Arbitrary
--
--------------------------------------------------------------------------------

instance Arbitrary PrimOp where
  arbitrary = elements allPrimOps

instance Arbitrary Expr where
  arbitrary = sized (arbExprWith xs)
   where
    xs = take 3 (identsNotIn [])

  shrink (LitInt k) = [ LitInt k' | k' <- shrink k ]  -- SLPJ: other literals?

  shrink (Op _)       = [ LitInt 0, LitInt 1 ]   -- See Note [Shrinking expressions: ops] SLPJ: explain

  shrink (Arr es)     = es
                     ++ [ Arr es' | es' <- shrink es ]
  shrink (Tru e)      = [ e ] ++ [ Tru e'  | e' <- shrink e ]
  shrink (Lam bnd)    = shrinkBind Lam bnd
  shrink (e1 :=: e2)  = [ e1, e2 ]
                     ++ [ e1' :=: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :=: e2' | e2' <- shrink e2 ]
  shrink (e1 :>: e2)  = [ e1, e2 ]
                     ++ [ e1' :>: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :>: e2' | e2' <- shrink e2 ]
  shrink (e1 :|: e2)  = [ e1, e2 ]
                     ++ [ e1' :|: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :|: e2' | e2' <- shrink e2 ]
  shrink (e1 :@: e2)  = [ e1, e2 ]
                     ++ [ e1' :@: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :@: e2' | e2' <- shrink e2 ]
  shrink (Some e)     = [ e ] ++ [ Some e' | e' <- shrink e ]
  shrink (e1 :>>: e2) = [ e1, e2 ]
                     ++ [ e1' :>>: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :>>: e2' | e2' <- shrink e2 ]
  shrink (Check fx e) = [ e ]
                     ++ [ Check fx e' | e' <- shrink e ]
  shrink (Exi bnd)    = shrinkBind Exi bnd
  shrink Fail         = [ LitInt 0 ]
  --shrink (Verify bnd) = error "shrink Verify undefined"
  shrink _            = []

arbExprWith :: [Ident] -> Int -> Gen Expr
arbExprWith xs n =
  frequency $
  [ (1, Var `fmap` elements xs) | not (null xs) ] ++
  [ (1, LitInt `fmap` arbitrary)
  , (a, Arr `fmap` arbExprs)
  , (a, Tru `fmap` arbExpr1)
  , (a, Lam `fmap` arbBind)
  , (1, Op  `fmap` arbitrary)
  , (b, liftM2 (:=:) arbExpr2 arbExpr2)
  , (b, liftM2 (:>:) arbExpr2 arbExpr2)
  , (b, liftM2 (:|:) arbExpr2 arbExpr2)
  , (a, liftM2 (:@:) arbExpr2 arbExpr2)
  , (b, Exi `fmap` arbBind)
  , (1, return Fail)
{-
  | Some Val
  | Val :>>: Expr    -- guard           |>   <-- black triangle
  | Check Effect Expr
  | Verify (BindList ([Assump],Expr))
-}
  ]
 where
  a = 0 `max` (n `min` 5) -- for bigger values
  b = 0 `max` n           -- for recursive expressions
  arbExpr1 = arbExprWith xs (n-1)
  arbExpr2 = arbExprWith xs (n `div` 2)
  arbExprs = do k <- elements [0,1,2,3,5]
                sequence [ arbExprWith xs (if k <= 1 then n-k else n`div`k)
                         | _ <- [1..k]
                         ]
  arbBind  = frequency $
             [ (1, liftM2 bind (elements xs) (arbExprWith xs (n-1))) | not (null xs) ] ++
             [ (4, let x = identNotIn xs in bind x `fmap` arbExprWith (x:xs) (n-1)) ]

shrinkBind :: Arbitrary a => (Bind a -> a) -> Bind a -> [a]
shrinkBind con bnd = [ t ] ++ [ con (bind x t') | t' <- shrink t ]
 where
  (x,t) = unsafeUnbind bnd

instance CoArbitrary Expr where
  coarbitrary = coarbitrary . show -- not completely honest!

--------------------------------------------------------------------------------
--
--            Contexts
--
--------------------------------------------------------------------------------

{- Note [Contexts]
~~~~~~~~~~~~~~~~~~
... blah blah...

* No HOLE inside Verify

-}

type Context = Expr

(<@) :: Context -> Expr -> Expr
-- (C <@ e) fills the hole in C with e. Often written C[e]
Arr as        <@ h = Arr (map (<@ h) as)
Tru a         <@ h = Tru (a <@ h)
Lam bnd       <@ h = Lam (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
(e1 :>: e2)   <@ h = (e1 <@ h) :>: (e2 <@ h)
(e1 :=: e2)   <@ h = (e1 <@ h) :=: (e2 <@ h)
(e1 :|: e2)   <@ h = (e1 <@ h) :|: (e2 <@ h)
(e1 :@: e2)   <@ h = (e1 <@ h) :@: (e2 <@ h)
Exi bnd       <@ h = Exi (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
Iter e1 e2 e3 e4 <@ h = Iter (e1 <@ h) (e2 <@ h) (e3 <@ h) (e4 <@ h)
Some e        <@ h = Some (e <@ h)
(e1 :>>: e2)  <@ h = (e1 <@ h) :>>: (e2 <@ h)
Check fx e    <@ h = Check fx (e <@ h)
e@(Verify {}) <@ _ = e   -- No HOLE inside Verify. SLPJ: check
HOLE          <@ h = h
e             <@ _ = e

bvs :: Context -> [Ident]
-- Returns all the binders that are in scope at the HOLE
bvs ctx = explore [] ctx
 where
  explore xs (Arr es)     = foldr union [] (map (explore xs) es)
  explore xs (Tru e)      = explore xs e
  explore xs (Lam bnd)    = exploreBind xs bnd
  explore xs (e1 :=: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :>: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :|: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :@: e2)  = explore xs e1 `union` explore xs e2
  explore xs (Iter e1 e2 e3 e4) = explore xs e1 `union` explore xs e2 `union` explore xs e3 `union` explore xs e4
  explore xs (Some e)     = explore xs e
  explore xs (e1 :>>: e2) = explore xs e1 `union` explore xs e2
  explore xs (Check _ e)  = explore xs e
  explore xs (Exi bnd)    = exploreBind xs bnd
  explore _  (Verify {})  = []  -- HOLE is not inside Verify{}
  explore xs HOLE         = xs
  explore _xs _e          = []

  exploreBind xs bnd = explore ([x] `union` xs) e where (x,e) = unsafeUnbind bnd

isContext :: Context -> Bool
-- There is a HOLE, outside a Verify (SLPJ: is the "outside Verify" right?)
isContext (Arr es)     = any isContext es
isContext (Tru e)      = isContext e
isContext (Lam bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (e1 :=: e2)  = isContext e1 || isContext e2
isContext (e1 :>: e2)  = isContext e1 || isContext e2
isContext (e1 :|: e2)  = isContext e1 || isContext e2
isContext (e1 :@: e2)  = isContext e1 || isContext e2
isContext (Iter e1 e2 e3 e4) = isContext e1 || isContext e2 || isContext e3 || isContext e4
isContext (Some e)     = isContext e
isContext (e1 :>>: e2) = isContext e1 || isContext e2
isContext (Check _ e)  = isContext e
isContext (Exi bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (Verify {})  = False
isContext HOLE         = True
isContext _            = False

-- Unpack a correct Iter construct
unIter :: Expr -> Maybe (Expr, Expr, (Ident, Ident, Ident, Expr), (Ident, Expr))
unIter (Iter e1 e2 (Lam b3) (Lam b4)) |
    (x, Lam b3')  <- unsafeUnbind b3,
    (y, Lam b3'') <- unsafeUnbind b3',
    (z, eb3) <- unsafeUnbind b3'',
    (w, eb4) <- unsafeUnbind b4 = Just (e1, e2, (x, y, z, eb3), (w, eb4))
unIter _ = Nothing
