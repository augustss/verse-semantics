{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use :" #-}

module Core.Expr
  ( -- The data type itself
    Expr(..), Val, pattern LitInt, pattern LitStr
  , Ident(..)
  , Lit(..), Ptr, Path(..)
  , isVal, isHNF, isComparable
  , valid, prep, norm
  , pPrintSmallExpr
  , Iter(..), iterApply, iterChoiceFree

    -- Particular expressions
  , someAny, someNat, nat, inRange, inRangeType
  , litInt, litIntZero, mkSeq, (>>>)
  , mkExis, mkApp, mkEqual, mkIf, mkOne, mkAll, mkFor, mkCheck, matchCheck
  , mkCount
  , lamUnderscore, someUnderscore, wrong
  , mkDef

    -- Assupmtions
  , GroundVal(..)
  , Assump(..), PredAssump(..), FailableAssump(..), AssumpOp(..)
  , notPred

    -- Rewriting
  , Context, isContext, (<@)

    -- Binding and substitution
  , subst, substAssump
  , unbindAs
  , alphaRename
  , alphaRenameVerify

    -- Effects
  , Effect(..), canSucceed, canFail, intersectEffect

  -- Primops
  , PrimOp(..), allPrimOps, primOpString, primOpCanFail, primOpIsTypeTest, primOpIsCheck
  , isBinOp, isBinRelOp, primOpPreCond
  ) where

import Prelude hiding( (<>) )
import Epic.Print

import Data.Data(Data)
import Data.List( union )
import Core.Bind
import Core.Traced
import Test.QuickCheck hiding( Some )

import Control.Monad( liftM2 )
import Data.Scientific(Scientific)

--import qualified Debug.Trace

infixr 5 :>:

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
  | Tup [Val]
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
  | Err String

  -- Iterator over choices
  | Iter Iter Expr Expr -- choice iteration; see Note [iter]
  -- | All Expr

  -- Verifier
  | Some Val
  | Val :>>: Expr    -- guard |>   <-- black triangle
  -- | Check Effect Expr
  | Verify (BindList ([Assump],Expr))

  | Arr    Val Expr
  | Choose Val Expr
  -- | Size   Val Expr

  -- HOLE, only for contexts
  | HOLE
 deriving ( Eq, Ord, Show )

data Iter
  = IterIf
  | IterOne
  | IterAll
  | IterFor
  | IterCount
 deriving ( Eq, Ord )

instance Show Iter where
  show IterIf    = "IF"
  show IterOne   = "ONE"
  show IterAll   = "ALL"
  show IterFor   = "FOR"
  show IterCount = "COUNT"

iterChoiceFree :: Iter -> Bool
iterChoiceFree IterIf    = False
iterChoiceFree IterOne   = True
iterChoiceFree IterAll   = True
iterChoiceFree IterFor   = False
iterChoiceFree IterCount = True

iterApply :: Iter -> Val -> Expr -> Expr
-- If we see iter(f){v}{e0}, where the main argument of `iter`
-- is a value `v`, we call (iterApply f v e0)

-- IF(v){e0}  -->  v[]
iterApply IterIf v _e0 = v :@: Tup []

-- ONE{v}{e0} --> e0
iterApply IterOne v _e0 = v

-- COUNT(v){e0} --> 1+e0
iterApply IterCount _v  e0
 = letBind fvs e0 $ \i0 ->
   Op Add :@: Tup [Lit (LInt 1), i0]
 where
   fvs = free e0

-- FOR(v){e0}  -->   x:=v[]; ALL{x}
iterApply IterFor f e0
  = letBind fvs (f :@: Tup []) $ \fapp ->
    iterApply IterAll fapp e0
 where
    fvs = free (f,e0)

-- ALL(v){e0} -->  exists ys; xs := e0; ArrApp$[<v>, xs, ys]; ys
--   with a short cut if e0 is a value
iterApply IterAll v e0
  | Tup vs <- e0      -- OPTIONAL Short cut for very common case
  = Tup (v:vs)
  | otherwise
  = letBind (ys:fvs) e0 $ \i0 ->
    Exi $ bind ys $
    (Op ArrApp :@: Tup [Tup [v], i0, Var ys])
    >>> Var ys
 where
  fvs = free (v,e0)
  ys  = identNotIn fvs

letBind :: [Ident]   -- Variables to avoid
        -> Expr
        -> (Expr -> Expr)
        -> Expr
-- This function has a shortcut, so that instead of introducing
--    exists x. x=v; ...x...
-- where v is a value, it just returns
--    ...v...
letBind fvs e k
  | isVal e   = k e
  | otherwise = Exi $ bind x $
                (Var x :=: e) :>: k (Var x)
  where
    x = identNotIn fvs

{- Note [iter]
The iter construct is a (right) fold over choices.

In the expression iter(f){e}{e0}
  * e is the choices we are iterating over
  * f is what to do if e produces a value (taken from a finite set of possibilities)
  * e0 is what to do if e fails

The function f will be called with
  * value from the first argument to iter
  * a thunk for the value of the rest of the choices

Iter is just a fold over the choices, with the following property
   iter(f)( v1 | v2 | .. vn ){e0} = f[v1, f[ v2, ... f[vn, e0 ] ] ]

Iter has the following reduction rules:
(ITER-VALUE)   iter(f){v      }{e0}  -->  f v (\_ . e0)
(ITER-FAIL)    iter(f){fail   }{e0}  -->  e0
(ITER-CHOICE)  iter(f){e1 | e2}{e0}  -->  iter(f){e1}{iter(f){e2}{e0}}

Note that ITER-CHOICE has no requirement on e1 being a value.

If choices are always under if/one/all/for then the rules
CHOICE-ASSOC, CHOICE-FAIL-L, and CHOICE-FAIL-R are not needed.

Here's how if/one/all/for are encoded using iter.

  * if(e1){e2}else{e3}  -->
      --> iter(\ f _ . f[]){e1; \_ . e2}{\_ . e3}  ,  or
      --> iter(\ v _ . v){e1; \_ . e2}{\_ . e3}[]  ,  alternatively.

  * one{e}  -->  iter(\ v _ . v){e}{fail}

  * all{e}  -->  iter(\ v r . cons(v, r[])){e}{<>}
      The array is built in the accumulator; built up by consing
      new elements.

  * for(e1){e2}  -->  iter(\ f r . cons(f[],r[])){e1; \_ . e2}{<>}

where cons(x,xs) = arrApp$[<x>, xs, _]

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

(>>>) :: Expr -> Expr -> Expr -- e1 >>> e2 = (_ = e1); e2
eq@(_ :=: _) >>> e2 = eq :>: e2
e1           >>> e2 = (Var underscore :=: e1) :>: e2

mkSeq :: [Expr] -> Expr -- mkSeq [e1,e2,e3] = e1 >>> (e2 >>> e3)
mkSeq [] = Tup [] -- otherwise this crashes on an empty list
mkSeq es = foldr1 (>>>) es

mkEqual :: Expr -> Expr -> Expr -> Expr
-- mkEqual e1 e2 e3 =   (e1 :=: e2); e3
-- Or more precisely   exists x. (x=e1; x=e2; e3)
mkEqual e1 e2 e3
  = Exi $ bind x $
    mkSeq [Var x :=: e1, Var x :=: e2, e3]
  where
    x = identNotIn $ free (e1,e2,e3)

mkIf :: Expr -> Expr -> Expr
mkIf e1 e2 = Iter IterIf e1 e2

mkExis :: [Ident] -> Expr -> Expr
mkExis []     e = e
mkExis (x:xs) e = Exi (bind x (mkExis xs e))

mkOne :: Expr -> Expr
mkOne e = Iter IterOne e Fail

mkFor :: Expr -> Expr
mkFor e = Iter IterFor e (Tup [])

mkAll :: Expr -> Expr
mkAll e = Iter IterAll e (Tup [])

mkCount :: Expr -> Expr
mkCount e = Iter IterCount e (Lit (LInt 0))

-- mkCheck encode check<fx>{e}
-- See Note [mkCheck]
mkCheck :: Effect -> Expr -> Expr
-- check<iterate>{e} --> e
mkCheck Iterates e = e

-- check<fails>{e} --> if(e){WRONG}{fail}
mkCheck Fails e
  | isVal e   = wrongFx Fails  -- Short cut: see Note [mkCheck]
  | otherwise = mkIf (e >>> lamUnderscore (wrongFx Fails)) Fail

-- check<succeeds>{e} --> if(<x>:=all{e}){x}{WRONG}
mkCheck Succeeds e
  | isVal e = e  -- Short cut: see Note [mkCheck]
  | otherwise
  = mkIf (Exi $ bind x $
           (Tup [Var x] :=: mkAll e) :>: lamUnderscore (Var x))
         (wrongFx Succeeds)
 where
  x:_ = identsNotIn (free e)

-- check<decides>{e} --> if(a:=all{e};a=(<>|<_>)){<x>:=a;x}{WRONG}
mkCheck Decides e
  | isVal e = e  -- Short cut: see Note [mkCheck]
  | otherwise
  = mkIf (Exi $ bind a $
           (Var a :=: mkAll e) :>:
           (Exi $ bind y $
             (Var a :=: (Tup [] :|: Tup [Var y])) :>:
             lamUnderscore (Exi $ bind x $ (Var a :=: Tup [Var x]) :>: Var x)
         ) )
         (wrongFx Decides)
 where
  a:x:y:_ = identsNotIn (free e)

-- WRONG for definitely-failed check<fx>{e}
-- Change this definition to get more informative stuck states
wrongFx :: Effect -> Expr
wrongFx fx =  -- See Note [wrongFx] for these two alternative implementations
  Err ("check<" ++ show fx ++ ">")                        -- (A) Use Err (more compact)
--  Lit (LStr ("check<" ++ show fx ++ ">")) :@: Tup []   -- (B) Stuck (more informative)
-- NB: using (B) instead of (A) breaks tests arr-uni2a and arr-uni3

{- Note [mkCheck}
~~~~~~~~~~~~~~~~~
The desugaring can generate a lot of
    Check<succeeds>{x}
and it eliminates clutter to discard them early
For example:
   type{e}, desugars to fun(x:=e}<succeeds>{x}, and
            we don't really want to generate check<succeeds>{x}

Note [wrongFx]
~~~~~~~~~~~~~~
With the Err form
    check<succeeds>{fail}; fail
    --> Err("check<succeeds>"); fail
    --> Err("check<succeeds>")

With the stuck form
    check<succeeds>{fail}; fail
    --> "check<succeeds>"[]; fail   -- Hack: string[] is simply stuck
    --> fail
-}

-- matches expression against encoding of check<fx>{e} --> Just(fx,e)
matchCheck :: Expr -> Maybe (Effect,Expr)
matchCheck e0@(Iter IterIf e1 _)
  | (_ :=: e) :>: _ <- e1
  , ce <- mkCheck Fails e
  , norm ce == norm e0
  = Just (Fails, e)

matchCheck e0@(Iter IterIf (Exi bnd) _)
  | (_, (_ :=: Iter IterAll e _) :>: _) <- unsafeUnbind bnd
  = head $ [ Just (fx, e)
           | fx <- [Succeeds, Decides]
           , let ce = mkCheck fx e
           , norm ce == norm e0
           ] ++ [Nothing]

matchCheck _ = Nothing

lamUnderscore :: Expr -> Expr
lamUnderscore e = Lam (bind underscore e)

someAny :: Expr
-- The expression: some( any )
someAny = Some (Lam (bind x (Var x)))
  where
    x = ident "x"

someNat :: Expr
-- The expression: some( nat )
someNat = Some nat

someUnderscore :: Expr -> Expr
-- The expression: some( \_.e )
someUnderscore (Some e) = Some e  -- OPTIONAL  some(\_.some(t)) = some(t)
someUnderscore e        = Some (lamUnderscore e)

mkDef :: Ident -> Expr -> (Val -> Expr) -> Expr
mkDef x e k | isVal e   = k e
            | otherwise = Exi $ bind x $ (Var x :=: e) :>: k (Var x)

mkApp :: Expr -> Expr -> Expr
mkApp fun arg = mkDef f fun $ \f' -> mkDef a arg $ \a' -> f' :@: a'
 where
  f:a:_ = identsNotIn (free (fun,arg))

litInt :: Integer -> Expr
litInt n = Lit (LInt n)

litIntZero :: Expr
litIntZero = litInt 0

nat :: Expr
-- nat = \x. isInt$[x]; x>=0
nat = Lam $ bind x $
      mkSeq [ Op IsInt :@: Var x
            , Op GEq :@: Tup [Var x, litIntZero]
            , Var x ]
  where
    x = ident "x"

inRange :: Val -> Val -> Expr
-- (inrange i n) retuns the expression (isInt$[i]; i >= 0; i < n; i)
inRange i n = mkSeq [ Op IsInt :@: i
                    , Op IsInt :@: n
                    , Op GEq :@: Tup [i, litIntZero]
                    , Op Lt :@: Tup [i,n]
                    , i ]

inRangeType :: Val -> Expr
-- inRangeType n =    \i. inrange[i,n]
inRangeType n
 = Lam $ bind i (inRange (Var i) n)
 where
   i = identNotIn (free n)

--------------------------------------------------------------------------------
--
--                 PrimOps
--
--------------------------------------------------------------------------------

data PrimOp
 = -- Operations on integers
   Add | Sub | Mul | Div | Neg
 | Pls                               -- prefix +

   -- Operations on arrays
 | ArrLen
 | DotDot     -- dotDot$[v,n] = (v = (0 | 1 | .. | n)); ()
 | ArrApp     -- arrApp$[ Tup as, Tup bs, r ] =  r=(as++bs); r
 | ArrMap     -- arrMap$[t,a]
 | ArrCons    -- arrCons$[x,xs] = <x,..xs>

   -- Relational
 | Gt | Lt | NEq | GEq | LEq

   -- Type tests
 | IsInt | IsStr | IsChar | IsArr | IsTru | IsGround | IsFun
 | IsComp | IsAny | IsType | IsMap

 -- Maps
 | MkMap

   -- Checking
 | ChkFails | ChkSucceeds | ChkDecides
 deriving
   ( Eq, Ord, Bounded, Enum, Data )

instance Show PrimOp where
  show op = primOpString op

allPrimOps :: [PrimOp]
allPrimOps = [minBound .. maxBound]

primOpString :: PrimOp -> String
primOpString Add = "intAdd$"
primOpString Sub = "intSub$"
primOpString Mul = "intMul$"
primOpString Div = "intDiv$"
primOpString Neg = "intNeg$"
primOpString Pls = "intPls$"

primOpString ArrLen   = "arrLen$"
primOpString DotDot   = "dotDot$"
primOpString ArrApp   = "arrApp$"
primOpString ArrMap   = "arrMap$"
primOpString ArrCons  = "arrCons$"

primOpString Gt  = "intGT$"
primOpString GEq = "intGE$"
primOpString Lt  = "intLT$"
primOpString LEq = "intLE$"
primOpString NEq = "intNE$"

primOpString IsInt    = "isInt$"
primOpString IsStr    = "isStr$"
primOpString IsChar   = "isChar$"
primOpString IsArr    = "isArr$"
primOpString IsMap    = "isMap$"
primOpString IsComp   = "isComp$"
primOpString IsTru    = "isTru$"
primOpString IsFun    = "isFun$"
primOpString IsGround = "isGround$"
primOpString IsAny    = "isAny$"
primOpString IsType   = "isType$"

primOpString MkMap    = "mkMap$"

primOpString ChkFails    = "check<fails>"
primOpString ChkSucceeds = "check<succeeds>"
primOpString ChkDecides  = "check<decides>"

primOpIsCheck :: PrimOp -> Bool
primOpIsCheck ChkFails    = True
primOpIsCheck ChkSucceeds = True
primOpIsCheck ChkDecides  = True
primOpIsCheck _           = False

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
primOpCanFail IsMap  = True
primOpCanFail IsFun  = True
primOpCanFail IsComp = True
primOpCanFail IsTru  = True
primOpCanFail IsType = True
primOpCanFail ArrApp = True
primOpCanFail DotDot = True  -- Can fail when unification fails
primOpCanFail ArrCons = True  -- if run backwards

primOpCanFail MkMap  = False

-- chk<succeeds>[ <> ] is simply stuck; can't fail
primOpCanFail ChkSucceeds = False
primOpCanFail ChkFails    = True
primOpCanFail ChkDecides  = True

-- These operations /can't/ fail, and /do/ produce a value
primOpCanFail Add      = False
primOpCanFail Sub      = False
primOpCanFail Mul      = False
primOpCanFail Div      = False
primOpCanFail Neg      = False
primOpCanFail Pls      = False
primOpCanFail ArrLen   = False
primOpCanFail ArrMap   = False

-- These operations can't fail, and produce no value
primOpCanFail IsGround = False
primOpCanFail IsAny    = False

primOpIsTypeTest :: PrimOp -> Bool
-- Type tests; all mutually exclusive
primOpIsTypeTest IsInt  = True
primOpIsTypeTest IsStr  = True
primOpIsTypeTest IsChar = True
primOpIsTypeTest IsTru  = True
primOpIsTypeTest IsArr  = True
primOpIsTypeTest IsMap  = True
primOpIsTypeTest IsFun  = True
primOpIsTypeTest IsType = True
primOpIsTypeTest IsComp = True
primOpIsTypeTest _      = False

isBinOp :: PrimOp -> Bool
isBinOp Add    = True
isBinOp Sub    = True
isBinOp Mul    = True
isBinOp Div    = True
isBinOp DotDot = True
isBinOp ArrMap = True
isBinOp op     = isBinRelOp op

isBinRelOp :: PrimOp -> Bool
isBinRelOp Gt  = True
isBinRelOp Lt  = True
isBinRelOp NEq = True
isBinRelOp GEq = True
isBinRelOp LEq = True
isBinRelOp _   = False

primOpPreCond :: PrimOp -> GroundVal -> Maybe [PredAssump]
-- Returns a list of preconditions must hold before the primop can fire
-- (e.g. isInt$[arg] or not(arg = 0)

primOpPreCond Pls    = unOpPreCond IsInt
primOpPreCond Neg    = unOpPreCond IsInt
primOpPreCond ArrLen = unOpPreCond IsArr
primOpPreCond op | primOpIsTypeTest op = \_ -> Just []

primOpPreCond Add =  binOpPreCond intIntPreCond
primOpPreCond Sub =  binOpPreCond intIntPreCond
primOpPreCond Mul =  binOpPreCond intIntPreCond
primOpPreCond Div =  binOpPreCond divPreCond

primOpPreCond DotDot =  binOpPreCond intIntPreCond
primOpPreCond ArrMap =  binOpPreCond $ \a1 a2 ->
                        mkTypeTest IsFun a1 ++ mkTypeTest IsArr a2

primOpPreCond Gt  =  binOpPreCond intIntPreCond
primOpPreCond Lt  =  binOpPreCond intIntPreCond
primOpPreCond GEq =  binOpPreCond intIntPreCond
primOpPreCond NEq =  binOpPreCond intIntPreCond
primOpPreCond LEq =  binOpPreCond intIntPreCond

primOpPreCond op = error ("primOpPreCond " ++ show op)

unOpPreCond :: PrimOp -> GroundVal -> Maybe [PredAssump]
unOpPreCond tt_op gv = Just [A_Pos (A_RelOp tt_op gv)]

binOpPreCond :: (GroundVal -> GroundVal -> [PredAssump])
             -> GroundVal -> Maybe [PredAssump]
binOpPreCond pre (GVArr [a1,a2]) = Just (pre a1 a2)
binOpPreCond _   _               = Nothing

divPreCond :: GroundVal -> GroundVal -> [PredAssump]
divPreCond a1 a2 = intIntPreCond a1 a2 ++
                   [ A_Neg (A_GVEq a2 (GVLit (LInt 0))) ]

intIntPreCond :: GroundVal -> GroundVal -> [PredAssump]
intIntPreCond a1 a2 = mkTypeTest IsInt a1 ++ mkTypeTest IsInt a2

mkTypeTest :: PrimOp -> GroundVal -> [PredAssump]
mkTypeTest IsAny  _                  = []
mkTypeTest IsInt  (GVLit (LInt {}))  = []
mkTypeTest IsChar (GVLit (LChar {})) = []
mkTypeTest IsArr  (GVArr {})         = []
mkTypeTest op     gv                 = [A_Pos $ A_RelOp op gv]

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

pattern LitStr :: String -> Expr
pattern LitStr s = Lit (LStr s)

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
  = GVVar SkolIdent
  | GVLit Lit
  | GVArr [GroundVal]
  | GVTru GroundVal
  deriving( Eq, Ord, Show )

data Assump
  = A_Pred PredAssump                    -- e.g r = <s,t> or  not(r>s)
  | A_PrimOp Ident AssumpOp GroundVal    -- e.g. r = op[v],  (primOpCanFail op) is False
  deriving( Eq, Ord, Show )

data PredAssump
  = A_Pos FailableAssump    -- e.g. r = <s,t>  or  r>s
  | A_Neg FailableAssump    -- e.g. not(r = <s,t>) or not(r>s)
  deriving( Eq, Ord, Show )

data FailableAssump
  = A_GVEq  GroundVal GroundVal  -- e.g. r=3  or  r = <s,t>
  | A_RelOp PrimOp GroundVal     -- e.g. isInt$[r], or intGT$[r1,r2]
                                 -- A_RelOp invariant: (primOpCanFail op) is True
  deriving ( Eq, Ord, Show )

data AssumpOp
  = AO_Apply                            -- AO_apply [r,a]    means  r[a], r applied to a
  | AO_Prim PrimOp                      -- (primOpCanFail op) is False
  deriving( Eq, Ord, Show )

instance Pretty AssumpOp where
  pPrint AO_Apply     = text "Apply"
  pPrint (AO_Prim op) = pPrint op

instance Pretty Assump where
  pPrint (A_Pred p) = pPrint p
  pPrint (A_PrimOp i AO_Apply (GVArr [fun,arg]))
                            = pPrint i <+> text "=" <+> pPrint fun <> brackets (pPrint arg)
  pPrint (A_PrimOp i op gv) = pPrint i <+> text "=" <+> pPrint op <> brackets (pPrint gv)

instance Pretty PredAssump where
  pPrint (A_Pos a) = pPrint a
  pPrint (A_Neg a) = text "not" <> parens (pPrint a)

instance Pretty FailableAssump where
  pPrint (A_GVEq gv1 gv2)   = pPrint gv1 <+> text "="  <+> pPrint gv2
  pPrint (A_RelOp op gv)    = pPrint op <> brackets (pPrint gv)

instance Pretty GroundVal where
  pPrint (GVVar i)   = pPrint i
  pPrint (GVLit l)   = pPrint l
  pPrint (GVArr gvs) = char '<' <> fsep (punctuate comma $ map pPrint gvs) <> char '>'
  pPrint (GVTru gv)  = text "truth" <> braces (pPrint gv)

notPred :: PredAssump -> PredAssump
notPred (A_Pos p) = A_Neg p
notPred (A_Neg p) = A_Pos p

--------------------------------------------------------------------------------
--
--                 Effects
--
--------------------------------------------------------------------------------

data Effect
  = Fails
  | Succeeds
  | Decides
  | Iterates
 deriving ( Eq, Ord, Data )

instance Show Effect where
  show Fails    = "fails"
  show Succeeds = "succeeds"
  show Decides  = "decides"
  show Iterates = "iterates"

instance Pretty Effect where
  pPrint eff = text (show eff)

canSucceed :: Effect -> Bool
-- True if one result is acceptable
canSucceed Iterates = True
canSucceed Succeeds = True
canSucceed Decides  = True
canSucceed Fails    = False

canFail :: Effect -> Bool
-- True if no results is acceptable
canFail Iterates = True
canFail Succeeds = False
canFail Decides  = True
canFail Fails    = True

intersectEffect :: Effect -> Effect -> Effect
intersectEffect Iterates fx2      = fx2
intersectEffect Decides  Iterates = Decides
intersectEffect Decides  fx2      = fx2
intersectEffect Fails    Succeeds = contradiction
intersectEffect Fails    _        = Fails
intersectEffect Succeeds Fails    = contradiction
intersectEffect Succeeds _        = Succeeds

contradiction :: Effect
contradiction = error "intersectEffect"

wrong :: Expr
wrong = Lit (LInt 0) :@: Lit (LInt 0)

--------------------------------------------------------------------------------
--
--                 Pretty-printing
--
--------------------------------------------------------------------------------

instance Pretty Expr where
  pPrintPrec = pPrintPrecE

instance PrettyBrief Expr where
  pPrintBrief e = text "size:" <> int (exprSize e)

instance Pretty PrimOp where
  pPrint op = text (primOpString op)

pPrintPrecE :: PrettyLevel -> Rational -> Expr -> Doc
pPrintPrecE lvl prec the_expr
  = case the_expr of
       -- basic
       HOLE       -> text "HOLE"
       Fail       -> text "fail"
       Err s      -> text ("Err(" ++ s ++ ")")
       Var x      -> pPrint x
       Lit i      -> pPrint i
       Op op      -> pPrint op

       -- special pretty printing to help debugging
       e | Just (fx,b) <- matchCheck e ->
         block "{}" (text ("CHECK<" ++ show fx ++ ">")) b

       --e | Just (n,b) <- matchSize e ->
       --  block "{}" (text ("SIZE(" ++ show n ++ ")")) b

       -- combinators
       e1 :=: e2   -> mbPar0 $ ppr1 e1 <+> char '=' <+> ppr1 e2
       e1 :|: e2   -> mbPar0 $ sep [ ppr1 e1, char '|' <+> ppr1 e2 ]
       e1 :@: e2   -> block' "[]" (ppr1 e1) (pp_call_arg e2)
       e@(_ :>: _) -> mbPar0 $ sep (punctuate semi $ map ppr1 (gatherSeqs e))

       -- Guards; print together
       g1 :>>: e  -> mbPar0 $ sep [ fsep [ ppr0 g <+> text ";;" | g <- all_gs ]
                                  , ppr1 etail ]
                  where
                        (all_gs, etail) = grab [g1] e
                        grab gs (g :>>: e2) = grab (g:gs) e2
                        grab gs et          = (reverse gs, et)

       Tup as  -> char '<' <> fsep (punctuate comma $ map ppr0 as) <> char '>'
       Tru a   -> block "{}" (text "truth") a
       Iter f e e0 -> {- text "iter"  <> parens (text (show f)) -}
                      block "{}" (text (show f)) e <> braces (ppr0 e0)
       --All e   -> text "all"  <> braces (ppr0 e)
       Lam bnd -> mbPar0 $ char '\\' <> pprBind bnd
       Exi {}  -> mbPar0 $ sep [ text "∃" <> fsep (map pPrint bndrs) <> char '.'
                               , indent (ppr0 body) ]
               where
                  (bndrs, body) = unpackExis the_expr

       Some e  -> block "()" (text "some") e
       --Check fx e -> cat [ text "check" <> char '<' <> pPrint fx <> char '>'
       --                  , indent (braces (ppr0 e)) ]

       Verify bl ->
         block "{}" (text "verify" <> parens (sep [ fsep (punctuate comma (map pPrint ids)) <> char ';'
                                             , fsep (punctuate comma (map pPrint as)) ]))
               body
           where
             (ids, (as, body)) = alphaRenameVerify (free bl) bl


       Arr    sz e -> block "{}" (text "Arr" <> ppr_sz sz) e
       Choose sz e -> block "{}" (text "Choose" <> ppr_sz sz) e
       --Size   sz e -> text "Size"   <> ppr_sz sz <> braces (ppr0 e)

  where
    ppr0 = pPrintPrecE lvl 0
    ppr1 = pPrintPrecE lvl 1

    ppr_sz v = parens (ppr0 v)

    mbPar0 = maybeParens (prec > 0)

    -- Reduce clutter: omit the angle brackets for a multi-arg call.
    -- That is, print f[3,2] rather than f[<3,2>]
    pp_call_arg (Tup es) = fsep (punctuate comma $ map ppr0 es)
    pp_call_arg e2       = ppr0 e2

    block  pq hdr e   = block' pq hdr (ppr0 e)
    block' pq hdr doc = cat ([ hdr <> text (take 1 pq), indent doc, text (drop 1 pq) ])

pPrintSmallExpr :: Expr -> Doc
-- Show only a small expression; otherwise return "<big>"
pPrintSmallExpr e
  | exprSize e < 10 = pPrint e
  | otherwise       = text "<big>"

gatherSeqs :: Expr -> [Expr]
gatherSeqs ((Var u :=: e1) :>: e2) | isUnderscore u = e1 : gatherSeqs e2 -- just trying this out
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
exprSize (Err _)       = 1
exprSize HOLE          = 1
exprSize (Tup as)      = 1 + sum (map exprSize as)
exprSize (Tru a)       = 1 + exprSize a
exprSize (Lam bnd)     = 1 + bindSize bnd
exprSize (e1 :>: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :=: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :|: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :@: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :>>: e2)  = 1 + exprSize e1 + exprSize e2
exprSize (Exi bnd)     = 1 + bindSize bnd
exprSize (Iter _ e e0) = 1 + exprSize e + exprSize e0
--exprSize (All e)       = 1 + exprSize e
exprSize (Some a)      = 1 + exprSize a
--exprSize (Check _ e)   = 1 + exprSize e
exprSize (Verify bl)   = 10 + exprSize e
                       where
                         (_rs,(_as,e)) = unsafeUnbindList bl
exprSize (Arr sz e)    = 1 + exprSize sz + exprSize e
--exprSize (Size sz e)   = 1 + exprSize sz + exprSize e
exprSize (Choose sz e) = 1 + exprSize sz + exprSize e

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
isHNF (Tup es) = all isVal es
isHNF (Arr {}) = True
isHNF (Tru e)  = isVal e
isHNF (Lam {}) = True
isHNF _        = False

isComparable :: Expr -> Bool
isComparable (Lit (LChar {})) = True
isComparable (Lit (LInt  {})) = True
isComparable (Lit (LStr  {})) = True
isComparable (Tup es)         = all isComparable es
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
valid (a1 :@: a2)         = is_val a1 && is_val a2
valid (Exi bnd)           = valid e where (_,e) = unsafeUnbind bnd
  -- SLPJ: todo: check binder is not _
valid (Lam bnd)           = valid e where (_,e) = unsafeUnbind bnd
valid Fail                = True
valid (Err _)             = True
valid (Some a)            = is_val a
--valid (All e)             = valid e
valid (a :>>: e)          = is_val a && valid e  -- Guard
valid (Iter _ e e0)       = valid e && valid e0
--valid (Check _ e)         = valid e
valid (Verify bl)         = valid e where (_, (_as,e)) = unsafeUnbindList bl
valid (Arr    sz e)       = is_val sz && valid e
--valid (Size   sz e)       = is_val sz && valid e
valid (Choose sz e)       = is_val sz && valid e
valid e                   = is_val e

is_val :: Expr -> Bool
is_val e = if isVal e then True else ppTrace "not valid" (pPrint e) False
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
prep top_e = prepExpr (free top_e) top_e
  where

prepExpr :: [Ident] -> Expr -> Expr
prepExpr _ e | isAtomic e = e

prepExpr xs (Tup as)     = prepVals xs as (\_ vs -> Tup vs)
prepExpr xs (Tru a)      = prepVal xs  a  (\_ v  -> Tru v)

prepExpr xs (Lam bnd)    = Lam (bind x (prepExpr (x:xs) e))
                   where (x,e) = unsafeUnbind bnd
prepExpr xs (Exi bnd)    = Exi (bind x (prepExpr (x:xs) e))
                   where (x,e) = unsafeUnbind bnd

prepExpr xs (e1 :>: e2)
  | (a :=: rhs) <- e1 = prepVal xs a (\xs' v -> (v :=: prepExpr xs' rhs) :>: prepExpr xs' e2)
  | otherwise         = (Var underscore :=: prepExpr xs e1) :>: prepExpr xs e2

prepExpr xs (e1 :|: e2)  = prepExpr xs e1 :|: prepExpr xs e2
prepExpr xs (a  :=: e)   = prepVal xs a (\xs' v -> (v :=: prepExpr xs' e) :>: v)
prepExpr xs (a :>>: e)   = prepVal xs a (\xs' v -> v :>>: prepExpr xs' e)
prepExpr xs (Some a)     = prepVal xs a (\_ v -> Some v)
prepExpr xs (a1 :@: a2)  = prepVal xs a1 (\xs1 v1 -> prepVal xs1 a2 (\_ v2 -> v1 :@: v2))

prepExpr xs (Verify bl)  = Verify (bindList ys (as, prepExpr (ys ++ xs) e))
                   where (ys,(as,e)) = unsafeUnbindList bl

prepExpr xs (Iter f e e0) = Iter f (prepExpr xs e) (prepExpr xs e0)

prepExpr _ e  -- HOLE, Arr, Choose, Size
  = error ("prep bad: " ++ show e)


prepVal :: [Ident] -> Expr -> ([Ident] -> Val -> Expr) -> Expr
-- (prepVal e K) applies K to the value of e,
-- perhaps by adding an existential, thus (exi x. x = e; K[x])
prepVal xs a k | isAtomic a = k xs a
prepVal xs (Tup as) k = prepVals xs as $ \xs' vs -> k xs' (Tup vs)
prepVal xs (Tru a)  k = prepVal  xs a  $ \xs' v  -> k xs' (Tru v)
prepVal xs (e1 :=: e2) k = prepVal xs e1 $ \xs1 v1 ->
                           (v1 :=: prepExpr xs1 e2) :>: k xs1 v1
prepVal xs a        k = Exi (bind x ((Var x :=: prepExpr (x:xs) a) :>: k (x:xs) (Var x)))
  where
    x = identNotIn xs

prepVals :: [Ident] -> [Expr] -> ([Ident] -> [Val] -> Expr) -> Expr
prepVals xs []     k = k xs []
prepVals xs (e:es) k = prepVal xs   e  $ \xs1 v ->
                       prepVals xs1 es $ \xs2 vs ->
                       k xs2 (v:vs)

isAtomic :: Expr -> Bool
isAtomic (Var {}) = True
isAtomic (Lit {}) = True
isAtomic (Op {})  = True
isAtomic Fail     = True
isAtomic (Err {}) = True
isAtomic _        = False

--------------------------------------------------------------------------------
--
--                 Variables
--
--------------------------------------------------------------------------------

instance Variables Expr where
  variables _ (Lit {})      = []
  variables _ (Op {})       = []
  variables _ Fail          = []
  variables _ (Err _)       = []
  variables _ HOLE          = []
  variables f (Var x)       = variables f x
  variables f (Tup es)      = variables f es
  variables f (Tru e)       = variables f e
  variables f (Lam bnd)     = variables f bnd
  variables f (e1 :=: e2)   = variables f (e1,e2)
  variables f (e1 :>: e2)   = variables f (e1,e2)
  variables f (e1 :|: e2)   = variables f (e1,e2)
  variables f (e1 :@: e2)   = variables f (e1,e2)
  variables f (Some e)      = variables f e
  variables f (e1 :>>: e2)  = variables f (e1,e2)
  variables f (Exi bnd)     = variables f bnd
  variables f (Arr sz e)    = variables f (sz,e)
  variables f (Choose sz e) = variables f (sz,e)
  variables f (Verify bnd)  = variables f bnd
  variables f (Iter _ e e0) = variables f (e, e0)

instance Variables FailableAssump where
  variables f (A_GVEq gv1 gv2) = variables f gv1 `union` variables f gv2
  variables f (A_RelOp _ gv)   = variables f gv

instance Variables Assump where
  variables f (A_Pred p)          = variables f p
  variables f (A_PrimOp i _ gv)   = [i] `union` variables f gv

instance Variables PredAssump where
  variables f (A_Pos a)           = variables f a
  variables f (A_Neg a)           = variables f a

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

  alpha _ e@(Lit {})   = e
  alpha _ e@(Var {})   = e
  alpha _ e@(Op {})    = e
  alpha _ e@(Fail {})  = e
  alpha _ e@(HOLE {})  = e
  alpha _ e@(Err {})   = e

  alpha k (Tup es)     = Tup (map (alpha k) es)
  alpha k (Tru e)      = Tru ((alpha k) e)
  alpha k (Lam bnd)    = Lam (bind x (alpha (k+1) e))
                       where x = var k; e = unbindAs x bnd
  alpha k (e1 :=: e2)  = alpha k e1 :=: alpha k e2
  alpha k (e1 :>: e2)  = alpha k e1 :>: alpha k e2
  alpha k (e1 :|: e2)  = alpha k e1 :|: alpha k e2
  alpha k (e1 :@: e2)  = alpha k e1 :@: alpha k e2
  alpha k (Some e)     = Some (alpha k e)
  --alpha k (All e)      = All (alpha k e)
  alpha k (Arr  s e)   = Arr    (alpha k s) (alpha k e)
  --alpha k (Size s e)   = Size   (alpha k s) (alpha k e)
  alpha k (Choose s e) = Choose (alpha k s) (alpha k e)
  alpha k (e1 :>>: e2) = alpha k e1 :>>: alpha k e2
  --alpha k (Check fx e) = Check fx (alpha k e)
  alpha k e@(Exi _)    = alphaExi k [] e
  alpha k (Verify bl)  = let (rs, (as,e)) = unsafeUnbindList bl
                             rs' = map skvar [k+1..k+n]
                             n   = length rs
                             sub = rs `zip` rs'
                             e'  = alpha (k+n) e
                         in Verify (bindList rs' (map (substAssump sub) as, substSkol sub e'))
  alpha k (Iter f e e0) = Iter f (alpha k e) (alpha k e0)

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
    go e@(Lit {})   = e
    go e@(Op {})    = e
    go e@(Fail {})  = e
    go e@(Err {})   = e
    go e@(HOLE {})  = e

    go (Var x)      = head $ [e | (y,e) <- sub, y == x] ++ [Var x]
    go (Tup es)     = Tup (map go es)
    go (Tru e)      = Tru (go e)
    go (Lam bnd)    = Lam (substBind subst_e_ops sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    --go (All e)      = All (go e)
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    --go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  subst_e_ops sub bnd)
    go (Arr  s e)   = Arr    (go s) (go e)
    --go (Size s e)   = Size   (go s) (go e)
    go (Choose s e) = Choose (go s) (go e)
    go (Verify bl)  = Verify (substBinds subst_verify_ops sub bl)

    go (Iter f e e0) = Iter f (go e) (go e0)

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
    go (Tup es)     = Tup (map go es)
    go (Tru e)      = Tru (go e)
    go (Lam bnd)    = Lam (substBind subst_e_ops sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    --go (All e)      = All (go e)
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    --go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  subst_e_ops sub bnd)
    go (Verify bl)  = Verify (substBinds subst_verify_ops sub bl)
    go (Iter f e e0) = Iter f (go e) (go e0)
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
    go (A_Pred p)         = A_Pred (goP p)
    go (A_PrimOp x op gv) = A_PrimOp (lookupIdSubst sub x) op (substGV sub gv)

    goP (A_Pos a) = A_Pos (goF a)
    goP (A_Neg a) = A_Neg (goF a)

    goF (A_GVEq gv1 gv2)  = A_GVEq   (substGV sub gv1) (substGV sub gv2)
    goF (A_RelOp op gv)   = A_RelOp  op (substGV sub gv)

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
--            Arbitrary
--
--------------------------------------------------------------------------------

instance Arbitrary PrimOp where
  arbitrary = elements allPrimOps

instance Arbitrary Expr where
  arbitrary = sized (arbExprWith xs)
   where
    xs = take 3 (identsNotIn [])

  shrink (LitInt k)   = [ LitInt k' | k' <- shrink k ]  -- SLPJ: other literals?
  shrink (Op op)      = [ LitInt 0, LitInt 1 ] ++ [ Op IsInt | op /= IsInt ]   -- See Note [Shrinking expressions: ops] SLPJ: explain
  shrink (Tup es)     = es
                     ++ [ Tup es' | es' <- shrink es ]
  shrink (Tru e)      = [ e, Tup [e] ] ++ [ Tru e' | e' <- shrink e ]
  shrink (Lam bnd)    = shrinkBind Lam bnd
{-
  -- this shrink rule only makes sense if = can appear by itself:
  shrink (e1 :=: e2)  = [ e1, e2 ]
                     ++ [ e1' :=: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :=: e2' | e2' <- shrink e2 ]
-}
  shrink ((v :=: e1) :>: e2)
                      = [ v, e1, e2 ]
                     ++ [ Exi (bind x ((Var x :=: e1) :>: e2))
                        | v == Var underscore
                        , let x = identNotIn (free (e1,e2))
                        ]
                     ++ [ (w :=: d1) :>: ((v :=: d2) :>: e2)
                        | (w :=: d1) :>: d2 <- [e1]
                        ]
                     ++ [ (v' :=: e1)  :>: e2  | v'  <- shrink v  ]
                     ++ [ (v  :=: e1') :>: e2  | e1' <- shrink e1 ]
                     ++ [ (v  :=: e1)  :>: e2' | e2' <- shrink e2 ]
  shrink (e1 :|: e2)  = [ e1, e2 ]
                     ++ [ e1' :|: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :|: e2' | e2' <- shrink e2 ]
  shrink (e1 :@: e2)  = [ e1, e2 ]
                     ++ [ e1' :@: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :@: e2' | e2' <- shrink e2 ]
  --shrink (All e)      = [ e ] ++ [ All e'  | e' <- shrink e ]
  shrink (Iter f e e0)
                      = [ e
                        , e0
                        , iterApply f (Tup []) e0
                        ]
                     ++ [ Iter f e' e0 | e' <- shrink e ]
                     ++ [ Iter f e e0' | e0' <- shrink e0 ]
                     ++ [ Iter f' e e0 | f' <- takeWhile (/=f) [IterOne,IterAll] ]
  shrink (Some e)     = [ e ] ++ [ Some e' | e' <- shrink e ]
  shrink (e1 :>>: e2) = [ e1, e2 ]
                     ++ [ e1' :>>: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :>>: e2' | e2' <- shrink e2 ]
  --shrink (Check fx e) = [ e ]
  --                   ++ [ Check fx e' | e' <- shrink e ]
  shrink (Exi bnd)    = shrinkBind Exi bnd
  shrink Fail         = [ LitInt 0 ]
  --shrink (Verify bnd) = error "shrink Verify undefined"
  shrink _            = []

arbExprWith :: [Ident] -> Int -> Gen Expr
arbExprWith xs n =
  frequency $
  [ (1, Var `fmap` elements xs) | not (null xs) ] ++
  [ (1, LitInt `fmap` arbitrary)
  , (a, Tup `fmap` arbExprs)
  , (a, Tru `fmap` arbExpr1)
  , (a, Lam `fmap` arbBind)
  , (1, Op  `fmap` arbitrary)
  , (b, liftM2 (:=:) arbExpr2 arbExpr2)
  , (b, liftM2 (:>:) arbExpr2 arbExpr2)
  , (b, liftM2 (:|:) arbExpr2 arbExpr2)
  , (a, liftM2 (:@:) arbExpr2 arbExpr2)
  , (b, Exi `fmap` arbBind)
  -- , (a, liftM3 Iter arbExpr2 arbExpr2 arbExpr2)
  , (a, mkOne `fmap` arbExpr1)
  , (a, mkAll `fmap` arbExpr1)
  , (a, liftM2 mkCheck (elements [Fails, Succeeds, Decides]) arbExpr1)
  , (1, return Fail)
  , (1, return (Err ""))
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
HOLE          <@ h = h
e@(Var {})    <@ _ = e
e@(Lit {})    <@ _ = e
e@(Op  {})    <@ _ = e
Fail          <@ _ = Fail
Err s         <@ _ = Err s
Tup as        <@ h = Tup (map (<@ h) as)
Tru a         <@ h = Tru (a <@ h)
Lam bnd       <@ h = Lam (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
(e1 :>: e2)   <@ h = (e1 <@ h) :>: (e2 <@ h)
(e1 :=: e2)   <@ h = (e1 <@ h) :=: (e2 <@ h)
(e1 :|: e2)   <@ h = (e1 <@ h) :|: (e2 <@ h)
(e1 :@: e2)   <@ h = (e1 <@ h) :@: (e2 <@ h)
Exi bnd       <@ h = Exi (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
Iter f e e0   <@ h = Iter f (e <@ h) (e0 <@ h)
Some e        <@ h = Some (e <@ h)
--All  e        <@ h = All  (e <@ h)
(e1 :>>: e2)  <@ h = (e1 <@ h) :>>: (e2 <@ h)
--Check fx e    <@ h = Check fx (e <@ h)
e@(Verify {}) <@ _ = e   -- No HOLE inside Verify. SLPJ: check
--Size   v e    <@ h = Size   v (e <@ h)
Arr    v e    <@ h = Arr    v (e <@ h)
Choose v e    <@ h = Choose v (e <@ h)

{-  Not used
boundVars :: Context -> [Ident]
-- Returns all the binders that are in scope at the HOLE
boundVars ctx = explore [] ctx
 where
  go xs (Tup es)     = foldr union [] (map (go xs) es)
  go xs (Tru e)      = go xs e
  go xs (Lam bnd)    = goBind xs bnd
  go xs (e1 :=: e2)  = go xs e1 `union` go xs e2
  go xs (e1 :>: e2)  = go xs e1 `union` go xs e2
  go xs (e1 :|: e2)  = go xs e1 `union` go xs e2
  go xs (e1 :@: e2)  = go xs e1 `union` go xs e2
  go xs (Iter e1 e2 e3) = go xs e1 `union` go xs e2 `union` go xs e3
  go xs (All e)      = go xs e
  go xs (Some e)     = go xs e
  go xs (e1 :>>: e2) = go xs e1 `union` go xs e2
  go xs (Check _ e)  = go xs e
  go xs (Exi bnd)    = goBind xs bnd
  go _  (Verify {})  = []  -- HOLE is not inside Verify{}
  go xs HOLE         = xs

  goBind xs bnd = go ([x] `union` xs) e where (x,e) = unsafeUnbind bnd
-}

isContext :: Context -> Bool
-- There is a HOLE, outside a Verify (SLPJ: is the "outside Verify" right?)
isContext (Tup es)     = any isContext es
isContext (Tru e)      = isContext e
isContext (Lam bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (e1 :=: e2)  = isContext e1 || isContext e2
isContext (e1 :>: e2)  = isContext e1 || isContext e2
isContext (e1 :|: e2)  = isContext e1 || isContext e2
isContext (e1 :@: e2)  = isContext e1 || isContext e2
isContext (Iter _ e e0) = isContext e || isContext e0
--isContext (All e)      = isContext e
isContext (Some e)     = isContext e
isContext (e1 :>>: e2) = isContext e1 || isContext e2
--isContext (Check _ e)  = isContext e
isContext (Exi bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (Verify {})  = False
isContext HOLE         = True
isContext _            = False

{-
-- Unpack a correct Iter construct
unIter :: Expr -> Maybe (Expr, (Ident, Ident, Expr), Expr)
-- iter e1 e2 (\x y z. e3) (\w. e4)
--   returns (e1, e2, (x,y,z,e3), (w,e4))
unIter (Iter e1 (Lam b2) (Lam b3))
    | (x, Lam b2') <- unsafeUnbind b2
    , (y, eb2) <- unsafeUnbind b2'
    , (z, eb3) <- unsafeUnbind b3
    , isUnderscore z
    = Just (e1, (x, y, eb2), eb3)
unIter _ = Nothing
-}
