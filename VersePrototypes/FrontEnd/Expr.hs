{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FrontEnd.Expr(
      Loc, noLoc, mkLoc

    , Ident(..), identLoc, identString
    , SrcExpr(..), Lit(..), Path(..)
    , Aperture(..), defaultAperture
    , SrcPat, SrcEssential, SrcMini, SrcCore, SrcBlk, SrcValue
    , PrimOp(..), MVLWrap(..)

      -- Predicates on SrcExpr
    , isConst, isAtomic, isValue

      -- Building SrcExpr
    , eFalse, eAny, eMkMap, eHavoc, eGuard, eSome, eOne
    , eAll, eExists, eCheck, eApplyD, eVerify, eUnit
    , eThunk, eForce, eForceLam, existsXX, eSomeAny
    , mkSeq, eSeq, eUnify, eFunction, fvArray
    , srcUnderscore, isSrcUnderscore, identX

    , Store(..), Ptr

    , EffString, Eff(..), intersectEffects, toEff, isTopEff
    , CardEff(..), SideEff(..), effTop, effSucceeds, effDecides, effFails

    , Op, pattern Op
    , compos, composOp
    , getLoc

    , getFree, getAllIdents, getVisibleBinders, getAllBinders, getVar
    , fixity,
  ) where

import Prelude hiding ((<>))  -- Epic.Print exports (<>)

import Core.Expr( Lit(..), Ptr, Path(..), PrimOp(..) )

import FrontEnd.Error
import Epic.List
import Epic.Print

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Data (Data)
import qualified Data.IntMap as IM
import Data.Maybe
import Data.List( partition )

import GHC.Stack( HasCallStack )

import Text.Megaparsec (SourcePos(..), mkPos, initialPos, sourcePosPretty)

{- Note [The SrcExpr lifecycle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Source expressions are parsed into `SrcExpr`.
  At this stage lots of things appear as Macro1/2 or InfixOp;
  See Note [How SrcExpr is parsed]

* S-desugaring ("S" for superficial) desugars into `SrcEssential`.
  Here we convert lots of InfixOp/Macro1/2 into proper data constructors.
  This is done by `sDesugarExpr`.

After this there are no more macros

* M-desugaring desugars into `SrcCore`, which has fewer data constructors.
  This is done by `mDesugarExpr`.

* ToCore.convertToCore converts SrcCore to the true Core language.
  It has two steps:
   - addScope: replaces (... x:=e ...) with (exists x. ...x=e...)
   - convert: moves from SrcCore to Core

Note [How SrcExpr is parsed]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Initially we parse SrcExpr into a bunch of Macro1/2 ane InfixOp.
Notable examples:

              This                 is initially parsed as
-------------------------------------------------------------------------
Type sig      e1:e2                InfixOp e1 ":" e2
Binding       p := e               InfixOp p ":=" e
Choice        e1 | e2              InfixOp e1 "|" e2
Unification   e1 = e2              InfixOp e1 "=" e2
Where         e1 where e2          InfixOp e1 "where" e2
Binding       f(p)<fx> := e        InfixOp (EffAttr (ApplyS f p) fx) ":=" e
Guard         v ;;<fx> e           Macro2 "guard" <fx> e
Failure       fail                 Variable "fail"
-}

--------------------------------------------------------
--
--         The main SrcExpr data type
--
--------------------------------------------------------

data SrcExpr  -- See Note [The SrcExpr lifecycle]
  = -- Source Verse
  -- The first block of constructors are eliminated by the superficial S-desugaring
    QualVariable SrcExpr Ident   -- (e:)x
  | Tuple [SrcExpr]              -- e1,e2,...             -- Will be turned into Array
  | ApplyS SrcExpr SrcExpr       -- f(e)
  | EffAttr SrcExpr EffString    -- f<e>

  -- Prefix and infix operator application
  | PrefixOp Op SrcExpr          -- op e
  | PostfixOp SrcExpr Op         -- e op
  | InfixOp SrcExpr Op SrcExpr   -- e1 op e2

  | If1 SrcBlk                   -- if{e}
  | If2 SrcExpr SrcBlk           -- if(e1) then e2
  | If2E SrcExpr SrcBlk          -- if(e1) else e2

  | For1 SrcBlk                    -- for{e}
  | For2 SrcExpr SrcBlk            -- for(e1) in e2

  | Let SrcExpr SrcBlk             -- let(e1) in e2
  | Block SrcBlk                   -- do e
  | Case1 SrcBlk                   -- case{e1; e2; ... } block treated in a non-standard way
  | Case2 SrcExpr SrcBlk           -- case(e) of {e1; e2; ... } block treated in a non-standard way
  | Blk [SrcExpr]                -- { e1; e2; ... }
  | Option (Maybe SrcExpr)       -- option{e}
  | Parens SrcExpr               -- (e)

  | Macro1 Ident [EffString] SrcBlk  -- m<a>{e}
  | Macro2 Ident SrcExpr SrcBlk      -- m(e1){e2}
  | Return SrcExpr                   -- return e

  -----------------------------------------------------------
  -- Essential Verse
  -- Only constructors below here appear in the output of S-desugaring
  -- See Note [How SrcExpr is parsed] and the dsSmall function
  | DefineV Ident                      -- (exists i)  Bring `i` into scope in the entire
                                       --    innermost scoping context, with no type constraint
                                       -- This is a valid expression; eg <exists x, exists y>
                                       -- is like (exists x; exists y; <x,y>)
  | Function Aperture SrcExpr Eff SrcBlk -- function(e)<eff>{e}

  | OfType SrcExpr Eff SrcExpr       -- e |>{fx} t
                                     -- Empty [Eff] means "all effects" (not none!)

  | DefineIE Ident SrcExpr             -- i->t    Capture input
  | Where SrcBlk SrcExpr               -- e1 where e2
  | If3 SrcExpr SrcBlk SrcBlk          -- if(e1) then e2 else e3
  | Splice SrcExpr                     -- Array splicing ..e

  -----------------------------------------------------------
  -- Mini Verse
  -- Only constructors below here appear in the output of the unwrapping desugaring
  -- See Note [How SrcExpr is parsed] and the dsSmall function

  | MVLam { mvl_fxs  :: Eff      -- Effects
          , mvl_i    :: Ident    -- Binder: scopes over mvl_dom and mvl_rng
          , mvl_dom  :: SrcExpr  -- Domain
          , mvl_wrap :: MVLWrap  -- The wrapped function, if any
          , mvl_rng  :: SrcExpr  -- Range
    }

  -----------------------------------------------------------
  -- Big Core: only constructors below here appear in the output of M-desugaring
  | Lit Lit                            -- k
  | Variable Ident                     -- x
  | DefineE Ident SrcExpr              -- i := e
  | ApplyD SrcExpr SrcExpr             -- f[e]
  | Range SrcExpr                      -- :e
  | Check Eff SrcExpr                  -- check<fx>{e}
  | Array [SrcExpr]                    -- array{e1;e2;...}
  | Seq SrcExpr SrcExpr                -- e1;e2;...
  | Choice SrcBlk SrcBlk               -- e1 | e2
  | Unify SrcExpr SrcExpr              -- e1 = e2
  | EPrim PrimOp                       -- Primop

  -- Mutable variables
  | Set SrcExpr Ident SrcExpr       -- set e1 = e2
  | MVar Ident (Maybe SrcExpr) (Maybe SrcExpr)      -- var i : t = e
  | MRef Ident (Maybe SrcExpr) (Maybe SrcExpr)      -- ref i : t = e
  | MAlias Ident (Maybe SrcExpr) (Maybe SrcExpr)    -- alias i : t = e

  -- Verification stuff
  | Verify [Ident] SrcExpr         -- verify fs . e
                                   --  (we only need assumptions when we get to the core language)
  | Guard SrcExpr SrcExpr          -- guard(v){e}
  | Some SrcExpr                   -- some{e}

  -- Embed Core into Expr
  | One SrcExpr                    -- one{e}
  | All SrcExpr                    -- all{e}
  | Lam Ident SrcExpr              -- ICFP lambda:   \ x . e.  We include \_.e
  | Wrong String                   -- wrong
  | Fail                           -- :false
  | Exists [Ident] SrcExpr         -- exists xs . e
  | Map [SrcExpr]                  -- map{e1;e2; ... }
  | Truth SrcExpr                  -- truth{e}

  -- These are used when translating back from Rules.Core.SrcExpr
  | EStore Store SrcExpr

  deriving (Eq, Ord, Show, Data)

-- SrcPat synonym is used for syntax of 'p' in the source language
type SrcPat = SrcExpr

-- SrcEssential synonym is used for the reduced subset of SrcExpr
-- where superficial syntactic sugar has been removed
type SrcEssential = SrcExpr

-- SrcMini synonym is used for the result of the "wrapping" desugaring
type SrcMini = SrcExpr

-- SrcCore synonym is used for the very reduced subset of SrcExpr
-- that can be directly translated to Rules.Core.Expr
type SrcCore  = SrcExpr

type SrcValue = SrcExpr
type SrcBlk   = SrcExpr

--------------------------------------------------------
--      Aperture: open or closed

data Aperture = Open | Closed
  deriving (Eq, Ord, Show, Data)

defaultAperture :: Aperture
defaultAperture = Closed  -- Just for now

--------------------------------------------------------
--      Wrapping

data MVLWrap
  = NoWrap
  | Wrap { wp_x :: Ident   -- Occurrence: bound in mvl_dom
         , wp_h :: Ident   -- Occurrence: bound outside
         , wp_y :: Ident   -- Binder; scopes over mvl_rng
         }
  deriving (Eq, Ord, Show, Data)

--------------------------------------------------------
--      Smart constructors to construct SrcExpr
--
-- Warning: these functions are helpful to avoid clutter
--          but they may implicitly be doing rewrites.
--          Be very sure that these rewrites are correct!
--------------------------------------------------------

identX :: Ident
identX = Ident noLoc "x"

srcUnderscore :: Ident
srcUnderscore = Ident noLoc "_"

isSrcUnderscore :: Ident -> Bool
isSrcUnderscore (Ident _ s) = s == "_"

{- Note [Treatment of underscore in SrcExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Underscore is:
* parser:   parses "_" as an identifier (Ident "_")
* sDesugar: passes it through
* mDesugar: converts it to (exists x.x)
* Used in thunks (\_.e); see eThunk
-}

existsXX :: SrcExpr
-- Returns (exists x. x)
-- This is what the source-code "_" desugars to
existsXX = Exists [identX] (Variable identX)

eUnify :: SrcExpr -> SrcExpr -> SrcExpr
-- Smart constructors just floats things out of the arms
eUnify (Seq e1 e2) e3              = mkSeq e1 (eUnify e2 e3)
eUnify e1 (Seq e2 e3) | isValue e1 = mkSeq e2 (eUnify e1 e3)
eUnify e1 e2                       = Unify e1 e2

eSeq :: [SrcExpr] -> SrcExpr
eSeq [] = eUnit
eSeq es = foldr1 mkSeq es

mkSeq :: SrcExpr -> SrcExpr -> SrcExpr
-- Smart constructor that tries to right-associate ";"
mkSeq (Seq e1 e2) e3 = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2
  | isValue e1       = e2
  | otherwise        = Seq e1 e2

eUnit, eFalse :: SrcExpr
eFalse = Array []
eUnit  = Array []

eAny :: SrcExpr
eAny = Variable (Ident noLoc "any")

eMkMap :: Loc -> SrcExpr
eMkMap l = Variable (Ident l "mkMap$")


eHavoc :: Eff -> SrcExpr
eHavoc (Eff { eff_card = c }) = havoc1 c
  where
    havoc1 CSucceeds = eUnit
    havoc1 CFails    = Fail
    havoc1 CDecides  = Unify eSomeAny eUnit
    havoc1 CIterates = error "eHavoc:iterates"

eThunk :: SrcExpr -> SrcExpr
-- Delay `e` by wrapping it in a lambda (\_.e)
eThunk e = Lam srcUnderscore e

eForce :: SrcExpr -> SrcExpr
-- Force a (\_.e) thunk, by applying it to <>
eForce e = ApplyD e eUnit

eForceLam :: SrcExpr
-- \t. t[]
eForceLam = Lam identX (ApplyD (Variable identX) eUnit)

eAll :: SrcExpr -> SrcExpr
eAll = All

eOne :: SrcExpr -> SrcExpr
eOne = One

eVerify :: [Ident] -> SrcExpr -> SrcExpr
eVerify = Verify

eSome :: SrcExpr -> SrcExpr
eSome = Some

eSomeAny :: SrcExpr
-- some(any), just a completely unconstrained skolem
eSomeAny = Some (Lam identX (Variable identX))

eGuard :: [Ident] -> SrcExpr -> SrcExpr
-- Smart constructor, drops empty guard
-- It's better to do a fold, to get  x ;; y ;; z ;; e
--    rather than <x,y,z> ;; e
--     because then we can drop the individual elements as they become konwn.
-- This reduces clutter when `x` is, say `int`, and we inline the lambda
eGuard xs orig_e = foldr gd orig_e xs
  where
   gd x e = Guard (Variable x) e

eCheck :: Eff -> SrcExpr -> SrcExpr
-- Smart constructor for (Check fxs e):
--  * Combines with nested Check
--  * Discards entirely when effects are "all effects",
--
-- But do NOT discard the Check for (Check fxs (\x.(e1){e2})), say, because
-- tha lambda generates a `verify` that must be inside a Check
eCheck fxs1 (Check fxs2 e)  = eCheck (fxs1 `intersectEffects` fxs2) e
eCheck fxs e | isTopEff fxs = e
             | otherwise    = Check fxs e

eExists :: [Ident] -> SrcExpr -> SrcExpr
-- Smart constructor, drops empty list of binders
eExists [] e = e
eExists is e = Exists is e

eApplyD :: SrcExpr -> SrcExpr -> SrcExpr
-- (eApply f x)  returns  f[x]
eApplyD f x = ApplyD f x

-- Used to create the array of free variables passed from the domain to the range
-- of for/if.  If it's just a single variable, don't use an array.
fvArray :: [Ident] -> SrcExpr
fvArray [x] = Variable x
fvArray xs = Array (map Variable xs)

eFunction :: SrcExpr -> [EffString] -> SrcExpr -> SrcExpr
eFunction arg fxs body
  = Function aperture arg (toEff effSucceeds effs) body
        -- effSucceeds: default is <succeeds>
  where
    (ocs, effs) = partition is_open_or_closed fxs

    aperture
      | has_open, not has_closed     = Open
      | has_closed, not has_open     = Closed
      | not has_open, not has_closed = defaultAperture
      | otherwise = error "A function can't be both open and closed"

    has_open   = any (== "open")   ocs
    has_closed = any (== "closed") ocs

    is_open_or_closed "open"   = True
    is_open_or_closed "closed" = True
    is_open_or_closed _        = False

--------------------------------------------------------
--               Op
--------------------------------------------------------

type Op = Ident
pattern Op :: String -> Op
pattern Op s <- Ident _ s
  where Op s = Ident noLoc s

--------------------------------------------------------
--               Loc
--------------------------------------------------------

type Loc = SourcePos
noLoc :: Loc
noLoc = initialPos ""

mkLoc :: String -> Int -> Int -> Loc
mkLoc f l c = SourcePos f (mkPos l) (mkPos c)

instance Pretty Loc where
  pPrintPrec _ _ = text . sourcePosPretty

--------------------------------------------------------
--               Ident
--------------------------------------------------------

data Ident = Ident Loc String
  deriving ({-Eq, Ord, Show,-} Data)

-- Ignore location for comparison
instance Eq Ident where x == y  =  compare x y == EQ
instance Ord Ident where compare (Ident _ x) (Ident _ y) = compare x y

instance Show Ident where
  show (Ident _ s) = show s

identString :: Ident -> String
identString (Ident _ s) = s

identLoc :: Ident -> Loc
identLoc (Ident l _) = l

instance Pretty Ident where
  pPrintPrec _ _ (Ident _ i) = text i


--------------------------------------------------------
--               Eff
--
-- Includes:
--   Cardinality: <succeeds>, <decides>, <fails>
--   Purity:      <computes>
--   Open/closed: <open>, <closed>
--------------------------------------------------------

type EffString = String

data Eff = Eff { eff_card :: CardEff
               , eff_side :: SideEff }
  deriving( Eq, Ord, Show, Data )

data CardEff  -- Cardinality effects
  = CFails     -- {0}
  | CSucceeds  -- {1}
  | CDecides   -- {0,1}
  | CIterates  -- {0,1,2,...}   Top of the lattice
  deriving( Eq, Ord, Data, Bounded, Enum )

data SideEff    -- Side effects
  = SComputes          -- Pure, no side effets, bottom of lattice
  | SAllocates | SReads | SWrites | SInteracts | STransacts
  | STop               -- Any side effect, top of the lattice
  deriving( Eq, Ord, Data, Bounded, Enum )

effTop, effSucceeds, effDecides, effFails :: Eff

effTop      = Eff { eff_card = CDecides,  eff_side = STop }
              -- <decides> is the "top" of the cardinality
              -- lattice for now; at least in DSTY2

effSucceeds = Eff { eff_card = CSucceeds, eff_side = STop }
effDecides  = Eff { eff_card = CDecides,  eff_side = STop }
effFails    = Eff { eff_card = CFails,    eff_side = STop }

isTopEff :: Eff -> Bool
isTopEff eff = eff == effTop

instance Show CardEff where
  show CIterates  = "iterates"
  show CSucceeds  = "succeeds"
  show CDecides   = "decides"
  show CFails     = "fails"

instance Show SideEff where
  show SAllocates = "allocates"
  show SReads     = "reads"
  show SWrites    = "writes"
  show SInteracts = "interacts"
  show STransacts = "transacts"
  show SComputes  = "computes"
  show STop       = "top"

instance Pretty Eff where
  pPrintPrec _ _ (Eff ceff seff) = pPrint ceff <> pPrint seff

instance Pretty CardEff where
  pPrintPrec _ _ CIterates = empty   -- Suppress <iterates>
  pPrintPrec _ _ eff       = angleBrackets (text (show eff))

instance Pretty SideEff where
  pPrintPrec _ _ STop = empty        -- Suppress <top>
  pPrintPrec _ _ eff  = angleBrackets (text (show eff))

toEff :: Eff          -- Default effect: use this if the user specifies no explicit effects
      -> [EffString]  -- What the user specified
      -> Eff
toEff (Eff {eff_card = default_card, eff_side = default_side }) effs
  = Eff { eff_card = get_card effs, eff_side = get_side effs }
  where
    get_card :: [EffString] -> CardEff
    get_card fxs = case get fxs of
                      []   -> default_card
                      [ce] -> ce
                      _    -> error ("toEff1: " ++ (show fxs))
    get_side :: [EffString] -> SideEff
    get_side fxs = case get fxs of
                      []   -> default_side
                      [se] -> se
                      _    -> error ("toEff2: " ++ (show fxs))

    get :: (Enum a, Bounded a, Show a) => [EffString] -> [a]
    get fxs = [ e | e <- [minBound..maxBound]
                   , fx <- fxs
                   , fx == show e ]

intersectEffects :: Eff -> Eff -> Eff
intersectEffects (Eff { eff_card = c1, eff_side = s1 })
                 (Eff { eff_card = c2, eff_side = s2 })
   = Eff { eff_card = c1 `intersectCard` c2
         , eff_side = s1 `intersectSide` s2 }

intersectSide :: SideEff -> SideEff -> SideEff
intersectSide STop s = s
intersectSide s STop = s
intersectSide s1 s2 | s1 == s2 = s1
intersectSide s1 s2 = error ("intersectSide incomplete:" ++ show s1 ++ " " ++ show s2)

intersectCard :: CardEff -> CardEff -> CardEff
intersectCard CIterates CFails    = CFails
intersectCard CIterates CDecides  = CDecides
intersectCard CIterates CSucceeds = CSucceeds
intersectCard CIterates CIterates = CIterates

intersectCard CDecides  CFails    = error "intersectCard-1"
intersectCard CDecides  CDecides  = CDecides
intersectCard CDecides  CIterates = CDecides
intersectCard CDecides  CSucceeds = CSucceeds

intersectCard CSucceeds CFails    = error "intersectCard-2"
intersectCard CSucceeds CSucceeds = CSucceeds
intersectCard CSucceeds CDecides  = CSucceeds
intersectCard CSucceeds CIterates = CSucceeds

intersectCard CFails    CFails    = CFails
intersectCard CFails    CDecides  = CFails
intersectCard CFails    CIterates = CFails
intersectCard CFails    CSucceeds = error "intersectCard-3"

--------------------------------------------------------
--               Store
--------------------------------------------------------

data Store = Store { refMap :: IM.IntMap SrcValue
                   , outputs :: [SrcCore] }
  deriving (Show, Eq, Ord, Data)

--------------------------------------------------------
--               Pretty printing
--------------------------------------------------------

instance Pretty Aperture where
  pPrintPrec _ _ q = angleBrackets $
                     case q of
                        Open   -> char 'o'
                        Closed -> char 'c'

instance Pretty SrcExpr where
  pPrintPrec lvl p
    | lvl > prettyNormal = ppNormal
    | otherwise          = ppNice
    where
      -- Pretty-print the argument of a call f[a] or f(a)
      --   A user call f[]    <-->  ApplyD f (Array [])
      --   A user call f[a]   <-->  ApplyD f (a)
      --   A user call f[a,b] <-->  ApplyD f (Array [a,b])
      -- Hence the special case for length es /= 1
      ppArg (Array es) | length es /= 1 = ppEs es
      ppArg e                           = ppr 0 e

      ppB (Blk es) = braces $ ppSeq lvl es
      ppB e        = braces $ ppr 0 e

      ppEs = fsep . punctuate comma . map (pPrintPrec lvl 1)

      ppr :: (Pretty a) => Rational -> a -> Doc
      ppr = pPrintPrec lvl

      ppOp = ppr 0

      ppNice expr =
        case expr of
          Array es | length es /= 1 -> parens $ ppEs es
--          Define i (Range t) -> ppNice $ InfixOp (Variable i) (Ident noLoc ":") t
          _ -> ppNormal expr

      ppIdent v -- Don't hide operator for now.
                --x | l == prettyNormal, Just r <- stripPrefix "operator'" (unIdent v) = text (init r)
                | otherwise = ppr 0 v

      ppNormal expr =
        case expr of
          Lit lit    -> ppr p lit
          Variable v -> ppIdent v
          EPrim s    -> pPrint s
          QualVariable e v -> parens (ppr 0 e <> text ":") <> ppr 0 v
          Array es   -> text "array" <> braces (ppSeq lvl es)
          Splice e   -> text "splice" <> braces (ppr 0 e)
          Tuple es   -> parens (ppEs es)
          Seq e1 e2  -> maybeParens (p > 0) $ ppSeq lvl (e1 : grab e2)
                     where  -- Flatten the list (e1; e2; e3; e4)
                        grab (Seq s1 s2) = s1 : grab s2
                        grab s           = [s]

          ApplyS  f a -> maybeParens (p > q) $ ppr ql f <> parens (ppArg a)
            where (q, ql, _) = fixity "()"
          ApplyD f a -> maybeParens (p > q) $ ppr ql f <> brackets (ppArg a)
            where (q, ql, _) = fixity "()"

          PrefixOp o e -> maybeParens (p > q) $ ppOp o <> ppr qr e
            where (q, _, qr) = fixity ("pre" ++ identString o)
          PostfixOp e o -> maybeParens (p > q) $ ppr ql e <> ppOp o
            where (q, ql, _) = fixity ("post" ++ identString o)
          InfixOp e1 o e2 -> maybeParens (p > q) $ sep [ppr ql e1 <+> ppOp o, indent $ ppr qr e2]
            where (q, ql, qr) = fixity (identString o)

          EffAttr f a -> maybeParens (p > q) $ ppr ql f <> text "<" <> text a <> text ">"
            where (q, ql, _) = fixity "()"

          If1 e1 -> maybeParens (p > 0) $ text "if" <+> ppB e1
          If2 e1 e2 -> maybeParens (p > 0) $ sep [text "if" <+> parens (ppr 0 e1) <+> text "then",
                                                        indent $ ppr 0 e2]
          If2E e1 e2 -> maybeParens (p > 0) $ sep [text "if" <+> parens (ppr 0 e1) <+> text "else",
                                                        indent $ ppr 0 e2]
          If3 e1 e2 e3 -> maybeParens (p > 0) $ sep [text "if" <+> parens (ppr 0 e1) <+> text "then",
                                                        indent $ ppr 0 e2,
                                                      text "else",
                                                        indent $ ppr 0 e3]

          For1 e1 -> maybeParens (p > 0) $ text "for" <+> ppB e1
          For2 e1 e2 -> maybeParens (p > 0) $ sep [text "for" <+> parens (ppr 0 e1) <+> text "do",
                                                      indent $ ppr 0 e2]

          Let e1 e2 -> maybeParens (p > 0) $ sep [text "let" <+> parens (ppr 0 e1),
                                                   text "do",
                                                     indent $ ppr 0 e2]

          Block e1 -> maybeParens (p > 0) $ sep [text "block" <+> indent (ppr 0 e1)]

          Case1 bs ->
            maybeParens (p > 0) $ sep [ text "case", indent $ ppr 0 bs ]
          Case2 e bs ->
            maybeParens (p > 0) $ sep [ text "case" <+> parens (ppArg e) <+> text "of",
                                           indent $ ppr 0 bs ]
          Function {} -> maybeParens (p > 0) $
                         cat [ text "fun" <> hcat (map ppArs args)
                             , indent (ppB body) ]
                where
                  -- Print fun(x:int)(y:int){body} rather than
                  --       fun(x:int){fun(y:int){body}}
                  (args,body) = split_args [] expr
                  split_args acc (Function q a fxs b) = split_args ((q,a,fxs):acc) b
                  split_args acc b                    = (reverse acc, b)

                  ppArs (q, e, rs) = parens (ppArg e) <> pPrint q <> pPrint rs

          Blk es       -> braces $ ppSeq lvl es
          Option me    -> text "option" <> braces (maybe empty (ppr 0) me)
          Parens e     -> parens (ppr 0 e)
          Set e1 op e2 -> text "set" <+> ppr 0 (InfixOp e1 op e2)
          MVar i t e   -> ppVRA "var" i t e
          MRef i t e   -> ppVRA "ref" i t e
          MAlias i t e -> ppVRA "alias" i t e

          Macro1 (Ident _ m) rs e  -> cat [ text m <> ppEffs rs, indent (ppB e) ]
          Macro2 (Ident _ m) e1 e2 -> cat [text m <> parens (ppr 0 e1), indent (ppB e2)]
          Return e                 -> maybeParens (p>0) $ text "return" <+> ppr 2 e

          ----
          DefineV i      -> text "exists" <+> pPrint i
          DefineE i e    -> pPrintPrec lvl p (InfixOp (Variable i) (Ident noLoc ":=") e)
          DefineIE i e   -> pPrintPrec lvl p (InfixOp (Variable i) (Op "->") e)
          Choice e1 e2   -> pPrintPrec lvl p (InfixOp e1 (Op "|") e2)
          Unify e1 e2    -> pPrintPrec lvl p (InfixOp e1 (Op "=") e2)
          Fail           -> text "fail"
          Wrong s        -> text $ "WRONG'" ++ s ++ "'"

          Range e     -> ppNormal (PrefixOp (Ident noLoc ":") e)
          Exists is e -> maybeParens (p > 0) $ sep [text "exists" <+> hsep (map (ppr 0) is) <> text ".", ppr 0 e]
          Verify is e -> maybeParens (p > 0) $
                         cat [ text "verify" <> parens (hsep (map (ppr 0) is))
                             , indent (braces (ppr 0 e)) ]

          OfType e fx t -> maybeParens (p>0) $
                          sep [ (ppr 1 e)
                              , text "|>" <> pPrint fx <+> ppr 1 t ]

          Where e1 e2 -> maybeParens (p>0) $ sep [ ppr 0 e1, text "where" <+> ppr 0 e2 ]
          Some e      -> text "some" <> parens (ppr 0 e)
          One e       -> text "one" <> parens (ppr 0 e)
          All e       -> text "all" <> parens (ppr 0 e)
          Check fx e  -> cat [ text "check" <> pPrint fx
                             , indent (braces (ppr 0 e)) ]

          Guard g1 e -> maybeParens (p>0) $
                        sep [ fsep [ ppr 0 g <+> text ";;" | g <- all_gs ]
                            , ppr 0 etail ]
                     where
                        (all_gs, etail) = grab [g1] e
                        grab gs (Guard g e2) = grab (g:gs) e2
                        grab gs et           = (reverse gs, et)

          MVLam { mvl_fxs = fxs, mvl_i = i, mvl_dom = e1, mvl_wrap = wrap, mvl_rng = e2 }
            -> maybeParens (p > 0) $
               text "\\" <> ppr 0 fxs <> ppr 0 i <> text "."
                         <> sep [ parens (ppr 0 e1)
                                , parens (pp_wrap wrap) <> braces (ppr 0 e2) ]
            where
              pp_wrap NoWrap = empty
              pp_wrap (Wrap { wp_x = x, wp_h = h, wp_y = y })
                 = ppr 0 y <> text ":=" <> ppr 0 h <> brackets (ppr 0 x)

          Lam i e -> maybeParens (p > 0) $ text "\\" <> ppr 0 i <> text "." <+> ppr 0 e
          Map es -> text "map" <> braces (ppSeq lvl es)
          Truth e -> text "truth" <> braces (ppr 0 e)
          EStore s e ->
            maybeParens (p > 0) $ fsep [text "store"<+> pPrintPrec lvl p s <+> text "in", indent $ braces (pPrintPrec lvl 0 e)]

      ppVRA _ _ Nothing  Nothing  = undefined
      ppVRA s i (Just t) Nothing  = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc ":") t)
      ppVRA s i Nothing  (Just e) = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc "=") e)
      ppVRA s i (Just t) (Just e) = text s <+> ppr 0 (InfixOp (InfixOp (Variable i) (Ident noLoc ":") t) (Ident noLoc "=") e)

instance Pretty Store where
  pPrintPrec l _ (Store m _) = fsep . punctuate comma . map (pPrintPrec l 0) . IM.toList $ m -- XXX

ppEffs :: [EffString] -> Doc
ppEffs rs = mconcat (map (\ r -> text "<" <> text r <> text ">") rs)

ppSeq :: PrettyLevel -> [SrcExpr] -> Doc
ppSeq l es = sep $ punctuate (text ";") $
             map (pPrintPrec l 0) es

--------------------------------------------------------
--               Knowledge of fixity
--------------------------------------------------------

fixity :: String -> (Rational, Rational, Rational)
fixity op = fromMaybe (internalErrorMsg op) $ lookup op tbl
  where
    --                L    R
    inn s p = (s, (p, p+1, p+1))
    inl s p = (s, (p, p,   p+1))
    inr s p = (s, (p, p+1, p))
    tbl =
      [ --inn ","     1
        inn ":-"      (-1)
      , inn "pre.."   0
      , inn "where"   1
      , inr "=>"      2
      , inn ":="      3 -- XXX This is probably the wrong level
      , inn "+="      3
      , inn "-="      3
      , inn "*="      3
      , inn "/="      3
      , inn ".="      3
      , inl "="       3 -- XXX is this right
      , inl ">>"      3 -- XXX is this right
--      , inr "||"      4
      , inr "|||"      7
      , inr "or"      4
--      , inr "&&"      5
      , inr "and"     5
      , inr ":"       6
      , inr "<>"      6
      , inr "<="      6
      , inr ">="      6
      , inr "<"       6
      , inr ">"       6
      , inl "|"       7
      , inl ".."      7
      , inr "->"      7
      , inr "~>"      7
      , inl "+"       8
      , inl "-"       8
      , inl "*"       9
      , inl "/"       9
      , inn "post^"  10
      , inn "post?"  10
      , inn "."      10
      , inn "pre-"   11
      , inn "pre+"   11
      , inn "pre^"   11
      , inn "pre?"   11
      , inn "pre:"   11
--      , inn "pre!"   11
      , inn "prenot" 11
      , inn "pre[]"  11
      , inn "macro"  12
      , inl "()"     13
      , inl "&"      13
      ]

--------------------------------------------------------
--               Utility functions
--------------------------------------------------------

compos :: (Applicative m) => (SrcExpr -> m SrcExpr) -> SrcExpr -> m SrcExpr
-- (compose f e) applies f to the top-level SrcExpr children of the SrcExpr
compos _ e@Lit{}            = pure e
compos _ e@Variable{}       = pure e
compos f (QualVariable e v) = QualVariable <$> f e <*> pure v
compos f (Array es)         = Array <$> traverse f es
compos f (Tuple es)         = Tuple <$> traverse f es
compos f (Seq e1 e2)        = Seq    <$> f e1 <*> f e2
compos f (ApplyS e1 e2)     = ApplyS <$> f e1 <*> f e2
compos f (ApplyD e1 e2)     = ApplyD <$> f e1 <*> f e2
compos f (EffAttr e r)      = EffAttr <$> f e <*> pure r
compos f (PrefixOp op e)    = PrefixOp op <$> f e
compos f (PostfixOp e op)   = PostfixOp <$> f e <*> pure op
compos f (InfixOp e1 op e2) = InfixOp <$> f e1 <*> pure op <*> f e2
compos f (If1 b)            = If1 <$> f b
compos f (If2 e b)          = If2 <$> f e <*> f b
compos f (If2E e b)         = If2E <$> f e <*> f b
compos f (If3 e b1 b2)      = If3 <$> f e <*> f b1 <*> f b2
compos f (For1 b)           = For1 <$> f b
compos f (For2 e b)         = For2 <$> f e <*> f b
compos f (Let e b)          = Let <$> f e <*> f b
compos f (Block b)          = Block <$> f b
compos f (Case1 b)          = Case1 <$> f b
compos f (Case2 e b)        = Case2 <$> f e <*> f b
compos f (Function q e fx b)= Function q <$> f e <*> pure fx <*> f b
compos f (Blk es)           = Blk <$> traverse f es
compos f (Option me)        = Option <$> traverse f me
compos f (Parens e)         = Parens <$> f e
compos f (Set e1 op e2)     = Set <$> f e1 <*> pure op <*> f e2
compos f (MVar i e1 e2)     = MVar i <$> traverse f e1 <*> traverse f e2
compos f (MRef i e1 e2)     = MVar i <$> traverse f e1 <*> traverse f e2
compos f (MAlias i e1 e2)   = MVar i <$> traverse f e1 <*> traverse f e2
compos f (Macro1 m as b)    = Macro1 m as <$> f b
compos f (Macro2 m a b)     = Macro2 m <$> f a <*> f b
compos f (Return e)         = Return <$> f e
compos _ (DefineV i)        = pure $ DefineV i
compos f (DefineE i e)      = DefineE i <$> f e
compos f (DefineIE i e)     = DefineIE i <$> f e
compos f (Choice e1 e2)     = Choice <$> f e1 <*> f e2
compos f (Unify e1 e2)      = Unify <$> f e1 <*> f e2
compos f (Range e)          = Range <$> f e
compos _ e@Wrong{}          = pure e
compos f (Exists is e)      = Exists is <$> f e
compos f (Where e1 e2)      = Where <$> f e1 <*> f e2
compos f (Guard e1 e2)      = Guard <$> f e1 <*> f e2
compos f (Check fx e)       = Check <$> pure fx <*> f e
compos f (Some e)           = Some <$> f e
compos f (One e)            = One <$> f e
compos f (All e)            = All <$> f e
compos f (Verify is e)      = Verify is <$> f e
compos f (OfType e1 fx e2)  = OfType <$> f e1 <*> pure fx <*> f e2
compos _ e@EPrim{}          = pure e
compos f (Lam i e)          = Lam i <$> f e
compos f e@(MVLam { mvl_dom = e1, mvl_rng = e2 })
                            = (\e1' e2' -> e { mvl_dom = e1', mvl_rng = e2' })
                              <$> f e1 <*> f e2
compos _ e@Fail             = pure e
compos f (Map es)           = Map <$> traverse f es
compos f (Truth e)          = Truth <$> f e
compos f (Splice e)         = Splice <$> f e
compos f (EStore s e)       = EStore <$> storeMapA f s <*> f e

storeMapA :: (Applicative a) => (SrcValue -> a SrcValue) -> Store -> a Store
storeMapA f s = Store <$> sequenceA (IM.map f (refMap s)) <*> pure (outputs s)

composOp :: (SrcExpr -> SrcExpr) -> SrcExpr -> SrcExpr
composOp f = runIdentity . compos (pure . f)

-- XXX fix this
getLoc :: SrcExpr -> Loc
getLoc _ = noLoc

isConst :: SrcExpr -> Bool
isConst Lit{}   = True
isConst EPrim{} = True
isConst _       = False

-- Values, except lambda
isValue :: SrcExpr -> Bool
isValue Variable{} = True
isValue (Lam {})   = True
isValue (MVLam {}) = True
isValue (Array es) = all isValue es
isValue (Truth e)  = isValue e
isValue e          = isConst e

isAtomic :: SrcExpr -> Bool
-- True of small expressions
isAtomic (Variable {}) = True
isAtomic e             = isConst e




---------------------------------------------------------
-- Functions that only work on the core subset of SrcExpr
---------------------------------------------------------

-- Get all visible binders from i := e
-- By "visible" we mean not nested inside another scope.
-- E.g.   getVisible ( x:=3; (y:=4 | 5);
--        returns [x,y], but not z.
getVisibleBinders :: HasCallStack => SrcExpr -> [Ident]
getVisibleBinders = go
  where
    -- These three equations are the main payload
    go (DefineV i)     = [i]
    go (DefineE i e)   = i : go e
    go (Exists is e)   = go e \\ is

    -- The rest is just recursive traversal
    go Lit{}          = []
    go EPrim{}        = []
    go Variable{}     = []
    go (Array es)     = concatMap go es
    go (Tuple es)     = concatMap go es
    go (Seq e1 e2)    = go e1 ++ go e2
    go (ApplyS e1 e2) = go e1 ++ go e2
    go (ApplyD e1 e2) = go e1 ++ go e2
    go (Unify e1 e2)  = go e1 ++ go e2
    go (Range e)      = go e
    go (Guard e1 _)   = go e1
    go (Truth e)      = go e

    go (If3 {})   = []  -- NB: Variables defined in scrutinee are not visible outside the 'if'
                        --     So this would be wrong: go (If3 e _ _) = go e
    go For2{}     = []
    go Block{}    = []
    go Let{}      = []  -- nothing visible from a let
    go Choice{}   = []
    go Function{} = []
    go MVLam{}    = []
    go Check {}   = []  -- check<fx>{ e } is a new scope
    go Some{}     = []  -- Ditto some(e), one{e}, all{e}
    go One{}      = []
    go All{}      = []
    go Verify {}  = []  -- verify is a new scope
    go OfType{}   = []
    go Lam{}      = []
    go Fail       = []
    go Macro1 {}  = []

    --go (Map es)      = concatMap go es
    go e = impossible "getVisibleBinders" e

getFree :: HasCallStack => SrcExpr -> [Ident]
getFree = fvs_blk
  where
    fvs_blk e = fvs e `remove` getVisibleBinders e

    fvs :: HasCallStack => SrcExpr -> [Ident]
    fvs (Variable i)      = [i]
    fvs (Lit _)           = []
    fvs (EPrim _)         = []
    fvs Fail              = []
    fvs (Wrong {})        = []
    fvs (Array es)        = concatMap fvs es
    fvs (Truth e)         = fvs e
    fvs (Tuple es)        = concatMap fvs es
    fvs (EffAttr e _)     = fvs e
    fvs (PrefixOp _ e)    = fvs e
    fvs (PostfixOp e _)   = fvs e
    fvs (InfixOp e1 _ e2) = fvs e1 ++ fvs e2
    fvs (Lam i e)         = fvs_blk e `remove` [i]
    fvs (ApplyD e1 e2)    = fvs e1 ++ fvs e2
    fvs (Unify e1 e2)     = fvs e1 ++ fvs e2
    fvs (Choice b1 b2)    = fvs_blk b1 ++ fvs_blk b2
    fvs (Seq e1 e2)       = fvs e1 ++ fvs e2
    fvs (Exists is e)     = fvs e `remove` is
    fvs (Verify is e)     = fvs e `remove` is
    fvs (Macro1 _ _ e)    = fvs e
    fvs (Macro2 _ e b)    = fvs e ++ fvs_blk b
    fvs (For2 e1 e2)      = (fvs e1 ++ fvs e2)
                            `remove` getVisibleBinders e1
    fvs (DefineE _ e)     = fvs e
    fvs (DefineV {})      = []
    fvs (Range e)         = fvs_blk e
    fvs (Check _ e)       = fvs_blk e
    fvs (Some e)          = fvs_blk e
    fvs (One e)           = fvs_blk e
    fvs (All e)           = fvs_blk e
    fvs (Guard e1 e2)     = fvs e1 ++ fvs_blk e2

    -- In (if e1 then e2 else e3), the binders of e1 scope over e2
    fvs (If3 e1 e2 e3)    = (fvs e1 ++ fvs_blk e2) `remove` bs
                            ++ fvs_blk e3
                          where
                            bs = getVisibleBinders e1

    fvs (Function _ arg _ body)
      = (fvs arg ++ fvs_blk body) `remove` getVisibleBinders arg

    fvs (MVLam { mvl_i = i, mvl_wrap = wrap, mvl_dom = e1, mvl_rng = e2 })
      = (fvs e1 ++ fvs_wrap wrap (fvs_blk e2)) `remove` bndrs
      where
        bndrs = i : getVisibleBinders e1
        fvs_wrap NoWrap                        fvs2 = fvs2
        fvs_wrap (Wrap { wp_h = h, wp_y = y }) fvs2 = h : (fvs2 `remove` [y])

    fvs e = impossible "getFree" e

    remove xs bndrs = filter (`notElem` bndrs) (nub xs)

getVar :: HasCallStack => SrcExpr -> [Ident]
-- Get mutable variables
getVar Lit{}            = []
getVar Variable{}       = []
getVar (Array es)       = concatMap getVar es
getVar (Seq e1 e2)      = getVar e1 ++ getVar e2
getVar (ApplyS e1 e2)   = getVar e1 ++ getVar e2
getVar (ApplyD e1 e2)   = getVar e1 ++ getVar e2
getVar (If3 e _ _)      = getVar e
getVar For2{}           = []
getVar (Let _ e)        = getVar e
getVar Block{}          = []
getVar (Unify e1 e2)    = getVar e1 ++ getVar e2
getVar Macro1 {}        = []
getVar (DefineV _)      = []
getVar (DefineE _ e)    = getVar e
getVar (DefineIE _ e)   = getVar e
getVar Choice{}         = []
getVar (Set _ _ e)      = getVar e
getVar (MVar i t e)     = i : maybe [] getVar t ++ maybe [] getVar e
getVar (Range e)        = getVar e
getVar Function{}       = []
getVar (Exists _ e)     = getVar e
getVar (Verify _ e)     = getVar e
getVar (OfType e _ t)   = getVar e ++ getVar t
getVar Lam{}            = []
getVar Fail             = []
getVar EPrim{}          = []
getVar e                = impossible "getVar" e


getAllIdents :: SrcExpr -> [Ident]
-- Find all occurrences, ignoring binders (hence hacky)
getAllIdents orig_e = Epic.List.nub (execWriter (vars orig_e))
  where
    vars :: SrcExpr -> Writer [Ident] SrcExpr
    vars ev@(Variable i)       = do { tell [i]; pure ev }
    vars ev@(PrefixOp op e)    = do { tell [opName "prefix"   op]; _ <- vars e; pure ev }
    vars ev@(PostfixOp e op)   = do { tell [opName "postfix"  op]; _ <- vars e; pure ev }
    vars ev@(InfixOp e1 op e2) = do { tell [opName "operator" op]; _ <- vars e1; _ <- vars e2; pure ev }
    vars ev                    = compos vars ev
    opName p (Ident l s) = Ident l (p ++ "'" ++ s ++ "'")

getAllBinders :: SrcCore -> [Ident]
-- Finds all binders in e
getAllBinders expr = Epic.List.nub (execWriter (vars expr))
  where
    vars :: SrcCore -> Writer [Ident] SrcCore
    vars e@(Variable i)   = do tell [i]; pure e
    vars e@(Lam i e')     = do tell [i]; _ <- vars e'; pure e
    vars e@(Exists is e') = do tell is; _ <- vars e'; pure e
    vars e@(Verify is e') = do tell is; _ <- vars e'; pure e
    vars e                = compos vars e
