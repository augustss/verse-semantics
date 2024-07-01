{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FrontEnd.Expr(
  Loc, noLoc,
  Ident(..), unIdent,
  SrcExpr(..),
  Lit(..),
  Path(..),
  SrcCore, SrcBlk, SrcValue,
  pattern Typedef, pattern Check, pattern Guard, pattern Some, 
--  pattern Range,
  Store(..), Ptr,
  Eff, effSucceeds, effDecides, effFails, isOpenClosed,
  Op,
  pattern Op,
  compos, composOp, unSeq

  seqE,
  getLoc,
  isLiteral, isAtomic, isValue,

  getFree, getAllIdents, getVisibleBinders, getAllBinders, getVar,
  substMany, closed
  ) where

import Prelude hiding ((<>))  -- Epic.Print exports (<>)

import FrontEnd.Error
import Epic.Print
import Epic.List

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Data (Data)
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Scientific(Scientific)

import GHC.Stack

import Text.Megaparsec (SourcePos, initialPos, sourcePosPretty)


{- Note [How SrcExpr is parsed]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

              This                 is represented as
-------------------------------------------------------------------------
Binding       p := e               InfixOp p ":=" e
Binding       f(p)<fx> := e        InfixOp (EffAttr (ApplyS f p) fx) ":=" e
Guard         v ;;<fx> e           Macro2 "guard" <fx> e

-}

--------------------------------------------------------
--
--         The main SrcExpr data type
--
--------------------------------------------------------

data SrcExpr
  = Lit Lit                   -- k
  | Variable Ident            -- x
  | QualVariable SrcExpr Ident   -- (e:)x
  | Array [SrcExpr]              -- array{e1;e2;...}
  | Tuple [SrcExpr]              -- e1,e2,...             -- will be turned into Array
  | ApplyS SrcExpr SrcExpr       -- f(e)
  | ApplyD SrcExpr SrcExpr       -- f[e]
  | EffAttr SrcExpr Eff          -- f<e>

  -- Prefix and infix operator application
  | PrefixOp Op SrcExpr          -- op e
  | PostfixOp SrcExpr Op         -- e op
  | InfixOp SrcExpr Op SrcExpr   -- e1 op e2

  | If1 SrcBlk                   -- if{e}
  | If2 SrcExpr SrcBlk           -- if(e1) then e2
  | If2E SrcExpr SrcBlk          -- if(e1) else e2
  | If3 SrcExpr SrcBlk SrcBlk          -- if(e1) then e2 else e3
  | If3B [Ident] SrcExpr SrcBlk SrcBlk -- if(exists is . e1) then e2 else e3
                                       --  where 'is' are the identifiers bound by e1

  | For1 SrcBlk                    -- for{e}
  | For2 SrcExpr SrcBlk            -- for(e1) in e2
  | For2B [Ident] SrcExpr SrcBlk   -- for(exists is . e1) in e2

  | Let SrcExpr SrcBlk             -- let(e1) in e2
  | Block SrcBlk                   -- do e
  | Case1 SrcBlk                   -- case{e1; e2; ... } block treated in a non-standard way
  | Case2 SrcExpr SrcBlk           -- case(e) of {e1; e2; ... } block treated in a non-standard way

  | Function [(SrcExpr, [Eff])] SrcBlk -- function(e)<eff>...{e}

  | Blk [SrcExpr]                -- { e1; e2; ... }
  | Option (Maybe SrcExpr)       -- option{e}
  | Parens SrcExpr               -- (e)

  -- Mutable variables
  | Set SrcExpr Ident SrcExpr       -- set e1 = e2
  | MVar Ident (Maybe SrcExpr) (Maybe SrcExpr)      -- var i : t = e
  | MRef Ident (Maybe SrcExpr) (Maybe SrcExpr)      -- ref i : t = e
  | MAlias Ident (Maybe SrcExpr) (Maybe SrcExpr)    -- alias i : t = e

  -- Some 1-argument macros
  | Macro1 Ident [Eff] SrcBlk    -- m<a>{e}
  | Macro2 Ident SrcExpr SrcBlk  -- m(e1){e2}
  | Return SrcExpr               -- return e

  -- Initial desugaring turns some operators into more easily recognizable forms
  | Seq [SrcExpr]                -- e1;e2;...
  | DefineV Ident                -- i:any
  | DefineE Ident SrcExpr        -- i := e
  | DefineIE Ident Ident SrcExpr -- (i->x) := e
  | Choice SrcBlk SrcBlk         -- e1 | e2
  | Unify SrcExpr SrcExpr        -- e1 = e2
  | Range [Eff] SrcExpr          -- :{fx}e

  -- Below here, not source language
  | Wrong String              -- wrong
  | Exists [Ident] SrcExpr    -- exists xs . e

  | Verify [Ident] SrcExpr    -- verify fs . e
                              --  (we only need assumptions when we get to the core language)

  | OfType SrcExpr [Eff] SrcExpr    -- e |>{fx} t

  | EPrim String                   -- Primop
  | Lam Ident SrcExpr              -- ICFP lambda:   \ x . e
  | Split SrcExpr SrcExpr SrcExpr  -- split(e1){e2}{e3}
  | Fail                           -- :false
  | Map [SrcExpr]                  -- map{e1;e2; ... }
  | Truth SrcExpr                  -- truth{e}

  -- These are used when translating back from Rules.Core.SrcExpr
  | EStore Store SrcExpr

  deriving (Eq, Ord, Show, Data)

-- SrcCore synonym is used for the very reduced subset of SrcExpr
-- that can be directly translated to Rules.Core.Expr
type SrcCore  = SrcExpr

type SrcValue = SrcExpr
type SrcBlk   = SrcExpr

--------------------------------------------------------
--               Pattern synoyms for SrcExpr
--------------------------------------------------------

-- type{e}
pattern Typedef :: SrcBlk -> SrcExpr
pattern Typedef e <- Macro1 (Ident _ "type") [] e
  where Typedef e = Macro1 (Ident noLoc "type") [] e

-- check<fx>{e}
pattern Check :: [Eff] -> SrcExpr -> SrcExpr
pattern Check fx e <- Macro1 (Ident _ "check") fx e
  where Check fx e = Macro1 (Ident noLoc "check") fx e

-- some{e}
pattern Some :: SrcExpr -> SrcExpr
pattern Some e <- Macro1 (Ident _ "some") [] e
  where Some e = Macro1 (Ident noLoc "some") [] e

-- one{e}
pattern One :: SrcExpr -> SrcExpr
pattern One e <- Macro1 (Ident _ "one") [] e
  where One e = Macro1 (Ident noLoc "one") [] e

-- all{e}
pattern One :: SrcExpr -> SrcExpr
pattern One e <- Macro1 (Ident _ "all") [] e
  where One e = Macro1 (Ident noLoc "all") [] e

-- guard(v){e}
pattern Guard :: SrcExpr -> SrcExpr -> SrcExpr
pattern Guard v e <- Macro2 (Ident _ "guard") v e
  where Guard v e = Macro2 (Ident noLoc "guard") v e


--------------------------------------------------------
--            Decomposing SrcExpr
--------------------------------------------------------

unSeq :: [SrcExpr] -> ([SrcExpr], SrcExpr)
-- Extracts the last expression of Seq
unSeq = go []
  where
    go acc []     = (reverse acc, Array [])
    go acc [t]    = (reverse acc, t)
    go acc (t:ts) = go (t:acc) ts


--------------------------------------------------------
--               Lit
--------------------------------------------------------

data Lit
  = LitInt Integer            -- d
  | LitRat Scientific String  -- d.d
  | LitChar Char              -- 'c'
  | LitStr String             -- "str"
  | LitPath Path              -- /path/to/something
  | LitPtr Ptr                -- not a textual literal, just used when translating back.
  deriving (Eq, Ord, Show, Data)

instance Pretty Lit where
  pPrintPrec l p lit =
    case lit of
      LitInt i
        | i >= 0 -> text $ show i
        | otherwise -> maybeParens (p >= 10) $ text $ show i
      LitRat r s -> text (show r ++ s)
      LitChar c -> text (show c)
      LitStr s -> text (show s)
      LitPath s -> pPrintPrec l p s
      LitPtr ptr -> text ("R#" ++ show ptr)

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

unIdent :: Ident -> String
unIdent (Ident _ s) = s

instance Pretty Ident where
  pPrintPrec _ _ (Ident _ i) = text i


--------------------------------------------------------
--               Eff
--------------------------------------------------------

type Eff = Ident

effSucceeds, effDecides, effFails :: Eff
effSucceeds = Ident noLoc "succeeds"
effDecides  = Ident noLoc "decides"
effFails    = Ident noLoc "fails"

isOpenClosed :: Eff -> Bool
isOpenClosed (Ident _ "open")   = True
isOpenClosed (Ident _ "closed") = True
isOpenClosed _                  = False

--------------------------------------------------------
--               Store
--------------------------------------------------------

data Store = Store { refMap :: IM.IntMap SrcValue
                   , outputs :: [SrcCore] }
  deriving (Show, Eq, Ord, Data)

type Ptr = Int

--------------------------------------------------------
--               Path
--------------------------------------------------------

newtype Path = Path String
  deriving (Eq, Ord, Show, Data)

instance Pretty Path where
  pPrintPrec _ _ (Path s) = text s


--------------------------------------------------------
--               Pretty printing
--------------------------------------------------------

instance Pretty SrcExpr where
  pPrintPrec l p
    | l > prettyNormal = ppNormal
    | otherwise = ppNice
    where
      ppA (Array es) = ppEs es
      ppA e          = ppr 0 e

      ppB (Blk es) = braces $ ppSeq l es
      ppB e        = braces $ ppr 0 e

      ppEs = fsep . punctuate comma . map (pPrintPrec l 1)

      ppr :: (Pretty a) => Rational -> a -> Doc
      ppr = pPrintPrec l

      ppOp = ppr 0

      ppNice expr =
        case expr of
          Array es | length es /= 1 -> parens $ ppEs es
--          Define i (Range t) -> ppNice $ InfixOp (Variable i) (Ident noLoc ":") t
          _ -> ppNormal expr

      ppNormal expr =
        case expr of
          Lit lit    -> ppr p lit
          Variable v -> ppr 0 v
          EPrim s    -> char '!' <> ppNormal (Variable (Ident noLoc s))
          QualVariable e v -> parens (ppr 0 e <> text ":") <> ppr 0 v
          Array es   -> text "array" <> braces (ppSeq l es)
          Tuple es   -> parens (ppEs es)
          Seq es     -> maybeParens (p > 0) $ ppSeq l es

          ApplyS  f a -> maybeParens (p > q) $ ppr ql f <> parens (ppA a)
            where (q, ql, _) = fixity "()"
          ApplyD f a -> maybeParens (p > q) $ ppr ql f <> brackets (ppA a)
            where (q, ql, _) = fixity "()"

          PrefixOp o e -> maybeParens (p > q) $ ppOp o <> ppr qr e
            where (q, _, qr) = fixity ("pre" ++ unIdent o)
          PostfixOp e o -> maybeParens (p > q) $ ppr ql e <> ppOp o
            where (q, ql, _) = fixity ("post" ++ unIdent o)
          InfixOp e1 o e2 -> maybeParens (p > q) $ sep [ppr ql e1 <+> ppOp o, indent $ ppr qr e2]
            where (q, ql, qr) = fixity (unIdent o)

          EffAttr f a -> maybeParens (p > q) $ ppr ql f <> text "<" <> ppr 0 a <> text ">"
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
          If3B is e1 e2 e3 -> ppNormal $ If3 (Exists is e1) e2 e3

          For1 e1 -> maybeParens (p > 0) $ text "for" <+> ppB e1
          For2 e1 e2 -> maybeParens (p > 0) $ sep [text "for" <+> parens (ppr 0 e1) <+> text "do",
                                                      indent $ ppr 0 e2]
          For2B is e1 e2 -> ppNormal $ For2 (Exists is e1) e2

          Let e1 e2 -> maybeParens (p > 0) $ sep [text "let" <+> parens (ppr 0 e1),
                                                   text "do",
                                                     indent $ ppr 0 e2]

          Block e1 -> maybeParens (p > 0) $ sep [text "block" <+> indent (ppr 0 e1)]

          Case1 bs ->
            maybeParens (p > 0) $ sep [ text "case", indent $ ppr 0 bs ]
          Case2 e bs ->
            maybeParens (p > 0) $ sep [ text "case" <+> parens (pPrintL l e) <+> text "of",
                                           indent $ ppr 0 bs ]
          Function ars b -> maybeParens (p > 0) $
                            cat [ text "fun" <> hcat (map ppArs ars)
                                , indent (ppB b) ]
                where ppArs (e, rs) = parens (pPrintL l e) <> ppEffs rs
--          Typedef e -> text "type" <> ppB e
          Blk es       -> braces $ ppSeq l es
          Option me    -> text "option" <> braces (maybe empty (ppr 0) me)
          Parens e     -> parens (ppr 0 e)
          Set e1 op e2 -> text "set" <+> ppr 0 (InfixOp e1 op e2)
          MVar i t e   -> ppVRA "var" i t e
          MRef i t e   -> ppVRA "ref" i t e
          MAlias i t e -> ppVRA "alias" i t e

          Macro1 (Ident _ m) rs e  -> cat [ text m <> ppEffs rs
                                          , indent (ppB e) ]
          Macro2 (Ident _ m) e1 e2 -> cat [text m <> parens (ppr 0 e1), indent (ppB e2)]

          Return e -> maybeParens (p>0) $ text "return" <+> ppr 2 e

          ----
          DefineV i -> pPrintPrec l p (InfixOp (Variable i) (Ident noLoc ":") (Variable (Ident noLoc "any")))
          DefineE i e -> pPrintPrec l p (InfixOp (Variable i) (Ident noLoc ":=") e)
          DefineIE i x e -> pPrintPrec l p (InfixOp (InfixOp (Variable i) (Op "->") (Variable x)) (Op ":=") e)
          Choice e1 e2 -> pPrintPrec l p (InfixOp e1 (Op "|") e2)
          Unify e1 e2 -> pPrintPrec l p (InfixOp e1 (Op "=") e2)
          Fail -> text "fail"
          Range fx e -> --pPrintPrec l p (PrefixOp (Ident noLoc ":") e)
                        text "range" <> ppEffs fx <> braces (ppr 0 e)
          Wrong s -> text $ "WRONG'" ++ s ++ "'"
          Exists is e -> maybeParens (p > 0) $ sep [text "exists" <+> hsep (map (ppr 0) is) <> text ".", ppr 0 e]
          Verify is e -> maybeParens (p > 0) $
                         cat [text "verify" <> parens (hsep (map (ppr 0) is))
                             , indent (braces (ppr 0 e)) ]

          OfType e fx t -> --ppNormal (InfixOp e (Op ":") t)
                           cat [ text "ofType" <> ppEffs fx <> parens (ppr 0 e)
                               , indent (braces (ppr 0 t)) ]

          Lam i e -> maybeParens (p > 0) $ text "\\" <> ppr 0 i <> text "." <+> ppr 0 e
          Split e1 e2 e3 -> text "split" <> sep [parens (ppr 0 e1), braces (ppr 0 e2), braces (ppr 0 e3)]
          Map es -> text "map" <> braces (ppSeq l es)
          Truth e -> text "truth" <> braces (ppr 0 e)
          EStore s e ->
            maybeParens (p > 0) $ fsep [text "store"<+> pPrintPrec l p s <+> text "in", indent $ braces (pPrintPrec l 0 e)]
      ppVRA _ _ Nothing  Nothing  = undefined
      ppVRA s i (Just t) Nothing  = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc ":") t)
      ppVRA s i Nothing  (Just e) = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc "=") e)
      ppVRA s i (Just t) (Just e) = text s <+> ppr 0 (InfixOp (InfixOp (Variable i) (Ident noLoc ":") t) (Ident noLoc "=") e)

instance Pretty Store where
  pPrintPrec l _ (Store m _) = fsep . punctuate comma . map (pPrintPrec l 0) . IM.toList $ m -- XXX

ppEffs :: [Eff] -> Doc
ppEffs rs = mconcat (map (\ r -> text "<" <> pPrint r <> text ">") rs)

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
compos f (Seq es)           = Seq <$> traverse f es
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
compos f (If3B is e b1 b2)  = If3B is <$> f e <*> f b1 <*> f b2
compos f (For1 b)           = For1 <$> f b
compos f (For2 e b)         = For2 <$> f e <*> f b
compos f (For2B is e b)     = For2B is <$> f e <*> f b
compos f (Let e b)          = Let <$> f e <*> f b
compos f (Block b)          = Block <$> f b
compos f (Case1 b)          = Case1 <$> f b
compos f (Case2 e b)        = Case2 <$> f e <*> f b
compos f (Function ers b)   = Function <$> traverse g ers <*> f b
  where g (e, r) = (,) <$> f e <*> pure r
compos f (Blk es)           = Blk <$> traverse f es
compos f (Option me)        = Option <$> traverse f me
--compos f (Typedef b) = Typedef <$> f b
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
compos f (DefineIE i x e)   = DefineIE i x <$> f e
compos f (Choice e1 e2)     = Choice <$> f e1 <*> f e2
compos f (Unify e1 e2)      = Unify <$> f e1 <*> f e2
compos f (Range fx e)       = Range <$> pure fx <*> f e
compos _ e@Wrong{}          = pure e
compos f (Exists is e)      = Exists is <$> f e
compos f (Verify is e)      = Verify is <$> f e
compos f (OfType e1 fx e2)  = OfType <$> f e1 <*> pure fx <*> f e2
compos _ e@EPrim{}          = pure e
compos f (Lam i e)          = Lam i <$> f e
compos f (Split e1 e2 e3)   = Split <$> f e1 <*> f e2 <*> f e3
compos _ e@Fail             = pure e
compos f (Map es)           = Map <$> traverse f es
compos f (Truth e)          = Truth <$> f e
compos f (EStore s e)       = EStore <$> storeMapA f s <*> f e

storeMapA :: (Applicative a) => (SrcValue -> a SrcValue) -> Store -> a Store
storeMapA f s = Store <$> sequenceA (IM.map f (refMap s)) <*> pure (outputs s)

composOp :: (SrcExpr -> SrcExpr) -> SrcExpr -> SrcExpr
composOp f = runIdentity . compos (pure . f)

seqE :: [SrcExpr] -> SrcExpr
seqE = mk . concatMap flat
  where flat (Seq es) = es
        flat e = [e]
        mk [e] = e
        mk es = Seq es

-- XXX fix this
getLoc :: SrcExpr -> Loc
getLoc _ = noLoc

isLiteral :: SrcExpr -> Bool
isLiteral Lit{} = True
isLiteral _ = False

-- Values, except lambda
isValue :: SrcExpr -> Bool
isValue Variable{} = True
isValue EPrim{} = True
isValue (Array es) = all isValue es
isValue e = isLiteral e

isAtomic :: SrcExpr -> Bool
-- True of small expressions
isAtomic (Variable {}) = True
isAtomic (Lit {})      = True
isAtomic (EPrim {})    = True
isAtomic _             = False



---------------------------------------------------------
-- Functions that only work on the core subset of SrcExpr
---------------------------------------------------------

-- Get all visible binders from i := e
-- By "visible" we mean not nested inside anoter scope.
-- E.g.   getVisible ( x:=3; (y:=4 | 5);
--        returns [x,y], but not z.
getVisibleBinders :: HasCallStack => SrcExpr -> [Ident]
getVisibleBinders = go
  where
    -- These two equations are the main payload
    go (DefineV i)     = [i]
    go (DefineE i e)   = i : go e

    -- The rest is just recursive traversal
    go Lit{}          = []
    go EPrim{}        = []
    go Variable{}     = []
    go (Seq es)       = concatMap go es
    go (Array es)     = concatMap go es
    go (Tuple es)     = concatMap go es
    go (ApplyS e1 e2) = go e1 ++ go e2
    go (ApplyD e1 e2) = go e1 ++ go e2
    go (Let _ e)      = go e   -- ToDo: why not first arg?
    go (Unify e1 e2)  = go e1 ++ go e2
    go (Range _fx e)   = go e
    --go (Typedef _)  = []

    go (If3 {})   = []  -- NB: Variables defined in scrutinee are not visible outside the 'if'
                        --     So this would be wrong: go (If3 e _ _) = go e
    go For2{}     = []
    go Block{}    = []
    go Choice{}   = []
    go Function{} = []
    go Exists {}  = []
    go Verify {}  = []  -- verify is a new scope
    go OfType{}   = []
    go Lam{}      = []
    go Fail       = []

    go (Macro1 (Ident _ "some") _ e)    = go e
    go (Macro2 (Ident _ "guard") e1 _b) = go e1
    go Macro1 {}                        = []

    --go (Map es)      = concatMap go es
    go e = impossible e

getFree :: SrcExpr -> [Ident]
getFree = fvs_blk
  where
    fvs_blk e = Epic.List.nub (fvs e) `remove` getVisibleBinders e

    fvs (Variable i)      = [i]
    fvs (Lit _)           = []
    fvs (EPrim _)         = []
    fvs Fail              = []
    fvs (Wrong {})        = []
    fvs (Array es)        = concatMap fvs es
    fvs (Tuple es)        = concatMap fvs es
    fvs (EffAttr e _)     = fvs e
    fvs (PrefixOp _ e)    = fvs e
    fvs (PostfixOp e _)   = fvs e
    fvs (InfixOp e1 _ e2) = fvs e1 ++ fvs e2
    fvs (Lam i e)         = fvs_blk e `remove` [i]
    fvs (ApplyD e1 e2)    = fvs e1 ++ fvs e2
    fvs (Unify e1 e2)     = fvs e1 ++ fvs e2
    fvs (Choice b1 b2)    = fvs_blk b1 ++ fvs_blk b2
    fvs (Seq es)          = concatMap fvs es
    fvs (Exists is e)     = fvs e `remove` is
    fvs (Verify is e)     = fvs e `remove` is
    fvs (Macro1 _ _ e)    = fvs e
    fvs (Macro2 _ e b)    = fvs e ++ fvs_blk b
    fvs (Split e1 e2 e3)  = fvs e1 ++ fvs e2 ++ fvs e3
    fvs (If3 (Exists is e1) e2 e3) = fvs (Exists is (Seq [e1, e2])) ++ fvs e3
--    fvs (Map es) = concatMap fvs es
    fvs e = impossible e

    remove xs bndrs = filter (`notElem` bndrs) xs

closed :: SrcCore -> Bool
closed = null . getFree


getVar :: HasCallStack => SrcExpr -> [Ident]
-- Get mutable variables
getVar Lit{} = []
getVar Variable{} = []
getVar (Array es) = concatMap getVar es
getVar (Seq es) = concatMap getVar es
getVar (ApplyS e1 e2) = getVar e1 ++ getVar e2
getVar (ApplyD e1 e2) = getVar e1 ++ getVar e2
-- getVar If3{} = []
getVar (If3 e _ _) = getVar e
getVar For2{} = []
getVar (Let _ e) = getVar e
getVar Block{} = []
getVar (Unify e1 e2) = getVar e1 ++ getVar e2
getVar Macro1 {} = []
getVar (DefineV _) = []
getVar (DefineE _ e) = getVar e
getVar (DefineIE _ _ e) = getVar e
getVar Choice{} = []
getVar (Set _ _ e) = getVar e
getVar (MVar i t e) = i : maybe [] getVar t ++ maybe [] getVar e
getVar (Range _fx e) = getVar e
getVar Function{} = []
getVar (Exists _ e)   = getVar e
getVar (Verify _ e)   = getVar e
getVar (OfType e _ t) = getVar e ++ getVar t
getVar Lam{} = []
getVar Fail = []
getVar EPrim{} = []
getVar e = impossible e


getAllIdents :: SrcExpr -> [Ident]
-- Find all occurrences, ignoring binders (hence hacky)
getAllIdents e = Epic.List.nub (execWriter (vars e))
  where
    vars :: SrcExpr -> Writer [Ident] SrcExpr
    vars ev@(Variable i) = do tell [i]; pure ev
    vars ev              = compos vars ev

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

---------------------------------------------------------
-- Functions that only work on the core subset of SrcExpr
---------------------------------------------------------

substMany :: [(Ident, SrcCore)] -> SrcCore -> SrcCore
substMany [] = id
substMany sb = sub
  where
    bs = getFree $ Seq $ map snd sb
    sub :: SrcCore -> SrcCore
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
    sub (Verify [] e) = Verify [] (sub e)
    sub (Verify (i:is) e) = binder i (forall1 i) (Verify is e)
    sub e@Wrong{} = e
    sub (Macro1 i rs e) = Macro1 i rs (sub e)
    sub (Split e1 e2 e3) = Split (sub e1) (sub e2) (sub e3)
    sub (If3 e1 e2 e3) = If3 (sub e1) (sub e2) (sub e3)
    sub (If3B is e1 e2 e3) =
      let (is', e1', e2') = if3Hack sub is e1 e2
      in  If3B is' e1' e2' (sub e3)
    sub Fail = Fail
    sub e = impossible e

    binder :: Ident -> (SrcExpr -> SrcExpr) -> SrcExpr -> SrcExpr
    binder i con e | Just _ <- lookup i sb = substMany (filter ((/= i) . fst) sb) (con e)
                   | i `notElem` bs = con (sub e)
                   | otherwise = sub $ alphaConvert bs (con e)

    exists1 i (Exists is e) = Exists (i:is) e
    exists1 _ _ = undefined

    forall1 i (Verify is e) = Verify (i:is) e
    forall1 _ _ = undefined

if3Hack :: (SrcExpr -> SrcExpr) -> [Ident] -> SrcExpr -> SrcExpr -> ([Ident], SrcExpr, SrcExpr)
if3Hack f is e1 e2 =
  case f (Exists is (Array [e1, e2])) of
    Exists is' (Array [e1', e2']) -> (is', e1', e2')
--    Array [e1', e2'] -> ([], e1', e2')
    e -> impossible e

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> SrcCore -> SrcCore
alphaConvert vs = alpha []
  where
    alpha :: [(Ident, Ident)] -> SrcCore -> SrcCore
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

