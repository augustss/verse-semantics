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
  SrcCore,
  SrcBlk,
  pattern Unit,
  pattern Typedef,
  pattern Succeeds,
  pattern Check,
--  pattern Range,
  Store(..), Ptr,
  Eff,
  Op,
  pattern Op,
  compos, composOp,
  seqE,
  getLoc,
  isLiteral,
  isValue,
  ) where

import Prelude hiding ((<>))  -- Epic.Print exports (<>)

import FrontEnd.Error
import Epic.Print

import Control.Monad.Identity
import Data.Data (Data)
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Scientific(Scientific)

import Text.Megaparsec (SourcePos, initialPos, sourcePosPretty)


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
  | ApplyS SrcExpr SrcExpr          -- f(e)
  | ApplyD SrcExpr SrcExpr          -- f[e]
  | EffAttr SrcExpr Eff          -- f<e>

  -- Prefix and infix operator application
  | PrefixOp Op SrcExpr          -- op e
  | PostfixOp SrcExpr Op         -- e op
  | InfixOp SrcExpr Op SrcExpr      -- e1 op e2

  | If1 SrcBlk                   -- if{e}
  | If2 SrcExpr SrcBlk              -- if(e1) then e2
  | If2E SrcExpr SrcBlk             -- if(e1) else e2
  | If3 SrcExpr SrcBlk SrcBlk          -- if(e1) then e2 else e3
  | If3B [Ident] SrcExpr SrcBlk SrcBlk -- if(exists is . e1) then e2 else e3
                              --  where 'is' are the identifiers bound by e1

  | For1 SrcBlk                  -- for{e}
  | For2 SrcExpr SrcBlk             -- for(e1) in e2
  | For2B [Ident] SrcExpr SrcBlk    -- for(exists is . e1) in e2

  | Let SrcExpr SrcBlk              -- let(e1) in e2
  | Block SrcBlk                 -- do e

  | Case1 SrcBlk                 -- case{e1; e2; ... } block treated in a non-standard way
  | Case2 SrcExpr SrcBlk            -- case(e) of {e1; e2; ... } block treated in a non-standard way
  | Function [(SrcExpr, [Eff])] SrcBlk -- function(e)<eff>...{e}
--  | Typedef SrcBlk             -- type{e}
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
  | Macro2 Ident SrcExpr SrcBlk     -- m(e1){e2}
  | Return SrcExpr               -- return e

  -- Initial desugaring turns some operators into more easily recognizable forms
  | Seq [SrcExpr]                -- e1;e2;...
  | DefineV Ident             -- i:any
  | DefineE Ident SrcExpr        -- i := e
  | DefineIE Ident Ident SrcExpr -- (i->x) := e
  | Choice SrcExpr SrcExpr          -- e | e
  | Unify SrcExpr SrcExpr           -- e1 = e2
  | Range SrcExpr                -- :e

  -- Below here, not source language
  | Wrong String              -- wrong
  | Exists [Ident] SrcExpr       -- exists xs . e
  | Forall [Ident] SrcExpr       -- forall xs . e
  | OfType SrcExpr SrcExpr          -- e:t, but only type known to verifier
  | TLam Ident [Eff] SrcExpr SrcExpr
                              -- function(x:any where e1)<eff>{e2}, e1 can make bindings visible in e2.
                              -- The last argument is a possible type, (e2:t)
  | DomainFail                -- either Wrong or try next overload
  | EPrim String              -- primop
  | Lam Ident SrcExpr            -- \ x . e
  | Split SrcExpr SrcExpr SrcExpr      -- split(e1){e2}{e3}
  | Fail                      -- :false
  | Map [SrcExpr]                -- map{e1;e2; ... }
  | Truth SrcExpr                -- truth{e}
  -- These are used when translating back from Rules.Core.SrcExpr
  | EStore Store SrcExpr
  deriving (Eq, Ord, Show, Data)

-- SrcCore synonym is used for the very reduced subset of SrcExpr that
-- can be directly translated to Rules.Core.Expr
type SrcCore  = SrcExpr
type SrcValue = SrcExpr
type SrcBlk   = SrcExpr

--------------------------------------------------------
--               Pattern synoyms for SrcExpr
--------------------------------------------------------

--pattern Range :: SrcExpr -> SrcExpr
--pattern Range e = ApplyD e AnyT
pattern Unit :: SrcExpr
pattern Unit = Array []
pattern Typedef :: SrcBlk -> SrcExpr
pattern Typedef e <- Macro1 (Ident _ "type") [] e
  where Typedef e = Macro1 (Ident noLoc "type") [] e
pattern Succeeds :: SrcBlk -> SrcExpr
pattern Succeeds e <- Macro1 (Ident _ "succeeds") [] e
  where Succeeds e = Macro1 (Ident noLoc "succeeds") [] e
pattern Check :: [Ident] -> SrcExpr -> SrcExpr
pattern Check ps e <- Macro1 (Ident _ "check") ps e
  where Check ps e = Macro1 (Ident noLoc "succeeds") ps e


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

type Eff = Ident

type Op = Ident
pattern Op :: String -> Op
pattern Op s <- Ident _ s
  where Op s = Ident noLoc s

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
      ppA e = ppr 0 e
      ppB (Blk es) = braces $ ppSeq l es
      ppB e = braces (ppr 0 e)
      ppEs = fsep . punctuate comma . map (pPrintPrec l 1)
      ppEffs rs = mconcat (map (\ r -> text "<" <> pPrintL l r <> text ">") rs)
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
          Lit lit -> ppr p lit
          Array es -> text "array" <> braces (ppSeq l es)
          Tuple es -> parens (ppEs es)
          Seq es -> maybeParens (p > 0) $ ppSeq l es
          Variable v -> ppr 0 v
          QualVariable e v -> parens (ppr 0 e <> text ":") <> ppr 0 v
          ApplyS  f a -> maybeParens (p > q) $ ppr ql f <> parens (ppA a)
            where (q, ql, _) = fixity "()"
          ApplyD f a -> maybeParens (p > q) $ ppr ql f <> brackets (ppA a)
            where (q, ql, _) = fixity "()"
          EffAttr f a -> maybeParens (p > q) $ ppr ql f <> text "<" <> ppr 0 a <> text ">"
            where (q, ql, _) = fixity "()"
          PrefixOp o e -> maybeParens (p > q) $ ppOp o <> ppr qr e
            where (q, _, qr) = fixity ("pre" ++ unIdent o)
          PostfixOp e o -> maybeParens (p > q) $ ppr ql e <> ppOp o
            where (q, ql, _) = fixity ("post" ++ unIdent o)
          InfixOp e1 o e2 -> maybeParens (p > q) $ sep [ppr ql e1 <+> ppOp o, indent $ ppr qr e2]
            where (q, ql, qr) = fixity (unIdent o)
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
          Function ars b -> maybeParens (p > 0) $ text "function" <> hcat (map ppArs ars) <> ppB b
            where ppArs (e, rs) = parens (pPrintL l e) <> ppEffs rs
          Blk es -> braces $ ppSeq l es
--          Typedef e -> text "type" <> ppB e
          Option me -> text "option" <> braces (maybe empty (ppr 0) me)
          Parens e -> parens (ppr 0 e)
          Set e1 op e2 -> text "set" <+> ppr 0 (InfixOp e1 op e2)
          MVar i t e -> ppVRA "var" i t e
          MRef i t e -> ppVRA "ref" i t e
          MAlias i t e -> ppVRA "alias" i t e
          Macro1 (Ident _ m) rs e -> text m <> ppEffs rs <> ppB e
          Macro2 (Ident _ m) e1 e2 -> text m <> parens (ppr 0 e1) <> ppB e2
          Return e -> maybeParens (p>0) $ text "return" <+> ppr 2 e
          ----
          DefineV i -> pPrintPrec l p (InfixOp (Variable i) (Ident noLoc ":") (Variable (Ident noLoc "any")))
          DefineE i e -> pPrintPrec l p (InfixOp (Variable i) (Ident noLoc ":=") e)
          DefineIE i x e -> pPrintPrec l p (InfixOp (InfixOp (Variable i) (Op "->") (Variable x)) (Op ":=") e)
          Choice e1 e2 -> pPrintPrec l p (InfixOp e1 (Op "|") e2)
          Unify e1 e2 -> pPrintPrec l p (InfixOp e1 (Op "=") e2)
          Fail -> text "fail"
          Range e -> --pPrintPrec l p (PrefixOp (Ident noLoc ":") e)
                     text "range" <> braces (ppr 0 e)
          Wrong s -> text $ "WRONG'" ++ s ++ "'"
          Exists is e -> maybeParens (p > 0) $ sep [text "exists" <+> hsep (map (ppr 0) is) <+> text ".", ppr 0 e]
          Forall is e -> maybeParens (p > 0) $ sep [text "forall" <+> hsep (map (ppr 0) is) <+> text ".", ppr 0 e]
          OfType e t -> --ppNormal (InfixOp e (Op ":") t)
                         text "ofType" <> parens (ppr 0 e) <> braces (ppr 0 t)
          TLam i rs e1 e2 -> text "tlam" <>
                                 parens (ppr 0 i) <> ppEffs rs <> braces (ppr 0 e1) <> braces (ppr 0 e2)
          DomainFail -> text "DomainFail"
          EPrim s -> ppNormal (Variable (Ident noLoc s))
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

ppSeq :: PrettyLevel -> [SrcExpr] -> Doc
ppSeq l es = sep $ punctuate (text ";") (map (pPrintPrec l 0) es)

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

compos :: (Applicative f) => (SrcExpr -> f SrcExpr) -> SrcExpr -> f SrcExpr
compos _ e@Lit{} = pure e
compos _ e@Variable{} = pure e
compos f (QualVariable e v) = QualVariable <$> f e <*> pure v
compos f (Array es) = Array <$> traverse f es
compos f (Tuple es) = Tuple <$> traverse f es
compos f (Seq es) = Seq <$> traverse f es
compos f (ApplyS e1 e2) = ApplyS <$> f e1 <*> f e2
compos f (ApplyD e1 e2) = ApplyD <$> f e1 <*> f e2
compos f (EffAttr e r) = EffAttr <$> f e <*> pure r
compos f (PrefixOp op e) = PrefixOp op <$> f e
compos f (PostfixOp e op) = PostfixOp <$> f e <*> pure op
compos f (InfixOp e1 op e2) = InfixOp <$> f e1 <*> pure op <*> f e2
compos f (If1 b) = If1 <$> f b
compos f (If2 e b) = If2 <$> f e <*> f b
compos f (If2E e b) = If2E <$> f e <*> f b
compos f (If3 e b1 b2) = If3 <$> f e <*> f b1 <*> f b2
compos f (If3B is e b1 b2) = If3B is <$> f e <*> f b1 <*> f b2
compos f (For1 b) = For1 <$> f b
compos f (For2 e b) = For2 <$> f e <*> f b
compos f (For2B is e b) = For2B is <$> f e <*> f b
compos f (Let e b) = Let <$> f e <*> f b
compos f (Block b) = Block <$> f b
compos f (Case1 b) = Case1 <$> f b
compos f (Case2 e b) = Case2 <$> f e <*> f b
compos f (Function ers b) = Function <$> traverse g ers <*> f b
  where g (e, r) = (,) <$> f e <*> pure r
compos f (Blk es) = Blk <$> traverse f es
compos f (Option me) = Option <$> traverse f me
--compos f (Typedef b) = Typedef <$> f b
compos f (Parens e) = Parens <$> f e
compos f (Set e1 op e2) = Set <$> f e1 <*> pure op <*> f e2
compos f (MVar i e1 e2) = MVar i <$> traverse f e1 <*> traverse f e2
compos f (MRef i e1 e2) = MVar i <$> traverse f e1 <*> traverse f e2
compos f (MAlias i e1 e2) = MVar i <$> traverse f e1 <*> traverse f e2
compos f (Macro1 m as b) = Macro1 m as <$> f b
compos f (Macro2 m a b) = Macro2 m <$> f a <*> f b
compos f (Return e) = Return <$> f e
compos _ (DefineV i) = pure $ DefineV i
compos f (DefineE i e) = DefineE i <$> f e
compos f (DefineIE i x e) = DefineIE i x <$> f e
compos f (Choice e1 e2) = Choice <$> f e1 <*> f e2
compos f (Unify e1 e2) = Unify <$> f e1 <*> f e2
compos f (Range e) = Range <$> f e
compos _ e@Wrong{} = pure e
compos f (Exists is e) = Exists is <$> f e
compos f (Forall is e) = Forall is <$> f e
compos f (OfType e1 e2) = OfType <$> f e1 <*> f e2
compos f (TLam i rs e1 e2) = TLam i rs <$> f e1 <*> f e2
compos _ e@DomainFail = pure e
compos _ e@EPrim{} = pure e
compos f (Lam i e) = Lam i <$> f e
compos f (Split e1 e2 e3) = Split <$> f e1 <*> f e2 <*> f e3
compos _ e@Fail = pure e
compos f (Map es) = Map <$> traverse f es
compos f (Truth e) = Truth <$> f e
compos f (EStore s e) = EStore <$> storeMapA f s <*> f e

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
