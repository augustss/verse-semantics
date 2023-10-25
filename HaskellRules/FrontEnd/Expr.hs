{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- x# LANGUAGE ViewPatterns # -}

module FrontEnd.Expr(
  Loc, noLoc,
  Ident(..), unIdent,
  Expr(..),
  Lit(..),
  Core,
  pattern Unit,
  pattern Typedef,
  pattern Succeeds,
  pattern Check,
--  pattern Range,
  Store(..), Ptr,
  Blk,
  Eff,
  Op,
  pattern Op,
  compos, composOp,
  seqE,
  getLoc,
  isLiteral,
  isValue,
  ) where
import Control.Monad.Identity
import Data.Data (Data)
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Ratio
import Data.Scientific(Scientific)
import Epic.Print
import Prelude hiding ((<>))
import Text.Megaparsec (SourcePos, initialPos, sourcePosPretty)

import FrontEnd.Error

type Loc = SourcePos
noLoc :: Loc
noLoc = initialPos ""

instance Pretty Loc where
  pPrintPrec _ _ = text . sourcePosPretty

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

data Expr
  = Lit Lit                   -- k
  | Variable Ident            -- x
  | QualVariable Expr Ident   -- (e:)x
  | Array [Expr]              -- array{e1;e2;...}
  | Tuple [Expr]              -- e1,e2,...             -- will be turned into Array
  | ApplyS Expr Expr          -- f(e)
  | ApplyD Expr Expr          -- f[e]
  | EffAttr Expr Eff          -- f<e>
  | PrefixOp Op Expr          -- op e
  | PostfixOp Expr Op         -- e op
  | InfixOp Expr Op Expr      -- e1 op e2
  | If1 Blk                 -- if{e}
  | If2 Expr Blk            -- if(e1) then e2
  | If2E Expr Blk           -- if(e1) else e2
  | If3 Expr Blk Blk      -- if(e1) then e2 else e3
  | For1 Blk                -- for{e}
  | For2 Expr Blk           -- for(e1) in e2
  | Let Expr Blk            -- let(e1) in e2
  | Block Blk                  -- do e
  | Case1 Blk               -- case{e1; e2; ... } block treated in a non-standard way
  | Case2 Expr Blk          -- case(e) of {e1; e2; ... } block treated in a non-standard way
  | Function [(Expr, [Eff])] Blk -- function(e)<eff>...{e}
--  | Typedef Blk             -- type{e}
  | Blk [Expr]              -- { e1; e2; ... }
  | Option (Maybe Expr)       -- option{e}
  | Parens Expr               -- (e)
  | Set Expr Ident Expr       -- set e1 = e2
  | MVar Ident (Maybe Expr) (Maybe Expr)      -- var i : t = e
  | MRef Ident (Maybe Expr) (Maybe Expr)      -- ref i : t = e
  | MAlias Ident (Maybe Expr) (Maybe Expr)    -- alias i : t = e
  -- Some 1-argument macros
  | Macro1 Ident [Eff] Blk  -- m<a>{e}
  | Macro2 Ident Expr Blk   -- m(e1){e2}
  | Return Expr               -- return e
  -- Initial desugaring turns some operators into more easily recognizable forms
  | Seq [Expr]                -- e1;e2;...
  | DefineV Ident             -- i:any
  | DefineE Ident Expr        -- i := e
  | DefineIE Ident Ident Expr -- (i->x) := e
  | Choice Expr Expr          -- e | e
  | Unify Expr Expr           -- e1 = e2
  | Range Expr                -- :e
  | Wrong String              -- wrong
  | Exists [Ident] Expr       -- exists xs . e
  | Forall [Ident] Expr       -- forall xs . e
  | OfType Expr Expr         -- e:t, but only type known to verifier
  | TLam Ident [Eff] Expr Expr
                              -- function(x:any where e1)<eff>{e2}, e1 can make bindings visible in e2.
                              -- The last argument is a possible type, (e2:t)  
  | DomainFail                -- either Wrong or try next overload
  | EPrim String              -- primop
  | Lam Ident Expr            -- \ x . e
  | Split Expr Expr Expr      -- split(e1){e2}{e3}
  | Fail
  -- These are used when translating back from Rules.Core.Expr
  | EStore Store Expr
  deriving (Eq, Ord, Show, Data)

-- This synonym is used for the very reduced subset of Expr that
-- can be directly translated to Rules.Core.Expr
type Core = Expr
type Value = Expr

data Store = Store { refMap :: IM.IntMap Value, outputs :: [Core] }
  deriving (Show, Eq, Ord, Data)
type Ptr = Int

data Lit
  = LitInt Integer            -- d
  | LitRat Scientific String  -- d.d
  | LitChar Char              -- 'c'
  | LitStr String             -- "str"
  | LitPtr Ptr                -- not a textual literal, just used when translating back.
  deriving (Eq, Ord, Show, Data)

instance Pretty Lit where
  pPrintPrec _ p lit =
    case lit of
      LitInt i
        | i >= 0 -> text $ show i
        | otherwise -> maybeParens (p >= 10) $ text $ show i
      LitRat r s -> text (show r ++ s)
      LitChar c -> text (show c)
      LitStr s -> text (show s)
      LitPtr ptr -> text ("R#" ++ show ptr)

--pattern Range :: Expr -> Expr
--pattern Range e = ApplyD e AnyT
pattern Unit :: Expr
pattern Unit = Array []
pattern Typedef :: Blk -> Expr
pattern Typedef e <- Macro1 (Ident _ "type") [] e
  where Typedef e = Macro1 (Ident noLoc "type") [] e
pattern Succeeds :: Blk -> Expr
pattern Succeeds e <- Macro1 (Ident _ "succeeds") [] e
  where Succeeds e = Macro1 (Ident noLoc "succeeds") [] e
pattern Check :: [Ident] -> Expr -> Expr
pattern Check ps e <- Macro1 (Ident _ "check") ps e
  where Check ps e = Macro1 (Ident noLoc "succeeds") ps e

type Eff = Ident

type Op = Ident
pattern Op :: String -> Op
pattern Op s <- Ident _ s
  where Op s = Ident noLoc s

type Blk = Expr

instance Pretty Expr where
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
          EStore s e ->
            maybeParens (p > 0) $ fsep [text "store"<+> pPrintPrec l p s <+> text "in", indent $ braces (pPrintPrec l 0 e)]
      ppVRA _ _ Nothing  Nothing  = undefined
      ppVRA s i (Just t) Nothing  = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc ":") t)
      ppVRA s i Nothing  (Just e) = text s <+> ppr 0 (InfixOp (Variable i) (Ident noLoc "=") e)
      ppVRA s i (Just t) (Just e) = text s <+> ppr 0 (InfixOp (InfixOp (Variable i) (Ident noLoc ":") t) (Ident noLoc "=") e)

instance Pretty Store where
  pPrintPrec l _ (Store m _) = fsep . punctuate comma . map (pPrintPrec l 0) . IM.toList $ m -- XXX

ppSeq :: PrettyLevel -> [Expr] -> Doc
ppSeq l es = sep $ punctuate (text ";") (map (pPrintPrec l 0) es)

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

compos :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
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
compos f (For1 b) = For1 <$> f b
compos f (For2 e b) = For2 <$> f e <*> f b
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
compos f (EStore s e) = EStore <$> storeMapA f s <*> f e

storeMapA :: (Applicative a) => (Value -> a Value) -> Store -> a Store
storeMapA f s = Store <$> sequenceA (IM.map f (refMap s)) <*> pure (outputs s)

composOp :: (Expr -> Expr) -> Expr -> Expr
composOp f = runIdentity . compos (pure . f)

seqE :: [Expr] -> Expr
seqE = mk . concatMap flat
  where flat (Seq es) = es
        flat e = [e]
        mk [e] = e
        mk es = Seq es

-- XXX fix this
getLoc :: Expr -> Loc
getLoc _ = noLoc

isLiteral :: Expr -> Bool
isLiteral Lit{} = True
isLiteral _ = False

-- Values, except lambda
isValue :: Expr -> Bool
isValue Variable{} = True
isValue EPrim{} = True
isValue (Array es) = all isValue es
isValue e = isLiteral e

