{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- x# LANGUAGE ViewPatterns # -}

module Expr(
  Loc, noLoc,
  Ident(..),
  Expr(..),
  pattern Fail,
  pattern Unit,
--  pattern Range,
  Block,
  Eff,
  Op,
  compos, composOp,
  seqE,
  ) where
import Control.Monad.Identity
import Data.Data (Data)
import Data.Maybe
import Data.Ratio
import Print
import Prelude hiding ((<>))
import Text.Megaparsec (SourcePos, initialPos, sourcePosPretty)

import Error

type Loc = SourcePos
noLoc :: Loc
noLoc = initialPos ""

instance Pretty Loc where
  pPrintPrec _ _ = text . sourcePosPretty

data Ident = Ident Loc String
  deriving ({-Eq, Ord, Show,-} Data)

instance Eq Ident where x == y  =  compare x y == EQ
instance Ord Ident where compare (Ident _ x) (Ident _ y) = compare x y

instance Show Ident where
  show (Ident _ s) = show s

unIdent :: Ident -> String
unIdent (Ident _ s) = s

instance Pretty Ident where
  pPrintPrec _ _ (Ident _ i) = text i

data Expr
  = LitInt Integer            -- d
  | LitRat Rational           -- d.d
  | Variable Ident            -- x
  | Array [Expr]              -- e1,e2,...
  | ApplyS Expr Expr          -- f(e)
  | ApplyD Expr Expr          -- f[e]
  | EffAttr Expr Eff          -- f<e>
  | PrefixOp Op Expr          -- op e
  | PostfixOp Expr Op         -- e op
  | InfixOp Expr Op Expr      -- e1 op e2
  | If1 Block                 -- if{e}
  | If2 Expr Block            -- if(e1) then e2
  | If2E Expr Block           -- if(e1) else e2
  | If3 Expr Block Block      -- if(e1) then e2 else e3
  | For1 Block                -- for{e}
  | For2 Expr Block           -- for(e1) in e2
  | Let Expr Block            -- let(e1) in e2
  | Do Block                  -- do e
  | Case1 Block               -- case{e1; e2; ... } block treated in a non-standard way
  | Case2 Expr Block          -- case(e) of {e1; e2; ... } block treated in a non-standard way
  | Function [(Expr, [Eff])] Block -- function(e)<eff>...{e}
  | Typedef Block             -- typedef{e}
  | Block [Expr]              -- { e1; e2; ... }
  | Option (Maybe Expr)       -- option{e}
  | Parens Expr               -- (e)
  -- Initial desugaring turns some operators into more easily recognizable forms
  | Seq [Expr]                -- e1;e2;...
  | Define Ident Expr         -- i := e
  | Define2 Ident Ident Expr  -- i~x := e
  | Choice Expr Expr          -- e | e
  | Unify Expr Expr           -- e1 = e2
  | Range Expr                -- :e
  | Where Expr Expr           -- e1 where e2
  | AnyT                      -- :any
  deriving (Eq, Ord, Show, Data)

--pattern Range :: Expr -> Expr
--pattern Range e = ApplyD e AnyT
pattern Fail :: Expr
pattern Fail = Range Unit
pattern Unit :: Expr
pattern Unit = Array []

type Eff = Ident

type Op = Ident

type Block = Expr

instance Pretty Expr where
  pPrintPrec l p
    | l > prettyNormal = ppNormal
    | otherwise = ppNice
    where
      ppA (Array es) = ppEs es
      ppA e = ppr 0 e
      ppB (Block es) = braces $ ppSeq l es
      ppB e = braces (ppr 0 e)
      ppEs = commaSep l 1
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
          LitInt i
            | i >= 0 -> ppr p i
            | otherwise -> maybeParens (p >= 10) $ text $ show i
          LitRat r
            | denominator r == 1 -> text $ show (numerator r)
            | otherwise -> maybeParens (p >= 9) $ text $ show (numerator r) ++ "/" ++ show (denominator r)
          Array es -> text "array" <> braces (ppSeq l es)
          Seq es -> maybeParens (p > 0) $ ppSeq l es
          Variable v -> ppr 0 v
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
          InfixOp e1 o e2 -> maybeParens (p > q) $ ppr ql e1 <+> ppOp o <+> ppr qr e2
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
          For2 e1 e2 -> maybeParens (p > 0) $ sep [text "for" <+> parens (ppr 0 e1) <+> text "in",
                                                      indent $ ppr 0 e2]
          Let e1 e2 -> maybeParens (p > 0) $ sep [text "let" <+> parens (ppr 0 e1),
                                                   text "in",
                                                     indent $ ppr 0 e2]
          Do e1 -> maybeParens (p > 0) $ sep [text "do" <+> indent (ppr 0 e1)]
          Case1 bs ->
            maybeParens (p > 0) $ sep [ text "case", indent $ ppr 0 bs ]
          Case2 e bs ->
            maybeParens (p > 0) $ sep [ text "case" <+> parens (pPrintL l e) <+> text "of",
                                           indent $ ppr 0 bs ]
          Function ars b -> maybeParens (p > 0) $ text "fn" <> hcat (map ppArs ars) <> ppB b
            where ppArs (e, rs) = parens (pPrintL l e) <> effs
                     where effs = mconcat (map (\ r -> text "<" <> pPrintL l r <> text ">") rs)
          Block es -> braces $ ppSeq l es
          Typedef e -> text "type" <> braces (ppr 0 e)
          Option me -> text "option" <> braces (maybe empty (ppr 0) me)
          Parens e -> parens (ppr 0 e)
          ----
          Define i e -> pPrintPrec l p (InfixOp (Variable i) (Ident noLoc ":=") e)
          Define2 i j e -> pPrintPrec l p (InfixOp (InfixOp (Variable i) (Ident noLoc "~>") (Variable j)) (Ident noLoc ":=") e)
          Choice e1 e2 -> pPrintPrec l p (InfixOp e1 (Ident noLoc "|") e2)
          Unify e1 e2 -> pPrintPrec l p (InfixOp e1 (Ident noLoc "=") e2)
          Range e -> pPrintPrec l p (PrefixOp (Ident noLoc ":") e)
          Where e1 e2 -> pPrintPrec l p (InfixOp e1 (Ident noLoc "where") e2)
          AnyT -> pPrintPrec l p (Variable (Ident noLoc ":any"))

ppSeq :: PrettyLevel -> [Expr] -> Doc
ppSeq l es = sep $ punctuate (text ";") (map (pPrintPrec l 0) es)

fixity :: String -> (Rational, Rational, Rational)
fixity op = fromMaybe internalError $ lookup op tbl
  where
    --                L    R
    inn s p = (s, (p, p+1, p+1))
    inl s p = (s, (p, p,   p+1))
    inr s p = (s, (p, p+1, p))
    tbl =
      [ --inn ","     1
        inn "pre.."   0
      , inn "where"   1
      , inr "=>"      2
      , inn ":="      3
      , inl "="       6     
      , inr "||"      4
      , inr "&&"      5
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
      , inn "pre^"   11
      , inn "pre?"   11
      , inn "pre:"   11
      , inn "pre!"   11
      , inn "pre[]"  11
      , inn "macro"  12
      , inl "()"     13
      ]

compos :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
compos _ e@LitInt{} = pure e
compos _ e@LitRat{} = pure e
compos _ e@Variable{} = pure e
compos f (Array es) = Array <$> traverse f es
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
compos f (Do b) = Do <$> f b
compos f (Case1 b) = Case1 <$> f b
compos f (Case2 e b) = Case2 <$> f e <*> f b
compos f (Function ers b) = Function <$> traverse g ers <*> f b
  where g (e, r) = (,) <$> f e <*> pure r
compos f (Block es) = Block <$> traverse f es
compos f (Option me) = Option <$> traverse f me
compos f (Typedef b) = Typedef <$> f b
compos f (Parens e) = Parens <$> f e
compos f (Define i e) = Define i <$> f e
compos f (Define2 i j e) = Define2 i j <$> f e
compos f (Choice e1 e2) = Choice <$> f e1 <*> f e2
compos f (Unify e1 e2) = Unify <$> f e1 <*> f e2
compos f (Where e1 e2) = Where <$> f e1 <*> f e2
compos f (Range e) = Range <$> f e
compos _ AnyT = pure AnyT

composOp :: (Expr -> Expr) -> Expr -> Expr
composOp f = runIdentity . compos (pure . f)

seqE :: [Expr] -> Expr
seqE = mk . concatMap flat
  where flat (Seq es) = es
        flat e = [e]
        mk [e] = e
        mk es = Seq es

