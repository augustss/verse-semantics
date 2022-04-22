{-# LANGUAGE DeriveDataTypeable #-}
{- x# LANGUAGE PatternSynonyms # -}
{- x# LANGUAGE ViewPatterns # -}

module Expr(
  Ident(..),
  Expr(..),
  Block(..),
  Eff,
  Op,
  arrayS,
  SourcePos, initialPos,
  ) where

import Data.Data (Data)
import Data.Maybe
import Data.Ratio
import Epic.Print
import Prelude hiding ((<>))
import Text.Megaparsec (SourcePos, initialPos)

import Error

data Ident = Ident SourcePos String
  deriving (Eq, Ord{-, Show-}, Data)

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
  | Array Block               -- e1,e2,...
  | Seq [Expr]                -- e1;e2;...
  | Call Expr Expr            -- f(e)
  | Index Expr Expr           -- f[e]
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
  | Function Expr [Eff] Block -- function(e)<eff>{e}
  deriving (Eq, Ord, Show, Data)

type Eff = Ident

type Op = Ident

data Block
  = BExprs [Expr]
  | BExpr Expr
  deriving (Eq, Ord, Show, Data)

instance Pretty Expr where
  pPrintPrec l p
    | l > prettyNormal = ppNormal
    | otherwise = ppNice
    where
      ppA (Array (BExprs es)) = ppEs es
      ppA e = ppr 0 e
      ppB b@BExprs{} = ppr 0 b
      ppB (BExpr e) = braces (ppr 0 e)
      ppEs = fsep . punctuate comma . map (ppr 1)
      ppr :: (Pretty a) => Rational -> a -> Doc
      ppr = pPrintPrec l
      ppOp = ppr 0
      ppNice expr =
        case expr of
          Array (BExprs es) | length es /= 1 -> parens $ ppEs es
          _ -> ppNormal expr
      ppNormal expr =
        case expr of
          LitInt i
            | i >= 0 -> ppr p i
            | otherwise -> maybeParens (p >= 10) $ text $ show i
          LitRat r
            | denominator r == 1 -> text $ show (numerator r)
            | otherwise -> maybeParens (p >= 9) $ text $ show (numerator r) ++ "/" ++ show (denominator r)
          Array e -> text "array" <> ppr 0 e
          Seq es -> ppSeq l es
          Variable v -> ppr 0 v
          Call  f a -> maybeParens (p > q) $ ppr ql f <> parens (ppA a)
            where (q, ql, _) = fixity "()"
          Index f a -> maybeParens (p > q) $ ppr ql f <> brackets (ppA a)
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
          Function a es b -> maybeParens (p > 0) $ text "fn" <> parens (pPrintL l a) <> effs <> ppr 0 b
            where effs = mconcat (map (\ e -> text "<" <> pPrintL l e <> text ">") es)

ppSeq :: PrettyLevel -> [Expr] -> Doc
ppSeq l es = sep $ punctuate (text ";") (map (pPrintPrec l 0) es)

instance Pretty Block where
  pPrintPrec l _ (BExprs es) = braces $ ppSeq l es
  pPrintPrec l p (BExpr e) = pPrintPrec l p e

fixity :: String -> (Rational, Rational, Rational)
fixity op = fromMaybe internalError $ lookup op tbl
  where
    --                L    R
    inn s p = (s, (p, p+1, p+1))
    inl s p = (s, (p, p,   p+1))
    inr s p = (s, (p, p+1, p))
    tbl =
      [ --inn ","     1
        inr "=>"      2
      , inn ":="      3
      , inr "||"      4
      , inr "&&"      5
      , inr ":"       6     
      , inr "="       6     
      , inr "<>"      6     
      , inr "<="      6     
      , inr ">="      6     
      , inr "<"       6     
      , inr ">"       6     
      , inl "|"       7
      , inl ".."      7
      , inr "->"      7
      , inl "+"       8
      , inl "-"       8
      , inl "*"       9
      , inl "/"       9
      , inn "post^"  10
      , inn "pre:"   11
      , inn "macro"  12
      , inl "()"     13
      ]

arrayS :: [Expr] -> Expr
arrayS [e] = e
arrayS es = Array (BExprs es)

{-
-- Find all variables defined in the scope of an expression.
-- Does not include variables from nested scopes.
definedVars :: Expr -> [Ident]
definedVars = expr
  where
    expr LitInt {} = []
    expr LitRat {} = []
    expr (Array es) = concatMap expr es
    expr (Seq es) = concatMap expr es
    expr Lambda {} = []
    expr Variable {} = []
    expr (Apply _ e1 e2) = expr e1 ++ expr e2
    expr Or {} = []
    expr (Range e) = expr e
    expr (Unify e1 e2) = expr e1 ++ expr e2
    expr (Define i e) = i : expr e
    expr If {} = []
    expr For {} = []
    expr (Let _ e) = expr e
    expr (PrimOp _ es) = concatMap expr es
    expr Case {} = internalError
    expr Function {} = internalError

-- All free variables in an expressions
freeVars :: Expr -> [Ident]
freeVars = block . Block
  where
    expr LitInt {} = []
    expr LitRat {} = []
    expr (Array es) = foldr (union . freeVars) [] es
    expr (Seq es) = foldr (union . freeVars) [] es
    expr (Lambda i b) = block b \\ [i]
    expr (Variable v) = [v]
    expr (Apply _ e1 e2) = expr e1 `union` expr e2
    expr (Or b1 b2) = block b1 `union` block b2
    expr (Range e) = expr e
    expr (Unify e1 e2) = expr e1 `union` expr e2
    expr (Define _ e) = expr e
    expr (If b1 b2 b3) = block b1 `union` block b2 `union` block b3
    expr (For b1 b2) = block b1 `union` block b2
    expr (Let (Block b) e) = (expr b `union` expr e) \\ definedVars b
    expr (PrimOp _ es) = foldr (union . freeVars) [] es
    expr Case {} = internalError
    expr Function {} = internalError
    block (Block e) = expr e \\ definedVars e

-}
