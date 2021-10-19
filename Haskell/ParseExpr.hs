module ParseExpr(
  Ident(..),
  Expr(..),
  Type,
  Pat,
  ) where
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJClass

-----
pPrintL :: (Pretty a) => PrettyLevel -> a -> Doc
pPrintL l = pPrintPrec l 0

indent :: Doc -> Doc
indent = nest 2
-----

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

instance Pretty Ident where
  pPrintPrec _ _ (Ident s) = text s

--

-- Syntax tree from parsing, includes all "macros"
data Expr
  = Def Ident                        -- def{x}
  | Var Ident                        -- x
  | Int Integer                      -- i
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Lambda Pat Expr                  -- p => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  | Let Expr Expr                    -- let (e1) in e2
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  ---
  | Do Expr                          -- do e
  | Define Pat Expr                  -- p := e
  | HasType Expr Type                -- x : t
  | Range Type                       -- :t
  | Call Expr Expr                   -- e1(e2)
  | TypeDef Expr                     -- typedef{e}
  | Where Expr Expr                  -- e1 where e2
  | Case Expr [Expr]                 -- case(e) of { e1; ...; en }
  deriving (Eq, Ord, Show)

type Type = Expr

-- Stuff on the left of a :=
-- Only Var, HasType, Call are allowed
type Pat = Expr

----------------------

commaSep :: [Doc] -> Doc
commaSep = fsep . punctuate comma

instance Pretty Expr where
  pPrintPrec l p
    | l > prettyNormal = ppNormal
    | otherwise = ppNice
    where
      ppr :: (Pretty a) => Rational -> a -> Doc
      ppr = pPrintPrec l
      ppNice expr =
        case expr of
--          Array es | length es /= 1 -> parens $ commaSep $ map (ppr 1) es
--          Define i (Range e) -> ppr 6 i <> text ":" <+> ppr 5 e
          _ -> ppNormal expr
      ppNormal expr =
        case expr of
          Def n -> text "def" <> braces (pPrintPrec l 0 n)
          Var n -> pPrintPrec l 0 n
          Int i
            | i >= 0 -> ppr p i
            | otherwise -> maybeParens (p >= 10) $ text $ show i
          Unify e1 e2 -> maybeParens (p >= 6) $ ppr 6 e1 <+> text "=" <+> ppr 5 e2
          Apply f a -> maybeParens (p > 13) $ ppr 13 f <> brackets (ppr 0 a)
          Lambda a e -> maybeParens (p > 4) $ ppr 5 a <> text "=>" <> ppr 4 e
          Alt e1 e2 -> maybeParens (p >= 7) $ ppr 6 e1 <+> text "|" <+> ppr 6 e2
          Array es -> text "array" <> braces (commaSep (map (ppr 1) es))
          If e1 e2 e3 -> maybeParens (p >= 0) $ sep [text "if" <+> parens (ppr 0 e1), indent $ ppBlock e2, text "else", indent $ ppBlock e3]
          For e1 e2 -> maybeParens (p >= 0) $ sep [text "for" <+> parens (ppr 0 e1), indent $ ppBlock e2]
          Let e1 e2 -> maybeParens (p >= 0) $ sep [text "let" <+> parens (ppr 0 e1), text "in", indent $ ppr 0 e2]
          Seq es -> braces $ vcat $ punctuate (text ";") $ map (ppr 0) es
          --
          Do e -> maybeParens (p > 0) $ text "do" <+> indent (ppBlock e)
          Define i e -> maybeParens (p >= 3) $ ppr 3 i <> text ":=" <+> ppr 2 e
          HasType i e -> ppr 6 i <> text ":" <+> ppr 5 e
          Range e -> maybeParens (p >= 10) $ text ":" <> ppr 10 e
          Call f a -> maybeParens (p > 13) $ ppr 13 f <> parens (ppr 0 a)
          TypeDef e -> text "typedef" <+> braces (ppr 0 e)
          Where e1 e2 -> maybeParens (p > 0) $ ppr 1 e1 <+> text "where" <+> ppr 1 e2
          Case e bs ->
            maybeParens (p >= 0) $
              sep
                [ text "case" <+> parens (pPrintL l e) <+> text "of",
                  indent $ ppBlock (Seq bs)
                ]
      ppBlock e = pPrintL l e
