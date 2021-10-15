module CoreExpr(
  P.Ident,
  Expr(..),
  toParseExpr,
  ) where
import Text.PrettyPrint.HughesPJClass

import qualified ParseExpr as P

-- After desugaring
data Expr
  = Def P.Ident                      -- def{x}
  | Var P.Ident                      -- x
  | Int Integer                      -- i
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Lambda P.Ident Expr              -- x => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  | Let Expr Expr                    -- let (e1) in e2
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  deriving (Eq, Ord, Show)

instance Pretty Expr where
  pPrintPrec l p = pPrintPrec l p . toParseExpr

toParseExpr :: Expr -> P.Expr
toParseExpr (Def x) = P.Def x
toParseExpr (Var x) = P.Var x
toParseExpr (Int x) = P.Int x
toParseExpr (Unify e1 e2) = P.Unify (toParseExpr e1) (toParseExpr e2)
toParseExpr (Apply e1 e2) = P.Apply (toParseExpr e1) (toParseExpr e2)
toParseExpr (Array es) = P.Array (map toParseExpr es)
toParseExpr (Lambda x e) = P.Lambda (P.Var x) (toParseExpr e)
toParseExpr (Alt e1 e2) = P.Alt (toParseExpr e1) (toParseExpr e2)
toParseExpr (If e1 e2 e3) = P.If (toParseExpr e1) (toParseExpr e2) (toParseExpr e3)
toParseExpr (For e1 e2) = P.For (toParseExpr e1) (toParseExpr e2)
toParseExpr (Let e1 e2) = P.Let (toParseExpr e1) (toParseExpr e2)
toParseExpr (Seq es) = P.Seq (map toParseExpr es)
