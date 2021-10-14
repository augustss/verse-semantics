module CoreExpr(
  Ident,
  Expr(..),
  ) where

import ParseExpr(Ident)

-- After desugaring
data Expr
  = Def Ident                        -- def{x}
  | Var Ident                        -- x
  | Int Integer                      -- i
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Lambda Ident Expr                -- x => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  | Let Expr Expr                    -- let (e1) in e2
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  deriving (Eq, Ord, Show)
