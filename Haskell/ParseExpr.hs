module ParseExpr(
  Ident,
  Expr(..),
  Type,
  Pat,
  ) where

type Ident = String

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
