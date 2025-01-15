
data Expr
  -- values
  = Var Ident
  | Skol Ident
  | Int Integer
  | Arr [Expr]
  | Lam (Bind Expr)
  | Op Op
  
  -- programs
  | Expr :=: Expr    -- unification      =
  | Expr :>: Expr    -- seq. composition ;
  | Expr :|: Expr    -- choice           |
  | Expr :@: Expr    -- application      v1[v2]
  | Exi (Bind Expr)
  | Fail

  -- one/all
  | One Expr
  | All Expr
  -- | Split Expr
  
  -- verifier
  | Some Expr
  | Expr :>>: Expr   -- guard           |>   <-- black triangle
  | Check Effect Expr
  --| Verify [Ident] [Assump] Expr
  | Verify (BindSet ([Assump],Expr))

  -- only for contexts
  | Hole
 deriving ( Eq )

data Op = Add | Sub | Gt | IsInt
 deriving ( Eq, Ord, Show )

data Assump
  = Ident 
 deriving ( Eq, Ord, Show )

type Context = Expr
type Val     = Expr


hole = Var "#"

data Bind a = Bind Ident a -- hidden

alphaRename :: Bind a -> [Ident] -> (Ident, a)


data BindSet a = Done a | One (Bind (BindSet a))

