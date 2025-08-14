Expr  : Type
PrimOp : Type
LitType : Type
IterType : Type

list : Functor


Lit   : LitType -> Expr
Tup   : "list"(Expr) -> Expr
Tru   : Expr -> Expr
Lam   : (bind Expr in Expr) -> Expr
Op    : PrimOp -> Expr

Unify : Expr -> Expr -> Expr   -- :=:   unification "="
Seq   : Expr -> Expr -> Expr  -- :>:   seq. composition
Or    : Expr -> Expr -> Expr  -- :|:   choice "|"
App   : Expr -> Expr -> Expr    -- :@:   application v1[v2]

Exi   : (bind Expr in Expr) -> Expr
Fail  : Expr 
Iter  : IterType -> Expr -> Expr -> Expr    -- iterator over choices
-- All   : Expr -> Expr

-- MISSING: Some, guard, Check, Cerify, Size, HOLE

