
type Var
  = String

data Val
  = Var Var
  | Arr [Val]
  | Int Integer
  | Lam Var Expr
 deriving ( Eq, Ord )

data Expr
  = Val Val
  | Expr :>: Expr
  | Val :=: Expr
  | Expr :|: Expr
  | Exi Var Expr
  | One Expr
  | All Expr
  | Succeeds Expr
  | Decides Expr
  | Iterates Expr
 deriving ( Eq, Ord )

data Sem
  = Nil
  | Alt Form
  | Sem :++: Sem
  | Sem :**: Sem
  | Alts (Term -> Form) (Term -> Form)

results :: Expr -> Maybe Term -> Sem
results (Val v) mt =
  Alt (maybe (.=. term v) true mt)
  
results (e1 :>: e2) mt =
  results e1 Nothing :**: results e2 mt

results (e1 :|: e2) mt =
  results e1 mt :++: results e2 mt 

results (v := e) mt =
  mapSem (maybe ((.&&.) . (.=. term v)) id mt) (results e (Just (term v)))

results (Exi x e) mt =
  mapSem (Exists x) (results e mt)

results (One e) mt =
  Alt (Exists x one (
 where
  x = 
