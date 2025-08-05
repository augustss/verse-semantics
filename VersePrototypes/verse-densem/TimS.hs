module TimS where
import ValueS
import ENVS

data Expr
  = Wild                  -- _
  | Atom Atom             -- x, k
  | Array [Expr]          -- array{t1,...}
  deriving (Eq, Ord)

instance Show Expr where
  show Wild = "_"
  show (Atom a) = show a
  show (Array es) = "array{" ++ intercalate "," (map show es) ++ "}"

dS :: Ident -> Expr -> Ident -> [ENV]
dS u Wild     v = [ u .=. v ]
dS u (Atom a) v = [ u .=. v /\ v .= a ]
dS u (Array es) v = 
