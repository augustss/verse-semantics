
type Heap = [([Ident], HNF)]

data Expr
  = Seq [Equ] Ident
  | Int Integer
  | Op Op
  | Arr [Ident]
  | Lam (Bind Expr)
  | Expr :|: Expr
  | Ident :@: Ident
  | Exi (Bind Expr)
  | One Expr
  | All Expr
  | Fail
 deriving ( Show )

data Equ = Ident := Expr
 deriving ( Show )

type Program = [Equ]

propagate :: Heap -> Program -> Maybe (Heap, Program)
propagate h [] =
  Just (h, [])

propagate h ((x, val):p) | isVal val =
  unify h x val p

propagate h ((x, Fail):p) =
  Nothing



step :: [Equ] -> Heap -> [(Heap,[Equ])]
step [] h =
  [(h,[])]

step ((x, val):q) h | isVal val =
  stepUnify h x val q

step ((x, Fail):q) h =
  []

step ((x, e1 :|: e2):q) h =
  step ((x,e1):q) h ++ step ((x,e2):q) h

step ((x, Exi bnd):q) h =
  step ((x,e):q) h
 where
  Bind y e = alphaRename (x,q,h) bnd

step ((y, f :@: x):q) h =
  case look f h of
    Nothing      -> [ (h', (y,f :@: x):q') | (h',q') <- step q h ]
    Just (Lam b) -> step ((y, b x):q) h
    Just _       -> []

step ((x, One e):q) h =
  [ (h2,q2) 
  | (h1,q1) <- step [(x,e)] h
  , (h2,q2) <- case q1 of
                 [] -> step q h1
                 _  -> [ (,) | 
  ]

step ((x,res):q) h =
  [ (h',(x,res):q')
  | (h',q') <- step q h
  ]

--

subst :: Name -> Expr -> Expr -> Expr
subst x e (App a b) = App (subst x e a) (subst x e b)
subst x e (Lam y b) = Lam y (subst x e b)
subst x e (Var y)
  | x == y          = e
  | Var y           = Var y

subst :: Name -> Expr -> Expr -> Expr
subst x e (App a b) = App (subst x e a) (subst x e b)
subst x e (Lam bnd) = Lam y (subst x e b) where (y,b) = bnd `open` x:free e
subst x e (Var y)
  | x == y          = e
  | Var y           = Var y


