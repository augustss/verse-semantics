module ENV2 where

import Data.List( group, sort, intercalate, transpose, (\\) )

----------------------------------------------------------------------------------------

newtype Ident = Id String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Id x) = x

freshList :: [Ident] -> [Ident]
freshList xs = ys \\ xs
 where
  ys = [ Id ("z" ++ show i) | i <- [1..] ]

fresh :: [Ident] -> Ident
fresh xs = head (freshList xs)

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun FUN
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k)   = show k
  show (Fun fun) = show fun

instance Num Value where
  fromInteger k = Int k

  abs    = error "abs on Value"
  signum = error "signum on Value"
  (*)    = error "(*) on Value"
  (+)    = error "(+) on Value"
  (-)    = error "(-) on Value"

type PartialFun = ENV -- with just two variables x and y!
type FUN = [PartialFun]

----------------------------------------------------------------------------------------

data Thing
  = Value Value
  | Ident Ident
 deriving ( Eq, Ord )

instance Show Thing where
  show (Value v) = show v
  show (Ident x) = show x

data CONSTR
  = Ident :=: Thing
  | Ident :/=: Thing
 deriving ( Eq, Ord )

instance Show CONSTR where
  show (x :=: t)  = show x ++ "=" ++ show t
  show (x :/=: t) = show x ++ "≠" ++ show t

neg :: CONSTR -> CONSTR
neg (x :=: t)  = x :/=: t
neg (x :/=: t) = x :=: t

add :: CONSTR -> [CONSTR] -> Maybe [CONSTR]
add constr cs0 = normConstr constr
 where
  norm (Value v) _  = Value v
  norm t         [] = t
  norm (Ident x) ((y :=: t):cs)
    | x == y        = t
    | otherwise     = norm (Ident x) cs
  norm t (_:cs)     = norm t cs

  normConstr (x :=: t) =
    case (norm (Ident x) cs0, norm t cs0) of
      (Value a, Value b)
        | a /= b -> Nothing

      (t1, t2) ->
        case t1 `compare` t2 of
          EQ -> Just cs0
          GT -> addEq t1 t2 cs0
          LT -> addEq t2 t1 cs0

  normConstr (x :/=: t) =
    case (norm (Ident x) cs0, norm t cs0) of
      (Value a, Value b)
        | a /= b -> Just cs0

      (t1, t2) ->
        case t1 `compare` t2 of
          EQ -> Nothing
          GT -> addNeq t1 t2 cs0
          LT -> addNeq t2 t1 cs0

  addEq  (Ident x) t cs = ((x :=: t) `uinsert`) `fmap` subst x t cs
  addNeq (Ident x) t cs = Just ((x :/=: t) `uinsert` cs)

  subst x t ((y :=: Ident x'):cs) | x == x' =
    ((y :=: t) `uinsert`) `fmap` subst x t cs
    
  subst x t ((x' :/=: t'):cs) | x == x' =
    case (t, t') of
      (t, t')            | t == t' -> Nothing
      (Value a, Value b) | a /= b  -> subst x t cs
      (Ident z, t') | Ident z > t' -> ((z :/=: t') `uinsert`) `fmap` subst x t cs
      (t, Ident z)                 -> ((z :/=: t)  `uinsert`) `fmap` subst x t cs
  
  subst x t ((y :/=: Ident x'):cs) | x == x' =
    case t of
      t | t == Ident y -> Nothing
        | otherwise    -> ((y :/=: t) `uinsert`) `fmap` subst x t cs

  subst x t (c:cs) = (c `uinsert`) `fmap` subst x t cs
  subst x t []     = Just []

data CASE
  = YES [CONSTR]
  | NO
 deriving ( Eq, Ord )

instance Show CASE where
  show (YES []) = "U"
  show (YES cs) = intercalate ";" (map show cs)
  show NO       = "∅"
  
true :: CASE
true = YES []

(&&&) :: CASE -> CASE -> CASE
YES []      &&& alt     = alt
YES (c:cs1) &&& YES cs2 = case add c cs2 of
                            Just cs2' -> YES cs1 &&& YES cs2'
                            Nothing   -> NO
_           &&& _       = NO

hideCase :: Ident -> CASE -> CASE
hideCase x NO       = NO
hideCase x (YES cs) =
  (case [ u | (u :=: Ident x') <- cs, x' == x ] of
     -- x was not a rep
     [] -> true
     -- x was a rep
     us -> YES [ c'
               | c <- cs
               , c' <- case c of
                         y :=: Ident x'  | x' == x -> [ y :=: Ident u ]
                         x' :/=: t       | x' == x -> [ u :/=: t ]
                         t :/=: Ident x' | x' == x -> [ t :/=: Ident u ]
                         _                         -> []
               ]
      where
       u = minimum us {- u is the new rep for x's equivalence class -})
  &&&
  YES [ c
      | c <- cs
      , case c of
          y :=: _        | y == x -> False
          _ :=: Ident y  | y == x -> False
          y :/=: _       | y == x -> False
          _ :/=: Ident y | y == x -> False
          _                       -> True
      ]

----------------------------------------------------------------------------------------

data ENV = OR [ CASE ]
 deriving ( Eq, Ord )

instance Show ENV where
  show (OR [c]) | c == true = "U"
  show (OR cs)              = "{" ++ intercalate "," (map show cs) ++ "}"

disj :: [CASE] -> ENV
disj as = OR (usort [ a | a@(YES _) <- as ])

----------------------------------------------------------------------------------------

infix  5 .=, .=.
infixr 4 /\
infixr 3 \/

empty :: ENV
empty = OR []

univ :: ENV
univ = OR [ true ]

(/\) :: ENV -> ENV -> ENV
OR as /\ OR bs = disj [ a &&& b | a <- as, b <- bs ]

(\/) :: ENV -> ENV -> ENV
OR as \/ OR bs = disj (as ++ bs)

(.=) :: Ident -> Value -> ENV
x .= v = OR [ YES [x :=: Value v] ]

(.=.) :: Ident -> Ident -> ENV
x .=. y = OR [ YES (case x `compare` y of
                      LT -> [y :=: Ident x]
                      EQ -> []
                      GT -> [x :=: Ident y]) ]

hide :: Ident -> ENV -> ENV
hide x (OR cs) = disj [ hideCase x c | c <- cs ]

hides :: [Ident] -> ENV -> ENV
hides xs env = foldr hide env xs

compl :: ENV -> ENV
compl (OR as) =
  foldr (/\) univ
  [ foldr (\/) empty $
    [ OR [YES [neg c]]
    | c <- cs
    ]
  | YES cs <- as
  ]

----------------------------------------------------------------------------------------

apply :: FUN -> Ident -> Ident -> [ENV]
apply hs x y =
  [ ren tmp y
  $ ren funx x 
  $ ren funy tmp
  $ h
  | h <- hs
  ]

funx, funy :: Ident
funx = Id "$x"
funy = Id "$y"

tmp :: Ident
tmp = Id "$tmp"

ren :: Ident -> Ident -> ENV -> ENV
ren x y env
  | x == y    = env
  | otherwise = hide x ((x .=. y) /\ env)

----------------------------------------------------------------------------------------

data Expr
  = Var Ident
  | Con Integer
  | Tup [Ident]
  | Ident := Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | If Expr Expr Expr
  | All Expr
  | Exi Ident
  | Scope Expr
  | Ident :@: Ident
 deriving ( Eq, Ord, Show )

instance Num Expr where
  fromInteger k = Con k

  abs    = error "abs on Expr"
  signum = error "signum on Expr"
  (*)    = error "(*) on Expr"
  (+)    = error "(+) on Expr"
  (-)    = error "(-) on Expr"

vars :: Expr -> [Ident]
vars (Var x)      = [x]
vars (Tup xs)     = xs
vars (x := e)     = x : vars e
vars (e1 :>: e2)  = vars e1 ++ vars e2
vars (e1 :|: e2)  = vars e1 ++ vars e2
vars (If e e1 e2) = vars e ++ vars e1 ++ vars e2
vars (Exi x)      = [x]
vars (Scope e)    = vars e
vars (f :@: x)    = [f,x]
vars _            = []

exis :: Expr -> [Ident]
exis (x := e)     = exis e
exis (e1 :>: e2)  = exis e1 ++ exis e2
exis (Exi x)      = [x]
exis _           = []

----------------------------------------------------------------------------------------

sem :: Expr -> Ident -> [ENV]
sem (Var x) r =
  [ x .=. r ]
  
sem (Con k) r =
  [ r .= Int k ]

sem (Tup xs) r =
  [ bigUnion
    [ foldr (/\) (r .= Fun fun)
      [ hides [funx,funy] $
          h /\ funx .= Int i /\ funy .=. x
      | (i,(h,x)) <- [0..] `zip` (fun `zip` xs)
      ]
    | fun <- funs
    , length fun == length xs
    ]
  ]

{-
sem (All e) r =
  [ bigUnion
    [ foldr (/\) (r .= Fun fun)
      [ hide funx $ hide funy $
            h /\ funx .= Int i /\ funy .=. x
      | (i,(h,x)) <- [0..] `zip` (fun `zip` xs)
      ]
    | fun <- funs
    , length fun == length xs
    ]
  ]
-}

sem (x := e) r =
  [ x .=. r /\ env
  | env <- sem e r
  ]
  
sem (e1 :>: e2) r =
  [ hide u env1 /\ env2
  | env1 <- sem e1 u
  , env2 <- sem e2 r
  ]
 where
  u = fresh (r : vars e1 ++ vars e2)

sem (e1 :|: e2) r =
  sem e1 r ++ sem e2 r

sem (If e1 e2 e3) r =
  dodgyUnion
  [ [ hides ys (env1 /\ env2) | env2 <- sem e2 r ]
  , [ compl env1 /\ hides ys env3 | env3 <- sem e3 r ]
  ]
 where
  [env1] = map (hide z) (sem e1 z) -- this is a hack, should use FIRST instead
  z  = fresh (r : vars (If e1 e2 e3))
  ys = exis e1

sem (Exi x) r =
  [ univ ]

sem (Scope e) r =
  [ hides (exis e) env
  | env <- sem e r
  ]

sem (f :@: x) r =
  dodgyUnion
  [ [ f .= Fun fun /\ env | env <- apply fun x r ]
  | fun <- funs
  ]

funs :: [FUN]
funs = [ [ funx .= 0 /\ funy .= 1
         , funx .= 1 /\ funy .= 0
         ] 
       ]

---

x, y, z, r :: Ident
x = Id "x"
y = Id "y"
z = Id "z"
r = Id "r"

ex1 = Tup [x, y]
ex2 = (z := ex1) :>: (z :@: x)

---

bigUnion :: [ENV] -> ENV
bigUnion = foldr (\/) empty

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (/\) univ

dodgyUnion :: [ [ENV] ] -> [ENV]
dodgyUnion []    = []
dodgyUnion envss = bigUnion [ env | env:_ <- envss ]
                 : dodgyUnion [ envs | _:envs <- envss, not (null envs) ]

{-
extract :: Ident -> ENV -> ([(ENV,a)],[(ENV,a)])
extract x (OR as) = OR [ | a@(YES ps ns x) <- as ]
-}

{-
----------------------------------------------------------------------------------------

extract :: Ident -> ENV -> [(Value, ENV)]

extracts :: [Ident] -> ENV -> [([Value], ENV)]

----------------------------------------------------------------------------------------
-- derived operators

bigUnion :: [ENV] -> ENV
bigUnion = foldr (%\/) failE

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (%/\) univE

bigUnique :: [ENV] -> ENV
bigUnique envs = go envs cenvs (tail nenvs)
 where
  cenvs = map compl envs
  nenvs = scanr (%/\) univE cenvs

  go [] _ _ = failE
  go (env:envs) (cenv:cenvs) (nenv:nenvs) =
    (env %/\ nenv) %\/ (cenv %/\ go envs cenvs nenvs)
  go _ _ _ = error "bigUnique"

quant :: Ident -> ([ENV] -> ENV) -> ENV -> ENV
quant x bigOp env = bigOp [ env' | (_,env') <- extract x env ]

(%\\) :: ENV -> ENV -> ENV
env1 %\\ env2 = env1 %/\ compl env2

----------------------------------------------------------------------------------------

clean :: [ENV] -> [ENV]
clean ss = [ s | s <- ss, s /= ENV [] ]
-}

----------------------------------------------------------------------------------------

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

uinsert :: Ord a => a -> [a] -> [a]
uinsert x []     = [x]
uinsert x (y:ys) = case x `compare` y of
                     LT -> x : y : ys
                     EQ -> y : ys
                     GT -> y : uinsert x ys

----------------------------------------------------------------------------------------



