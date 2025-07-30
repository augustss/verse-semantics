{-# LANGUAGE PatternSynonyms #-}
module ENV3 where
import Control.Monad(zipWithM)
import Data.List( group, sort, intercalate, transpose, (\\), replicate )

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

type FUN = [PartialFun]

instance Show Value where
  show (Int k)   = show k
  show (Fun fun) = showFUN fun

showFUN :: FUN -> String
showFUN f | Just xs <- getTuple f = "<" ++ intercalate "," (map show xs) ++ ">"
          | otherwise = show f

getTuple :: FUN -> Maybe [Value]
getTuple = zipWithM get [0..]
  where
    get :: Value -> ENV -> Maybe Value
    get i (OR [YES [x :=: Value vx, y :=: Value vy]]) | x == funx && y == funy && vx == i = Just vy
    get _ _ = Nothing

instance Num Value where
  fromInteger k = Int k

  abs    = error "abs on Value"
  signum = error "signum on Value"
  (*)    = error "(*) on Value"
  (+)    = error "(+) on Value"
  (-)    = error "(-) on Value"

instance Enum Value where
  toEnum i = Int (toInteger i)
  fromEnum (Int i) = fromInteger i
  enumFrom (Int i) = map Int (enumFrom i)
  enumFromTo (Int i) (Int j) = map Int (enumFromTo i j)

----------------------------------------------------------------------------------------

data Thing
  = Value Value
  | Ident Ident
 deriving ( Eq, Ord )

instance Show Thing where
  show (Value v) = show v
  show (Ident x) = show x

(.=?) :: Ident -> SimpleExpr -> ENV
x .=? SVar y = x .=. y
x .=? SVal y = x .=  y

(?=?) :: SimpleExpr -> SimpleExpr -> ENV
SVar x ?=? SVar y = x .=. y
SVar x ?=? SVal y = x .=  y
SVal x ?=? SVar y = y .=  x
SVal x ?=? SVal y | x == y    = univ
                  | otherwise = empty

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
  norm :: Thing -> [CONSTR] -> Thing
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

(\\\) :: ENV -> ENV -> ENV
x \\\ y = x /\ compl y

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
  [ OR [YES [neg c] | c <- cs]
  | YES cs <- as
  ]

----------------------------------------------------------------------------------------

type PartialFun = ENV -- with just two variables $x and $y!

apply :: PartialFun -> SimpleExpr -> SimpleExpr -> ENV
apply h x y =
  ren tmp y $ ren funx x $ ren funy (SVar tmp) $ h

funx, funy :: Ident
funx = Id "$x"
funy = Id "$y"

tmp :: Ident
tmp = Id "$tmp"

ren :: Ident -> SimpleExpr -> ENV -> ENV
ren x y env
  | SVar x == y = env
  | otherwise    = hide x ((x .=? y) /\ env)

----------------------------------------------------------------------------------------

data Expr
  = Simp SimpleExpr
  | Tup [Ident]
  | Ident := Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | If Expr Expr Expr
  | If2 Expr Expr Expr
  | All Expr
  | Exi Ident
  | Scope Expr
  | SimpleExpr :@: SimpleExpr
 deriving ( Eq, Ord, Show )

data SimpleExpr
  = SVar Ident
  | SVal Value
 deriving ( Eq, Ord, Show )

pattern Var :: Ident -> Expr
pattern Var x = Simp (SVar x)
pattern Con :: Integer -> Expr
pattern Con i = Simp (SVal (Int i))

infixr 1 :>:
infix  2 :=
infixr 3 :|:

instance Num Expr where
  fromInteger k = Con k

  abs    = error "abs on Expr"
  signum = error "signum on Expr"
  (*)    = error "(*) on Expr"
  (+)    = error "(+) on Expr"
  (-)    = error "(-) on Expr"

vars :: Expr -> [Ident]
vars (Simp x)     = varsS x
vars (Tup xs)     = xs
vars (x := e)     = x : vars e
vars (e1 :>: e2)  = vars e1 ++ vars e2
vars (e1 :|: e2)  = vars e1 ++ vars e2
vars (If e e1 e2) = vars e ++ vars e1 ++ vars e2
vars (If2 e e1 e2) = vars e ++ vars e1 ++ vars e2
vars (Exi x)      = [x]
vars (Scope e)    = vars e
vars (f :@: x)    = varsS f ++ varsS x
vars e            = error $ "vars: " ++ show e

varsS :: SimpleExpr -> [Ident]
varsS (SVar i) = [i]
varsS (SVal _) = []

exis :: Expr -> [Ident]
exis (x := e)    = exis e
exis (e1 :>: e2) = exis e1 ++ exis e2
exis (Exi x)     = [x]
exis _           = []

----------------------------------------------------------------------------------------

sem :: Expr -> Ident -> [ENV]
sem (Simp e) r = semS e r

sem (Tup xs) r =
  squash
  [ bigUnion
    [ foldr (/\) (r .= Fun fun)
      [ hides [funx,funy] $
          h /\ funx .= Int i /\ funy .=. x
      | (i,h,x) <- zip3 [0..] fun xs
      ]
    | fun <- funs
    , length fun == length xs
    ]
  ]

sem (All e) r =
  squash
  [ bigUnion
    [ foldr (/\) (r .= Fun fun)
      [ hides xs $ env /\ (hides [funx,funy] $
                  h /\ funx .= Int i /\ funy .=. x)
      | (i,(h,x)) <- [0..] `zip` (fun `zip` xs)
      ]
    | (env,xs) <- combine [ (ren z (SVar zi) env, zi) | (env,zi) <- sem (Scope e) z `zip` zs ]
    , fun <- funs
    , length fun == length xs
    ]
  ]
 where
  z:zs = freshList (r : vars e)
 
sem (x := e) r =
  squash
  [ x .=. r /\ env
  | env <- sem e r
  ]
  
sem (e1 :>: e2) r =
  squash
  [ hide u env1 /\ env2
  | env1 <- sem e1 u
  , env2 <- sem e2 r
  ]
 where
  u = fresh (r : vars e1 ++ vars e2)

sem (e1 :|: e2) r =
  sem (Scope e1) r ++ sem (Scope e2) r

sem (If e1 e2 e3) r =
  squash $
  dodgyUnion
  [ [ hides ys (env1 /\ env2) | env2 <- sem e2 r ]
  , [ compl env1 /\ hides ys env3 | env3 <- sem e3 r ]
  ]
 where
  env1 = first ys (map (hide z) $ sem e1 z)
  z  = fresh (r : vars (If e1 e2 e3))
  ys = exis e1

sem (If2 e1 e2 e3) r =
  [ hides (exis e1) (d /\ d1) | d <- sem (Scope e2) r ] ++
  [ d \\\ d1 | d <- sem (Scope e3) r ]
 where
  d1 = hide z $ bigUnion $ sem e1 z
  z  = fresh (r : vars (If e1 e2 e3))
  
sem (Exi x) r =
  [ univ ]

sem (Scope e) r =
  [ hides (exis e) env
  | env <- sem e r
  ]

sem (f :@: x) r =
  squash $
  dodgyUnion
  [ [ f ?=? SVal (Fun fun) /\ apply h x (SVar r) | h <- fun ]
  | fun <- funs
  ]

semS :: SimpleExpr -> Ident -> [ENV]
semS (SVar x) r =
  [ x .=. r ]
  
semS (SVal k) r =
  [ r .= k ]


xfun :: FUN
xfun = 
         [ funx .= 0 /\ funy .= 1
         , funx .= 1 /\ funy .= 0
         ]

{-
funs :: [FUN]
funs = [ -- <1, 0>
         [ funx .= 0 /\ funy .= 1
         , funx .= 1 /\ funy .= 0
         ]
       , -- <0>
         [ funx .= 0 /\ funy .= 0
         ]
       , -- <1>
         [ funx .= 0 /\ funy .= 1
         ]
       , -- <2>
         [ funx .= 0 /\ funy .= 2
         ]
       , -- <7>
         [ funx .= 0 /\ funy .= 7
         ]
       , -- <1,2>
         [ funx .= 0 /\ funy .= 1
         , funx .= 1 /\ funy .= 2
         ]
       , -- <0,1>
         [ funx .= 0 /\ funy .= 0
         , funx .= 1 /\ funy .= 1
         ]
       , -- <1,2,3>
         [ funx .= 0 /\ funy .= 1
         , funx .= 1 /\ funy .= 2
         , funx .= 2 /\ funy .= 3
         ]
       ]
-}
funs :: [FUN]
funs = [ f | i <- [0..5]  -- up to 5-tuples
           , f <- map tup $ sequence $ replicate i [0..7]
       ]
  where tup xys = [ funx .= x /\ funy .= y | (x, y) <- zip [0..] xys ]

squash :: [ENV] -> [ENV]
squash envs = [ env | env <- envs, env /= empty ]

first :: [Ident] -> [ENV] -> ENV
first ys []         = empty
first ys (env:envs) = env \/ (compl (hides ys env) /\ first ys envs)

combine :: [(ENV,Ident)] -> [(ENV,[Ident])]
combine []              = [(univ,[])]
combine ((env,x):envxs) =
  squash' $
  [ (env /\ env', x:xs)
  | (env',xs) <- envxss
  ] ++
  [ (compl (hide x env) /\ env', xs)
  | (env',xs) <- envxss
  ]
 where
  envxss = combine envxs

squash' envxss = [ (env,xs) | (env,xs) <- envxss, env /= empty ]

----------------------------------------------------------------------------------------

bigUnion :: [ENV] -> ENV
bigUnion = foldr (\/) empty

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (/\) univ

dodgyUnion :: [ [ENV] ] -> [ENV]
dodgyUnion []    = []
dodgyUnion envss = bigUnion [ env | env:_ <- envss ]
                 : dodgyUnion [ envs | _:envs <- envss, not (null envs) ]

----------------------------------------------------------------------------------------

x, y, z, r :: Ident
x = Id "x"
y = Id "y"
z = Id "z"
r = Id "r"

ex1 = Tup [x, y]
ex2 = (z := ex1) :>: (SVar z :@: SVar x)
ex3   = (y := If  (x := 1) 5 7) :>: (y := 7)
ex3_2 = (y := If2 (x := 1) 5 7) :>: (y := 7)
ex4 = All ((x := 1) :|: (y := 7))

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



ex5   = All (Exi x :>: If  (x:=0) (1:|:2) (3:|:4:|:5))
ex5_2 = All (Exi x :>: If2 (x:=0) (1:|:2) (3:|:4:|:5))
ex6   = Scope (Exi x :>: If  ((x:=0):|:(x:=0)) 1 5)
ex6_2 = Scope (Exi x :>: If2 ((x:=0):|:(x:=0)) 1 5)
ex7   = Scope (Exi x :>: If  ((x:=0):|:(x:=0)) (Var x) 5)
ex7_2 = Scope (Exi x :>: If2 ((x:=0):|:(x:=0)) (Var x) 5)
ex8   = Scope (Exi x :>: x:=0 :>: Exi y :>: If  ((x:=0 :>: y:=1):|:(x:=0 :>: y:=2)) (Var y) 5)
ex8_2 = Scope (Exi x :>: x:=0 :>: Exi y :>: If2 ((x:=0 :>: y:=1):|:(x:=0 :>: y:=2)) (Var y) 5)

ex9 =   Scope (Exi x :>: x:=0 :>: If  ((x:=0):|:(x:=0)) 1 5)
ex9_2 = Scope (Exi x :>: x:=0 :>: If2 ((x:=0):|:(x:=0)) 1 5)

chkNE :: Expr -> Expr -> IO ()
chkNE e1 e2 = if sem e1 r == sem e2 r then putStrLn $ "unexpected equal " ++ show (e1, e2) else return ()

chkEQ :: Expr -> Expr -> IO ()
chkEQ e1 e2 = if sem e1 r /= sem e2 r then putStrLn $ "unexpected not equal " ++ show (e1, e2) else return ()

main = do
  chkEQ ex3 ex3_2
  chkNE ex5 ex5_2
  chkNE ex6 ex6_2
  chkNE ex7 ex7_2
  chkNE ex8 ex8_2
  chkEQ ex9 ex9_2
