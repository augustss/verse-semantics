module Main where

import Data.List
import qualified Data.Set as S

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------

data Expr
  = Const Integer
  | Var Ident
  | Tup [Expr]
  | Exi Ident Expr
  | Expr :=: Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | Expr :@: Expr
  | Fail
  | All Expr
  | One Expr

  -- concrete functions
  | F -- fun(x:=1|2){3-x}
  | G -- fun(x:int where 1<=x<=2){3-x}
 deriving ( Eq, Ord )

instance Show Expr where
  show (Const k)   = show k
  show (Var x)     = show x
  show (Tup es)    = "<" ++ intercalate "," (map show es) ++ ">"
  show (Exi x e)   = "∃" ++ show x ++ "." ++ show e
  show (e1 :=: e2) = show1 ""  e1 ++ "=" ++ show1 ""  e2
  show (e1 :>: e2) = show1 ";" e1 ++ ";" ++ show1 ";" e2
  show (e1 :|: e2) = show1 "|" e1 ++ "|" ++ show1 "|" e2
  show (e1 :@: e2) = show1 "@" e1 ++ "[" ++ show e2 ++ "]"
  show Fail        = "fail"
  show (All e)     = "all{" ++ show e ++ "}"
  show (One e)     = "one{" ++ show e ++ "}"
  show F           = "F(x:=1|2){3-x}"
  show G           = "G(x:int where 1<=x<=2){3-x}"

show1 :: String -> Expr -> String
show1 _  e@(Exi _ _) = showp e
show1 op e@(_ :>: _) = if op==";" then show e else showp e
show1 op e@(_ :|: _) = if op=="|" then show e else showp e
show1 _  e           = show e

showp :: Expr -> String
showp e = "(" ++ show e ++ ")"

free :: Expr -> [Ident]
free (Var x)     = [x]
free (Tup es)    = foldr union [] (map free es)
free (Exi x e)   = free e \\ [x]
free (e1 :=: e2) = free e1 `union` free e2
free (e1 :>: e2) = free e1 `union` free e2
free (e1 :|: e2) = free e1 `union` free e2
free (e1 :@: e2) = free e1 `union` free e2
free (All e)     = free e
free (One e)     = free e
free _           = []

----------------------------------------------------------------------------------------

newtype Set a = Set (S.Set a)
 deriving ( Eq, Ord )

instance Show a => Show (Set a) where
  show (Set s) = "{" ++ intercalate "," (map show (S.toList s)) ++ "}"

set :: Ord a => [a] -> Set a
set xs = Set (S.fromList $ xs)

from :: Set a -> [a]
from (Set s) = S.toList s

(\/) :: Ord a => Set a -> Set a -> Set a
Set s1 \/ Set s2 = Set (s1 `S.union` s2)

----------------------------------------------------------------------------------------

flat :: Ord a => [Set [a]] -> Set [a]
flat []       = set[ [] ]
flat (s:sets) = set[ xs++ys | xs <-from$ s, ys <-from$ flat sets ]

----------------------------------------------------------------------------------------

data Pair a b
  = a :-> b
 deriving ( Eq, Ord )

instance (Show a, Show b) => Show (Pair a b) where
  show (x :-> y) = show x ++ "->" ++ show y

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun [Pair Value Value]
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k)    = show k
  show (Fun xys)
    | and [ x == Int i | (i,x:->_) <- [0..] `zip` xys ] = 
        "<" ++ intercalate "," [ show y | (_:->y) <- xys ] ++ ">"

    | otherwise =
        show xys

instance Num Value where
  fromInteger n = Int n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

tup :: [Value] -> Value
tup ys = Fun [ Int i :-> y | (i,y) <- [0..] `zip` ys ]

----------------------------------------------------------------------------------------

univ :: Set Value
univ = set$[ Int i | i <- [1..3] ] ++ [ Fun[1:->2,2:->1] , Fun[2:->1,1:->2] ]

type Env = Set (Pair Ident Value)

(?) :: Env -> Ident -> Value
env ? x = head $ [ v | (y :-> v) <-from$ env, y==x ] ++ error (show x ++ " not in " ++ show env)

del :: Ident -> Env -> Env
del x env = set[ p | p@(y:->_) <-from$ env, y/=x ]

(~=) :: Env -> Env -> Bool
env1 ~= env2 = and [ v==w | (x:->v) <-from$ env1, (y:->w) <-from$ env2, x==y ]

----------------------------------------------------------------------------------------

fuse :: Ord a => Set [a] -> [Set a]
fuse s = [ set[ x | xs <-from$ s, x:_ <- [drop i xs] ] | i <- [0..m-1] ]
 where
  m = maximum [ length xs | xs <-from$ s ]

sequ :: Ord a => [Set a] -> Set [a]
sequ []     = set[ [] ]
sequ (s:ss) = set[ x:xs | x <-from$ s, xs <-from$ sequ ss ]

----------------------------------------------------------------------------------------

sem :: Expr -> Set [(Env,Value)]
sem (Const k) =
  set[ [ (set[], Int k) ]
     ]

sem (Var x) =
  set[ [ (set[x:->v], v) ]
     | v <-from$ univ
     ]

sem (Tup []) =
  set[ [ (set[], Fun []) ]
     ]

sem (Tup (u:us)) =
  set[ [ (env1 \/ env2, Fun((Int 0:->v):[Int (i+1):->w|(Int i:->w)<-ivs])) ]
     | [(env1,v)]       <-from$ sem u
     , [(env2,Fun ivs)] <-from$ sem (Tup us)
     , env1 ~= env2
     ]


sem (Exi x e) =
  set[ [ (del x env, v) | (env,v) <-evs ]
     | evs <-from$ sem e
     ]

sem (Var x :=: e) =
  set[ [ (env \/ envx,v)
       | (env,v) <- evs
       , let envx = set[x:->v]
       , env ~= envx
       ]
    | evs <-from$ sem e
    ]

sem (e1 :|: e2) =
  set[ evs1 ++ evs2
     | evs1 <-from$ sem e1
     , evs2 <-from$ sem e2
     ]

sem (e1 :>: e2) =
  sequ $
  map react $
  fuse $
  set[ [ ((env1, env2), v)
       | (env1,_) <- s1
       , (env2,v) <- s2
       ]
     | s1 <-from$ sem e1
     , s2 <-from$ sem e2
     ]
 where
  react :: Set ((Env,Env),Value) -> Set (Env,Value)
  react u = set[ (env1 \/ env2, v)
               | ((env1,env2),v) <-from$ u
               , env1 ~= env2
               ]

{-
[Set a] -> Set [b] -> Set [(a,b)]

[Set a] ->
[Set (a,[b])] ->
[Set [(a,b)]] ->
Set [(a,b)]
-}
{-
    sequ $
    filter (not . null . from) $
    [ set[ (env1 \/ env2,v)
         | (env1,_) <-from$ s
         , (env2,v) <-from$ r
         , env1 ~= env2
         ]
    | s <- fuse (sem e1) -- ss
    , r <- fuse (sem e2) -- rs
    ]
-}
{-
  sequ (fuse (sem e1) `seq2` fuse (sem e2))
 where
  seq2 :: [Set (Env,Value)] -> [Set (Env,Value)] -> [Set (Env,Value)]
  ss `seq2` rs =
    filter (not . null . from)
    [ set[ (env1 \/ env2,v)
         | (env1,_) <-from$ s
         , (env2,v) <-from$ r
         , env1 ~= env2
         ]
    | s <- ss
    , r <- rs
    ]
-}

sem fun | fun `elem` [F,G] =
  set[ [ (set[], Fun [Int i :-> Int (3-i) | i <- dom]) ]
     | dom <- [1,2] : [ [2,1] | fun == G ]
     ]

sem (Var f :@: Var x) =
  set[ [ (set[f:->fun,x:->a],y)
       | (a:->y) <- xys
       ]
     | fun@(Fun xys) <-from$ univ
     ]

{-
sem (One e) scope =
  set[ [(env,v) | env <- envs, v:_ <- [[ v | (env',v)<-s, env'==env ]] ]
     | s <-from$ sem e scope
     , envs <-from$ univEnvs scope
     ]

sem (All e) scope =
  set[ [(env, tup [ v | (env',v)<-s, env'==env ]) | env <- envs]
     | s <-from$ sem e scope
     , envs <-from$ univEnvs scope
     ]
-}

sem e =
  error ("no semantics yet for " ++ show e)

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn ("univ = " ++ show univ)
     putStrLn ""
     sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Expr -> IO ()
printSem e =
  do putStrLn ("> " ++ show e)
     putStrLn ("--> " ++ show (sem e))

----------------------------------------------------------------------------------------

x,y,z,f,g :: Expr
x = Var (Ident "x")
y = Var (Ident "y")
z = Var (Ident "z")
f = Var (Ident "f")
g = Var (Ident "g")

exi :: Expr -> Expr -> Expr
exi (Var x) e = Exi x e

instance Num Expr where
  fromInteger n = Const n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

examples :: [Expr]
examples =
  [ y:>:(x:=:(1:|:2))
  , (y:>:(x:=:(1:|:2))) :>: (y:=:(2:|:3))
  ] ++
  [ exi g $ (g :=: G) :>: (x :=: 1) :>: (g :@: x)
  ]
 where
  funExamples f fun =
    [ exi f $ (f :=: fun) :>: f
    , exi f $ (f :=: fun) :>: (f :@: x)
    , exi f $ (f :=: fun) :>: (x :=: 1) :>: (f :@: x)
    , exi f $ (f :=: fun) :>: (exi x $ f :@: x)
    , exi x $ exi f $ (f :=: fun) :>: (f :@: x)
    ]
  

{-
  , 1 :|: 2
  , x :=: (1 :|: 2)
  -- , (1 :|: 2) :=: x
  -- , (2 :|: 1) :=: (1 :|: 2 :|: 3)
  , (x :=: (1 :|: 2)) :>: (x :=: y)
  , exi x $ (x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 2:|: 3))

  , One x
  , All x
  , One ((x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 3)))
  , All ((x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 2 :|: 3)))
  , All ((x :=: (2 :|: 1)) :>: ((x :=: 1) :|: 2 :|: 2 :|: 3))
-}
  
----------------------------------------------------------------------------------------
