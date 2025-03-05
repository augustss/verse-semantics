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
  | Fail
  | All Expr
  | One Expr
 deriving ( Eq, Ord )

instance Show Expr where
  show (Const k)   = show k
  show (Var x)     = show x
  show (Tup es)    = "<" ++ intercalate "," (map show es) ++ ">"
  show (Exi x e)   = "∃" ++ show x ++ "." ++ show e
  show (e1 :=: e2) = show1 ""  e1 ++ "=" ++ show1 ""  e2
  show (e1 :>: e2) = show1 ";" e1 ++ ";" ++ show1 ";" e2
  show (e1 :|: e2) = show1 "|" e1 ++ "|" ++ show1 "|" e2
  show Fail        = "fail"
  show (All e)     = "all{" ++ show e ++ "}"
  show (One e)     = "one{" ++ show e ++ "}"

show1 :: String -> Expr -> String
show1 _  e@(Exi _ _) = showp e
--show1 op e@(_ :=: _) = show e
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
  show (Int k)   = show k
  show (Fun xys) = show xys

----------------------------------------------------------------------------------------

univ :: Set Value
univ = set[ Int i | i <- [1..3] ]

type Env = Set (Pair Ident Value)

(?) :: Env -> Ident -> Value
env ? x = head [ v | (y :-> v) <-from$ env, y==x ]

univEnv :: [Ident] -> Set Env
univEnv []     = set[ set[] ]
univEnv (x:xs) = set[ set ((x :-> v):from env) | v <-from$ univ, env <-from$ univEnv xs ]

(~=) :: Env -> Env -> Bool
env1 ~= env2 = and [ v==w | (x:->v) <- from env1, (y:->w) <- from env2, x==y ]

----------------------------------------------------------------------------------------

sem :: Expr -> Set [(Env,Value)]
sem (Const k) =
  set[ [ (set[], Int k) ] ]

sem (Var x) =
  set[ [ (set[ x:->v ], v) ]
     | v <-from$ univ
     ]

sem (e1 :|: e2) =
  set[ s1++s2
     | s1 <-from$ sem e1
     , s2 <-from$ sem e2
     ]

sem (e1 :=: e2) =
  set[ s3
     | s1 <-from$ sem e1
     , s3 <-from$ flat [ set[ [ (env1 \/ env2,v2) | (env2,v2)<-s2, env1 ~= env2, v1==v2 ]
                            | s2 <-from$ sem e2
                            ]
                       | (env1,v1) <- s1
                       ]
     ]

sem (e1 :>: e2) =
  set[ s3
     | s1 <-from$ sem e1
     , s3 <-from$ flat [ set[ [ (env1 \/ env2,v2) | (env2,v2)<-s2, env1 ~= env2 ]
                            | s2 <-from$ sem e2
                            ]
                       | (env1,_) <- s1
                       ]
     ]


sem _ =
  set[]

{-
sem :: Expr -> [Ident] -> Set [(Env,Value)]
sem (Const k) scope =
  set[ [ (env, Int k) ]
     | env <-from$ univEnv scope
     ]

sem (Var x) scope =
  set[ [ (env, env ? x) ]
     | env <-from$ univEnv scope
     ]

sem (e1 :|: e2) scope =
  set[ s1++s2
     | s1 <-from$ sem e1 scope
     , s2 <-from$ sem e2 scope
     ]

sem (e1 :=: e2) scope =
  set[ s3
     | s1 <-from$ sem e1 scope
     , s3 <-from$ flat [ set[ [ (env2,v2) | (env2,v2)<-s2, env1==env2, v1==v2 ]
                            | s2 <-from$ sem e2 scope
                            ]
                       | (env1,v1) <- s1
                       ]
     ]

sem (e1 :>: e2) scope =
  set[ s3
     | s1 <-from$ sem e1 scope
     , s3 <-from$ flat [ set[ [ (env2,v2) | (env2,v2)<-s2, env1==env2 ]
                            | s2 <-from$ sem e2 scope
                            ]
                       | (env1,_) <- s1
                       ]
     ]

sem _ _ =
  set[]
-}

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Expr -> IO ()
printSem e =
  do putStrLn ("> " ++ show e)
     --putStrLn ("--> " ++ show (sem e (free e)))
     putStrLn ("--> " ++ show (sem e))

----------------------------------------------------------------------------------------

x,y,z :: Expr
x = Var (Ident "x")
y = Var (Ident "y")
z = Var (Ident "z")

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
  [ 1
  , x
  , x :>: 1
  , x :=: 1
  , x :=: y

  , 1 :|: 2
  , x :=: (1 :|: 2)
  , (1 :|: 2) :=: x
  , (2 :|: 1) :=: (1 :|: 2 :|: 3)
  , (x :=: (1 :|: 2)) :>: (x :=: y)
  ]
  
----------------------------------------------------------------------------------------
