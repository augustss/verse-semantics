{-# LANGUAGE ViewPatterns #-}
module Bind where

{-
subst :: Name -> Expr -> Expr -> Expr
subst x e (App a b) = App (subst x e a) (subst x e b)
subst x e (Lam y b) = Lam y (subst x e b)
subst x e (Var y)
  | x == y          = e
  | Var y           = Var y
-}

type Name = String

data Bind a = BIND deriving ( Eq, Show, Ord )

bind :: Name -> a -> Bind a
bind = undefined

open :: [Name] -> Bind a -> (Name, a)
open = undefined

class Symbolic a where
  free :: a -> [Name]
  swap :: Name -> Name -> a -> a

instance Symbolic a => Symbolic (Bind a)

instance Symbolic Expr
  

data Expr
  = App Expr Expr
  | Lam (Bind Expr)
  | Var Name
 deriving ( Eq, Ord, Show )

subst :: Name -> Expr -> Expr -> Expr
subst x e (App a b) = App (subst x e a) (subst x e b)
subst x e (Lam (open env -> (y,b))) = Lam (bind y (subst x e b)) where env=x:free e
subst x e (Var y)
  | x == y          = e
  | otherwise       = Var y


