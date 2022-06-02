module Core where

import Show
import TRS
import Bind
import Data.List( intercalate, union )

--------------------------------------------------------------------------------

-- TODO: Lambda's (left out for now to get a first simple version)

data Expr
  = Val Value
  | Expr :=: Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | Expr :@: Expr
  | Fail
  | Def (Bind Expr)
  | One Expr
  | All Expr
 deriving ( Eq, Ord )

instance Show Expr where
  show (Val v)          = show v
  show (a :=: b)        = show' a ++ " = " ++ show b
  show (a :>: b)        = show' a ++ "; " ++ show' b
  show (a :|: b)        = show' a ++ " | " ++ show' b
  show (a :@: b)        = show' a ++ "@" ++ show' b
  show Fail             = "fail"
  show (Def (Bind x a)) = "def " ++ show x ++ " in {" ++ show a ++ "}"
  show (One a)          = "one {" ++ show a ++ "}"
  show (All a)          = "all {" ++ show a ++ "}"

instance Parens Expr where
  parens (_ :=: _) = True
  parens (_ :>: _) = True
  parens (_ :|: _) = True
  parens (_ :@: _) = True
  parens _         = False

--------------------------------------------------------------------------------

data Value
  = Var Ident
  | HNF HNF
 deriving ( Eq, Ord )

data HNF
  = Int Integer
  | Op Op
  | Arr [Value]
 deriving ( Eq, Ord )

data Op
  = Gt
  | Add
  | IsInt
 deriving ( Eq, Ord )

instance Show Value where
  show (Var x) = show x
  show (HNF a) = show a

instance Show HNF where
  show (Int k)  = show k
  show (Op op)  = show op
  show (Arr vs) = "arr{" ++ intercalate ", " (map show vs) ++ "}"

instance Show Op where
  show Gt    = "gt"
  show Add   = "add"
  show IsInt = "isInt"

--------------------------------------------------------------------------------

instance Rec Expr where
  rec r (a :=: b)        = [ a' :=: b | a' <- r a ] ++ [ a :=: b' | b' <- r b ]
  rec r (a :>: b)        = [ a' :>: b | a' <- r a ] ++ [ a :>: b' | b' <- r b ]
  rec r (a :|: b)        = [ a' :|: b | a' <- r a ] ++ [ a :|: b' | b' <- r b ]
  rec r (a :@: b)        = [ a' :@: b | a' <- r a ] ++ [ a :@: b' | b' <- r b ]
  rec r (Def (Bind x a)) = [ Def (Bind x a') | a' <- r a ]
  rec r (One a)          = [ One a' | a' <- r a]
  rec r (All a)          = [ All a' | a' <- r a]
  rec r _                = []

--------------------------------------------------------------------------------

instance Free Expr where
  free (Val v)   = free v
  free (a :=: b) = free a `union` free b
  free (a :>: b) = free a `union` free b
  free (a :|: b) = free a `union` free b
  free (a :@: b) = free a `union` free b
  free (Def bnd) = free bnd
  free (One a)   = free a
  free (All a)   = free a
  free _         = []

instance Free Value where
  free (Var x) = [x]
  free (HNF a) = free a

instance Free HNF where
  free (Arr vs) = free vs
  free _        = []

instance Binding Expr where
  binders (a :=: b) = binders a ++ binders b
  binders (a :>: b) = binders a ++ binders b
  binders (a :|: b) = binders a ++ binders b
  binders (a :@: b) = binders a ++ binders b
  binders (Def bnd) = [bnd]
  binders (One a)   = binders a
  binders (All a)   = binders a
  binders _         = []

--------------------------------------------------------------------------------

-- TODO: Arbitrary instances

--------------------------------------------------------------------------------


