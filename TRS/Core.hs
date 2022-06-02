{-# LANGUAGE PatternSynonyms #-}
module Core where

import Show
import TRS
import Bind
import Data.List( intercalate, union )
import Data.Maybe

--------------------------------------------------------------------------------

-- TODO: Lambda's (left out for now to get a first simple version)

data Expr
  = Val Value
  | Expr :=: Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | Value :@: Value
  | Fail
  | Def (Bind Expr)
  | One Expr
  | All Expr
 deriving ( Eq, Ord )

instance Show Expr where
  show (Val v)          = show v
  show (a :=: b)        = show' a ++ " = " ++ show' b
  show (a :>: b)        = show' a ++ "; " ++ show' b
  show (a :|: b)        = show' a ++ " | " ++ show' b
  show (a :@: b)        = show a ++ "@" ++ show b
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
-- patterns

-- Expr
pattern VAR v  = Val (Var v)
pattern INT n  = Val (VINT n)
pattern ARR vs = Val (VARR vs)

-- Value
pattern VINT n  = HNF (Int n)
pattern VARR vs = HNF (Arr vs)
pattern ADD     = HNF (Op Add)
pattern GRT     = HNF (Op Gt)
pattern IsINT   = HNF (Op IsInt)

--------------------------------------------------------------------------------

instance Rec Expr where
  rec r (a :=: b)        = [ a' :=: b | a' <- r a ] ++ [ a :=: b' | b' <- r b ]
  rec r (a :|: b)        = [ a' :|: b | a' <- r a ] ++ [ a :|: b' | b' <- r b ]
  rec r (a :>: b)        = [ a' :>: b | a' <- r a ] ++ [ a :>: b' | b' <- r b ]
  rec r (Def (Bind x a)) = [ Def (Bind x a') | a' <- r a ]
  rec r (One a)          = [ One a' | a' <- r a]
  rec r (All a)          = [ All a' | a' <- r a]
  rec r _                = []

{-
recAssoc :: (Expr -> [Expr]) -> Expr -> [Expr]
recAssoc r e =
     [ a' :=: b  | a :=: b <- es, a' <- r a ]
  ++ [ a  :=: b' | a :=: b <- es, b' <- r b ]
  ++ [ a' :>: b  | a :>: b <- es, a' <- r a ]
  ++ [ a  :>: b' | a :>: b <- es, b' <- r b ]
  ++ [ a' :|: b  | a :|: b <- es, a' <- r a ]
  ++ [ a  :|: b' | a :|: b <- es, b' <- r b ]
 where
  es = assoc e

-- normalizes associative operators on top-level
norm :: Expr -> Expr
norm ((a :=: b) :=: c) = norm (a :=: (b :=: c))
norm ((a :>: b) :>: c) = norm (a :>: (b :>: c))
norm ((a :|: b) :|: c) = norm (a :|: (b :|: c))
norm (a :=: b)         = a :=: norm b
norm (a :>: b)         = a :>: norm b
norm (a :|: b)         = a :|: norm b
norm a                 = a

-- mangles associative operators on top-level
assocs :: Expr -> [Expr]
assocs e@(a :=: (b :=: c)) = e : assocs ((a :=: b) :=: c)
assocs e@(a :>: (b :>: c)) = e : assocs ((a :>: b) :>: c)
assocs e@(a :|: (b :|: c)) = e : assocs ((a :|: b) :|: c)
assocs e                   = [e]

-- matcher to use for associative operators on top-level
assoc :: Expr -> [Expr]
assoc = assocs . norm
-}

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

{-
-- not using the "bind" trick for now
instance Binding Expr where
  binders (a :=: b) = binders a ++ binders b
  binders (a :>: b) = binders a ++ binders b
  binders (a :|: b) = binders a ++ binders b
  binders (a :@: b) = binders a ++ binders b
  binders (Def bnd) = [bnd]
  binders (One a)   = binders a
  binders (All a)   = binders a
  binders _         = []
-}

--------------------------------------------------------------------------------

class Term a where
  subst :: Subst Value -> a -> a

instance Term Value where
  subst sub (Var x) = fromMaybe (Var x) (lookup x sub)
  subst sub (HNF a) = HNF (subst sub a)

instance Term HNF where
  subst sub (Arr vs) = Arr (map (subst sub) vs)
  subst sub a        = a

instance Term Expr where
  subst sub (Val v)   = Val (subst sub v)
  subst sub (a :=: b) = subst sub a :=: subst sub b
  subst sub (a :>: b) = subst sub a :>: subst sub b
  subst sub (a :|: b) = subst sub a :|: subst sub b
  subst sub (a :@: b) = subst sub a :@: subst sub b
  subst sub Fail      = Fail
  subst sub (Def bnd) = Def (substBind Var subst sub bnd)
  subst sub (One a)   = One (subst sub a)
  subst sub (All a)   = All (subst sub a)

--------------------------------------------------------------------------------

-- TODO: Arbitrary instances

--------------------------------------------------------------------------------


