module Expr1 where

import TRS
import Bind
import Control.Monad( guard )
import Test.QuickCheck

--------------------------------------------------------------------------------

data Expr
  = Num Integer
  | Var Name
  | Expr :+: Expr
  | Let Expr (Bind Expr)
 deriving ( Show, Eq, Ord )

instance Rec Expr where
  rec r (a :+: b)              = [ a' :+: b | a' <- r a ]
                              ++ [ a :+: b' | b' <- r b ]
  rec r (Let a bnd@(Bind x b)) = [ Let a' bnd | a' <- r a ]
                              ++ [ Let a (Bind x b') | b' <- r b ]
  rec r _                      = []

instance Free Expr where
  free (a :+: b)   = free (a,b)
  free (Let a bnd) = free (a,bnd)
  free (Var x)     = [x]
  free _           = []

instance Binding Expr where
  binders (a :+: b)   = binders a ++ binders b
  binders (Let a bnd) = [ bnd ]
  binders _           = []

subst :: [(Name, Expr)] -> Expr -> Expr
subst sub (a :+: b)                        = subst sub a :+: subst sub b
subst sub (Let a bnd)                      = Let (subst sub a) (substBind Var subst sub bnd)
subst sub (Var y) | Just t <- lookup y sub = t
subst sub a                                = a

arbExpr :: Int -> [Name] -> Gen Expr
arbExpr n xs =
  frequency $
  [ (1, Var <$> elements xs) | not (null xs) ] ++
  [ (1, Num <$> arbitrary)
  , (n, (:+:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, Let <$> arbExpr n2 xs <*> arbBind n2 xs)
  ]
 where
  n2 = n `div` 2

arbBind :: Int -> [Name] -> Gen (Bind Expr)
arbBind n xs = Bind x <$> arbExpr n (x:xs) where x = varNotIn xs

instance Arbitrary Expr where
  arbitrary = sized (`arbExpr` [])
  
  shrink t =
    (case t of
       Num n                -> [ Num n' | n' <- shrink n ]
       a :+: b              -> [ a,b ]
       Let a bnd@(Bind x b) -> [ a, subst [(x,Num 1)] b, subst [(x,a)] b ]
       _                    -> [])
    ++ rec shrink t -- :-o !!

newtype OpenExpr = Open Expr deriving ( Eq, Ord, Show )

instance Arbitrary OpenExpr where
  arbitrary =
    do n <- choose (0,5::Int)
       Open <$> sized (`arbExpr` [ "x" ++ show i | i <- [1..n] ])
  
  shrink (Open t@(Let _ (Bind _ b))) = map Open (b : shrink t)
  shrink (Open t) = [ Open t' | t' <- shrink t ]

--------------------------------------------------------------------------------

assoc :: Rule Expr
assoc e =
  do (x :+: y) :+: z <- [e]
     pure (x :+: (y :+: z))

commut e =
  do x :+: y <- [e]
     guard (x > y)
     pure (y :+: x)
 ++
  do x :+: (y :+: z) <- [e]
     guard (x > y)
     pure (y :+: (x :+: z))

compute e =
  do Num i :+: Num j <- [e]
     pure (Num (i+j))

let_in e =
  do Let a (Bind x b) <- [e]
     guard (isValue a)
     pure (subst [(x,a)] b)
     
isValue (Num _) = True
isValue _       = False

trs = assoc +++ commut +++ compute +++ let_in

--------------------------------------------------------------------------------

t0 x y = (Var x :+: Num 1) :+: Var y
t1 x y = Let (t0 x y) $ bind $ \x -> (Var x :+: Var x)
t2     = Let (Num 3) $ bind $ \x -> Let (Num 4) $ bind $ \y -> t1 x y

--main = print (normalForms trs t2)

main = quickCheck prop

prop (Open t) =
  case normalFormsFuel 1000 trs t of
    n1 : n2 : _ -> whenFail (print n1 >> print n2) False
    _           -> property True

--------------------------------------------------------------------------------

