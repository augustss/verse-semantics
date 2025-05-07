module Sem where

import Data.List( partition )

type Ident = Int

type Exp = (Ident, [Set Val])

data Val
  = Int Integer
  | Tup [Val]
  | Fun (Val -> Exp)
  | Var Int
  | Op Op

data Op
  = GT
  | ADD
  | INT
 deriving ( Eq, Ord, Show )
  
free :: Val -> Ident
free (Var k)  = k
free (Tup vs) = maximum [ free v | v <- vs ]
free (Fun f)  = let (i,_) = f (Var 0) in i
free _        = 0

fail :: Exp
fail = (0, [])

val :: Val -> Exp
val v = (free v, [unit v])

(.|.) :: Exp -> Exp -> Exp
(i,ps) .|. (j,qs) = (i `max` j, ps ++ qs)

(.=.) :: Exp -> Exp -> Exp
(i,ps) .=. (j,qs) = (i `max` j, [ p /\ q | p <- ps, q <- qs ])

(.>.) :: Exp -> Exp -> Exp
(i,ps) .>. (j,qs) = (i `max` j, [ p >! q | p <- ps, q <- qs ])

(.@.) :: Val -> Val -> Exp
t@(Tup vs) .@. Int i = (free t, [ unit (vs!!j) | 0 <= j, j < length vs ]) where j = fromIntegral i
Fun f      .@. v     = f v
_          .@. _     = error "WRONG"

one :: Exp -> Exp
one (i,ps) = (i,first ps)
 where
  first [] = []
  first (p:ps) = 
    case view p of
      Nil      -> first ps
      Single x -> [unit x]
      Multi    -> error "WRONG"

alL :: Exp -> Exp
alL (i,ps) = (i,[unit (Tup (tup ps))])
 where
  tup []     = []
  tup (p:ps) =
    case view p of
      Nil      -> tup ps
      Single x -> x : tup ps
      Multi    -> error "WRONG"
{-
(.\/.) :: Exp -> Exp -> Exp
[]     .\/. []     = []
ps     .\/. []     = ps .\/. [emp]
[]     .\/. qs     = [emp] .\/. qs
(p:ps) .\/. (q:qs) = (p \/ q) : (ps .\/. qs)
-}

exists :: (Val -> Exp) -> Exp
exists f = (v, [ exi x p | p <- ps ])
 where
  (x0,_) = f (Var 0)
  x      = x0+1
  (v,ps) = f (Var x)

---

data Set a
  = Empty
  | Constr [Ident] [([Ident],a)] a

unit :: a -> Set a
unit x = Constr [] [] x

emp :: Set a
emp = Empty

(/\) :: Set Val -> Set Val -> Set Val
Empty /\ _     = Empty
_     /\ Empty = Empty
Constr vs as x /\ Constr ws bs y = undefined

(>!) :: Set a -> Set a -> Set a
(>!) = undefined

exi :: Ident -> Set a -> Set a
exi x Empty            = Empty
exi x (Constr vs as y) = Constr (x:vs) as y

unify :: Val -> Val -> Maybe ([([Ident],Maybe Val)], Val)
unify (Int a) (Int b) | a == b =
  Just ([], Int a)

unify (Tup []) (Tup []) =
  Just ([], Tup [])

unify (Tup (v:vs)) (Tup (w:ws)) =
  do (cs1,Tup rs) <- unify (Tup vs) (Tup ws)
     (cs2,r)      <- unify v w
     cs           <- unifyConstrs cs2 cs1
     return (cs,Tup (r:rs))

unify (Var x) (Var y) =
  Just ([([x,y],Nothing)], Var x)

unify t (Var x) =
  unify (Var x) t

unify (Var x) t =
  Just ([([x],t)],Var x)

unify _ _ =
  Nothing

unifyList :: [Val] -> Maybe ([([Ident],Maybe Val)], Val)
unifyList [v]    = Just ([],v)
unifyList (v:vs) =
  do (cs2,w) <- unifyList vs
     (cs1,r) <- unify v w
     cs      <- unifyConstrs cs2 cs1
     return (cs,r)

unifyConstrs :: [([Ident],Maybe Val)] -> [([Ident],Maybe Val)] -> Maybe [([Ident],Maybe Val)]
unifyConstrs [] cs2 =
  do return cs2
  
unifyConstrs ((xs,mv):cs1) cs2 =
  do (cs',r) <- unifyList [ v | Just v <- mv : [ mv | (_,mv) <- cs2L ] ]
     
 where
  (cs2L,cs2R) = partition (\(ys,_) -> any (`elem` ys) xs) cs2
  

data View a
  = Nil
  | Single a
  | Multi

view :: Set Val -> View Val
view = undefined

