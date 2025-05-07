module Sem where

import Data.List( partition )
import Control.Monad.State

--------------------------------------------------------------------------------

data Ident
  = Internal [Bool] Int
  | External Int
 deriving ( Eq, Ord, Show )

type SetIdent = Int

single :: Ident -> SetIdent
single (Internal _ _) = 0
single (External v)   = v

union :: [SetIdent] -> SetIdent
union ws = maximum (0:ws)

notIn :: SetIdent -> Ident
notIn v = External (vs+1)

inspect :: (Ident -> a) -> a
inspect f = f (External 0)

--------------------------------------------------------------------------------

class Free a where
  free :: a -> SetIdent

instance Free () where
  free _ = 0

instance (Free a, Free b) => Free (a,b) where
  free (x,y) = union [free x, free y]

instance Free a => Free [a] where
  free xs = union [ free x | x <- xs ]

--------------------------------------------------------------------------------

data WithIdent a = SetIdent :- a

make :: Free a => a -> WithIdent a
make x = free x :- x

instance Applicative WithIdent where
  pure x                = 0 :- x
  (v :- f) <$> (w :- x) = union [v,w] :- f x

instance Free (WithIdent a) where
  free (vs :- _) = vs

--------------------------------------------------------------------------------

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
  
instance Free Val where
  free (Var k)  = single k
  free (Tup vs) = union [ free v | v <- vs ]
  free (Fun f)  = free (inspect (f . Var))
  free _        = 0

--------------------------------------------------------------------------------

type Exp = WithIdent [Set Val]

fail :: Exp
fail = make []

val :: Val -> Exp
val v = make [unit v]

(.|.) :: Exp -> Exp -> Exp
e1 .|. e2 = pure (++) <$> e1 <$> e2

(.=.) :: Exp -> Exp -> Exp
e1 .=. e2 = pure (/\) <$> e1 <$> e2

(.>.) :: Exp -> Exp -> Exp
e1 .>. e2 = pure (?>) <$> e1 <$> e2

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
  | Constr [([Ident],a)] a

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

data View a
  = Nil
  | Single a
  | Multi

view :: Set Val -> View Val
view = undefined

--

newtype U a = U (Heap -> Maybe (Heap, a))

instance Applicative U where
  pure x        = U (\h -> Just (h, x))
  U uf <$> U ux = U (\h -> do (h',f) <- uf h
                              f (ux h'))

instance Functor U where
  fmap f = (pure f <$>)

instance Monad U where
  return     = pure
  U um >>= k = U (\h -> do (h',x) <- um h
                           let U um' = k x
                           um' h')

instance MonadFail U where
  fail _ = U (\_ -> Nothing)

type Heap = Map Ident Val

peek :: Ident -> U (Maybe Val)
peek x = U (\h -> Just (h, M.lookup x h))

store :: Ident -> Val -> U ()
store x t = U (\h -> Just (M.insert x t h, ())

point :: Ident -> Val -> U ()
point x t =
  do mt <- peek x
     case mt of
       Nothing ->
         do store x t
             
       Just t' ->
         do unify t' t
            return ()

unify :: Val -> Val -> U Val
unify (Int a) (Int b) | a == b =
  do return (Int a)

unify (Tup []) (Tup []) =
  do return (Tup [])

unify (Tup (v:vs)) (Tup (w:ws)) =
  do Tup rs <- unify (Tup vs) (Tup ws)
     r      <- unify v w
     return (Tup (r:rs))

unify (Var x) t =
  do point x t
     return (Var x)

unify t (Var x) =
  unify (Var x) t

unify _ _ =
  Nothing


