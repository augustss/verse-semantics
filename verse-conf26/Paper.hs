{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Paper where
import Prelude hiding((++), map, concat, pi)
import Control.Applicative(Alternative)
import Data.Kind
import GHC.Exts(IsList(..))

infix 2 ==>
(==>) :: Bool -> Bool -> Bool
(==>) = (<=)

type Z = Integer

----- Term -----
data Term
  = Var Iden
  | Int Z
  -- prim
  | Iden := Term
  | Iden :-> Term
  | Array [Term]
  | Fail
  | Term :|: Term
  | Term :|||: Term
  | Term :=: Term
  | Term :> Term
  | Term :.. Term
  | Term :@ Term
  | Col Term   -- :t
  | If Term Term Term
  | For Term Term
  | Fun Term Apt Fx Term

data Apt = O | C
data Fx = Succeeds | Decides

fresh :: [Term] -> [Iden]
fresh = undefined

dI :: Term -> Set Iden
dI = undefined

----- Val -----
data Val m = I Z | F (Fn m)
data Fn m = Fn (m (Val m, Val m))
allVals :: Set(Val m)
allVals = undefined
instance Eq (Val m)

----- Set -----
-- [ ] can be used for constants and comprehensions
type Set :: Type -> Type
data Set a
instance Functor Set
instance Applicative Set
instance Monad Set
instance Alternative Set
instance MonadFail Set
(∉) :: a -> Set a -> Bool
a ∉ as = undefined
ø :: Set a
ø = undefined
(∪) :: Set(a) -> Set(a) -> Set(a)
(∪) = undefined
(∩) :: Set(a) -> Set(a) -> Set(a)
(∩) = undefined
mkSet :: [a] -> Set a
mkSet = undefined
instance IsList (Set a) where
  type Item (Set a) = a
  fromList = mkSet

----- Iden -----
data Iden = U_ | Iden String
forAllIden :: (Iden -> Bool) -> Bool  -- 
forAllIden _p = undefined

----- Env -----
type Env m = Iden -> Val m

allEnvs :: Set(Env m)    -- set of all environments
allEnvs = undefined

----- ENV, constraints -----
type ENV m = Set (Env m)
cempty :: ENV m
cempty = undefined
univ :: ENV m
univ = undefined
(.=.) :: Iden -> Iden -> ENV m
(.=.) = undefined
(.=) :: Iden -> Val m -> ENV m
(.=) = undefined
infixl 3 /\
(/\) :: ENV m -> ENV m -> ENV m
(/\) = undefined
infixl 2 \/
(\/) :: ENV m -> ENV m -> ENV m
(\/) = undefined
compl :: ENV m -> ENV m
compl = undefined


-- Verse computation type
type Comp :: (Type -> Type) -> Constraint
class Comp m where
  empty    :: m(a)
  return   :: a -> m(a)
  inj      :: Set(a) -> m(a)
  (++)     :: m(a) -> m(a) -> m(a)
  (/*\)    :: m(a) -> m(a) -> m(a)
  (\*/)    :: m(a) -> m(a) -> m(a)
  (\./)    :: m(a) -> m(a) -> m(a)
  union    :: Set (m(a)) -> m(a)
  map      :: (a -> b) -> m(a) -> m(b)
  flatten  :: m (Set(a)) -> m(a)
  fold     :: (a -> m(b) -> m(b)) -> m(b) -> m(a) -> m(a)
  pi       :: m(a) -> (a -> Set a) -> Set (m (a, b))
  collapse :: m(a) -> Set(a)
  not      :: m(a) -> m(a)

-- Derived functions
flatmap :: Comp m =>
           (a -> Set(b)) -> m(a) -> m(b)
flatmap f s = flatten (map f s)

(\.) :: Env m -> Set(Iden) -> Set(Env m)
𝜌 \. vs = [ 𝜌' | 𝜌' <- allEnvs, forAllIden (\ x -> x ∉ vs ==> 𝜌(x) == 𝜌'(x)) ]
(\\) :: Comp m => m(Env m) -> Set(Iden) -> m(Env m)
s \\ vs = flatmap (\p -> p \. vs) s

one :: forall m . Comp m =>
       m(Env m) -> Set(Iden) -> m(Env m)
one s vs = fold op (inj ø) s
  where op :: Env m -> m(Env m) -> m(Env m)
        op p rest = inj [p] \*/ (inj(compl (p \. vs)) /*\ rest)

concat :: Comp m => [m(a)] -> m(a)
concat [] = empty
concat (s:ss) = s ++ concat ss

ε :: forall m . Comp m =>
     Term -> Iden -> Iden -> m(Env m)
ε (Var U_)  u v = inj (u .=. v)
ε (Var x)   u v = inj (u .=. v /\ v .=. x)
ε (Int k)   u v = inj (u .=. v /\ v .=  I k)
ε (x :=  t) u v = inj (x .=. v) /*\ ε (t) u v
ε (x :-> t) u v = inj (x .=. u) /*\ ε (t) u v
-- Array
ε (Fail)        u v = empty
ε (t₁ :|:   t₂) u v = ε (t₁) u v ++  ε (t₂) u v
ε (t₁ :|||: t₂) u v = ε (t₁) u v \./ ε (t₂) u v
ε (t₁ :=:   t₂) u v = ε (t₁) u v /*\ ε (t₂) u v
ε (t₁ :>    t₂) u v = c (t₁)     /*\ ε (t₂) u v
ε (t₁ :@    t₂) u v = (inj (u .=. v) /*\ ε (t₁) f g /*\ ε (t₂) p q /*\ dF g q v)
                      \\ [f,g,p,q]
  where [f,g,p,q] = fresh[t₁, t₂, Var u, Var v]
ε (Col t)       u v = (ε (t) p q /*\ dF q u v) \\ [p,q]
  where [p,q] = fresh[t,Var u, Var v]
ε (Fun(tₐ)(q)(𝜔)(tb)) h f =
  inj [ 𝜌
      | 𝜌 <- allEnvs
      , let dom :: m(Env m)
            dom = (inj[𝜌] \\ dI(tₐ) \\ [p,w]) /*\ ε(tₐ) p w
      , (fun :: m(Val m,Env m)) <-
          pi (map (\𝜌 ->𝜌(p)) dom)
             (\val -> bigU [ (𝜌₀ \. ([j,z] ∪ dI(tb))) ∩ (collapse(ε(tb)j z \\ dI(tₐ)))
                            | 𝜌₀ <- collapse(dom), 𝜌₀(p) == val ])

      , let ff, hh :: m(Val m,Val m)
            ff = map (\(_,𝜌) -> (𝜌(p),𝜌(z))) fun
            hh = map (\(_,𝜌) -> (𝜌(p),𝜌(j))) fun
      , 𝜌(f) == F(Fn ff)
      , 𝜌(h) == F(Fn hh)
      ]
  where [j,p,w,z] = fresh[tₐ,tb]

c :: Comp m =>
     Term -> m(Env m)
c (t) = ε (t) p q \\ [p, q]
  where [p,q] = fresh[t]

dF :: Comp m => Iden -> Iden -> Iden -> m(Env m)
dF f x r = union [ flatmap (\ (p,q) -> (f .= vf /\ x .= p /\ r .= q)) ff
                 | vf@(F(Fn ff)) <- allVals ]

-- XXX
bigU :: Set(Set (Env m)) -> Set(Val m)
bigU = undefined
