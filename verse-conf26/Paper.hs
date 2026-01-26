{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Paper where
import Prelude hiding((++), map, concat, pi, not, (>>=), mapM)
import qualified Prelude as P
import Control.Applicative(Alternative)
import Data.Kind
import GHC.Exts(IsList(..))

infix 2 ==>
(==>) :: Bool → Bool → Bool
(==>) = (<=)

type Z = Integer

----- Term -----
data Term
  = Var Iden               -- x, _
  | Int Z                  -- k
  -- prim
  | Iden := Term           -- x := t
  | Term :@ Term           -- t1[t2]
  | Term :> Term           -- t1; t2
  | Term `Where` Term      -- t1 where t2
  | Term :=: Term          -- t1 = t2
  | Term :~> Term          -- t1 ~> t2
  | Term :|: Term          -- t1 | t2
  | Term :.. Term          -- t1 .. t2
  | If Term Term Term      -- if(t1)then t2 else t3
  | For Term Term          -- for(t1)do t2
  | Array [Term]           -- array{t1, ... tn}
  | Fun Term Apt Fx Term   -- fun(t1)<q><w>{t2}
  | Rng Term               -- :t

{-
  | Iden :→ Term
  | Fail
  | Term :|||: Term
-}

data Apt = O | C
data Fx = Succeeds | Decides

data Op = Add | Sub | Append

fresh :: [Term] → [Iden]
fresh = undefined

iI :: Term → Set Iden
iI = undefined

----- Val -----
data Val m = I Z | F (Fn m)
data Fn m = Fn (m (Val m, Val m))
allVals :: Set(Val m)
allVals = undefined
instance Eq (Val m)
--type PFUN m = Set(Val m ⤇ Val m)
--allPFUN :: Set(PFUN m)
--allPFUN = undefined
type a ⇀ b = Set(a :⇒ b)
allFuns :: Set(a ⇀ b)
allFuns = undefined
dom :: (a ⇀ b) → Set(a)
dom = undefined

type a :⇒ b = (a, b)
pattern a :⇒ b = (a, b)

----- Set -----
-- [ ] can be used for constants and comprehensions
type Set :: Type → Type
data Set a
instance {-Eq a =>-} Eq (Set a)
instance Functor Set
instance Applicative Set
instance Monad Set
instance Alternative Set
instance MonadFail Set
(∉) :: a → Set a → Bool
a ∉ as = undefined
(∈) :: a → Set a → Bool
a ∈ as = undefined
ø :: Set a
ø = undefined
(∪) :: Set(a) → Set(a) → Set(a)
(∪) = undefined
(∩) :: Set(a) → Set(a) → Set(a)
(∩) = undefined
mkSet :: [a] → Set a
mkSet = undefined
instance IsList (Set a) where
  type Item (Set a) = a
  fromList = mkSet
(⋃) :: Set(Set(a)) → Set(a)
(⋃) = undefined
-- the following 3 make Set a monad
sing :: a → Set a
sing x = mkSet [x]
mapSet :: (a → b, Set(a)) → Set(b)
mapSet = undefined
bigIntersect :: Set(Set(a)) → Set(a)
bigIntersect = undefined

----- Iden -----
data Iden = U_ | Iden String
forAllIden :: (Iden → Bool) → Bool  -- 
forAllIden _p = undefined

----- Env -----
type Env m = Iden → Val m

allEnvs :: Set(Env m)    -- set of all environments
allEnvs = undefined

----- Simple values -----
data Atom m = V (Val m) | X Iden | AOp Op [Atom m] | ATup [Atom m]
allN :: Set(Atom m)
allN = undefined
instance Enum (Atom m)
instance Num (Atom m)
(⨄) :: Iden → Iden → Atom m
x ⨄ y = AOp Append [X x, X y]
nil :: Atom m
nil = ATup []

oneTuple :: Iden → Atom m
oneTuple x = ATup [X x]

----- ENV, constraints -----
type ENV m = Set (Env m)
cempty :: ENV m
cempty = undefined
univ :: ENV m
univ = undefined
compl :: ENV m → ENV m
compl = undefined
infixl 5 \\
(\\) :: ENV m → Set(Iden) → ENV m
(\\) = undefined
infixl 2 \/
(\/) :: ENV m → ENV m → ENV m
(\/) = undefined
infixl 3 /\
(/\) :: ENV m → ENV m → ENV m
(/\) = undefined
(.=.) :: Iden → Iden → ENV m
(.=.) = undefined
(.=) :: Iden → Val m → ENV m
(.=) = undefined

(.=:) :: Iden → Atom m → ENV m
(.=:) = undefined
(.<=) :: Atom m → Atom m → ENV m
(.<=) = undefined

(⭄) :: (Iden :⇒ Iden) → (Val m ⇀ Val m) → ENV m
(⭄) = undefined

----- Verse computation type -----
type Comp :: (Type → Type) → Constraint
class Comp m where
  empty    :: m(a)
  inj      :: Set(a) → m(a)
  (++)     :: m(a) → m(a) → m(a)
  (⎧*⎫)    :: m(a) → m(a) → m(a)
  (⎩*⎭)    :: m(a) → m(a) → m(a)
  (⊍)      :: m(a) → m(a) → m(a)
  unionS   :: Set(m(a)) → m(a)
--  mapS     :: (Set(a) → Set(b), m(a)) → m(b)
  fold     :: ((Set(a), m(b)) → m(b), m(b), m(a)) → m(b)
  piSM     :: (m(a), a → Set(b)) → Set(m(a :⇒ b))
  piM      :: (m(a), a → m(b)) → m(m(a :⇒ b))
  collapse :: m(a) → Set(a)
  mapM     :: (a→b, m(a)) → m(b)

-- Derived functions
mapS :: Comp m => (Set(a) → Set(b), m(a)) → m(b)
mapS(f, s) = fold( \(x,t)→inj(f(x)) ++ t, empty, s )

infixl 5 \\\
(\\\) :: Comp m => m(Env m) → Set(Iden) → m(Env m)
s \\\ vs = --mapS (\ d → d \\ vs, s)
           fold(\(d,t)→inj(d \\ vs) ++ t,empty,s)

not :: Comp m => m(Env m) → m(Env m)
not(s) = inj(compl(collapse(s)))

prune :: forall m a . (Comp m, Eq a) =>
         m(a) → m(a)
prune(s) = fold(op,empty,s)
  where op :: (Set(a),m(a)) → m(a)
        op(d,rest) | d == ø    = rest
                   | otherwise = inj(d) ++ rest

one :: forall m . Comp m =>
       (m(Env m), Set(Iden)) → m(Env m)
one(s,vs) = fold(op,empty,s)
  where op :: (Set(Env m), m(Env m)) → m(Env m)
        op(d,rest) = inj(d) ⎩*⎭ (inj(compl(d \\ vs)) ⎧*⎫ rest)

concat :: Comp m => [m(a)] → m(a)
concat [] = empty
concat (s:ss) = s ++ concat ss

---------------------------

instance Comp Set where
  empty = ø
  inj(d) = d
  (++) = (∪)
  fold(k,z,s) = k(s,z)
  (⎧*⎫) = (∩)
  (⎩*⎭) = (∪)
  (⊍) = (∪)
  unionS = (⋃)
  piSM     :: (Set(a), a → Set(b)) → Set(Set(a :⇒ b))
  piSM(s,f) = [ g | g ← allFuns, dom(g) == s, (x:⇒y) ← g, y ∈ f(x) ]
  piM = piSM
  collapse(s) = s
  mapM = mapSet

---------------------------
{-
newtype SeqSet a = S [Set(a)]
(⩂) :: SeqSet a → SeqSet a → SeqSet a  -- dodgy union
(⩂) = undefined

instance Comp SeqSet where
  empty = S [ø]
  inj(d) = S [d]
  S s ++ S t = S (s P.++ t)
  S s ⎧*⎫ S t = S [ d₁ ∩ d₂ | d₁ ← s, d₂ ← t ]
  (⎩*⎭) = (⩂)
  (⊍) = (⩂)
  unionS(ss) = error "infinite dodgy union"
  mapS(f,S s) = S [ mapS(f,x) | x ← s ]
  fold(k,z,S[]) = z
  fold(k,z,S(x:xs)) = k(x,fold(k,z,S xs))
  piSM = error "???"
  collapse(S[]) = ø
  collapse(S(x:xs)) = x ∪ collapse(S xs)

---------------------------

data Tree a = L(Set(a)) | Tree(a) :∪ Tree(a) | Tree(a) :++ Tree(a)
(>>=) :: Tree(a) → (Set(a) → Tree(b)) → Tree(b)
(>>=) = undefined

instance Comp Tree where
  empty = L ø
  inj(d) = L d
  (++) = (:++)
  s ⎧*⎫ t = s >>= \d₁→ t >>= \d₂→ L (d₁ ∩ d₂)
  s ⎩*⎭ t = s >>= \d₁→ t >>= \d₂→ L (d₁ ∪ d₂)
  (⊍) = (:∪)
  unionS = error "??"
  mapS(f,s) = s >>= \d→ inj(f(d))
  fold(k,z,s) = error "??"
  piSM = error "???"
  collapse = error "collapse"
-}
---------------------------

ε :: forall m . Comp m =>
     Term → Iden → Iden → m(Env m)
ε (Var U_)  u v = inj (u .=. v)
ε (Var x)   u v = inj (u .=. v /\ v .=. x)
ε (Int k)   u v = inj (u .=. v /\ v .=  I k)
-- prim
ε (x :=  t) u v = inj (x .=. v) ⎧*⎫ ε (t) u v
-- Array
ε (t₁ :|:   t₂) u v = ε (t₁) u v ++  ε (t₂) u v
ε (t₁ :=:   t₂) u v = ε (t₁) u v ⎧*⎫ ε (t₂) u v
ε (t₁ :~>   t₂) u v = ε (t₁) u w ⎧*⎫ ε (t₂) w v \\\ [w] where [w] = fresh[t₁, t₂, Var u, Var v]
ε (t₁ :>    t₂) u v = cC(t₁)     ⎧*⎫ ε (t₂) u v
ε (t₁ `Where` t₂) u v = ε (t₁) u v ⎧*⎫ cC (t₂)
ε (t₁ :..   t₂) u v = inj(u .=. v) ⎧*⎫ ε (t₁) p₁ q₁ ⎧*⎫ ε (t₂) p₂ q₂ ⎧*⎫
  unionS[ concat[ inj(v .=: (X q₁ + i) /\ i .<= (X q₁ - X q₂))  | i ← [0..n]] | n ← allN ]
  \\\ [p₁, q₁, p₂, q₂]
  where [p₁, q₁, p₂, q₂] = fresh[t₁, t₂, Var u, Var v]
ε (t₁ :@    t₂) u v = (inj (u .=. v) ⎧*⎫ ε (t₁) f g ⎧*⎫ ε (t₂) p q ⎧*⎫ dF g q v)
                      \\\ [f,g,p,q]
  where [f,g,p,q] = fresh[t₁, t₂, Var u, Var v]
ε (Rng t)       u v = ε (t) p q ⎧*⎫ dF q u v \\\ [p,q]
  where [p,q] = fresh[t,Var u, Var v]
ε (If t₀ t₁ t₂) u v = (s₀ ⎧*⎫ bB (t₁) u v \\\ xs) ⊍ (not (s₀ \\\ xs) ⎧*⎫ bB (t₂) u v)
  where xs = iI(t₀); s₀ = one(cC(t₀),xs)
ε (For t₀ t₁) u v = fold(op,z,cC(t₀))
  where [p,q,u₁,u₂,v₁,v₂]  = fresh[t₀, t₁, Var u, Var v]
        xs             = iI(t₀)
        s₁ :: m(Env m) = bB(t₁) p q
        z  :: m(Env m) = inj(u .=: nil /\ v .=: nil)
        op :: (Set(Env m), m(Env m)) → m(Env m)
        op(d,m)        =     inj(u .=: (u₁ ⨄ u₂) /\ v .=: (v₁ ⨄ v₂))
                         ⎧*⎫ ((inj(d) ⎧*⎫ s₁ ⎧*⎫ inj(u₁ .=: oneTuple(p) /\ v₁ .=: oneTuple(q)) \\\ [p,q])
                              ⊍ (inj(compl(d \\ xs)) ⎧*⎫ inj(u₁ .=: nil /\ v₁ .=: nil)))
                         ⎧*⎫ ((m ⎧*⎫ inj(u .=. u₂ /\ v .=. v₂)) \\\ [u,v])
                         \\\ [u₁,u₂,v₁,v₂]

-- Using piSM
ε (Fun(tₐ)(q)(ω)(tb)) f h =
 inj
  [ ρ
  | ρ ← allEnvs
  , fun ← funs(ρ)
  , ρ(f) == F (xfn fun x z)
  , ρ(h) == F (xfn fun w j)
  ]
  where
    [x,w,j,z] = fresh[tₐ,tb,Var f,Var h]
    avs = iI(tₐ)
    bvs = iI(tb)

    xfn :: m(Val m, Env m) → Iden → Iden → Fn m
    xfn fun x y = Fn ( prune(mapM(\(_,ρc) → (ρc(x),ρc(y)),fun)) )
    
    funs :: Env m → Set (m (Val m, Env m))
    funs(ρ) = piSM(domvs, rngfun)
      where
        dom :: m(Env m) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ε (tₐ) x w
        domvs :: m(Val m)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val m → Set(Env m)
        rngfun(xv) = bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ε (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ]
                            
-- Using piM
ε (Fun(tₐ)(q)(ω)(tb)) f h =
 unionS
  [ mapFilterS(\(fun :: m(Val m, Env m)) → [ ρ | ρ(f) == F (xfn fun x z)
                                               , ρ(h) == F (xfn fun w j) ]

              ,funs(ρ))
  | ρ ← allEnvs
  ]
  where
    [x,w,j,z] = fresh[tₐ,tb,Var f,Var h]
    avs = iI(tₐ)
    bvs = iI(tb)

    xfn :: m(Val m, Env m) → Iden → Iden → Fn m
    xfn fun x y = Fn ( prune(mapM(\(_,ρc) → (ρc(x),ρc(y)),fun)) )
    
    funs :: Env m → m (m (Val m, Env m))
    funs(ρ) = piM(domvs, rngfun)
      where
        dom :: m(Env m) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ε (tₐ) x w
        domvs :: m(Val m)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val m → m(Env m)
        rngfun(xv) =
                inj (bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ε (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ])
                            
cC :: Comp m =>
      Term → m(Env m)
cC (t) = ε (t) p q \\\ [p, q]
  where [p,q] = fresh[t]

bB :: Comp m =>
      Term → Iden → Iden → m(Env m)
bB (t) u v = ε (t) u v \\\ iI(t)
  where [p,q] = fresh[t]

dF :: forall m . Comp m =>
      Iden → Iden → Iden → m(Env m)
dF f x r = unionS [ mapS (\ (prs :: Val m ⇀ Val m) →
                            (f .= vf /\ ((x :⇒ r) ⭄ prs)), ff)
                  | vf@(F(Fn ff)) ← allVals ]
-- 

mapFilterS :: Comp m => (a → Set(b), m(a)) → m(b)
mapFilterS = undefined

