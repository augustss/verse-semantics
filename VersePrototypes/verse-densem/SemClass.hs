{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module SemClass where
import Prelude hiding((++), map, concat, pi, not, (>>=), mapM)
import qualified Prelude as P
import qualified Data.List as L
import Control.Applicative(Alternative)
import Data.Kind
import qualified FrontEnd.Expr as F
import qualified Set
import qualified EQD

-----

-- Totally abstract M
data M a
instance Eq (M a)
instance Ord (M a)
instance Show (M a)
empty = undefined
inj(d) = undefined
(++) = undefined
fold(k,z,s) = undefined
(⎧*⎫) = undefined
(⎩*⎭) = undefined
(⊍) = undefined
unionS = undefined
{-
  piSM     :: (Set(a), a → Set(b)) → Set(Set(a :⇒ b))
  piSM(s,f) = [ g | g ← allFuns, dom(g) == s, (x:⇒y) ← g, y ∈ f(x) ]
  piM = piSM
-}
collapse(s) = undefined
mapM = undefined

-----

{-
type M a = Set a
empty = ø
inj(d) = d
(++) = (∪)
fold(k,z,s) = k(s,z)
(⎧*⎫) = (∩)
(⎩*⎭) = (∪)
(⊍) = (∪)
unionS = (⋃)
{-
  piSM     :: (Set(a), a → Set(b)) → Set(Set(a :⇒ b))
  piSM(s,f) = [ g | g ← allFuns, dom(g) == s, (x:⇒y) ← g, y ∈ f(x) ]
  piM = piSM
-}
collapse(s) = s
mapM = mapSet
-}

-----

--type M a = Set [a]
-- ...

-----


infix 2 ==>
(==>) :: Bool → Bool → Bool
(==>) = (<=)

type Z = Integer

----- Term -----
-- Adadpter for SrcEssential
type Term = F.SrcEssential
type Op = F.PrimOp

pattern U_ :: Term
pattern U_ <- (F.Variable (F.isSrcUnderscore -> True))
pattern Var :: Iden -> Term
pattern Var i = F.Variable i
pattern (:=) :: Iden -> Term -> Term
pattern i := t = F.DefineE i t
pattern (:|:) :: Term -> Term -> Term
pattern t1 :|: t2 = F.Choice t1 t2
pattern (:=:) :: Term -> Term -> Term
pattern t1 :=: t2 = F.Unify t1 t2
--pattern (:~>) :: Term -> Term -> Term
--pattern t1 :~> t2 = ???
pattern (:->) :: Iden -> Term -> Term
pattern i :-> t = F.DefineIE i t
pattern (:>) :: Term -> Term -> Term
pattern t1 :> t2 = F.Seq t1 t2
pattern (:..) :: Term -> Term -> Term
pattern t1 :.. t2 = F.ApplyD (F.EPrim F.DotDot) (F.Array [t1, t2])
pattern (:@) :: Term -> Term -> Term
pattern t1 :@ t2 = F.ApplyD t1 t2
pattern Rng :: Term -> Term
pattern Rng t = F.Range t
pattern If :: Term -> Term -> Term -> Term
pattern If t1 t2 t3 = F.If3 t1 t2 t3
pattern For :: Term -> Term -> Term
pattern For t1 t2 = F.For2 t1 t2
pattern Fun t1 q w t2 = F.Function q t1 w t2
pattern Where :: Term -> Term -> Term
pattern t1 `Where` t2 = F.Where t1 t2
pattern Int :: Z -> Term
pattern Int k = F.Lit (F.LInt k)
pattern Block :: Term -> Term
pattern Block t = F.Block t

-- Make fresh identifiers from the templates ss, avoid identifiers in ts
fresh :: [String] -> [Term] -> [Iden]
fresh ss ts = snd $ L.mapAccumL f (concatMap F.getAllBinders ts) ss
  where
    f :: [Iden] -> String -> ([Iden], Iden)
    f is s = (i:is, i)
          where i = (free L.\\ is) !! 0         -- avoid head
                free = [ F.Ident F.noLoc (s P.++ {-"_" ++-} i) | i <- "" : P.map show [1 :: Integer ..] ]

-- All top level binders
iI :: Term → Set Iden
iI = mkSet . F.getVisibleBinders

----- Val -----
data Val = I Z | F Fn
  deriving (Eq, Ord, Show)

data Fn = Fn (M (Val, Val))
  deriving (Eq, Ord, Show)

type a ⇀ b = Set(a :⇒ b)   -- partial functions from a to b
dom :: (a ⇀ b) → Set(a)
dom = fmap fst

type a :⇒ b = (a, b)       -- pairs used to form functions
pattern a :⇒ b = (a, b)

----- "all" values -----
numZ :: Z
numZ = 4

allZ :: Set Z
allZ = mkSet [i | i <- [0 .. numZ-1] ]

allVals :: Set(Val)
allVals = [ I i | i <- allZ ]
--        ∪ mkSet [ ... allFuns ... ]

allFuns :: Set(a ⇀ b)
allFuns = undefined

----- XSet -----
type family XSet a = r | r -> a where
  XSet Env = ENV
  XSet a   = Set(a)

class IsEmpty a where
  isEmpty :: a -> Bool

----- Set -----
-- Adapter for Set module
-- [ ] can be used for constants and comprehensions
type Set :: Type → Type
type Set = Set.Set
(∉) :: (Eq a) => a → Set a → Bool
a ∉ as = P.not (Set.member a as)
(∈) :: (Eq a) => a → Set a → Bool
a ∈ as = Set.member a as
ø :: Set a
ø = Set.empty
(∪) :: Set(a) → Set(a) → Set(a)
(∪) = Set.union
(∩) :: (Eq a) => Set(a) → Set(a) → Set(a)
(∩) = Set.intersect
(⋃) :: Set(Set(a)) → Set(a)
(⋃) = Set.bigUnion
sing :: a → Set a
sing = Set.singleton
mapSet :: (a → b, Set(a)) → Set(b)
mapSet(f,s) = fmap f s
bigIntersect :: Eq a => Set(Set(a)) → Set(a)
bigIntersect = Set.bigIntersect
mkSet :: [a] -> Set a
mkSet = Set.mkSet

instance IsEmpty (Set a) where
  isEmpty = Set.isEmpty

----- Iden -----
type Iden = F.Ident

----- Env -----
data Env
instance Eq Env
instance Ord Env

allEnvs :: Set(Env)    -- set of all environments
allEnvs = undefined

----- Simple values -----
data Atom = V Val | X Iden | AOp Op [Atom] | ATup [Atom]
  deriving (Eq, Ord, Show)

allN :: Set(Atom)
allN = [ V (I i) | i <- allZ, i >= 0 ]

instance Enum (Atom) where
  toEnum = V . I . toInteger
  fromEnum (V (I i)) = fromInteger i
  fromEnum _ = error "Enum (Atom): fromEnum"

instance Num (Atom) where
  x + y = AOp F.Add [x, y]
  x - y = AOp F.Sub [x, y]
  x * y = AOp F.Mul [x, y]
  fromInteger = V . I

(⨄) :: Iden → Iden → Atom
x ⨄ y = AOp F.ArrApp [X x, X y]
nil :: Atom
nil = ATup []

oneTuple :: Iden → Atom
oneTuple x = ATup [X x]

----- ENV, constraints -----
type ENV = EQD.EQD Iden Val
cempty :: ENV
cempty = EQD.false
univ :: ENV
univ = EQD.true
compl :: ENV → ENV
compl = EQD.nt
infixl 5 \\
(\\) :: ENV → Set(Iden) → ENV
(\\) e is = EQD.qexis (Set.toList is) e
infixl 2 \/
(\/) :: ENV → ENV → ENV
(\/) = (EQD.\/)
infixl 3 /\
(/\) :: ENV → ENV → ENV
(/\) = (EQD./\)
(.=.) :: Iden → Iden → ENV
(.=.) = (EQD.=~)
(.=) :: Iden → Val → ENV
(.=) = (EQD.=:)

(.=:) :: Iden → Atom → ENV
(.=:) = undefined
(.<=) :: Atom → Atom → ENV
(.<=) = undefined

(⭄) :: (Iden :⇒ Iden) → (Val ⇀ Val) → ENV
(⭄) = undefined

instance IsEmpty ENV where
  isEmpty x = x == cempty

----- Verse computation type -----
empty    :: M(a)
inj      :: XSet(a) → M(a)
(++)     :: M(a) → M(a) → M(a)
(⎧*⎫)    :: Ord a => M(a) → M(a) → M(a)
(⎩*⎭)    :: M(a) → M(a) → M(a)
(⊍)      :: M(a) → M(a) → M(a)
unionS   :: Set(M(a)) → M(a)
--  mapS     :: (Set(a) → Set(b), m(a)) → m(b)
fold     :: ((XSet(a), M(b)) → M(b), M(b), M(a)) → M(b)
{-
  piSM     :: (m(a), a → Set(b)) → Set(m(a :⇒ b))
  piM      :: (m(a), a → m(b)) → m(m(a :⇒ b))
-}
collapse :: M(a) → XSet(a)
mapM     :: (a→b, M(a)) → M(b)

-- Derived functions
mapS :: (XSet(a) → XSet(b), M(a)) → M(b)
mapS(f, s) = fold( \(x,t)→inj(f(x)) ++ t, empty, s )

infixl 5 \\\
(\\\) :: M(Env) → Set(Iden) → M(Env)
s \\\ vs = --mapS (\ d → d \\ vs, s)
           fold(\(d,t)→inj(d \\ vs) ++ t,empty,s)

not :: M(Env) → M(Env)
not(s) = inj(compl(collapse(s)))

prune :: forall a . (Ord a, IsEmpty (XSet a)) =>
         M(a) → M(a)
prune(s) = fold(op,empty,s)
  where op :: (XSet(a),M(a)) → M(a)
        op(d,rest) | isEmpty d = rest
                   | otherwise = inj(d) ++ rest

one :: (M(Env), Set(Iden)) → M(Env)
one(s,vs) = fold(op,empty,s)
  where op :: (XSet(Env), M(Env)) → M(Env)
        op(d,rest) = inj(d) ⎩*⎭ (inj(compl(d \\ vs)) ⎧*⎫ rest)

concat :: [M(a)] → M(a)
concat [] = empty
concat (s:ss) = s ++ concat ss

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

ε :: Term → Iden → Iden → M(Env)
ε (U_)      u v = inj (u .=. v)
ε (Var x)   u v = inj (u .=. v /\ v .=. x)
ε (Int k)   u v = inj (u .=. v /\ v .=  I k)
-- prim
ε (x :=  t) u v = inj (x .=. v) ⎧*⎫ ε (t) u v
ε (x :-> t) u v = inj (x .=. u) ⎧*⎫ ε (t) u v
-- Array
ε (t₁ :|:   t₂) u v = ε (t₁) u v ++  ε (t₂) u v
ε (t₁ :=:   t₂) u v = ε (t₁) u v ⎧*⎫ ε (t₂) u v
-- XXX ε (t₁ :~>   t₂) u v = ε (t₁) u w ⎧*⎫ ε (t₂) w v \\\ [w] where [w] = fresh ["w"] [t₁, t₂, Var u, Var v]
ε (t₁ :>    t₂) u v = cC(t₁)     ⎧*⎫ ε (t₂) u v
ε (t₁ `Where` t₂) u v = ε (t₁) u v ⎧*⎫ cC (t₂)
ε (t₁ :..   t₂) u v = inj(u .=. v) ⎧*⎫ ε (t₁) p₁ q₁ ⎧*⎫ ε (t₂) p₂ q₂ ⎧*⎫
  unionS[ concat[ inj(v .=: (X q₁ + i) /\ i .<= (X q₁ - X q₂))  | i ← [0..n]] | n ← allN ]
  \\\ [p₁, q₁, p₂, q₂]
  where [p₁, q₁, p₂, q₂] = fresh ["p1","q1","p2","q2"] [t₁, t₂, Var u, Var v]
ε (t₁ :@    t₂) u v = (inj (u .=. v) ⎧*⎫ ε (t₁) f g ⎧*⎫ ε (t₂) p q ⎧*⎫ dF g q v)
                      \\\ [f,g,p,q]
  where [f,g,p,q] = fresh ["f","g","p","q"] [t₁, t₂, Var u, Var v]
ε (Rng t)       u v = ε (t) p q ⎧*⎫ dF q u v \\\ [p,q]
  where [p,q] = fresh ["p","q"] [t,Var u, Var v]
ε (If t₀ t₁ t₂) u v = (s₀ ⎧*⎫ bB (t₁) u v \\\ xs) ⊍ (not (s₀ \\\ xs) ⎧*⎫ bB (t₂) u v)
  where xs = iI(t₀); s₀ = one(cC(t₀),xs)
ε (Block t) u v = bB (t) u v
ε (For t₀ t₁) u v = fold(op,z,cC(t₀))
  where [p,q,u₁,u₂,v₁,v₂]  = fresh ["p","q","u1","u2","v1","v2"] [t₀, t₁, Var u, Var v]
        xs             = iI(t₀)
        s₁ :: M(Env) = bB(t₁) p q
        z  :: M(Env) = inj(u .=: nil /\ v .=: nil)
        op :: (XSet(Env), M(Env)) → M(Env)
        op(d,m)        =     inj(u .=: (u₁ ⨄ u₂) /\ v .=: (v₁ ⨄ v₂))
                         ⎧*⎫ ((inj(d) ⎧*⎫ s₁ ⎧*⎫ inj(u₁ .=: oneTuple(p) /\ v₁ .=: oneTuple(q)) \\\ [p,q])
                              ⊍ (inj(compl(d \\ xs)) ⎧*⎫ inj(u₁ .=: nil /\ v₁ .=: nil)))
                         ⎧*⎫ ((m ⎧*⎫ inj(u .=. u₂ /\ v .=. v₂)) \\\ [u,v])
                         \\\ [u₁,u₂,v₁,v₂]
{-
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
    [x,w,j,z] = fresh ["x","w","j","z"] [tₐ,tb,Var f,Var h]
    avs = iI(tₐ)
    bvs = iI(tb)

    xfn :: m(Val, Env) → Iden → Iden → Fn m
    xfn fun x y = Fn ( prune(mapM(\(_,ρc) → (ρc(x),ρc(y)),fun)) )
    
    funs :: Env → Set (m (Val, Env))
    funs(ρ) = piSM(domvs, rngfun)
      where
        dom :: m(Env) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ε (tₐ) x w
        domvs :: m(Val)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val → Set(Env)
        rngfun(xv) = bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ε (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ]
                            
-- Using piM
ε (Fun(tₐ)(q)(ω)(tb)) f h =
 unionS
  [ mapFilterS(\(fun :: m(Val, Env)) → [ ρ | ρ(f) == F (xfn fun x z)
                                               , ρ(h) == F (xfn fun w j) ]

              ,funs(ρ))
  | ρ ← allEnvs
  ]
  where
    [x,w,j,z] = fresh ["x","w","j","z"] [tₐ,tb,Var f,Var h]
    avs = iI(tₐ)
    bvs = iI(tb)

    xfn :: m(Val, Env) → Iden → Iden → Fn m
    xfn fun x y = Fn ( prune(mapM(\(_,ρc) → (ρc(x),ρc(y)),fun)) )
    
    funs :: Env → m (m (Val, Env))
    funs(ρ) = piM(domvs, rngfun)
      where
        dom :: m(Env) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ε (tₐ) x w
        domvs :: m(Val)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val → M(Env)
        rngfun(xv) =
                inj (bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ε (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ])
-}

cC :: Term → M(Env)
cC (t) = ε (t) p q \\\ [p, q]
  where [p,q] = fresh ["p","q"] [t]

bB :: Term → Iden → Iden → M(Env)
bB (t) u v = ε (t) u v \\\ iI(t)

dF :: Iden → Iden → Iden → M(Env)
dF f x r = unionS [ mapS (\ (prs :: Val ⇀ Val) →
                            (f .= vf /\ ((x :⇒ r) ⭄ prs)), ff)
                  | vf@(F(Fn ff)) ← allVals ]
-- 

{-
mapFilterS :: (a → Set(b), M(a)) → M(b)
mapFilterS = undefined
-}

den :: Term -> ENV
den t = collapse $ ε (Block t) u v
  where [u, v] = fresh ["u", "v"] [t]
