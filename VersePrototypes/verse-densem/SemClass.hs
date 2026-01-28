{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module SemClass where
import Prelude hiding((++), map, concat, pi, not, (>>=), mapM)
import qualified Prelude as P
import qualified Control.Monad as M
import qualified Data.Maybe as P
import qualified Data.Char as P
import qualified Data.List as L
import Control.Applicative(Alternative)
import Data.Kind
import qualified FrontEnd.Expr as F
import qualified Set
import qualified EQD

-----

{-
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
-}

-----


type M a = XSet a
empty = ø
inj(d) = d
(++) = (∪)
(⎧*⎫) = (∩)
(⎩*⎭) = (∪)
(⊍) = (∪)
unionS s | Set.isEmpty s = empty
unionS s = Set.foldSet (∪) s
fold(k,z,s) = k(s,z)
collapse(s) = s
mapM = error "XSet mapM"
{-
  piSM     :: (Set(a), a → Set(b)) → Set(Set(a :⇒ b))
  piSM(s,f) = [ g | g ← allFuns, dom(g) == s, (x:⇒y) ← g, y ∈ f(x) ]
  piM = piSM
-}

-----



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
pattern Prim :: Op -> Term
pattern Prim o = F.EPrim o
pattern (:=) :: Iden -> Term -> Term
pattern i := t = F.DefineE i t
pattern (:|:) :: Term -> Term -> Term
pattern t1 :|: t2 = F.Choice t1 t2
pattern (:|||:) :: Term -> Term -> Term
pattern t1 :|||: t2 <- F.ApplyD (F.Variable (F.Ident _ "operator'|||'")) (F.Array [t1, t2])
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
pattern Exists :: Iden -> Term
pattern Exists i = F.DefineV i

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

----- primops -----

dP :: Op -> Fn
dP F.Neg = fun[funNegate]
dP F.IsInt = fun[funInt]
{-
dP Gt = fun[funGt]
dP Lt = fun[funLt]
dP Add = fun[funAdd]
dP Sub = fun[funSub]
dP Mul = fun[funMul]
dP Div = fun[funDiv]
-}
dP p = error $ "dP undefined " P.++ show p

knownFuns :: [(Fn, String)]
knownFuns = [ (dP o, P.map P.toLower (show o)) | o <- [F.Neg, F.IsInt] ]

fun :: [Val ⇀ Val] -> Fn
fun = Fn . concat . P.map inj

funNegate :: Val ⇀ Val
funNegate = [(I i, I ((-i) `mod` numZ)) | i <- allZ ]

funInt :: Val ⇀ Val
funInt = [(I i, I i) | i <- allZ ]

-- Apply a partial function
applyPF :: (Val ⇀ Val) -> Val -> Maybe Val
applyPF f x = Set.getSing $ Set.lookupSet x f

----- Val -----

data Val = I Z | F Fn
  | T [Val]                                -- XXX tuples, temporary
  deriving (Eq, Ord)
instance Show Val where
  showsPrec p (I i) = showsPrec p i
  showsPrec p (F f) = showsPrec p f
  showsPrec _ (T vs) = showString $ "〈" P.++ L.intercalate "," (P.map show vs) P.++ "〉"

data Fn = Fn (M (Val :⇒ Val))
  deriving (Eq, Ord)

instance Show Fn where
  show f | Just s <- P.lookup f knownFuns = s
  show (Fn f) = "Fn" P.++ show f

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
        ∪ [ F f | f <- allFuns ]
--        ∪ [ T t | t <- allTuples ]

allFuns :: Set(Fn)
allFuns = [ fun[funInt], fun[funNegate] ]

----- XSet -----

type ASet :: Type -> Constraint
class (Eq (XSet a)) => ASet a where
  type XSet a = r | r -> a
  ø :: XSet(a)
  isEmpty :: XSet(a) -> Bool
  (∪) :: XSet(a) → XSet(a) → XSet(a)
  (∩) :: XSet(a) → XSet(a) → XSet(a)

instance ASet Env where
  type XSet Env = ENV
  ø = EQD.false
  (∪) = (EQD.\/)
  (∩) = (EQD./\)
  isEmpty x = x == EQD.false

instance ASet Val where
  type XSet Val = Set Val
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty

instance (Ord a, Ord b, ASet a, ASet b) => ASet (a, b) where
  type XSet (a, b) = Set (a, b)
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  
instance (Ord a, ASet a) => ASet (Set(a)) where
  type XSet (Set a) = Set (Set(a))
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  

----- Set -----
-- Adapter for Set module
-- [ ] can be used for constants and comprehensions
type Set :: Type → Type
type Set = Set.Set
(∉) :: (Eq a) => a → Set a → Bool
a ∉ as = P.not (Set.member a as)
(∈) :: (Eq a) => a → Set a → Bool
a ∈ as = Set.member a as
--ø :: Set a
--ø = Set.empty
--(∪) :: Set(a) → Set(a) → Set(a)
--(∪) = Set.union
--(∩) :: (Eq a) => Set(a) → Set(a) → Set(a)
--(∩) = Set.intersect
--(⋃) :: Set(Set(a)) → Set(a)
--(⋃) = Set.bigUnion
sing :: a → Set a
sing = Set.singleton
mapSet :: (a → b, Set(a)) → Set(b)
mapSet(f,s) = fmap f s
bigIntersect :: Eq a => Set(Set(a)) → Set(a)
bigIntersect = Set.bigIntersect
mkSet :: [a] -> Set a
mkSet = Set.mkSet

----- Iden -----
type Iden = F.Ident

----- Env -----
data Env

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

{-
(⨄) :: Iden → Iden → Atom
x ⨄ y = AOp F.ArrApp [X x, X y]
-}
(⊕) :: Atom -> Atom -> Atom
x ⊕ y = AOp F.ArrApp [x, y]

nil :: Atom
nil = ATup []

oneTuple :: Iden → Atom
oneTuple x = ATup [X x]

type AEnv = [(Iden, Val)]

atomEval :: AEnv -> Atom -> Maybe Val
atomEval r (X x)      = P.lookup x r
atomEval _ (V v)      = Just v
atomEval r (ATup as)  = T <$> P.mapM (atomEval r) as
atomEval r (AOp o as) =
  case (o, P.mapM (atomEval r) as) of
    (F.Neg, Just [I i])        -> Just $ I $ (-i)    `mod` numZ
    (F.Add, Just [I i1, I i2]) -> Just $ I $ (i1+i2) `mod` numZ
    (F.Sub, Just [I i1, I i2]) -> Just $ I $ (i1-i2) `mod` numZ
    (F.Mul, Just [I i1, I i2]) -> Just $ I $ (i1*i2) `mod` numZ
    (F.Div, Just [I i1, I i2]) -> Just $ I $ (i1 `div` i2)
    _                          -> Nothing

atomVars :: Atom -> [Iden]
atomVars (X x) = [x]
atomVars (V _) = []
atomVars (ATup  as) = P.foldr L.union [] (P.map atomVars as)
atomVars (AOp o as) = P.foldr L.union [] (P.map atomVars as)

atomEnvs :: [Atom] -> [([Val], AEnv)]
atomEnvs as =
  let aenvs = P.map (P.zip is) (M.replicateM (length is) $ Set.toList allVals)
      is = P.foldr L.union [] $ P.map atomVars as
  in  [ (vs, r) | r <- aenvs, Just vs <- [P.mapM (atomEval r) as] ]

atomEnvToENV :: AEnv -> ENV
atomEnvToENV = foldr (/\) univ . P.map (uncurry (.=))

(.==) :: Iden → Atom → ENV
i .== a = unionENVs [ i .= v /\ atomEnvToENV r | ([v], r) <- atomEnvs [a] ]
(.<=) :: Atom → Atom → ENV
a1 .<= a2 = unionENVs [ atomEnvToENV r | ([I v1, I v2], r) <- atomEnvs [a1, a2], v1 <= v2 ]

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

unionENVs :: [ENV] -> ENV
unionENVs = P.foldr (\/) cempty

-- Environments where the pair (x :⇒ y) is in the function f
(⋵) :: (Iden :⇒ Iden) → (Val ⇀ Val) → ENV
(x :⇒ y) ⋵ f =
  unionENVs [ x .= u /\ y .= v | u <- Set.toList allVals, Just v <- [applyPF f u] ]

----- Verse computation type -----
empty    :: ASet a => M(a)
inj      :: ASet a => XSet(a) → M(a)
(++)     :: ASet a => M(a) → M(a) → M(a)
(⎧*⎫)    :: ASet a => M(a) → M(a) → M(a)
(⎩*⎭)    :: ASet a => M(a) → M(a) → M(a)
(⊍)      :: ASet a => M(a) → M(a) → M(a)
unionS   :: (ASet a, Ord (M a)) => Set(M(a)) → M(a)
--  mapS     :: (Set(a) → Set(b), m(a)) → m(b)
fold     :: ASet a => ((XSet(a), M(b)) → M(b), M(b), M(a)) → M(b)
{-
  piSM     :: (m(a), a → Set(b)) → Set(m(a :⇒ b))
  piM      :: (m(a), a → m(b)) → m(m(a :⇒ b))
-}
collapse :: (ASet a) => M(a) → XSet(a)
mapM     :: (a→b, M(a)) → M(b)

-- Derived functions
mapS :: (ASet a, ASet b) => (XSet(a) → XSet(b), M(a)) → M(b)
mapS(f, s) = fold( \(x,t)→inj(f(x)) ++ t, empty, s )

infixl 5 \\\
(\\\) :: M(Env) → Set(Iden) → M(Env)
s \\\ vs = --mapS (\ d → d \\ vs, s)
           fold(\(d,t)→inj(d \\ vs) ++ t,empty,s)

not :: M(Env) → M(Env)
not(s) = inj(compl(collapse(s)))

prune :: forall a . (ASet a) =>
         M(a) → M(a)
prune(s) = fold(op,empty,s)
  where op :: (XSet(a),M(a)) → M(a)
        op(d,rest) | isEmpty d = rest
                   | otherwise = inj(d) ++ rest

one :: (M(Env), Set(Iden)) → M(Env)
one(s,vs) = fold(op,empty,s)
  where op :: (XSet(Env), M(Env)) → M(Env)
        op(d,rest) = inj(d) ⎩*⎭ (inj(compl(d \\ vs)) ⎧*⎫ rest)

concat :: ASet a =>
          [M(a)] → M(a)
concat [] = empty
concat (s:ss) = s ++ concat ss

---------------------------

-- Use the prefix ɩ to work around Haskell's limitation of starting with a lowercase letter.

ɩℰ :: Term → Iden → Iden → M(Env)
ɩℰ (U_)      u v = inj (u .=. v)
ɩℰ (Var x)   u v = inj (u .=. v /\ v .=. x)
ɩℰ (Int k)   u v = inj (u .=. v /\ v .=  I k)
ɩℰ (Prim o)  u v = inj (u .=. v /\ v .=  F (dP o))
ɩℰ (x :=  t) u v = inj (x .=. v) ⎧*⎫ ɩℰ (t) u v
ɩℰ (x :-> t) u v = inj (x .=. u) ⎧*⎫ ɩℰ (t) u v
ɩℰ (Exists x)u v = inj (u .=. v /\ v .=. x)
-- Array
ɩℰ (t₁ :|:   t₂) u v = ɩℰ (t₁) u v ++  ɩℰ (t₂) u v
ɩℰ (t₁ :|||: t₂) u v = ɩℰ (t₁) u v ⎩*⎭ ɩℰ (t₂) u v
ɩℰ (t₁ :=:   t₂) u v = ɩℰ (t₁) u v ⎧*⎫ ɩℰ (t₂) u v
-- XXX ɩℰ (t₁ :~>   t₂) u v = ɩℰ (t₁) u w ⎧*⎫ ɩℰ (t₂) w v \\\ [w] where [w] = fresh ["w"] [t₁, t₂, Var u, Var v]
ɩℰ (t₁ :>    t₂) u v = ɩ𝒞 (t₁)     ⎧*⎫ ɩℰ (t₂) u v
ɩℰ (t₁ `Where` t₂) u v = ɩℰ (t₁) u v ⎧*⎫ ɩ𝒞 (t₂)
ɩℰ (t₁ :..   t₂) u v = inj(u .=. v) ⎧*⎫ ɩℰ (t₁) p₁ q₁ ⎧*⎫ ɩℰ (t₂) p₂ q₂ ⎧*⎫
  unionS[ concat[ inj(v .== (X q₁ + i) /\ i .<= (X q₂ - X q₁)) | i ← [0..n]] | n ← allN ]
  \\\ [p₁, q₁, p₂, q₂]
  where [p₁, q₁, p₂, q₂] = fresh ["p1","q1","p2","q2"] [t₁, t₂, Var u, Var v]
ɩℰ (t₁ :@    t₂) u v = (inj (u .=. v) ⎧*⎫ ɩℰ (t₁) f g ⎧*⎫ ɩℰ (t₂) p q ⎧*⎫ ɩℱ g q v)
                      \\\ [f,g,p,q]
  where [f,g,p,q] = fresh ["f","g","p","q"] [t₁, t₂, Var u, Var v]
ɩℰ (Rng t)       u v = ɩℰ (t) p q ⎧*⎫ ɩℱ q u v \\\ [p,q]
  where [p,q] = fresh ["p","q"] [t,Var u, Var v]
ɩℰ (If t₀ t₁ t₂) u v = (s₀ ⎧*⎫ ɩℬ (t₁) u v \\\ xs) ⊍ (not (s₀ \\\ xs) ⎧*⎫ ɩℬ (t₂) u v)
  where xs = iI(t₀); s₀ = one(ɩ𝒞(t₀),xs)
ɩℰ (Block t) u v = ɩℬ (t) u v
ɩℰ (For t₀ t₁) u v = fold(op,z,ɩ𝒞(t₀))
  where [p,q,u₁,u₂,v₁,v₂]  = fresh ["p","q","u1","u2","v1","v2"] [t₀, t₁, Var u, Var v]
        xs             = iI(t₀)
        s₁ :: M(Env) = ɩℬ(t₁) p q
        z  :: M(Env) = inj(u .== nil /\ v .== nil)
        op :: (XSet(Env), M(Env)) → M(Env)
        op(d,m)        =     inj(u .== (X u₁ ⊕ X u₂) /\ v .== (X v₁ ⊕ X v₂))
                         ⎧*⎫ ((inj(d) ⎧*⎫ s₁ ⎧*⎫ inj(u₁ .== oneTuple(p) /\ v₁ .== oneTuple(q)) \\\ [p,q])
                              ⊍ (inj(compl(d \\ xs)) ⎧*⎫ inj(u₁ .== nil /\ v₁ .== nil)))
                         ⎧*⎫ ((m ⎧*⎫ inj(u .=. u₂ /\ v .=. v₂)) \\\ [u,v])
                         \\\ [u₁,u₂,v₁,v₂]
{-
-- Using piSM
ɩℰ (Fun(tₐ)(q)(ω)(tb)) f h =
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
        dom :: m(Env) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ɩℰ (tₐ) x w
        domvs :: m(Val)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val → Set(Env)
        rngfun(xv) = bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ɩℰ (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ]
                            
-- Using piM
ɩℰ (Fun(tₐ)(q)(ω)(tb)) f h =
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
        dom :: m(Env) = (inj(sing(ρ)) \\\ avs \\\ [x,w]) ⎧*⎫ ɩℰ (tₐ) x w
        domvs :: m(Val)
        domvs = mapM(\ρ→ρ(x), dom)
        rngfun :: Val → M(Env)
        rngfun(xv) =
                inj (bigIntersect [ (sing(aρ) \\ [j,z] \\ bvs) ∩ collapse(ɩℰ (tb) j z)
                                  | aρ ← collapse(dom), aρ(x) == xv
                                  ])
-}
ɩℰ t _ _ = error $ "ɩℰ: unimplemented " P.++ show t

ɩ𝒞 :: Term → M(Env)
ɩ𝒞 (t) = ɩℰ (t) p q \\\ [p, q]
  where [p,q] = fresh ["p","q"] [t]

ɩℬ :: Term → Iden → Iden → M(Env)
ɩℬ (t) u v = ɩℰ (t) u v \\\ iI(t)

ɩℱ :: Iden → Iden → Iden → M(Env)
ɩℱ f x r = unionS [ mapS (\ (prs :: Val ⇀ Val) →
                            (f .= vf /\ ((x :⇒ r) ⋵ prs)), ff)
                  | vf@(F(Fn ff)) ← allVals ]

-- 

{-
mapFilterS :: (a → Set(b), M(a)) → M(b)
mapFilterS = undefined
-}

den :: Term -> ENV
den t = collapse $ ɩℰ (Block t) u v
  where [u, v] = fresh ["u", "v"] [t]
