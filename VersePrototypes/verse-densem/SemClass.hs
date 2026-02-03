{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
{-# LANGUAGE RebindableSyntax #-}
module SemClass where
import Prelude hiding((++), map, concat, pi, not, mapM, return, (>>=), (>>))
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
import Debug.Trace
--import qualified GHC.Exts as P
import GHC.Exts
import GHC.Stack
import qualified Epic.List as L

-- Use the prefix ɩ to work around Haskell's limitation of starting with a lowercase letter.

ɩℰ :: Term → Iden → Iden → M(Env)
ɩℰ (U_)      u v = inj (u .=. v)
ɩℰ (Var (F.Ident _ s)) u v | Just fn <- P.lookup s knownFunsF = inj (u .=. v /\ v .= F fn)
ɩℰ (Var x)   u v = inj (u .=. v /\ v .=. x)
ɩℰ (Int k)   u v = inj (u .=. v /\ v .=  I k)
ɩℰ (Prim o)  u v = inj (u .=. v /\ v .=  F (dP o))
ɩℰ (x :=  t) u v = inj (x .=. v) ⎧*⎫ ɩℰ (t) u v
ɩℰ (x :-> t) u v = inj (x .=. u) ⎧*⎫ ɩℰ (t) u v
ɩℰ (Exists x)u v = inj (u .=. v /\ v .=. x)
ɩℰ (Fail)    u v = empty
ɩℰ (Array ts)u v = bigStar (inj(u .== tup(us) /\ v .== tup(vs)) :
                            zipWith3 ɩℰ ts us vs)
                   \\\ mkSet (us P.++ vs)
  where us = fresh (P.map (const "u") ts) [Array ts, Var u, Var v]
        vs = fresh (P.map (const "v") ts) [Array ts, Var u, Var v]
ɩℰ (t₁ :|:   t₂) u v = ɩℬ (t₁) u v ++  ɩℬ (t₂) u v
ɩℰ (t₁ :|||: t₂) u v = ɩℬ (t₁) u v ⎩*⎭ ɩℬ (t₂) u v
ɩℰ (t₁ :=:   t₂) u v = ɩℰ (t₁) u v ⎧*⎫ ɩℰ (t₂) u v
-- XXX ɩℰ (t₁ :~>   t₂) u v = ɩℰ (t₁) u w ⎧*⎫ ɩℰ (t₂) w v \\\ [w]
--       where [w] = fresh ["w"] [t₁, t₂, Var u, Var v]
ɩℰ (t₁ :>    t₂) u v = ɩ𝒞 (t₁)     ⎧*⎫ ɩℰ (t₂) u v
ɩℰ (t₁ `Where` t₂) u v = ɩℰ (t₁) u v ⎧*⎫ ɩ𝒞 (t₂)
ɩℰ (t₁ :..   t₂) u v = inj(u .=. v) ⎧*⎫ ɩℰ (t₁) p₁ q₁ ⎧*⎫ ɩℰ (t₂) p₂ q₂ ⎧*⎫
  unionS[ concat[ inj(v .== (X q₁ + i) /\ i .<= (X q₂ - X q₁)) | i ← [0..n]] | n ← allN ]
  \\\ [p₁, q₁, p₂, q₂]
  where [p₁, q₁, p₂, q₂] = fresh ["p1","q1","p2","q2"] [t₁, t₂, Var u, Var v]
ɩℰ (ChIx t)      u v = choiceIndex t u v
ɩℰ (t₁ :@    t₂) u v = (inj (u .=. v) ⎧*⎫ ɩℰ (t₁) f g ⎧*⎫ ɩℰ (t₂) p q ⎧*⎫ ɩℱ g q v)
                      \\\ [f,g,p,q]
  where [f,g,p,q] = fresh ["f","g","p","q"] [t₁, t₂, Var u, Var v]
ɩℰ (Rng t)       u v = ɩℰ (t) p q ⎧*⎫ ɩℱ q u v \\\ [p,q]
  where [p,q] = fresh ["p","q"] [t,Var u, Var v]
ɩℰ (If t₀ t₁ t₂) u v = (s₀ ⎧*⎫ ɩℬ (t₁) u v \\\ xs) ⊍ (not (s₀ \\\ xs) ⎧*⎫ ɩℬ (t₂) u v)
  where xs = bvs(t₀); s₀ = one(ɩ𝒞(t₀),xs)
ɩℰ (Block t) u v = ɩℬ (t) u v
ɩℰ (For t₀ t₁) u v = fold(op,z,ɩ𝒞(t₀))
  where [p,q,u₁,u₂,v₁,v₂]  = fresh ["p","q","u1","u2","v1","v2"] [t₀, t₁, Var u, Var v]
        xs           = bvs(t₀)
        s₁ :: M(Env) = ɩℬ(t₁) p q
        z  :: M(Env) = inj(u .== nil /\ v .== nil)
        op :: (XSet(Env), M(Env)) → M(Env)
        op(d,m) =     inj(u .== (X u₁ ⊕ X u₂) /\ v .== (X v₁ ⊕ X v₂))
                  ⎧*⎫ ((inj(d) ⎧*⎫ s₁ ⎧*⎫ inj(u₁ .== oneTuple(p) /\ v₁ .== oneTuple(q)) \\\ [p,q] \\\ xs)
                      ⊍ (inj(compl(d \\ xs)) ⎧*⎫ inj(u₁ .== nil /\ v₁ .== nil)))
                  ⎧*⎫ ((m ⎧*⎫ inj(u .=. u₂ /\ v .=. v₂)) \\\ [u,v])
                 \\\ [u₁,u₂,v₁,v₂]
          
ɩℰ (Fun(tₐ)(C)(ω)(tb)) h f | ω == succeeds =
--  trace("enns=" P.++ show envs) $
--  trace("allfuns=" P.++ show (P.map funs (Set.toList' envs))) $
  unionS [ mapFilter(keep(ρ),funs(ρ)) | ρ <- envs ]
  where

    sa :: M(Env)
    sa = ɩℰ (tₐ) x w   -- pu=x, pv=w
    sb :: M(Env)
    sb = ɩℰ (tb) j z   -- ru=j, rv=z

    funs :: Env -> M(M(Val,(Val,Val,Val)))   -- (x,(w,j,z))
    funs(ρ) = piM(domvs, rngvs)
      where
        dom :: M(Env)
        dom = ([ρ] \\\ bavs \\\ [x,w]) ⎧*⎫ sa
        domvs :: M(Val)
        domvs = allValuesOf x dom
        rngvs :: M(Val,(Val,Val,Val))
        rngvs = unionSM(xSetToSet(collapse(domvs))) $ \ xv ->
                intersectSM[aρ | aρ <- xSetToSet(collapse(dom)), aρ·x == xv] $ \ aρ ->
--                trace("rng xv=" P.++ show xv P.++ " aρ=" P.++ show aρ P.++ " * " P.++ show (([aρ] \\\ bbvs \\\ [j,z]) ⎧*⎫ sb)) $
                [ (xv, (bρ·w, bρ·j, bρ·z)) | bρ <- ([aρ] \\\ bbvs \\\ [j,z]) ⎧*⎫ sb ]

    keep :: Env -> M(Val,(Val,Val,Val)) -> Set(Env)
    keep(ρ)(fun) --x | ρ·f == F fm && ρ·h == F hm = [ρ]
                 | isFunction fm && isFunction hm = [ρ `extend` [(f, F fm), (h, F hm)] ]
                 | otherwise                  = []
      where fm = Fn $ mapM'(\(x,(_,_,z))->(x,z), fun')
            hm = Fn $ mapM'(\(_,(w,j,_))->(w,j), fun')
            fun' = prune(fun)

    -----
    envs :: Set(AEnv)
    envs = generateENVs (favs ∪ (fbvs `Set.difference` bavs) ) -- ∪ [f, h])
    favs = fvs(tₐ)
    bavs = bvs(tₐ)
    fbvs = fvs(tb)
    bbvs = bvs(tb)
    [x,w,j,z] = fresh ["x","w","j","z"] [tₐ,tb,Var f,Var h]

ɩℰ t _ _ = error $ "ɩℰ: unimplemented " P.++ show t

ɩ𝒞 :: Term → M(Env)
ɩ𝒞 (t) = ɩℰ (t) p q \\\ [p, q]
  where [p,q] = fresh ["p","q"] [t]

ɩℬ :: Term → Iden → Iden → M(Env)
ɩℬ (t) u v = ɩℰ (t) u v \\\ bvs(t)

-- XXX only include Env where f succeeds somewhere?
ɩℱ :: Iden → Iden → Iden → M(Env)
ɩℱ f x r = --prune $  -- XXX
           unionS [ mapS (\ (prs :: Val ⇀ Val) →
                            (f .= vf /\ ((x :⇒ r) ⋵ prs)), ff)
                  | vf@(F(Fn ff)) ← allVals ]

choiceIndex :: Term -> Iden -> Iden -> M(Env)
choiceIndex at i x = unionSM (generateENVs (fvs(at))) $ \ ρ ->
  let aρm :: M(Env)
      aρm = prune(([ρ] \\\ bvs(at) \\\ [j]) ⎧*⎫ ɩℰ(at) j y \\\ [i,j,y])
      M (aρs :: [XSet(Env)]) = aρm
  in  M [ aρ /\ (i .= I k) /\ (x .= I k) | (aρ :: XSet(Env), k) <- zip aρs [0..] ]
  where [j,y] = fresh ["j","y"] [at]

{-
        ε⟦choice_index{at}⟧ix := ∪(ρ:env). 
            let(aρm:=Prune((<ρ>-BVS[at]-{j,y}) * ε⟦at⟧jy - {i})).
                <aρ | i:ℕm, i<|aρm|m, aρ:aρm_i * <bρ:env | bρ.i=i>>
-}

--------------------------------------------------------

den :: Term → M(Env)
den t = prune $ ɩℰ (Block t) u v
  where [u, v] = fresh ["u", "v"] [t]

--------------------------------------------------------

-- Below is all kinds of junk to make the semantics above look nice.


unionSM :: (ASet a, ASet b) => Set(a) → (a → M(b)) → M(b)
unionSM s f = unionS (fmap f s)

intersectSM :: forall a b . (ASet a, ASet b) => Set(a) -> (a -> M(b)) -> M(b)
intersectSM s f = dzip((∩), fmap f s)

mapFilter :: forall a b . (ASet a, ASet b) => (a → Set(b), M(a)) → M(b)
mapFilter (f, M (xs :: [XSet(a)])) = M [ setToXSet $ Set.bigUnion (fmap f (xSetToSet x)) | (x :: XSet(a)) <- xs ]

isFunction :: Fn -> Bool
isFunction (Fn (M xs)) =
  let d = P.map fst $ concatMap (toList . xSetToSet) xs
  in  P.not (L.anySame d)

allValuesOf :: Iden -> M(Env) -> M(Val)
allValuesOf x (M ss) = M (P.map f ss)
  where f s = [ v | v <- allVals, EQD.hasElem x v s ]

generateENVs :: Set(Iden) → Set(AEnv)
generateENVs = Set.mkSet . mkAEnvs . Set.toList 

--------------------------------------------------------

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

{-
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
-}
-----

newtype M a = M [XSet a]
deriving instance Eq (XSet a) => Eq (M a)
deriving instance Ord (XSet a) => Ord (M a)
--deriving instance Show (XSet a) => Show (M a)
instance Show (XSet a) => Show (M a) where
  show (M xs) = show xs
{-
instance (Show (XSet a)) => Show (M a) where
  showsPrec _ (M []) = showString "EMPTY"
  showsPrec p (M xs) = showParen (p > 0) $ showString $ L.intercalate " | " $ P.map show xs
-}
empty = M []
inj(d) = M [d]
M s ++ M t = M (s P.++ t)
M s ⎧*⎫ M t = M [ d1 ∩ d2 | d1 ← s, d2 ← t ]
M s ⎩*⎭ M t = M [ d1 ∪ d2 | d1 ← s, d2 ← t ]
s ⊍ t = dzip((∪), [s,t])
unionS ss = dzip((∪), ss)
fold(k,z,M s) = P.foldr (curry k) z s
collapse(M s) = unions s
mapM(f, M s) = undefined -- M $ P.map ( f) s
mapM'(f, M s) = M $ P.map (fmap f) s
piM (s, t) = pi s t

dzip :: (ASet a) => (XSet(a)→XSet(a)→XSet(a), Set(M(a))) → M(a)
dzip(f, xs) = M $ foldr (\ (M x) r → zipLong f x r) [] (Set.toList xs)

zipLong :: (a → a → a) → [a] → [a] → [a]
zipLong _ [] ys = ys
zipLong _ xs [] = xs
zipLong f (x:xs) (y:ys) = f x y : zipLong f xs ys

--mapFilterS :: (a → Set(b), M(a)) → M(b)
--mapFilterS = undefined

{-
instance Functor M
instance Applicative M
instance Monad M
-}

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
pattern U_ ← (F.Variable (F.isSrcUnderscore → True))
pattern Var :: Iden → Term
pattern Var i = F.Variable i
pattern Prim :: Op → Term
pattern Prim o = F.EPrim o
pattern (:=) :: Iden → Term → Term
pattern i := t = F.DefineE i t
pattern (:|:) :: Term → Term → Term
pattern t1 :|: t2 = F.Choice t1 t2
pattern (:|||:) :: Term → Term → Term
pattern t1 :|||: t2 ← F.ApplyD (F.Variable (F.Ident _       "operator'|||'")) (F.Array [t1, t2])
  where t1 :|||: t2 = F.ApplyD (F.Variable (F.Ident F.noLoc "operator'|||'")) (F.Array [t1, t2])
pattern (:=:) :: Term → Term → Term
pattern t1 :=: t2 = F.Unify t1 t2
--pattern (:~>) :: Term → Term → Term
--pattern t1 :~> t2 = ???
pattern (:->) :: Iden → Term → Term
pattern i :-> t = F.DefineIE i t
pattern (:>) :: Term → Term → Term
pattern t1 :> t2 = F.Seq t1 t2
pattern (:..) :: Term → Term → Term
pattern t1 :.. t2 = F.ApplyD (F.EPrim F.DotDot) (F.Array [t1, t2])
pattern (:@) :: Term → Term → Term
pattern t1 :@ t2 = F.ApplyD t1 t2
pattern Rng :: Term → Term
pattern Rng t = F.Range t
pattern If :: Term → Term → Term → Term
pattern If t1 t2 t3 = F.If3 t1 t2 t3
pattern For :: Term → Term → Term
pattern For t1 t2 = F.For2 t1 t2
pattern Fun t1 q w t2 = F.Function q t1 w t2
pattern Where :: Term → Term → Term
pattern t1 `Where` t2 = F.Where t1 t2
pattern Int :: Z → Term
pattern Int k = F.Lit (F.LInt k)
pattern Block :: Term → Term
pattern Block t = F.Block t
pattern Exists :: Iden → Term
pattern Exists i = F.DefineV i
pattern Array :: [Term] → Term
pattern Array as = F.Array as
pattern Fail :: Term
pattern Fail = F.Fail
pattern ChIx :: Term -> Term
pattern ChIx t = F.ChoiceIndex t

-- Make fresh identifiers from the templates ss, avoid identifiers in ts
fresh :: [String] → [Term] → [Iden]
fresh ss ts = snd $ L.mapAccumL f (concatMap F.getAllBinders ts) ss
  where
    f :: [Iden] → String → ([Iden], Iden)
    f is s = (i:is, i)
          where i = (free L.\\ is) !! 0         -- avoid head
                free = [ F.Ident F.noLoc (s P.++ {-"_" ++-} i) | i ← "" : P.map show [1 :: Integer ..] ]

-- All top level binders
bvs :: Term → Set Iden
bvs = mkSet . F.getVisibleBinders

fvs :: Term → Set Iden
fvs = mkSet . filter (\ (F.Ident _ s) -> s `notElem` globals) . F.getFree
  where globals = "operator'|||'" : P.map P.fst knownFunsF

type Aperture = F.Aperture
pattern O = F.Open
pattern C = F.Closed
type Eff = F.Eff
succeeds :: Eff
succeeds = F.effSucceeds

----- primops -----

dP :: Op → Fn
dP F.Neg = fun[funNegate]
dP F.IsInt = fun[funInt]
dP F.Add = fun[funAdd]
dP F.Sub = fun[funSub]
dP F.Mul = fun[funMul]
dP F.Div = fun[funDiv]
dP F.Gt = fun[funGt]
dP F.Lt = fun[funLt]
dP p = error $ "dP undefined " P.++ show p

knownFuns :: [(Fn, String)]
knownFuns =
       [ (dP o, P.map P.toLower (show o)) | o ← [F.Neg, F.IsInt, F.Add, F.Sub, F.Mul, F.Div, F.Gt, F.Lt] ]
  P.++ [ (fun[funSucc], "succ")
       , (fun[funPred], "pred")
       , (fun[funConst0], "const0")
       , (fun[funConst1], "const1")
       , (fun[fun0to1], "f0to1")
       , (fun[funEven], "even")
       , (fun[funOdd], "odd")
       -- 
       , (fun[funBin], "bin")
       , (fun[funBinInv], "binInv")
       , (fun[funBinCon0], "binCon0")
       , (fun[funBinCon1], "binCon1")
       , (fun[funBinHO1], "binHO1")
       ]

knownFunsF :: [(String, Fn)]
knownFunsF = P.map (\(x,y)->(y,x)) knownFuns

fun :: [Val ⇀ Val] → Fn
fun = Fn . concat . P.map inj

funNegate :: Val ⇀ Val
funNegate = [(I i, I ((-i) `mod` numZ)) | i ← allZ ]

funInt :: Val ⇀ Val
funInt = [(I i, I i) | i ← allZ ]

funEven :: Val ⇀ Val
funEven = [(I i, I i) | i ← allZ, even i ]

funOdd :: Val ⇀ Val
funOdd = [(I i, I i) | i ← allZ, odd i ]

funAdd :: Val ⇀ Val
funAdd = [ (T [I x, I y], I ((x+y) `mod` numZ)) | x ← allZ, y ← allZ ]

funSub :: Val ⇀ Val
funSub = [ (T [I x, I y], I ((x-y) `mod` numZ)) | x ← allZ, y ← allZ ]

funMul :: Val ⇀ Val
funMul = [ (T [I x, I y], I ((x*y) `mod` numZ)) | x ← allZ, y ← allZ ]

funDiv :: Val ⇀ Val
funDiv = [ (T [I x, I y], I ((x `div` y) `mod` numZ)) | x ← allZ, y ← allZ, y /= 0 ]

funGt :: Val ⇀ Val
funGt = [ (T [I x, I y], I x) | x ← allZ, y ← allZ, x > y ]

funLt :: Val ⇀ Val
funLt = [ (T [I x, I y], I x) | x ← allZ, y ← allZ, x < y ]

funSucc :: Val ⇀ Val
funSucc = [(I i, I ((i + 1) `mod` numZ)) | i ← allZ ]

funPred :: Val ⇀ Val
funPred = [(I i, I ((i - 1) `mod` numZ)) | i ← allZ ]

funConst0 :: Val ⇀ Val
funConst0 = [(I i, I 0) | i ← allZ ]

funConst1 :: Val ⇀ Val
funConst1 = [(I i, I 1) | i ← allZ ]

-- All functions with domain and range {0,1}
fun0to1 :: Val ⇀ Val
fun0to1 = [(I 0, I 1)]

funBin :: Val ⇀ Val
funBin = [(I 0, I 0), (I 1, I 1)]

funBinInv :: Val ⇀ Val
funBinInv = [(I 0, I 1), (I 1, I 0)]

funBinCon0 :: Val ⇀ Val
funBinCon0 = [(I 0, I 0), (I 1, I 0)]

funBinCon1 :: Val ⇀ Val
funBinCon1 = [(I 0, I 1), (I 1, I 1)]

funBinHO1 :: Val ⇀ Val
funBinHO1 = [(fcn "binCon0", I 0),(fcn "bin", I 0),(fcn "binInv", I 1),(fcn "binCon1", I 1)]

fcn :: String -> Val
fcn s = P.maybe (error $ "fcn " P.++ s) (\ f -> F f) $ P.lookup s knownFunsF


-- Apply a partial function
applyPF :: (Val ⇀ Val) → Val → Maybe Val
applyPF f x = Set.getSing $ Set.lookupSet x f

----- Val -----

data Val = I Z | F Fn
  deriving (Eq, Ord)
instance Show Val where
  showsPrec p (I i) = showsPrec p i
  showsPrec _ (T vs) = showString $ "〈" P.++ L.intercalate "," (P.map show vs) P.++ "〉"
  showsPrec p (F f) = showsPrec p f

pattern T :: [Val] -> Val
pattern T vs <- (getTuple -> Just vs)
  where
    T xs = F $ fun $ zipWith (PFSing . I) [0..] xs

getTuple :: Val -> Maybe [Val]
getTuple (I _) = Nothing
getTuple (F (Fn (M xs))) = M.zipWithM f [0..] xs
  where f i s | PFSing (I i') y <- s, i == i' = Just y
        f _ _ = Nothing

newtype Fn = Fn (M (Val :⇒ Val))
  deriving (Eq, Ord)

instance Show Fn where
  show f | Just s ← P.lookup f knownFuns = s
  show (Fn f) = "Fn" P.++ show f

pattern PFSing :: Val -> Val -> (Val ⇀ Val)
pattern PFSing x y <- (Set.getSing -> Just (x, y))
  where PFSing x y = Set.sing (x, y)

type a ⇀ b = Set(a :⇒ b)   -- partial functions from a to b
dom :: (a ⇀ b) → Set(a)
dom = fmap fst

infix 1 :⇒
type a :⇒ b = (a, b)       -- pairs used to form functions
pattern a :⇒ b = (a, b)

----- "all" values -----
numZ :: Z
numZ = 4

allZ :: Set Z
allZ = mkSet [i | i ← [0 .. numZ-1] ]

allVals :: Set(Val)
allVals = [ I i | i ← allZ ]
        ∪ allFuns
        ∪ allTuples
-- test        ∪ [ F (fun[funInt]), T [I 0, I 0] ]

allFuns :: Set(Val)
allFuns = mkSet [ F f | (f, _) ← knownFuns ]

-- 0, 1, 2-tuples of numbers
allTuples :: Set(Val)
allTuples = mkSet $ (P.concat :: [[Val]] → [Val])
  [ [ T []]
  , [ T [I x] | x ← [0 .. numZ-1] ]
  , [ T [I x, I y] | x ← [0 .. numZ-1], y ← [0 .. numZ-1] ]
  ]

----- XSet -----

type ASet :: Type → Constraint
class (Eq (XSet a), Ord (XSet a), Ord a, Show a, Show (XSet a)) => ASet a where
  type XSet a = r | r → a
  ø :: XSet(a)
  isEmpty :: XSet(a) → Bool
  (∪) :: XSet(a) → XSet(a) → XSet(a)
  (∩) :: XSet(a) → XSet(a) → XSet(a)
--  mapXSet :: (a → a) → XSet(a) → XSet(a)
  asing :: a → XSet a
--  abind :: XSet a → (a → XSet(b)) → XSet(b)
  setToXSet :: Set(a) → XSet(a)
  xSetToSet :: HasCallStack => XSet(a) → Set(a)

instance ASet Env where
  type XSet Env = ENV
  ø = EQD.false
  (∪) = (EQD.\/)
  (∩) = (EQD./\)
  isEmpty x = x == EQD.false
  asing = aEnvToENV
  setToXSet = Set.foldSetE (∪) ø . fmap asing
  xSetToSet :: ENV -> Set(Env)
  xSetToSet e = mkSet $ P.map f $ EQD.mcubes e
    where
      f = Env . P.map g
      g (EQD.Var x, EQD.Val v, True) = (x, v)
      g x = error $ "xSetToSet: unimplemented " P.++ show x

{-
instance ASet AEnv where
  type XSet AEnv = Set AEnv
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id
-}

-- coulbe be more clever if it mattered
instance ASet () where
  type XSet () = Set ()
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id

instance ASet Integer where
  type XSet Integer = Set Integer
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id

instance ASet Val where
  type XSet Val = Set Val
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id

instance ASet Iden where
  type XSet Iden = Set Iden
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id

instance (ASet a, ASet b) => ASet (a, b) where
  type XSet (a, b) = Set (a, b)
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id
  
instance (ASet a, ASet b, ASet c) => ASet (a, b, c) where
  type XSet (a, b, c) = Set (a, b, c)
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id
  xSetToSet = id
  
instance (ASet a) => ASet (Set(a)) where
  type XSet (Set a) = Set (Set(a))
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id  
  xSetToSet = id

instance (Ord (XSet a), Show (XSet a)) => ASet (M a) where
  type XSet (M a) = Set (M(a))
  ø = Set.empty
  (∪) = Set.union
  (∩) = Set.intersect
  isEmpty = Set.isEmpty
  asing = Set.singleton
  setToXSet = id  
  xSetToSet = id

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
mkSet :: [a] → Set a
mkSet = Set.mkSet

unions :: ASet a => [XSet(a)] → XSet(a)
unions = P.foldr (∪) ø

----- Iden -----
type Iden = F.Ident

----- Env -----
type Env = AEnv

newtype AEnv = Env [(Iden, Val)]   -- only used for functions
  deriving (Eq, Ord, Show)

(·) :: HasCallStack => AEnv → Iden → Val
Env kvs · x = P.fromMaybe (error $ "getSing " P.++ show x) $ P.lookup x kvs

extend :: AEnv → [(Iden, Val)] → AEnv
extend (Env kvs) kvs' = Env (kvs P.++ kvs')

aEnvToENV :: AEnv → ENV
aEnvToENV (Env kvs) = foldr (/\) univ $ P.map (uncurry (.=)) kvs

-- Make all possible environments with the given identifiers.
mkAEnvs :: [Iden] → [AEnv]
mkAEnvs is = P.map (Env . P.zip is) (M.replicateM (length is) $ Set.toList allVals)

aEnvsToENV :: Set(AEnv) → ENV
aEnvsToENV = foldr (\/) cempty . P.map aEnvToENV . Set.toList

----- Simple values -----

data Atom = V Val | X Iden | AOp Op [Atom] | ATup [Atom]
  deriving (Eq, Ord, Show)

allN :: Set(Atom)
allN = [ V (I i) | i ← allZ, i >= 0 ]

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
(⊕) :: Atom → Atom → Atom
x ⊕ y = AOp F.ArrApp [x, y]

nil :: Atom
nil = ATup []

tup :: [Iden] → Atom
tup vs = ATup (P.map X vs)

oneTuple :: Iden → Atom
oneTuple x = ATup [X x]

atomEval :: AEnv → Atom → Maybe Val
atomEval r (X x)      = Just (r · x)
atomEval _ (V v)      = Just v
atomEval r (ATup as)  = T <$> P.mapM (atomEval r) as
atomEval r (AOp o as) =
  case (o, P.mapM (atomEval r) as) of
    (F.Neg, Just [I i])        → Just $ I $ (-i)    `mod` numZ
    (F.Add, Just [I i1, I i2]) → Just $ I $ (i1+i2) `mod` numZ
    (F.Sub, Just [I i1, I i2]) → Just $ I $ (i1-i2) `mod` numZ
    (F.Mul, Just [I i1, I i2]) → Just $ I $ (i1*i2) `mod` numZ
    (F.Div, Just [I i1, I i2]) → Just $ I $ (i1 `div` i2)
    (F.ArrApp, Just[T xs1, T xs2]) → Just $ T $ xs1 P.++ xs2
    _                          → Nothing

atomVars :: Atom → [Iden]
atomVars (X x) = [x]
atomVars (V _) = []
atomVars (ATup  as) = P.foldr L.union [] (P.map atomVars as)
atomVars (AOp o as) = P.foldr L.union [] (P.map atomVars as)

atomEnvs :: [Atom] → [([Val], AEnv)]
atomEnvs as =
  let envs = mkAEnvs $ P.foldr L.union [] $ P.map atomVars as
  in  [ (vs, r) | r ← envs, Just vs ← [P.mapM (atomEval r) as] ]

(.==) :: Iden → Atom → ENV
i .== a = unionENVs [ i .= v /\ aEnvToENV r | ([v], r) ← atomEnvs [a], v ∈ allVals ]
(.<=) :: Atom → Atom → ENV
a1 .<= a2 = unionENVs [ aEnvToENV r | ([I v1, I v2], r) ← atomEnvs [a1, a2], v1 <= v2 ]

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

unionENVs :: [ENV] → ENV
unionENVs = P.foldr (\/) cempty

bigUnionENV :: Set(ENV) → ENV
bigUnionENV = unionENVs . Set.toList

-- Environments where the pair (x :⇒ y) is in the function f
(⋵) :: (Iden :⇒ Iden) → (Val ⇀ Val) → ENV
(x :⇒ y) ⋵ f =
  unionENVs [ x .= u /\ y .= v | u ← Set.toList allVals, Just v ← [applyPF f u] ]

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
--piM      :: (M(a), a → M(b)) → M(M(a :⇒ b))
piM      :: (ASet a, ASet b) => (M(a), M(a,b)) → M(M(a :⇒ b))
collapse :: (ASet a) => M(a) → XSet(a)
mapM     :: (ASet a, ASet b, HasCallStack) => (a→b, M(a)) → M(b)
--mapM     :: ASet a => (a→a, M(a)) → M(a)
mapM'    :: ((a,b)→(c,d), M(a,b)) → M(c,d)

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

bigStar :: [M(Env)] → M(Env)
bigStar [] = error "bigStar: []"
bigStar xs = P.foldr1 (⎧*⎫) xs

---------------------------

type MyMonad :: (Type → Type) → Constraint
class MyMonad m where
  type MValid m a :: Constraint
  return :: (HasCallStack, MValid m a) => a → m a
  (>>=)  :: (HasCallStack, MValid m a, MValid m b) => m a → (a → m b) → m b
  (>>)   :: (HasCallStack, MValid m a, MValid m b) => m a → m b → m b
  guard  :: (HasCallStack) => Bool → m ()
  a >> b = a >>= const b

instance MyMonad [] where
  type MValid [] a = ()
  return = P.return
  (>>=)  = (P.>>=)
  (>>)   = (P.>>)
  guard  = M.guard

instance MyMonad Set where
  type MValid Set a = ()
  return = Set.singleton
  (>>=)  = (P.>>=)
  guard  = M.guard

instance MyMonad M where
  type MValid M a = (ASet a)
  return x = M [asing x]
  -- Only allow comprehension syntax for fmap&filter like expressions.
  (>>=) :: forall a b . (HasCallStack, MValid M a, MValid M b) => M a → (a → M b) → M b

  M xs >>= k = M [ y | (x :: XSet a) <- xs, let M ys = unionS $ fmap k (xSetToSet x), y <- ys ]
{-
  M (as :: [XSet a]) >>= k = M $ P.map xmap as
    where purek :: HasCallStack => a → Maybe b
          purek x =
            case k x of
              M [s] | Just y ← Set.getSing (xSetToSet s) → Just y
              M [] -> Nothing
              _ → error "MyMonad M: k is not pure"
          xmap :: XSet a → XSet b
          xmap = setToXSet . Set.mapMaybe purek . xSetToSet
-}
  guard True = M [asing ()]
  guard False = M []

instance ASet a => IsList (M a) where
  type Item (M a) = a
  fromList [x] = M [asing x]
  toList = undefined

---------------------------


class Pi m where
  pi :: (ASet a, ASet b) => m a -> m (a,b) -> m (m (a,b))

instance Pi Set where
--  pi s t | trace("pi Set " P.++ show(s, t)) False = undefined
  pi s _ | Set.isEmpty s =
    [ [] ]
  pi (toList -> (x:xs)) ys =
    [ Set.add (x,y) xys
    | (x',y) <- ys
    , x == x'
    , xys <- pi (mkSet xs) ys
    ]

instance Pi [] where
  pi [] _ =
    [[]]

  pi (x:xs) ys =
    [ (x,y):xys
    | (x',y) <- ys
    , x==x'
    , xys <- pi xs ys
    ]

instance Pi M where
  pi :: forall a b . (ASet a, ASet b) => M a -> M (a,b) -> M (M (a,b))
--  pi x y | trace("pi M " P.++ show(x,y)) False = undefined
  pi (M []) _ =
    M [ [ M [] ] ]

  pi (M [xs]) (M [ys]) =
    M [ fmap (M . (:[])) $ pi (xSetToSet xs) (xSetToSet ys) ]

  pi (M [xs]) (M []) =
    M []
  pi (M [(xs :: XSet a)]) (M ((ys :: XSet(a,b)) : yss)) =
        pi (M [xs]) (M [ys])
--     ++ unionS middle
     ++ pi (M [xs]) (M yss)
     where middle :: Set(M(M(a,b)))
           middle = [ [ unionS [f1, f2] | f1 <- pi (M [xs1]) (M [ys])
                                        , f2 <- pi (M [xs2]) (M yss) ]
                    | (xs1, xs2) <- parts xs
                    , P.not (isEmpty xs1) && P.not (isEmpty xs2)
                    ]

  pi (M (xs:xss)) rng =
    [ x ++ y | x <- pi (M [xs]) rng, y <- pi (M xss) rng ]

parts :: (ASet a) => XSet(a) -> Set(XSet(a), XSet(a))
parts = fmap (\ (s1,s2) -> (setToXSet s1, setToXSet s2)) . Set.partitions . xSetToSet

xSetToList :: (ASet a) => XSet a -> [a]
xSetToList = Set.toList' . xSetToSet

listToXSet :: (ASet a) => [a] -> XSet a
listToXSet = setToXSet . mkSet

-----------------------------------------


x = F.Ident F.noLoc "x"
y = F.Ident F.noLoc "y"
z = F.Ident F.noLoc "z"
w = F.Ident F.noLoc "w"
j = F.Ident F.noLoc "j"
u = F.Ident F.noLoc "u"
v = F.Ident F.noLoc "v"
p = F.Ident F.noLoc "p"
q = F.Ident F.noLoc "q"
f = F.Ident F.noLoc "f"
u1 = F.Ident F.noLoc "u1"
v1 = F.Ident F.noLoc "v1"
u2 = F.Ident F.noLoc "u2"
v2 = F.Ident F.noLoc "v2"
fsucc = F.Ident F.noLoc "succ"
int = Prim F.IsInt
apa = F.Ident F.noLoc "apa"


eE = ɩℰ
bB = ɩℬ
cC = ɩ𝒞
fF = ɩℱ

cint :: Term
cint = Rng (Prim F.IsInt)

plus :: Term -> Term -> Term
plus x y = (Prim F.Add) :@ Array[x,y]

test1 = [ (x,x) | x <- return 1, x==2 ] :: M (Integer, Integer)

main = print $ den $ Array [Int 1, Int 2]

--  cons(x:A,xs:[]A) = for(a:=0|1; y:if(a=0){array{x}else{ys}){y}

