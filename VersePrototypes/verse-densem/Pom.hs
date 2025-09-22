{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pom where
import Control.Applicative
import Control.Monad
import Epic.List
import qualified Data.List as L
import FrontEnd.Expr hiding(Tuple)
import ValueS
import ENVS
import qualified Set
import Set(Set)
import Debug.Trace

default ()

infixl 6 :\/
infixl 8 :++

data P a
  = Empty
  | Unit a
  | P a :++ P a
  | P a :\/ P a
  deriving (Show)

--instance (Show a, Ord a) => Show (P a) where
--  showsPrec p s = showsPrec p (canon $ absorbEmpty s)

instance (Ord a) => Eq (P a) where
  x == y  =  compare x y == EQ

instance (Ord a) => Ord (P a) where
  compare x y = compare (canon x) (canon y)

unit :: a -> P a
unit a = Unit a

none :: P a
none = Empty

-- Smart constructor for :++
infixl 8 +++
(+++) :: P a -> P a -> P a
Empty +++ y = y
x +++ Empty = x
x +++     y = x :++ y

-- Smart constructor for :\/
infixl 6 `union`
union :: P a -> P a -> P a
union Empty y = y
union x Empty = x
union x     y = x :\/ y

instance Functor P where
  fmap f s = s >>= pure . f

instance Applicative P where
  pure = Unit
  (<*>) = ap

instance Monad P where
  return          = pure
  Empty     >>= _ = Empty
  Unit x    >>= k = k x
  (s :++ t) >>= k = (s >>= k) +++ (t >>= k)
  (s :\/ t) >>= k = (s >>= k) `union` (t >>= k)

instance Alternative P where
  empty = Empty
  (<|>) = union

cONC :: [P a] -> P a
cONC = foldr (+++) Empty

--pfilter :: (a -> Bool) -> P a -> P a
--pfilter p s = [ y | x <- s, y <- if p x then Unit x else Empty ]

----------------------------------------------------
--
--            Functions over PENV = P ENV
--
----------------------------------------------------

type PENV = P ENV
     -- Invariants of PENV
     --     (I1) Empty only at the root
     --     (I2) No (Unit ENVS.empty) anywhere

mkUnit :: ENV -> PENV
-- Establish (I2)
mkUnit d | d == ENVS.empty = Empty
         | otherwise       = Unit d

-- Sequencing
infixl 8 ***
(***) :: P ENV -> P ENV -> P ENV
s1 *** s2 = do { d1 <- s1
               ; d2 <- s2
               ; mkUnit (d1 /\ d2) }

-- Disjunction
disj :: P ENV -> P ENV -> P ENV
disj Empty s = s
disj s Empty = s
disj s1 s2 = [ d1 \/ d2 | d1 <- s1, d2 <- s2 ]

nOT :: PENV -> PENV
nOT Empty = unit univ
nOT s     = do { d <-s; mkUnit (compl d) }

infixl 7 >>>
(>>>) :: P ENV -> [Ident] -> P ENV
s >>> xs = fmap (hides xs) s

{-
This is hard to implement
uNION :: Set (P a) -> P a
-}

canon :: P a -> Set [a]
canon Empty = Set.empty
canon p = canon' p
  where
    canon' Empty = error "canon' : Empty"
    canon' (Unit a) = Set.singleton [a]
    canon' (s :\/ t) = canon' s `Set.union` canon' t
    canon' (Unit a :++ s) = [ a : as | as <- canon' s ]
    canon' ((s :\/ t) :++ r) = canon' (s :++ r) `Set.union` canon' (t :++ r)
    canon' ((s :++ t) :++ r) = canon' (s :++ (t :++ r))

uncanon :: Ord a => Set [a] -> P a
uncanon s | Set.isEmpty s = Empty
uncanon s = Set.foldSet union [ foldr (+++) Empty (map unit xs) | xs <- s ]

------------------

oNE :: [Ident] -> PENV -> PENV
oNE _ Empty = Empty
oNE _ (Unit d) = Unit d
oNE xs (s :\/ t) = oNE xs s `union` oNE xs t
oNE xs (s :++ t) = s1 `disj` (
    --(fmap compl s1 >>> xs)
    nOT (s1 >>> xs)
    *** oNE xs t)
  where s1 = oNE xs s

nil :: Value
nil = Tuple []
sing :: Value -> Value
sing x = Tuple [x]

fOR :: [Ident] -> P ENV -> SrcEssential -> Ident -> Ident -> P ENV
fOR _  Empty     _ i x = unit (i .= nil /\ x .= nil)
fOR xs d@Unit{}  t i x =
  d *** unit sings *** dE t p q >>> (p:q:xs)
  `union` 
  nOT (d >>> xs) *** unit (i .= nil /\ x .= nil)
  where (p, q) = fresh2 ("p", "q") (i:x:xs) t
        sings = bigUnion [i .= sing ip /\ x .= sing iq /\ p .= ip /\ q .= iq
                         | ip <- allInts
                         , iq <- allInts
                         ]
fOR xs (a :\/ b) t i x = fOR xs a t i x `union` fOR xs b t i x
fOR xs (a :++ b) t i x =
--  trace ("FOR " ++ show (fOR xs a t u1 v1, fOR xs b t u2 v2)) $
  (unit sings *** fora *** forb) >>> (u1:v1:u2:v2:xs)
  where fora = fOR xs a t u1 v1
        forb = fOR xs b t u2 v2
        (u1, v1) = fresh2 ("u1","v1") (i:x:xs) t
        (u2, v2) = fresh2 ("u2","v2") (i:x:xs) t
        sings = bigUnion [ i .= tu1 `app` tu2 /\ x .= tv1 `app` tv2 /\
                           u1 .= tu1 /\ u2 .= tu2 /\ v1 .= tv1 /\ v2 .= tv2
                         | tu1 <- allValuesOf u1 fora
                         , tu2 <- allValuesOf u2 forb
                         , tv1 <- allValuesOf v1 fora
                         , tv2 <- allValuesOf v2 forb
                         ]
        app (Tuple xs) (Tuple ys) = Tuple (xs ++ ys)
        app _ _ = undefined

allValuesOf :: Ident -> P ENV -> [Value]
allValuesOf _ Empty = []
allValuesOf x (a :++ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (a :\/ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (Unit d) = extractVar d x

dE :: SrcEssential -> Ident -> Ident -> P ENV
-- The denotational semantics itself (Fig 10)
dE (Lit (LInt k))                   i x = unit $ i .=. x /\ x .= Int (fromIntegral k)
dE (EPrim p)                        i x = unit $ i .=. x /\ x .= Fun (dP p)
dE (Variable (Ident _ "xf"))        i x = unit $ i .=. x /\ x .= Fun funXF -- hack for testing
dE (Variable v) i x | isSrcUnderscore v = unit $ i .=. x
                    | otherwise         = unit $ i .=. x /\ x .=. v
dE (DefineE y t)                    i x = unit (x .=. y) *** dE t i x
dE (DefineIE y t)                   i x = unit (i .=. y) *** dE t i x
dE (DefineV y)                      i x = unit $ i .=. x /\ x .=. y
dE (Unify t0 t1)                    i x = dE t0 i x *** dE t1 i x
dE (Choice t0 t1)                   i x = dB t0 i x +++ dB t1 i x
dE (Seq t0 t1)                      i x = dC t0     *** dE t1 i x
dE (Where t0 t1)                    i x = dE t0 i x *** dC t1
dE (Block t)                        i x = dB t i x
dE Fail                             _ _ = Empty
dE (ApplyD (Variable (Ident _ "operator'|||'")) (Array [t0, t1])) i x =
  dE t0 i x `union` dE t1 i x
dE (If3 t0 t1 t2)                   i x =
  s0 *** dB t1 i x >>> xs
  `union`
  nOT (s0 >>> xs) *** dB t2 i x
  where xs = bvs t0
        s0 = oNE xs (dC t0)
dE t@(For2 t0 t1) i x = fOR (bvs t0) (dC t0) t1 i x
dE t@(ApplyD (EPrim DotDot) (Array [t0, t1])) i x =
  dE t0 a l *** dE t1 b h *** unit (i .=. x) ***
  (let
    mkSeq :: Int -> Int -> [ENV]
    mkSeq lo hi = [ x .= Int v /\ l .= Int lo /\ h .= Int hi | v <- [ lo .. hi ] ]
    allSeqs :: Set [ENV]
    allSeqs = [ mkSeq start end
              | start <- Set.mkSetUnsafe allInts'
              , end   <- Set.mkSetUnsafe [ start .. numInt-1 ]
              ]
   in uncanon allSeqs
  ) >>> [a,l,b,h]
    where (a, l) = fresh2 ("a", "l") [i, x] t
          (b, h) = fresh2 ("b", "h") [i, x] t
dE t@(Array ts)                     i x =
  foldl1 (***) (et : es) >>> (is ++ xs)
  where n = length ts
        used = i:x:getFree t
        is = take n $ freshList "i" used
        xs = take n $ freshList "x" used
        es = zipWith3 dE ts is xs
        tupvals = allTuplesLen n
        et = unit $ bigUnion [ i .= Tuple ivals /\
                               x .= Tuple xvals /\
                               bigIntersect (zipWith (.=) is ivals) /\
                               bigIntersect (zipWith (.=) xs xvals)
                             | ivals <- tupvals, xvals <- tupvals
                             ]
-- A speedup for x:int
dE (Range (EPrim IsInt))            i x = unit (bigUnion [ i .= v /\ x .= v | v <- allInts ])
dE (Range t)                        i x =
  dE t j y *** dF y i x >>> [j,y]
    where (j, y) = fresh2 ("j", "y") [i, x] t
dE t@(ApplyD t0 t1)                 i x =
  dE t0 h f *** dE t1 j y *** dF f y x *** unit (i .=. x) >>> [h,f,j,y]
    where (h, f) = fresh2 ("h", "f") [i, x] t
          (j, y) = fresh2 ("j", "y") [i, x] t

dE e                               _ _ = error $ "dE: unimplemented " ++ show e

dF :: Ident -> Ident -> Ident -> P ENV
dF f a r =
  uncanon $
  [ [ f .= Fun hs /\ bigUnion [ a .= u /\ r .= v
                              | u <- allValues  -- list
                              , Just v <- [applyPF h u]
                              ]
    | h <- hs -- list
    ]
  | hs <- Set.mkSetUnsafe allFUNs -- set
  ] `Set.union`
  [ [ f .= tt /\ a .= Int u /\ r .= (vs !! u)
    | u <- [0 .. length vs - 1] -- list
    ]
  | tt@(Tuple vs) <- Set.mkSetUnsafe allTuples -- set
  ]

{-
-- A hack to avoid iterating over so many values
valsOf :: [Ident] -> ENV -> [Value]
valsOf is e = nub $ concatMap (extractVar e) is
-}


i=Ident noLoc "i"
j=Ident noLoc "j"
k=Ident noLoc "k"
x=Ident noLoc "x"
y=Ident noLoc "y"
z=Ident noLoc "z"
a=Ident noLoc "a"
h=Ident noLoc "h"
f=Ident noLoc "f"
r=Ident noLoc "r"
n=Ident noLoc "n"
p=Ident noLoc "p"
q=Ident noLoc "q"
xf=Ident noLoc "xf"
k0=Lit (LInt 0)
k1=Lit (LInt 1)
k2=Lit (LInt 2)
k3=Lit (LInt 3)
--t0= EPrim Gt
--t1= Array [Lit (LInt 1), Lit (LInt 0)]
--t0=DefineE a (Choice (Lit (LInt 1)) (Lit (LInt 2))) `Seq` ApplyD (EPrim Gt) (Array [Variable a, Lit (LInt 0)])
{-
t0=DefineE a (ApplyD (EPrim Gt) (Array [Lit (LInt 3), Variable x]) `Seq`
              ApplyD (EPrim Gt) (Array [Variable x, Lit (LInt 0)])
             )
t1=Variable a
-}
u=Ident noLoc "u"
v=Ident noLoc "v"
--t0=DefineIE x (k0 `Choice` k1)
--t1=For2 t0 (Variable x)
t0=DefineIE x (ApplyD (EPrim DotDot) (Array [k1, Variable n]))
t1=Variable x
tfor = For2 t0 t1
tn=Variable n `Unify` k1
tt = tn `Seq` tfor

{-
ix :: [ ENV ] -> Int -> ENV
ix es i | i >= 0 && i < length es = es !! i
        | otherwise = ENVS.empty

conc :: [Value] -> Value
conc vs = Tuple $ concatMap (\ (Tuple ys) -> ys) vs
-}

dB :: SrcEssential -> Ident -> Ident -> P ENV
dB e i x = dE e i x >>> bvs e

dC :: SrcEssential -> P ENV
dC e = dE e i x >>> [i,x]  where (i, x) = fresh2 ("i", "x") [] e

dP :: PrimOp -> FUN
dP Neg = [funNegate]
dP IsInt = [funInt]
dP Gt = [funGt]
dP Lt = [funLt]
dP Add = [funAdd]
dP Sub = [funSub]
dP Mul = [funMul]
dP Div = [funDiv]
dP p = error $ "dP undefined " ++ show p

{-
firstK :: [Ident] -> [ENV] -> ENV
firstK _  []         = empty
firstK ys (env:envs) = env \/ (compl (hides ys env) /\ firstK ys envs)

first :: [Ident] -> [ENV] -> ENV
first _  []     = empty
first xs (d:ds) = d \/ (first xs ds \\\ hides xs d)

squashTail :: [ENV] -> [ENV]
squashTail = revDropWhile (== empty)
-}

{-
squash :: P ENV -> P ENV
squash = fmap (filter (/= ENVS.empty))

  
outerUnion :: P ENV -> P ENV -> P ENV
outerUnion = union

lift2 :: forall a b c . (a -> b -> c) -> P a -> P b -> P c
lift2 op sl1 sl2 =
  [ concat ss 
  | s1 :: [a] <- sl1
  , let t  :: [Set[c]] = [ mapP (op d1) sl2 | d1 <- s1 ]
        ft :: Set [[c]]= flatten t
  , ss :: [[c]] <- ft
  ]

flatten :: [Set a] -> Set[a]
flatten []     = singleton []
flatten (s:ss) = [ x:xs | x <- s, xs <- flatten ss ]

remvL :: [ENV] -> [Ident] -> [ENV]
remvL s xs = map (hides xs) s
-}

fresh2 :: (String, String) -> [Ident] -> SrcEssential -> (Ident, Ident)
fresh2 (sx, sy) is t = (x, y)
  where x = fresh sx vs
        y = fresh sy (x:vs)
        vs = is ++ getAllBinders t

bvs :: SrcEssential -> [Ident]
bvs = getVisibleBinders

-------

den :: SrcEssential -> Set [ENV]
den t = canon $
        dE (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"
