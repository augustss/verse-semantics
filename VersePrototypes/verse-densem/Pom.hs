{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pom where
import Control.Monad
import Epic.List
import FrontEnd.Expr hiding(Tuple)
import ValueS
import ENVS
import qualified Set
--import Debug.Trace

default ()

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

infixl 8 +++
(+++) :: P a -> P a -> P a
(+++) = (:++)

infixl 8 ***
(***) :: P ENV -> P ENV -> P ENV
(***) = liftA2 (/\)
--  \ s t -> uncanon $ lift2 (/\) (canon s) (canon t)
--    \ s t -> liftA2 (/\) (uncanon $ canon s) (uncanon $ canon t)

infixl 6 `union`
union :: P a -> P a -> P a
union = (:\/)

instance Functor P where
  fmap f s = s >>= pure . f

instance Applicative P where
  pure = Unit
  (<*>) = ap

instance Monad P where
  return          = pure
  Empty     >>= _ = Empty
  Unit x    >>= k = k x
  (s :++ t) >>= k = (s >>= k) :++ (t >>= k)
  (s :\/ t) >>= k = (s >>= k) :\/ (t >>= k)

pfilter :: (a -> Bool) -> P a -> P a
pfilter p s = [ y | x <- s, y <- if p x then Unit x else Empty ]

infixl 7 >>>
(>>>) :: P ENV -> [Ident] -> P ENV
s >>> xs = fmap (hides xs) s

canon :: P a -> Set.Set [a]
canon Empty = Set.empty
canon (Unit a) = Set.singleton [a]
canon (s :\/ t) = canon s `Set.union` canon t
canon (Empty :++ s) = canon s
canon (Unit a :++ s) = [ a : as | as <- canon s ]
canon ((s :\/ t) :++ r) = canon (s :++ r) `Set.union` canon (t :++ r)
canon ((s :++ t) :++ r) = canon (s :++ (t :++ r))

absorbEmpty :: P a -> P a
absorbEmpty x@Empty = x
absorbEmpty x@Unit{} = x
absorbEmpty (x :++ y) =
  case (absorbEmpty x, absorbEmpty y) of
    (Empty, y') -> y'
    (x', Empty) -> x'
    (x', y') -> x' :++ y'
absorbEmpty (x :\/ y) =
  case (absorbEmpty x, absorbEmpty y) of
    (Empty, y') -> y'
    (x', Empty) -> x'
    (x', y') -> x' :\/ y'

uncanon :: Ord a => Set.Set [a] -> P a
uncanon s | Set.isEmpty s = Empty
uncanon s = Set.foldSet (:\/) [ foldr (:++) Empty (map unit xs) | xs <- s ]

oNE :: [Ident] -> P ENV -> P ENV
oNE _ Empty = Empty
oNE _ (Unit d) = Unit d
oNE xs (s :\/ t) = oNE xs s :\/ oNE xs t
oNE xs (s :++ t) = s1 :\/ (nOT xs s1 *** oNE xs t)
  where s1 = oNE xs s

nOT :: [Ident] -> P ENV -> P ENV
nOT xs s = fmap compl s >>> xs

nil :: Value
nil = Tuple []

fOR :: [Ident] -> P ENV -> SrcEssential -> Ident -> Ident -> P ENV
fOR _  Empty     _ _ _ = unit (u .= nil /\ v .= nil)
fOR xs d@Unit{}  t _ _ =
  ((d *** unit (u .=% sing p /\ v .=% sing q) *** dE t p q) >>> (p:q:xs))
  :\/
  (nOT xs d *** unit (u .=% nil /\ v .=% nil))
  where (p, q) = fresh2 ("p", "q") (i:x:xs) t
fOR xs (a :\/ b) t i x = fOR xs a t i x :\/ fOR xs b t i x
fOR xs (a :++ b) t i x =
  unit (u .=% u1 `app` u2 /\ v .=% v1 `app` v2) ***
  fOR xs a t u1 v1 *** fOR xs b t u2 v2
  where (u1, v1) = fresh2 ("u1","v1") (i:x:xs) t
        (u2, v2) = fresh2 ("u2","v2") (i:x:xs) t

dE :: SrcEssential -> Ident -> Ident -> P ENV
dE (Lit (LInt k))                   i x = unit $ i .=. x /\ x .= Int (fromIntegral k)
dE (EPrim p)                        i x = unit $ i .=. x /\ x .= Fun (dP p)
dE (Variable (Ident _ "xf"))        i x = unit $ i .=. x /\ x .= Fun funXF
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
dE Fail                             _ _ = none
dE (ApplyD (Variable (Ident _ "operator'|||'")) (Array [t0, t1])) i x =
  dE t0 i x `union` dE t1 i x
dE (If3 t0 t1 t2)                   i x =
  s0 *** dB t1 i x >>> xs
  `union`
  nOT xs s0 *** dB t2 i x
  where xs = bvs t0
        s0 = oNE xs (dC t0)
dE t@(For2 t0 t1) i x = fOR (bvs t0) (dC t0) t1 i x
{-
dE t@(Array ts)                     i x =
  foldl1 (***) (et : es) `remv` (is ++ xs)
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
dE (Range (EPrim IsInt))            i x = singleton [ bigUnion [ i .= v /\ x .= v | v <- allInts ] ]
dE (Range t)                        i x =
  (dE t j y *** dF y i x) `remv` [j,y]
    where (j, y) = fresh2 ("j", "y") [i, x] t
dE t@(ApplyD (EPrim DotDot) (Array [t0, t1])) i x =
  (dE t0 a l *** dE t1 b h *** unit (i .=. x) ***
  [ [ x .= Int v /\ l .= Int start /\ h .= Int end | v <- [ start .. end ] ]
  | start <- mkSetUnsafe allInts'
  , len   <- mkSetUnsafe allInts'
  , let end = start + len - 1
  , end < numInt
  ]) `remv` [a,l,b,h]
    where (a, l) = fresh2 ("a", "l") [i, x] t
          (b, h) = fresh2 ("b", "h") [i, x] t
dE t@(ApplyD t0 t1)                 i x =
  (dE t0 h f *** dE t1 j y *** dF f y x *** unit (i .=. x)) `remv` [h,f,j,y]
    where (h, f) = fresh2 ("h", "f") [i, x] t
          (j, y) = fresh2 ("j", "y") [i, x] t

dE (If3 t0 t1 t2)                   i x = join
 [ let a0' :: [ENV]
       a0' = a0 `remvL` bvs t0
       Snoc bs b = go ENVS.empty a0 a0'
       go :: ENV -> [ENV] -> [ENV] -> [ENV]
       go s [] [] = [univ \\\ s]
       go s (a:as) (a':as') = (a \\\ s) : go (s \/ a') as as'
       go _ _ _ = undefined
   in  ((singleton bs *** dB t1 i x) `remv` bvs t0) `outerUnion`
       ((unit b       *** dB t2 i x) `remv` bvs t0)
 | a0 :: [ENV] <- dC t0
 ]

-- For2
dE t@(For2 t0 t1) i x = join
  [ let rhoss  = foldr1 (***) [ c n | n <- [0..nAlts-1] ]
        is     = take nAlts $ freshList "i_" (i : x : getAllBinders t)
        xs     = take nAlts $ freshList "x_" (i : x : getAllBinders t)
        a' :: [ENV]
        a' = a `remvL` bvs t0 `remvL` [j, y]
        nAlts = length a
        c :: Int -> P ENV
        c n = ((unit (a `ix` n) *** [ [ bigUnion [rhos /\ k .= ki /\ z .= zi /\
                                                (is!!n) .= Tuple [ki] /\ (xs!!n) .= Tuple [zi]
                                               | ki <- allValues, zi <- allValues]
                                      | rhos <- s1
                                      ]
                                    | s1 <- dE t1 k z
                                    ])
               `remv` bvs t0 `remv` bvs t1 `remv` [j, k, y, z])
          `outerUnion`
              ((unit (univ \\\ (a' `ix` n)) *** unit ( (is!!n) .= empTup /\ (xs!!n) .= empTup )))
    in
      [
        [ bigUnion [ rhos /\ i .= conc ss /\ x .= conc ts /\
                     bigIntersect (zipWith (.=) is ss) /\
                     bigIntersect (zipWith (.=) xs ts)
                   | ss <- replicateM nAlts tups
                   , ts <- replicateM nAlts tups
                   ]
        | rhos <- s1
        ]
      | s1 <- rhoss
      ] `remv` is `remv` xs
  | a <- dE t0 j y
  ]
  where
    (j, y) = fresh2 ("j", "y") [i, x] t
    (k, z) = fresh2 ("k", "z") [i, x] t
    tups   = map Tuple (allTuplesLen 0 ++ allTuplesLen 1)
    empTup = Tuple []
-}
dE e                               _ _ = error $ "dE: unimplemented " ++ show e
{-
dF :: Ident -> Ident -> Ident -> P ENV
dF f a r = [ [ f .= Fun hs /\ bigUnion [ a .= u /\ r .= v
                                       | u <- allValues  -- list
                                       , Just v <- [applyPF h u]
                                       ]
             | h <- hs -- list
             ]
           | hs <- mkSetUnsafe allFUNs -- set
           ]
-}
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
t0=If3 (Variable x `Unify` k0) k2 k3
t1=DefineIE x (k0 `Choice` k1)

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

canonAll :: P ENV -> Set.Set [ENV]
canonAll = canon . absorbEmpty . pfilter (/= ENVS.empty)

den :: SrcEssential -> Set.Set [ENV]
den t = canonAll $
        dE (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"

{-
denU :: SrcEssential -> Set.Set [ENV]
denU t = canon $
         dE (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"
-}
