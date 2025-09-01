{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SLS where
import Control.Monad
import Epic.List
import FrontEnd.Expr hiding(Tuple)
import ValueS
import ENVS
import Set
--import Debug.Trace

default ()

type SL a = Set [a]

unit :: a -> SL a
unit a = singleton [a]

none :: SL a
none = Set.empty

mapSL :: (a -> b) -> SL a -> SL b
mapSL f = fmap (map f)

dE :: SrcEssential -> Ident -> Ident -> SL ENV
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
dE (Seq t0 t1)                      i x = dC t0 *** dE t1 i x
dE (Where t0 t1)                    i x = dE t0 i x *** dC t1
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
dE (Block t)                        i x = dB t i x
dE Fail                             _ _ = singleton []
{-
-- A speedup for x:int
dE (Range (EPrim IsInt))            i x = [ bigUnion [ i .= v /\ x .= v | v <- allInts ] ]
-}
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

dE t@(If3 t0 t1 t2)                   i x = join
 [
   let a0' :: [ENV]
       a0' = a0 `remvL` bvs t0 `remvL` [j, y]
       Snoc bs b = go ENVS.empty a0 a0'
       go :: ENV -> [ENV] -> [ENV] -> [ENV]
       go s [] [] = [univ \\\ s]
       go s (a:as) (a':as') = (a \\\ s) : go (s \/ a') as as'
       go _ _ _ = undefined
   in  ((singleton bs *** dB t1 i x) `remv` bvs t0 `remv` [j, y]) `union`
       ((unit b       *** dB t2 i x) `remv` bvs t0 `remv` [j, y])
 | a0 :: [ENV] <- dE t0 j y
 ] where (j, y) = fresh2 ("j", "y") [i, x] t

{-
-- For2

{- WRONG
dE t@(For2 t0 t1) i x =
  [ bigUnion [ rhos /\ i .= conc iss /\ x .= conc xss
             | iss <- sequence (map (extractVar rhos) is)
             , xss <- sequence (map (extractVar rhos) xs) ]
  | rhos <- rhoss
  ] `remv` is `remv` xs
-}
dE t@(For2 t0 t1) i x = [ bigUnion [ rhos /\ i .= conc ss /\ x .= conc ts /\
                                      bigIntersect (zipWith (.=) is ss) /\
                                      bigIntersect (zipWith (.=) xs ts)
                                    | ss <- replicateM nAlts (valsOf is rhos)
                                    , ts <- replicateM nAlts (valsOf xs rhos)
                                    ]
                         | rhos <- rhoss
                         ] `remv` is `remv` xs
{-
dE t@(For2 t0 t1) i x = [ bigUnion [ rhos /\ i .= conc ss /\ x .= conc ts /\
                                      bigIntersect (zipWith (.=) is ss) /\
                                      bigIntersect (zipWith (.=) xs ts)
                                    | ss <- replicateM nAlts tups
                                    , ts <- replicateM nAlts tups
                                    ]
                         | rhos <- rhoss
                         ] `remv` is `remv` xs
-}
  where
    rhoss  = foldr1 (***) [ c n | n <- [0..nAlts-1] ]
--    tups   = map Tuple (allTuplesLen 0 ++ allTuplesLen 1)
    (j, y) = fresh2 ("j", "y") [i, x] t
    (k, z) = fresh2 ("k", "z") [i, x] t
    is     = take nAlts $ freshList "i_" (i : x : getAllBinders t)
    xs     = take nAlts $ freshList "x_" (i : x : getAllBinders t)
    empTup = Tuple []
    a, a' :: [ENV]
    a = dE t0 j y   -- XXX No squash in Tim's version
    a' = a `remv` bvs t0 `remv` [j, y]
    nAlts = length a
    c :: Int -> [ENV]
    c n = (([a `ix` n] *** [ bigUnion [rhos /\ k .= ki /\ z .= zi /\ (is!!n) .= Tuple [ki] /\ (xs!!n) .= Tuple [zi]
                                      | ki <- allValues, zi <- allValues]
                           | rhos <- dE t1 k z])
            `remv` bvs t0 `remv` bvs t1 `remv` [j, k, y, z])
         ++
          (([univ \\\ (a' `ix` n)] *** [ (is!!n) .= empTup /\ (xs!!n) .= empTup ]))
-}

dE e                               _ _ = error $ "dE: unimplemented " ++ show e

dF :: Ident -> Ident -> Ident -> SL ENV
dF f a r = [ [ f .= Fun hs /\ bigUnion [ a .= u /\ r .= v
                                       | u <- allValues  -- list
                                       , Just v <- [applyPF h u]
                                       ]
             | h <- hs -- list
             ]
           | hs <- mkSetUnsafe allFUNs -- set
           ]

-- A hack to avoid iterating over so many values
valsOf :: [Ident] -> ENV -> [Value]
valsOf is e = nub $ concatMap (extractVar e) is

{-

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
t0=DefineE a (ApplyD (EPrim Gt) (Array [Lit (LInt 3), Variable x]) `Seq`
              ApplyD (EPrim Gt) (Array [Variable x, Lit (LInt 0)])
             )
t1=Variable a

ix :: [ ENV ] -> Int -> ENV
ix es i | i >= 0 && i < length es = es !! i
        | otherwise = empty
-}

conc :: [Value] -> Value
conc vs = Tuple $ concatMap (\ (Tuple ys) -> ys) vs

dB :: SrcEssential -> Ident -> Ident -> SL ENV
dB e i x = dE e i x `remv` bvs e

dC :: SrcEssential -> SL ENV
dC e = dE e i x `remv` [i,x]  where (i, x) = fresh2 ("i", "x") [] e

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

squash :: SL ENV -> SL ENV
squash = fmap (filter (/= ENVS.empty))

infixl 8 ***
(***) :: SL ENV -> SL ENV -> SL ENV
(***) = lift2 (/\)
{-
(***) :: [ENV] -> [ENV] -> [ENV]
s1 *** s2 = [ d1 /\ d2 | d1 <- s1, d2 <- s2 ]
-}
infixl 8 +++
(+++) :: SL ENV -> SL ENV -> SL ENV
s1 +++ s2 = [ l1 ++ l2 | l1 <- s1, l2 <- s2 ]
  
lift2 :: forall a b c . (a -> b -> c) -> SL a -> SL b -> SL c
lift2 op sl1 sl2 =
  [ concat ss 
  | s1 :: [a] <- sl1
  , let t  :: [Set[c]] = [ mapSL (op d1) sl2 | d1 <- s1 ]
        ft :: Set [[c]]= flatten t
  , ss :: [[c]] <- ft
  ]

flatten :: [Set a] -> Set[a]
flatten []     = singleton []
flatten (s:ss) = [ x:xs | x <- s, xs <- flatten ss ]

remv :: SL ENV -> [Ident] -> SL ENV
remv s xs = mapSL (hides xs) s

remvL :: [ENV] -> [Ident] -> [ENV]
remvL s xs = map (hides xs) s

fresh2 :: (String, String) -> [Ident] -> SrcEssential -> (Ident, Ident)
fresh2 (sx, sy) is t = (x, y)
  where x = fresh sx vs
        y = fresh sy (x:vs)
        vs = is ++ getAllBinders t

bvs :: SrcEssential -> [Ident]
bvs = getVisibleBinders

-------

den :: SrcEssential -> SL ENV
den t = squash $ dE (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"
