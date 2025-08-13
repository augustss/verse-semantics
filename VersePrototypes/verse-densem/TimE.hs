{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TimE where
import Control.Monad
import Epic.List
import FrontEnd.Expr hiding(Tuple)
import ValueS
import ENVS
--import Debug.Trace

dE :: SrcEssential -> Ident -> Ident -> [ENV]
dE (Lit (LInt k))                   i x = [ i .=. x /\ x .= Int k ]
dE (EPrim p)                        i x = [ i .=. x /\ x .= Fun (dP p) ]
dE (Variable v) i x | isSrcUnderscore v = [ i .=. x ]
                    | otherwise         = [ i .=. x /\ x .=. v ]
dE (DefineE y t)                    i x = [ x .=. y ] *** dE t i x
dE (DefineIE y t)                   i x = [ i .=. y ] *** dE t i x
dE (DefineV _)                      _ _ = [ univ ] -- [ i .=. x /\ x .=. y ]
dE (Unify t0 t1)                    i x = dE t0 i x *** dE t1 i x
dE (Choice t0 t1)                   i x = dB t0 i x ++ dB t1 i x
dE tt@(Seq t0 t1)                   i x =
--  trace ("Seq " ++ show (dE t0 j y,
--                         dE t1 i x)) $
  dE t0 j y `remv` [j, y] *** dE t1 i x
  where (j, y) = fresh2 ("j", "y") [i, x] tt
dE (Where t0 t1)                    i x = dE t0 i x `remv` [j, y] *** dE t1 j y
  where (j, y) = fresh2 ("j", "y") [i, x] t1
dE t@(Array ts)                     i x =
  foldl1 (***) (et : es) `remv` (is ++ xs)
  where n = length ts
        used = i:x:getFree t
        is = take n $ freshList "i" used
        xs = take n $ freshList "x" used
        es = zipWith3 dE ts is xs
        tupvals = allTuplesLen n
        et = [ bigUnion [ i .= Tuple ivals /\
                          x .= Tuple xvals /\
                          bigIntersect (zipWith (.=) is ivals) /\
                          bigIntersect (zipWith (.=) xs xvals)
                        | ivals <- tupvals, xvals <- tupvals
                        ]
             ]
dE (Block t)                        i x = dB t i x
dE Fail                             _ _ = []
dE (ApplyD (EPrim DotDot) (Array [_t0, _t1])) _i _x =
  [ error ".. not implemented yet" | _i <- allInts ]
dE (Range t)                        i x =
  (dE t j y ***
   [ bigUnion [ y .= Fun fss /\ i .= v /\ x .= r
              | fss <- allFUNs, length fss > n, v <- allValues, Just r <- [applyPF (fss !! n) v]
              ]
   | n <- allInts'
   ]
  ) `remv` [j,y]
    where (j, y) = fresh2 ("j", "y") [i, x] t
dE t@(ApplyD t0 t1)                 i x =
  (dE t0 h f *** dE t1 j y ***
   squashTail   -- squash out the excessive empty sets we get because of large n
   [ bigUnion [ f .= Fun fss /\ i .=. x /\ j .= v /\ x .= r
              | fss <- allFUNs, length fss > n, v <- allValues, Just r <- [applyPF (fss !! n) v]
              ]
   | n <- allInts'
   ]
  ) `remv` [h,f,j,y]
    where (h, f) = fresh2 ("h", "f") [i, x] t
          (j, y) = fresh2 ("j", "y") [i, x] t
dE t@(If3 t0 t1 t2)                   i x = -- squash $
{-
  -- According to Koen
  [ hides vs0 (d0 /\ d1)     | d1 <- dE t1 i x ] ++
  [ compl d0 /\ hides vs0 d2 | d2 <- dE t2 i x ]
  where d0 = firstK vs0 (dE t0 j y `remv` [j, y])
        vs0 = bvs t0
        (j, y) = fresh2 (If3 t0 t1 t2)
-}
{-
  -- According to Simon (Koen)
  (([      d0] *** dB t1 i x) `remv` vs0) ++
   [compl (hides vs0 d0)] *** dB t2 i x
  where d0 = first vs0 (dC t0)
        vs0 = bvs t0
-}
{-
  -- According to Tim, old, wrong
  ((bs  *** dB t1 i x) `remv` bvs t0 `remv` [j, y]) ++
  (([b] *** dB t2 i x) `remv` bvs t0 `remv` [j, y])
  where a0 = dE t0 j y
        (j, y) = fresh2 (Array [t0, t1, t2])
        Snoc bs b = go empty a0
        go s [] = [univ \\\ s]
        go s (a:as) = (a \\\ s) : go (s \/ a) as
-}
  -- According to Tim, new
  ((bs  *** dB t1 i x) `remv` bvs t0 `remv` [j, y]) ++
  (([b] *** dB t2 i x) `remv` bvs t0 `remv` [j, y])
  where a0 = dE t0 j y
        a0' = a0 `remv` bvs t0 `remv` [j, y]
        (j, y) = fresh2 ("j", "y") [i, x] t
        Snoc bs b = go empty a0 a0'
        go :: ENV -> [ENV] -> [ENV] -> [ENV]
        go s [] [] = [univ \\\ s]
        go s (a:as) (a':as') = (a \\\ s) : go (s \/ a') as as'
        go _ _ _ = undefined

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

dE e                               _ _ = error $ "dE: unimplemented " ++ show e

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
--t0= EPrim Gt
--t1= Array [Lit (LInt 1), Lit (LInt 0)]
--t0=DefineE a (Choice (Lit (LInt 1)) (Lit (LInt 2))) `Seq` ApplyD (EPrim Gt) (Array [Variable a, Lit (LInt 0)])
t0=DefineE a (ApplyD (EPrim Gt) (Array [Lit (LInt 3), Variable x]) `Seq`
              ApplyD (EPrim Gt) (Array [Variable x, Lit (LInt 0)])
             )
t1=Variable a
-}

ix :: [ ENV ] -> Int -> ENV
ix es i | i >= 0 && i < length es = es !! i
        | otherwise = empty

conc :: [Value] -> Value
conc vs = Tuple $ concatMap (\ (Tuple ys) -> ys) vs

dB :: SrcEssential -> Ident -> Ident -> [ENV]
dB e i x = dE e i x `remv` bvs e

dC :: SrcEssential -> [ENV]
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

firstK :: [Ident] -> [ENV] -> ENV
firstK _  []         = empty
firstK ys (env:envs) = env \/ (compl (hides ys env) /\ firstK ys envs)

first :: [Ident] -> [ENV] -> ENV
first _  []     = empty
first xs (d:ds) = d \/ (first xs ds \\\ hides xs d)

squash :: [ENV] -> [ENV]
squash = filter (/= empty)

squashTail :: [ENV] -> [ENV]
squashTail = revDropWhile (== empty)

infixl 8 ***
(***) :: [ENV] -> [ENV] -> [ENV]
s1 *** s2 = [ d1 /\ d2 | d1 <- s1, d2 <- s2 ]

remv :: [ENV] -> [Ident] -> [ENV]
remv s xs = map (hides xs) s

fresh2 :: (String, String) -> [Ident] -> SrcEssential -> (Ident, Ident)
fresh2 (sx, sy) is t = (x, y)
  where x = fresh sx vs
        y = fresh sy (x:vs)
        vs = is ++ getAllBinders t

bvs :: SrcEssential -> [Ident]
bvs = getVisibleBinders

-------

den :: SrcEssential -> [ENV]
den t = squash $ dE (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"
