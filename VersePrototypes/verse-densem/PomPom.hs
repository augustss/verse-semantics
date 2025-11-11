{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing -Wno-missing-signatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PomPom where
--import Epic.List
import qualified Data.List as L
import FrontEnd.Expr hiding(Tuple)
import ValueP
import ENVP as E
import qualified Set
import Set(Set)
import PomSet
import Debug.Trace

--pfilter :: (a -> Bool) -> P a -> P a
--pfilter p s = [ y | x <- s, y <- if p x then Unit x else Empty ]

-- A Config is pass down everywhere to pick various semantic variations.
data Config = Config
  { forUnionMode :: ForUnionMode
  }
  deriving (Show)

data ForUnionMode = FUMSem1 | FUMSem2 | FUMSemBadOld
  deriving (Eq, Ord, Show)

----------------------------------------------------
--
--            Functions over PENV = P ENV
--
----------------------------------------------------

type PENV = P ENV
     -- Invariants of PENV
     --     (I1) Empty only at the root
     --     (I2) No (Unit E.empty) anywhere

mkUnit :: ENV -> PENV
-- Establish (I2)
mkUnit d | d == E.empty = Empty
         | otherwise       = Unit d

-- Sequencing
infixl 8 ***
(***) :: P ENV -> P ENV -> P ENV
{-
s1 *** s2 = do { d1 <- s1
               ; d2 <- s2
               ; mkUnit (d1 /\ d2) }
-}
Empty     *** _ = Empty
(s :\/ t) *** r         = (s *** r) `union` (t *** r)
(s :++ t) *** r         = (s *** r) +++     (t *** r)
Unit _    *** Empty     = Empty
Unit d    *** (s :\/ t) = (Unit d *** s) `union` (Unit d *** t)
Unit d    *** (s :++ t) = (Unit d *** s) +++     (Unit d *** t)
Unit d1   *** Unit d2   = mkUnit (d1 /\ d2)

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

coll :: P ENV -> ENV
coll Empty     = E.empty
coll (a :\/ b) = coll a \/ coll b
coll (a :++ b) = coll a \/ coll b
coll (Unit e)  = e

fOR :: Config -> [Ident] -> P ENV -> SrcEssential -> Ident -> Ident -> P ENV
fOR _ _  Empty     _ i x = unit (i .= nil /\ x .= nil)
fOR cfg xs d@Unit{}  t i x =
  (d *** unit sings *** dB cfg t p q >>> (p:q:xs))
  `union`
  (nOT (d >>> xs) *** unit (i .= nil /\ x .= nil))
  where (p, q) = fresh2 ("p", "q") (i:x:xs) t
        allV = allInts ++ allTuples
        sings = bigUnion [i .= sing ip /\ x .= sing iq /\ p .= ip /\ q .= iq
                         | ip <- allV
                         , iq <- allV
                         ]
--fOR xs (a :\/ b) t i x = fOR xs a t i x `union` fOR xs b t i x
--fOR xs (a :\/ b) t i x = fOR xs a t i x +++ fOR xs b t i x
--fOR xs (a :\/ b) t i x = fOR xs (a :++ b) t i x

fOR cfg xs (a :\/ b) t i x
 | forUnionMode cfg == FUMSemBadOld =
-- This is ⊎
  (unit sings *** fora *** forb) >>> (u1:v1:u2:v2:xs)
  where fora = fOR cfg xs a t u1 v1
        forb = fOR cfg xs b t u2 v2
        (u1, v1) = fresh2 ("u1","v1") (i:x:xs) t
        (u2, v2) = fresh2 ("u2","v2") (i:x:xs) t
        sings = bigUnion [ i .= tu1 `utup` tu2 /\ x .= tv1 `utup` tv2 /\
                           u1 .= tu1 /\ u2 .= tu2 /\ v1 .= tv1 /\ v2 .= tv2
                         | tu1 <- allValuesOf u1 fora
                         , tu2 <- allValuesOf u2 forb
                         , tv1 <- allValuesOf v1 fora
                         , tv2 <- allValuesOf v2 forb
                         , consistent tu1 tu2
                         , consistent tv1 tv2
                         ]
        utup (Fun g) (Fun h) = Fun (funUnion g h)
        utup _ _ = error "utup"
        consistent (Fun g) (Fun h) = Set.isEmpty (funDomain g `Set.intersect` funDomain h)
        consistent _ _ = error "consistent"
{-
  (unit ca *** fOR xs a t i x) `union`
  (unit cb *** fOR xs b t i x)
  `union` (unit (compl (ca \/ cb) /\ i .= nil /\ x .= nil))
--  `union` (nOT (ab >>> xs) *** unit (i .= nil /\ x .= nil))
  where ca = coll (a >>> xs)
        cb = coll (b >>> xs)
-}

fOR cfg xs (a :\/ b) t i x
 | forUnionMode cfg == FUMSem1 =
 -- SEM1
 -- This is ⋓
  fOR cfg xs a t i x `uu` fOR cfg xs b t i x

fOR cfg xs (a :\/ b) t i x
 | forUnionMode cfg == FUMSem2 =
 -- SEM2
 -- This is U
  fOR cfg xs a t i x `union` fOR cfg xs b t i x

fOR cfg xs (a :++ b) t i x =
--  trace ("FOR " ++ show (a, b, t, i, x, fora, forb, sings)) $
  (unit sings *** fora *** forb) >>> (u1:v1:u2:v2:xs)
  where fora = fOR cfg xs a t u1 v1
        forb = fOR cfg xs b t u2 v2
        (u1, v1) = fresh2 ("u1","v1") (i:x:xs) t
        (u2, v2) = fresh2 ("u2","v2") (i:x:xs) t
        sings = bigUnion [ i .= tu1 `app` tu2 /\ x .= tv1 `app` tv2 /\
                           u1 .= tu1 /\ u2 .= tu2 /\ v1 .= tv1 /\ v2 .= tv2
                         | tu1 <- allValuesOf u1 fora
                         , tu2 <- allValuesOf u2 forb
                         , tv1 <- allValuesOf v1 fora
                         , tv2 <- allValuesOf v2 forb
                         ]
        app (Fun g) (Fun h) = Fun (tupConcat g h)
        app _ _ = error "app"

fOR _ _ _ _ _ _ = undefined

useSEM1 :: Bool
useSEM1 = False

uu :: PENV -> PENV -> PENV
uu xs ys = [ x \/ y | x <- xs, y <- ys ]
  
allValuesOf :: Ident -> P ENV -> [Value]
allValuesOf _ Empty = []
allValuesOf x (a :++ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (a :\/ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (Unit d) = extractVar d x

dE :: Config -> SrcEssential -> Ident -> Ident -> P ENV
-- The denotational semantics itself (Fig 10)
dE _   (Lit (LInt k))                   i x = unit $ i .=. x /\ x .= Int (fromIntegral k)
dE _   (EPrim p)                        i x = unit $ i .=. x /\ x .= Fun (dP p)
--dE cfg (Variable (Ident _ "xf"))        i x = unit $ i .=. x /\ x .= Fun funXF -- hack for testing
dE _   (Variable v) i x | isSrcUnderscore v = unit $ i .=. x
                         | otherwise         = unit $ i .=. x /\ x .=. v
dE cfg (DefineE y t)                    i x = unit (x .=. y) *** dE cfg t i x      -- y := t
dE cfg (DefineIE y t)                   i x = unit (i .=. y) *** dE cfg t i x      -- y ~> _ := t
dE _   (DefineV y)                      i x = unit $ i .=. x /\ x .=. y
dE cfg (Unify t0 t1)                    i x = dE cfg t0 i x *** dE cfg t1 i x
dE cfg (Choice t0 t1)                   i x = dB cfg t0 i x +++ dB cfg t1 i x
dE cfg (Seq t0 t1)                      i x = dC cfg t0     *** dE cfg t1 i x
dE cfg (Where t0 t1)                    i x = dE cfg t0 i x *** dC cfg t1
dE cfg (Block t)                        i x = dB cfg t i x
dE _   Fail                             _ _ = Empty
dE cfg (ApplyD (Variable (Ident _ "operator'|||'")) (Array [t0, t1])) i x =
  dE cfg t0 i x `union` dE cfg t1 i x
dE cfg (If3 t0 t1 t2)                   i x =
  (s0 *** dB cfg t1 i x >>> xs)
  `union`
  -- +++
  (nOT (s0 >>> xs) *** dB cfg t2 i x)
  where xs = bvs t0
        s0 = oNE xs (dC cfg t0)
dE cfg (For2 t0 t1) i x = fOR cfg (bvs t0) (dC cfg t0) t1 i x
dE cfg t@(ApplyD (EPrim DotDot) (Array [t0, t1])) i x =
  dE cfg t0 a l *** dE cfg t1 b h *** unit (i .=. x) ***
  (let
    mkSequ :: Int -> Int -> [ENV]
    mkSequ lo hi = [ x .= Int v /\ l .= Int lo /\ h .= Int hi | v <- [ lo .. hi ] ]
    allSeqs :: Set [ENV]
    allSeqs = [ mkSequ start end
              | start <- Set.mkSetUnsafe allInts'
              , end   <- Set.mkSetUnsafe [ start .. numInt-1 ]
              ]
   in uncanon allSeqs
  ) >>> [a,l,b,h]
    where (a, l) = fresh2 ("a", "l") [i, x] t
          (b, h) = fresh2 ("b", "h") [i, x] t
dE cfg t@(Array ts)                     i x =
  foldl1 (***) (et : es) >>> (is ++ xs)
  where n = length ts
        used = i:x:getFree t
        is = take n $ freshList "i" used
        xs = take n $ freshList "x" used
        es = zipWith3 (dE cfg) ts is xs
        tupvals = [ vs | Tuple vs <- allTuples, length vs == n ]
        et = unit $ bigUnion [ i .= Tuple ivals /\
                               x .= Tuple xvals /\
                               bigIntersect (zipWith (.=) is ivals) /\
                               bigIntersect (zipWith (.=) xs xvals)
                             | ivals <- tupvals, xvals <- tupvals
                             ]
-- A speedup for x:int
dE _   (Range (EPrim IsInt))            i x = unit (bigUnion [ i .= v /\ x .= v | v <- allInts ])
dE cfg (Range t)                        i x =
  dE cfg t j y *** dF y i x >>> [j,y]
    where (j, y) = fresh2 ("j", "y") [i, x] t
dE cfg t@(ApplyD t0 t1)                 i x =
  dE cfg t0 h f *** dE cfg t1 j y *** dF f y x *** unit (i .=. x) >>> [h,f,j,y]
    where (h, f) = fresh2 ("h", "f") [i, x] t
          (j, y) = fresh2 ("j", "y") [i, x] t

dE cfg t@(Function aprt t0 _ t1) i x = fUN cfg (getAllBinders t) aprt (bvs t0) (dE cfg t0 p q) t1 p q i x
  where (p, q) = fresh2 ("p", "q") [i, x] t
dE _   e                               _ _ = error $ "dE cfg: unimplemented " ++ show e

dF :: Ident -> Ident -> Ident -> P ENV
dF f a r =
  [ d
  | fp <- mkPomSetList allFUNs
  , h <- fp
  , let d = f .= Fun fp /\ funHasPair a r h
  , d /= E.empty
  ]

-- The constraint {{ (x => y) `elem` h }}
funHasPair :: Ident -> Ident -> PartialFun -> ENV
funHasPair x y h = bigUnion [ x .= u /\ y .= v | u <- allValues, Just v <- [applyPF h u] ]

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
u1=Ident noLoc "u1"
u2=Ident noLoc "u2"
v=Ident noLoc "v"
v1=Ident noLoc "v1"
v2=Ident noLoc "v2"
--t0=DefineE x (k0 `Choice` k1)
--t1=For2 t0 (Variable x)
--t0=DefineE x (ApplyD (EPrim DotDot) (Array [k1, Variable n]))
--t0=DefineE i (If3 (Variable n `Unify` k0) (Variable a `Unify` k1) (Variable a `Unify` k2))
--t0=DefineE i (k0 `Choice` k1)
--t0=Unify (Variable x) k0 `Choice` Unify (Variable x) k1
t0=DefineE a $ ApplyD (Variable (Ident noLoc "operator'|||'")) (Array [k0, k1])
t0c=DefineE a $ k0 `Choice` k1
t1=Variable a
tfor = For2 t0 t1
tn=Variable n `Unify` k1
tt = tn `Seq` tfor
ft0=DefineE a k0
ft0a=DefineE a tint
tint=Range (EPrim IsInt)
tneg=Range (EPrim Neg)
tadd x y = ApplyD (EPrim Add) (Array [x, y])
tlt  x y = ApplyD (EPrim Lt)  (Array [x, y])

tfun01 = Function Closed k0 effSucceeds k1
tfunho = Function Closed tfun01 effSucceeds k2

tfunx2 = Function Closed (Variable xx) effSucceeds k2
xx=Ident noLoc "x"

uChoice t0 t1 = ApplyD (Variable (Ident noLoc "operator'|||'")) (Array [t0, t1])
dotDot t0 t1 = ApplyD (EPrim DotDot) (Array [t0, t1])

td = dotDot k1 (Variable n) `Choice` k1
tdf = For2 (DefineE x td) (Variable x)

{-
ix :: [ ENV ] -> Int -> ENV
ix es i | i >= 0 && i < length es = es !! i
        | otherwise = E.empty

conc :: [Value] -> Value
conc vs = Tuple $ concatMap (\ (Tuple ys) -> ys) vs
-}

dB :: Config -> SrcEssential -> Ident -> Ident -> P ENV
dB cfg e i x = dE cfg e i x >>> bvs e

dC :: Config -> SrcEssential -> P ENV
dC cfg e = dE cfg e i x >>> [i,x]  where (i, x) = fresh2 ("i", "x") [] e

dP :: PrimOp -> FUN
dP Neg = fun[funNegate]
dP IsInt = fun[funInt]
dP Gt = fun[funGt]
dP Lt = fun[funLt]
dP Add = fun[funAdd]
dP Sub = fun[funSub]
dP Mul = fun[funMul]
dP Div = fun[funDiv]
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
squash = fmap (filter (/= E.empty))

  
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

fresh2' :: (String, String) -> [Ident] -> (Ident, Ident)
fresh2' (sx, sy) is = (x, y)
  where x = fresh sx is
        y = fresh sy (x:is)

bvs :: SrcEssential -> [Ident]
bvs = getVisibleBinders

-------

denS :: ForUnionMode -> SrcEssential -> Set [ENV]
denS m t = canon $
             dE cfg (Block t) i x -- `remv` [i]
  where (i, x) = fresh2 ("u", "v") [] t
        -- res = Ident noLoc "res"
        cfg = Config{ forUnionMode = m }

den :: SrcEssential -> Set [ENV]
den = denS FUMSem2

-------

-- The first argument to fUN is only used to make sure we make fresh variables
fUN :: Config -> [Ident] -> Aperture -> [Ident] -> PENV -> SrcEssential -> Ident -> Ident -> Ident -> Ident -> PENV
fUN _ _ _ _ Empty _ _ _ h f = unit $ h .= Fun (fun [funEmpty]) /\ f .= Fun (fun [funEmpty])
fUN cfg used apt xs (s :\/ t) t1 p q h f =
  (unit sings *** funa *** funb) >>> (h1:f1:h2:f2:xs)
  where funa = fUN cfg used apt xs s t1 p q h1 f1
        funb = fUN cfg used apt xs t t1 p q h2 f2
        (h1, f1) = fresh2' ("h1","f1") (h:f:used)
        (h2, f2) = fresh2' ("h2","f2") (h:f:used)
        sings = bigUnion [ h .= th1 `ufun` th2 /\ f .= tf1 `ufun` tf2 /\
                           h1 .= th1 /\ h2 .= th2 /\ f1 .= tf1 /\ f2 .= tf2
                         | th1 <- allValuesOf h1 funa
                         , th2 <- allValuesOf h2 funb
                         , tf1 <- allValuesOf f1 funa
                         , tf2 <- allValuesOf f2 funb
                         ]
        ufun (Fun g) (Fun h) = Fun (funUnion g h)
        ufun _ _ = error "ufun-1"
fUN cfg used apt xs (s :++ t) t1 p q h f =
  (unit sings *** funa *** funb) >>> (h1:f1:h2:f2:xs)
  where funa = fUN cfg used apt xs s t1 p q h1 f1
        funb = fUN cfg used apt xs t t1 p q h2 f2
        (h1, f1) = fresh2' ("h1","f1") (h:f:used)
        (h2, f2) = fresh2' ("h2","f2") (h:f:used)
        sings = bigUnion [ h .= th1 `ufun` th2 /\ f .= tf1 `ufun` tf2 /\
                           h1 .= th1 /\ h2 .= th2 /\ f1 .= tf1 /\ f2 .= tf2
                         | th1 <- allValuesOf h1 funa
                         , th2 <- allValuesOf h2 funb
                         , tf1 <- allValuesOf f1 funa
                         , tf2 <- allValuesOf f2 funb
                         ]
        ufun (Fun g) (Fun h) = Fun (funConcat g h)
        ufun _ _ = error "ufun-2"
fUN cfg used apt xs d@(Unit dd) t1 p q h f =
--trace ("fUN dd=" ++ show dd ++ " t1=" ++ show t1 ++ " (p,q,h,f)=" ++ show(p,q,h,f) ++ "xs=" ++ show xs) $
  (d *** unit sings >>> (p:q:xs))
  `union`
  (nOT (d >>> (p:q:xs)) *** unit (h .= Fun Empty /\ f .= Fun Empty))
  where
    (x, y) = fresh2' ("x", "y") (p:q:h:f:used)
    et1 = dE cfg t1 x y
    sings :: ENV
    sings = bigUnion
      [ h .= Fun (Unit hh) /\ f .= Fun (Unit ff) /\ env
      | hh <- allPFs            -- try all possible partial functions
--    , trace ("t1=" ++ show t1 ++ ", hh=" ++ show hh) True
      -- poss is a set of input-output pairs.
      -- For each input there is a set of possible outputs:
      --   if h[p] fails the set is empty
      --   each element is (output from t1, output from h, needed ENV)
      , let
          poss :: Set (Value, Set (Value, Value, ENV))
          poss =
            [ (pv, ves)
            | pv <- allValuesSet
            , let qvs = valsOf q (dd /\ p .= pv)  -- possible values for q
--          , trace ("qvs=" ++ show qvs ++ ", dd=" ++ show dd ++ ", pv=" ++ show pv) True
--          , qv <- allValuesSet  -- could use this
            , qv <- qvs                           -- try the q values
--          , trace("try hh[qv] hh=" ++ show hh ++ ", qv=" ++ show qv ++ " hh[qv]=" ++ show (applyPF hh qv)) True
            , let ves =
                    case applyPF hh qv of
                      Nothing -> Set.empty            -- h[q] failed
                      Just hq -> 
                        -- Possible range environments for this p=pv
                        let
                          et0 = Unit (dd /\ p .= pv /\ q .= qv) >>> [p, q]
                          res = Unit (x .= hq)
                          yrs :: Set ENV
                          yrs = singSeqChk (et0 *** et1 *** res)
                        in
--                        trace ("ok pv=" ++ show pv ++ " qv=" ++ show qv ++ " hh[qv]=" ++ show hq ++ " et0=" ++ show et0 ++ " et1*res=" ++ show (et1 *** res)) $
                          [ (yv, qv, (x:y:p:q:xs) `hides` r)
                          | yr <- yrs
                          , yv <- valsOf y yr  -- allValuesSet
                          , let r = yr /\ y .= yv
                          , r /= empty
                          ]
            ]
--      , trace ("poss=" ++ show poss) True
      , let poss' = poss -- Set.filterSet (not . Set.isEmpty . snd) poss
--      , trace ("poss'=" ++ show poss') True
      , let
          -- 
          sets :: [ [ ((Value, Value), Value, ENV) ] ]
          sets = traverse (\ (x, yes) -> [ ((x, y), q, e) | (y, q, e) <- Set.toList yes ])
                          (Set.toList poss')
          sets' :: [ (PartialFun, Set Value, ENV) ]
          sets' = [ (mkPFList xys, Set.mkSet qs, bigIntersect es)
                  | xyeqs <- sets
                  , let (xys, qs, es) = unzip3 xyeqs
                  ]
--    , trace ("sets=" ++ show sets) True
--    , trace ("sets'=" ++ show sets') True
      , (ff, qs, env) <- sets'
      , apt /= Closed || domPF hh == qs
--    , trace ("ok " ++ show (hh, ff)) True
      ]

-- Extract all possible values of a variable.
-- Unlike extractVar this doesn't fail, but it is slower.
valsOf :: Ident -> ENV -> Set Value
valsOf i d = [ x | x <- allValuesSet, empty /= (d /\ i .= x) ]

singSeqChk :: P a -> Set a
singSeqChk p = [ a | [a] <- canon p ]

bigIntersectSet :: Set ENV -> ENV
bigIntersectSet = bigIntersect . Set.toList
