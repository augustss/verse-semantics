{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PomPom where
--import Epic.List
--import Control.DeepSeq
import Control.Exception
import Data.Char(toLower)
import Data.IORef
import qualified Data.List as L
import FrontEnd.Expr hiding(Tuple)
import ValueP
import ENVP as E
import qualified MultiSet as Set
import MultiSet(Set)
import PomSet
import Debug.Trace
import System.IO.Unsafe
import Text.Printf
import Epic.Print hiding (empty)

--pfilter :: (a -> Bool) -> P a -> P a
--pfilter p s = [ y | x <- s, y <- if p x then Unit x else Empty ]

-- A Config is pass down everywhere to pick various semantic variations.
data Config = Config
  { forUnionMode :: ForUnionMode
  , forUnitMode  :: ForUnitMode
  , ifUnionMode  :: IfUnionMode
  , useTree      :: Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config { forUnionMode = ForUnionSem3, forUnitMode = ForUnitUnion, ifUnionMode = IfUnionUnion, useTree = True }

data ConfigRef = ConfigRef
  { config       :: Config
  , logUniq      :: IORef Int
  , logRef       :: IORef [String]     -- log, in reverse order
  , doTrace      :: Bool
  }

data ForUnionMode = ForUnionSem1 | ForUnionSem2 | ForUnionSem3
  deriving (Eq, Ord)

instance Show ForUnionMode where
  show ForUnionSem1 = "SEM1"
  show ForUnionSem2 = "SEM2"
  show ForUnionSem3 = "SEM3"

instance Read ForUnionMode where
  readsPrec _ s = [ (m, r)
                  | (t, r) <- lex s
                  , Just m <- pure $
                              case map toLower t of
                                "sem1" -> Just ForUnionSem1
                                "sem2" -> Just ForUnionSem2
                                "sem3" -> Just ForUnionSem3
                                _      -> Nothing
                  ]

data ForUnitMode = ForUnitFOR1 | ForUnitUnion | ForUnitDoubleUnion
  deriving (Eq, Ord)

instance Show ForUnitMode where
  show ForUnitUnion = "union"
  show ForUnitDoubleUnion = "doubleUnion"
  show ForUnitFOR1 = "for1"

instance Read ForUnitMode where
  readsPrec _ s = [ (m, r)
                  | (t, r) <- lex s
                  , Just m <- pure $
                              case map toLower t of
                                "union"       -> Just ForUnitUnion
                                "doubleunion" -> Just ForUnitDoubleUnion
                                "uunion"      -> Just ForUnitDoubleUnion
                                "for1"        -> Just ForUnitFOR1
                                _      -> Nothing
                  ]

data IfUnionMode = IfUnionUnion | IfUnionConcat | IfUnionDodgy

  deriving (Eq, Ord)

instance Show IfUnionMode where
  show IfUnionUnion = "union"
  show IfUnionDodgy = "dodgy"
  show IfUnionConcat = "concat"

instance Read IfUnionMode where
  readsPrec _ s = [ (m, r)
                  | (t, r) <- lex s
                  , Just m <- pure $
                              case map toLower t of
                                "union"  -> Just IfUnionUnion
                                "dodgy"  -> Just IfUnionDodgy
                                "concat" -> Just IfUnionConcat
                                _      -> Nothing
                  ]

mkConfigRefIO :: Config -> Bool -> IO ConfigRef
mkConfigRefIO cfg tr = do
  u <- newIORef 0
  r <- newIORef []
  return ConfigRef{ config = cfg, doTrace = tr, logUniq = u, logRef = r }

type ParamList = [(String, String)]

pv :: Pretty a => String -> a -> (String, String)
pv s a = (s, prettyShow a)

-- This is a pretty gruesome way to do the logging, but is saves us
-- from making the entire semantics monadic.
-- But maybe that would be better...
logStep :: (Pretty a, Pretty b) => ConfigRef -> String -> a -> ParamList -> b -> b
logStep cfg msg a ps b = unsafePerformIO $ do
  u <- readIORef (logUniq cfg)
  writeIORef (logUniq cfg) $! u + 1
  let arg = printf "%5s[ %s ] %s" msg (prettyShow a) ppl :: String
      s1  = printf "%5d { %s" u arg
      ppl = unwords $ map (\ (n,v) -> n ++ "=" ++ v) ps
--  putStr $ "*** " ++ s1
  True <- evaluate (s1==s1)
  l1 <- readIORef (logRef cfg)
  writeIORef (logRef cfg) (s1:l1)
  let s2 = printf "%5d } %s --> %s" u arg (prettyShow b)
--  putStr $ "*** " ++ s2
  True <- evaluate (s2==s2)
  l2 <- readIORef (logRef cfg)
  writeIORef (logRef cfg) (s2:l2)
  return b

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
         | otherwise    = Unit d

-- Sequencing
infixl 8 ***
(***) :: P ENV -> P ENV -> P ENV
s1 *** s2 = do { d1 <- s1
               ; d2 <- s2
               ; mkUnit (d1 /\ d2) }
{-
Empty     *** _ = Empty
(s :\/ t) *** r         = (s *** r) `union` (t *** r)
(s :++ t) *** r         = (s *** r) +++     (t *** r)
Unit _    *** Empty     = Empty
Unit d    *** (s :\/ t) = (Unit d *** s) `union` (Unit d *** t)
Unit d    *** (s :++ t) = (Unit d *** s) +++     (Unit d *** t)
Unit d1   *** Unit d2   = mkUnit (d1 /\ d2)
-}

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

------------------

oNE :: ConfigRef -> [Ident] -> PENV -> PENV
oNE _ _ Empty = Empty
oNE _ _ (Unit d) = Unit d
oNE cfg xs (s :\/ t) = ifUnion cfg (oNE cfg xs s) (oNE cfg xs t)
oNE cfg xs (s :++ t) = s1 `disj` (
    --(fmap compl s1 >>> xs)
    nOT (s1 >>> xs)
    *** oNE cfg xs t)
  where s1 = oNE cfg xs s

nil :: Value
nil = Tuple []
sing :: Value -> Value
sing x = Tuple [x]

coll :: P ENV -> ENV
coll Empty     = E.empty
coll (a :\/ b) = coll a \/ coll b
coll (a :++ b) = coll a \/ coll b
coll (Unit e)  = e

fOR1 :: ConfigRef -> [Ident] -> ENV -> PENV -> Ident -> Ident -> Ident -> Ident -> PENV
fOR1 cfg xs d0 tin u v x y =
  (if doTrace cfg then
     logStep cfg "FOR1" tin [pv "xs" xs, pv "d0" d0, pv "u" u, pv "v" v, pv "x" x, pv "y" y]
   else id) $
  fOR1' cfg xs d0 tin u v x y

fOR1' :: ConfigRef -> [Ident] -> ENV -> PENV -> Ident -> Ident -> Ident -> Ident -> PENV
fOR1' cfg xs d0 (s :\/ t) u v x y =
  fOR1 cfg xs d0 s u v x y `union` fOR1 cfg xs d0 t u v x y
fOR1' cfg xs d0 (s :++ t) u v x y =
  fOR1 cfg xs d0 s u v x y +++     fOR1 cfg xs d0 t u v x y
fOR1' _   _  _  Empty     _ _ _ _ = Empty
fOR1' _   xs d0 (Unit d1) u v x y =
  mkUnit $     ((d0 /\ sings /\ d1) \\\\ (x:y:xs))
            \/ (compl (d0 \\\\ xs) /\ (u.=Tuple[] /\ v.=Tuple[]))
  where sings = bigUnion [ u .= sing ix /\ v .= sing iy /\ x .= ix /\ y .= iy
                         | ix <- allInts -- XXX allV
                         , iy <- allInts -- XXX allV
                         ]

fOR :: ConfigRef -> [Ident] -> P ENV -> SrcEssential -> Ident -> Ident -> P ENV
fOR cfg xs tin t1 u v =
  (if doTrace cfg then
     logStep cfg "FOR" tin [pv "xs" xs, pv "t1" t1, pv "u" u, pv "v" v]
   else id) $
  fOR' cfg xs tin t1 u v

fOR' :: ConfigRef -> [Ident] -> P ENV -> SrcEssential -> Ident -> Ident -> P ENV
fOR' _  _   Empty     _ _ _ = unit (i .= nil /\ x .= nil)
fOR' cfg xs d@(Unit d0) t i x =
  case forUnitMode (config cfg) of
    ForUnitFOR1 ->
      fOR1 cfg xs d0 (dE cfg t xx yy) i x xx yy
      where (xx, yy) = fresh2 ("x","y") (i:x:xs) t
    m ->
      (if m == ForUnitUnion then union else uu)
        (d *** unit sings *** dB cfg t p q >>> (p:q:xs))
        (nOT (d >>> xs) *** unit (i .= nil /\ x .= nil))
      where (p, q) = fresh2 ("p", "q") (i:x:xs) t
            allV = allInts ++ allTuples
            sings = bigUnion [i .= sing ip /\ x .= sing iq /\ p .= ip /\ q .= iq
                             | ip <- allV
                             , iq <- allV
                             ]

fOR' cfg xs (a :\/ b) t i x = fORUnion cfg xs a b t i x

fOR' cfg xs (a :++ b) t i x =
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


fORUnion :: ConfigRef -> [Ident] -> PENV -> PENV -> SrcEssential -> Ident -> Ident -> PENV
fORUnion cfg xs a b t i x =
  case forUnionMode (config cfg) of
    ForUnionSem1 -> fora `uu` forb
        where fora = fOR cfg xs a t u1 v1
              forb = fOR cfg xs b t u2 v2
    ForUnionSem2 -> fora `union` forb
        where fora = fOR cfg xs a t u1 v1
              forb = fOR cfg xs b t u2 v2

    ForUnionSem3 ->
-- This is ⊎
      (unit sings *** fora *** forb) >>> (u1:v1:u2:v2:xs)
      where
        fora = fOR cfg xs a t u1 v1
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


uu :: PENV -> PENV -> PENV
uu xs ys = [ x \/ y | x <- xs, y <- ys ]
  
dodgy :: PENV -> PENV -> PENV
dodgy s t = uni (flat s) (flat t)
  where flat (Unit x) = [x]
        flat Empty = []
        flat (x :++ y) = flat x ++ flat y
        flat s = error $ "dodgy.flat " ++ show s
        uni [] ys = foldr (+++) Empty $ map Unit ys
        uni xs [] = foldr (+++) Empty $ map Unit xs
        uni (x:xs) (y:ys) = Unit (x \/ y) +++ uni xs ys

allValuesOf :: Ident -> P ENV -> [Value]
allValuesOf _ Empty = []
allValuesOf x (a :++ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (a :\/ b) = allValuesOf x a `L.union` allValuesOf x b
allValuesOf x (Unit d) = extractVar d x

ifUnion :: ConfigRef -> PENV -> PENV -> PENV
ifUnion cfg s t =
  case ifUnionMode (config cfg) of
    IfUnionUnion  -> s `union` t
    IfUnionConcat -> s +++ t
    IfUnionDodgy  -> s `dodgy` t

dE :: ConfigRef -> SrcEssential -> Ident -> Ident -> P ENV
dE cfg tin u v =
  (if doTrace cfg then
     logStep cfg "E" tin [pv "u" u, pv "v" v]
   else id) $
  dE' cfg tin u v

dE' :: ConfigRef -> SrcEssential -> Ident -> Ident -> P ENV
-- The denotational semantics itself (Fig 10)
dE' _   (Lit (LInt k))                   i x = unit $ i .=. x /\ x .= Int (fromIntegral k)
dE' _   (EPrim p)                        i x = unit $ i .=. x /\ x .= Fun (dP p)
--dE cfg (Variable (Ident _ "xf"))        i x = unit $ i .=. x /\ x .= Fun funXF -- hack for testing
dE' _   (Variable v) i x | isSrcUnderscore v = unit $ i .=. x
                         | otherwise         = unit $ i .=. x /\ x .=. v
dE' cfg (DefineE y t)                    i x = unit (x .=. y) *** dE cfg t i x      -- y := t
dE' cfg (DefineIE y t)                   i x = unit (i .=. y) *** dE cfg t i x      -- y ~> _ := t
dE' _   (DefineV y)                      i x = unit $ i .=. x /\ x .=. y
dE' cfg (Unify t0 t1)                    i x = cnorm cfg $ dE cfg t0 i x *** dE cfg t1 i x
dE' cfg (Choice t0 t1)                   i x = cnorm cfg $ dB cfg t0 i x +++ dB cfg t1 i x
dE' cfg (Seq t0 t1)                      i x = cnorm cfg $ dC cfg t0     *** dE cfg t1 i x
dE' cfg (Where t0 t1)                    i x = cnorm cfg $ dE cfg t0 i x *** dC cfg t1
dE' cfg (Block t)                        i x = dB cfg t i x
dE' _   Fail                             _ _ = Empty
dE' cfg (ApplyD (Variable (Ident _ "operator'|||'")) (Array [t0, t1])) i x =
  ifUnion cfg
    (dE cfg t0 i x)
    (dE cfg t1 i x)
dE' cfg (If3 t0 t1 t2)                   i x = cnorm cfg $
  ifUnion cfg
    (s0 *** dB cfg t1 i x >>> xs)
    (nOT (s0 >>> xs) *** dB cfg t2 i x)
  where xs = bvs t0
        s0 = oNE cfg xs (dC cfg t0)
dE' cfg (For2 t0 t1) i x = cnorm cfg $ fOR cfg (bvs t0) (dC cfg t0) t1 i x
dE' cfg t@(ApplyD (EPrim DotDot) (Array [t0, t1])) i x = cnorm cfg $
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
dE' cfg t@(Array ts)                     i x = cnorm cfg $ 
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
dE' _   (Range (EPrim IsInt))            i x = unit (bigUnion [ i .= v /\ x .= v | v <- allInts ])
dE' cfg (Range t)                        i x = cnorm cfg $ 
  dE cfg t j y *** dF y i x >>> [j,y]
    where (j, y) = fresh2 ("j", "y") [i, x] t
dE' cfg t@(ApplyD t0 t1)                 i x = cnorm cfg $
  dE cfg t0 h f *** dE cfg t1 j y *** dF f y x *** unit (i .=. x) >>> [h,f,j,y]
    where (h, f) = fresh2 ("h", "f") [i, x] t
          (j, y) = fresh2 ("j", "y") [i, x] t

dE' cfg t@(Function aprt t0 _ t1) i x = cnorm cfg $ fUN cfg (getAllBinders t) aprt (bvs t0) (dE cfg t0 p q) t1 p q i x
  where (p, q) = fresh2 ("p", "q") [i, x] t
dE' _   e                               _ _ = error $ "dE cfg: unimplemented " ++ show e

dF :: Ident -> Ident -> Ident -> P ENV
dF f a r =
  [ d
  | fp <- mkPomSetList allFUNs
  , h <- fp
  , let d = f .= Fun fp /\ funHasPair a r h
  , d /= E.empty
  ]

mkPomSetList :: [a] -> P a
mkPomSetList = foldr union Empty . map unit

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

dB :: ConfigRef -> SrcEssential -> Ident -> Ident -> P ENV
dB cfg tin u v =
  (if doTrace cfg then
     logStep cfg "B" tin [pv "u" u, pv "v" v]
   else
     id) $
  dB' cfg tin u v

dB' :: ConfigRef -> SrcEssential -> Ident -> Ident -> P ENV
dB' cfg e i x = dE cfg e i x >>> bvs e

dC :: ConfigRef -> SrcEssential -> P ENV
dC cfg tin =
  (if doTrace cfg then
     logStep cfg "C" tin []
   else id) $
  dC' cfg tin

dC' :: ConfigRef -> SrcEssential -> P ENV
dC' cfg e =
  dE cfg e i x >>> [i,x]  where (i, x) = fresh2 ("i", "x") [] e

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

(\\\\) :: ENV -> [Ident] -> ENV
(\\\\) = flip hides

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

cnorm :: ConfigRef -> PENV -> PENV
cnorm cfg | useTree (config cfg) = id
          | otherwise            = norm

-------

denS :: Config -> Bool -> SrcEssential -> IO (String, [String])
denS cfg trc t = do
  let (i, x) = fresh2 ("u", "v") [] t
  cfgr <- mkConfigRefIO cfg trc
  let res = show $ canon $ dE cfgr (Block t) i x
  True <- evaluate (res == res)
  l <- readIORef (logRef cfgr)
  return (res, reverse l)

-------

-- The first argument to fUN is only used to make sure we make fresh variables
fUN :: ConfigRef -> [Ident] -> Aperture -> [Ident] -> PENV -> SrcEssential -> Ident -> Ident -> Ident -> Ident -> PENV
fUN cfg used apt xs penv t1 c d h f =
  (if doTrace cfg then
     logStep cfg "FUN" penv [pv "apt" apt, pv "xs" xs, pv "t1" t1, pv "c" c, pv "d" d, pv "h" h, pv "f" f]
   else id) $
  fUN' cfg used apt xs penv t1 c d h f

fUN' :: ConfigRef -> [Ident] -> Aperture -> [Ident] -> PENV -> SrcEssential -> Ident -> Ident -> Ident -> Ident -> PENV
fUN' _ _ _ _ Empty _ _ _ h f = unit $ h .= Fun (fun [funEmpty]) /\ f .= Fun (fun [funEmpty])
fUN' cfg used apt xs (s :\/ t) t1 p q h f =
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
fUN' cfg used apt xs (s :++ t) t1 p q h f =
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
fUN' cfg used apt xs d@(Unit dd) t1 p q h f =
--trace ("fUN' dd=" ++ show dd ++ " t1=" ++ show t1 ++ " (p,q,h,f)=" ++ show(p,q,h,f) ++ "xs=" ++ show xs) $
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

tE :: SrcEssential -> Ident -> Ident -> IO ()
tE t u v = do
  cfg <- mkConfigRefIO defaultConfig True
  let r = dE cfg t u v
  print r
  l <- readIORef (logRef cfg)
  mapM_ putStrLn $ reverse l
