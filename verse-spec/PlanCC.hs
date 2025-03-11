{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main where
import Control.Arrow(second)
import Control.Monad hiding (ap)
import qualified Data.Map as M
--import Data.Maybe
import Data.Generics.Uniplate.Data(universe)
import Data.String
import GHC.Stack
import Exp hiding (dI)
import ExpSugar
import ValC
import SetX
import EnvC
import CExp
import Examples hiding ((===))
import Debug.Trace

implies :: Bool -> Bool -> Bool
implies x y = not x || y

compress :: [Bool] -> [a] -> [a]
compress bs xs = [ x | (True, x) <- zip bs xs ]

-------------------------------------------

-- Reintroduce CDef for definitions only mentioned to the right.
-- Together with the direct semantics for CDef this is a big speedup.
redef :: CExp -> CExp
redef = red []
  where red vs (CSeq (CExi x) (CEqu (CVar x') e)) | x == x', x `notElem` vs = CDef x (red vs e)
        red vs (CSeq (CSeq e1 e2) e3) = red vs (CSeq e1 (CSeq e2 e3))
        red vs (CSeq e1 e2) = CSeq (red vs e1) (red (allVars e1 ++ vs) e2)
        red vs (CEqu e1 e2) = CEqu (red vs e1) (red (allVars e1 ++ vs) e2)
        red vs (CApp e1 e2) = CApp (red vs e1) (red (allVars e1 ++ vs) e2)
        red _ (CLam q i e1 e2 me3) = CLam q i (red [] e1) (red [] e2) me3
        red _ e = e

allVars :: CExp -> [Ident]
allVars e = [ i | CVar i <- universe e ]

-------------------------------------------

{-
apply :: W -> W -> [W]
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = [ws !! k']  where k' = fromInteger k
apply (VFcn fs) w = [ y | Fcn _ xys <- fs, Just y <- [M.lookup w xys] ]
apply _ _ = []
-}

sap :: W -> Fcn -> Maybe W
sap a (Fcn _ xys) = M.lookup a xys

aap :: W -> (Integer, W) -> Maybe W
aap (VInt k) (i, w) | i == k = Just w
aap _ _ = Nothing

applyVFcn :: W -> WS -> [WS]
applyVFcn (VTup ws) as = --alt [ ws !! k' | VInt k <- as, let k' = fromInteger k, 0 <= k' && k' < length ws ]
                         joinSeqs [ map (aap a) (zip [0..] ws) | a <- as ]
applyVFcn (VFcn fs) as = joinSeqs [ map (sap a) fs | a <- as ]
applyVFcn _ _ = []

joinSeqs :: SetX [Maybe W] -> [WS]
joinSeqs = foldSet (zipWith union) . fmap (map f)
  where f Nothing  = empty
        f (Just x) = sing x

applySets :: WS -> WS -> [WS]
--applySets fs as | trace ("applySets fs,as" ++ show (fs, as)) False = undefined
applySets fs as | isEmpty fs || isEmpty as = [empty]
applySets fs as =
--  trace ("applySets " ++ show [ applyVFcn f as | f <- fs ]) $
  unionSetOfSeqs [ applyVFcn f as | f <- fs ]

cVTup :: [WS] -> WS
cVTup wss = fmap VTup $ cartProd wss

dE :: CExp -> Env -> [WS]
dE (CVar "_") _                             = -- error "stray _"
                                              [allWs]
dE (CVar x)  rho                            = [sing $ lookupEnv x rho]
dE (CInt k)    _                            = [sing $ VInt k]
dE (CPrim p)   _                            = [sing $ dO p]
dE (CTup es) rho                            = -- mkSet $ map VTup $ sequence $ map (\ e -> unSet (dE e rho)) es
                                              map cVTup $ mapM (\ e -> dE e rho) es

dE (CApp e1 e2)   rho                       = [ r | s1 <- dE e1 rho, s2 <- dE e2 rho, r <- applySets s1 s2 ]
dE (COfType e1 e2) rho                      = dE (CApp e2 e1) rho
dE (CEqu e1 e2)   rho                       = [ s1 `intersect` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CSeq e1 e2)   rho                       = [ if isEmpty s1 then empty else s2
                                              | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CWhere e1 e2)   rho                     = [ if isEmpty s2 then empty else s1
                                              | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CExi _)         _                       = [sing $ VInt 99999]
dE CFail            _                       = [empty]
dE (CIf e1 e2 e3) rho                       =
  let rhos = oneE e1 rho
  in  if isEmpty rhos then
        squash $ dD e3 rho
      else
        -- XXX what's the right one
        squash $ unionSetOfSeqs $ fmap (\ rho' -> squash $ dD e2 rho') rhos
        -- squash $ isectSetOfSeqs $ fmap (\ rho' -> squash $ dD e2 rho') rhos
dE (CLam q i e1 e2 me3) rho =
  let alts :: SetX (Val, SetX [Perhaps WS])
      alts =  [ (w, r) | w <- allWs, let r = useInput w, not (isEmpty r) ]
      useInput :: Val -> SetX [Perhaps WS]
      useInput w =       [ map (\ s -> if isEmpty s then No else Yes $ justOne $ dD e2 rho') w1s
                         | rho' <- dX e1 rho                    -- for each possible environment
                         , let w1s = dE e1 (extend rho' i w)    -- evaluate e1, a sequence of choices
                         , not $ all isEmpty w1s                -- and use it, if it succeeds at any choice
                         ]
      justOne [x] = x
      justOne _ = error "multi-valued rhs"
      seqPerhapsToSet :: [Perhaps a] -> SetX a
      seqPerhapsToSet xs = mkSet [ a | Yes a <- xs ]
      inOuts :: SetX (Val, [WS])
      inOuts = [ (x, joins r) | (x, r) <- alts ]
      joins :: SetX [Perhaps WS] -> [WS]
      joins ss =
        let ws :: WS
            ws = foldSet intersect . join . fmap seqPerhapsToSet $ ss
            comb :: [Perhaps WS] -> [Perhaps WS] -> [Perhaps WS]
            comb = zipWith f
              where f No No = No
                    f _ _   = Yes ws
            emp No = empty
            emp (Yes s) = s
            emp _ = undefined -- make GHC happy
        in  if isEmpty ws then [empty]
            else fmap emp $ foldSet comb ss
      inOuts' :: SetX (Val, [WS])
      inOuts' = emptyPosTrim $ timTrim inOuts
      inOutPairs :: SetX [(Val, WS)]
      inOutPairs = fmap (\ (v, s) -> [ (v, x) | x <- s ] ) inOuts'
      fcnDescs :: [SetX (Val, WS)]
      fcnDescs = foldSet (zipWith union) $ fmap (map sing) inOutPairs
      e1Dom = fmap fst alts
      fcns :: [SetX Fcn]
      fcns = map (getFcn q) fcnDescs
      fcnsDom = unions $ map (join . fmap dom) fcns
      vfcns = fmap VFcn $ cartProd fcns
      vfcns' =
        case q of
          Closed | e1Dom == fcnsDom -> vfcns
          Open | e1Dom `isSubsetOf` fcnsDom -> vfcns
          _ -> empty
  in  --error $ "\n" ++ show fcnDescs
      if chkClsd me3 rho then
        [vfcns']
      else
        [empty]

{-
dE ee@(CChkClsd x e) rho =
 case lookupEnv x rho of
    v@VFcn{} -> chk v
    v@VTup{} -> chk v
    _ -> empty
  where
    chk v =
      let xDom = domV' v
          eRng = dD e rho
      in  --trace ("\n=== " ++ show (ee, v, xDom == eRng)) $
        if xDom == eRng then   -- `lessEq` ??
          trace ("\n=== " ++ show (ee, lookupEnv x rho, xDom == eRng)) $
          sing (VInt 88888)
        else
          empty
-}

dE (CChoice e1 e2) rho = dD e1 rho ++ dD e2 rho
dE (CUChoice e1 e2)rho = re [ s1 `union` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CAll e) rho =
  let vs = squash $ dD e rho
  in  [ fmap VTup $ cartProd vs ]
dE e _ = error $ "unimplemented " ++ show e

getFcn :: OC -> SetX (Val, WS) -> SetX Fcn
{-
getFcn Closed xyss =
  let d = [ x | (x, ys) <- xyss, not (isEmpty ys) ]
  in  [ f
      | f <- mkSet allFcns
--      , trace (show (f, domFcn f, d)) True
      , dom f == d
--      , trace (show [(ap f x, ys) | (x, ys) <- xyss]) True
      , forAll xyss ( \ (x, ys) -> isEmpty ys || ap f x `member` ys)
      ]
getFcn Open _ = error "getFcn: Open"
-}
getFcn q xyss = close q
  [ f
  | f <- mkSet allFcns
  , domChk xyss f
  , forAll xyss ( \ (x, ys) -> isEmpty ys || ap f x `member` ys)
  ]
domChk :: SetX (Val, WS) -> Fcn -> Bool
domChk xyss f = forAll xyss (\ (x, ys) -> if isEmpty ys then not (inDom x f) else inDom x f)


newtype Perhaps a = P (Maybe a)
  deriving (Eq, Ord, Functor, Applicative, Monad)
pattern Yes :: a -> Perhaps a
pattern Yes a = P (Just a)
pattern No :: Perhaps a
pattern No = P Nothing
instance Show a => Show (Perhaps a) where
  show (Yes a) = "Y" ++ show a
  show No = "N"
  show _ = undefined -- make GHC happy
isYes :: Perhaps a -> Bool
isYes (Yes _) = True
isYes _ = False

-- Pick first non-empty in a sequence.
timTrim :: SetX (Val, [WS]) -> SetX (Val, [WS])
timTrim ss =
  let ss' = fmap (second trim) ss
      trim s =
        case span isEmpty s of
          (xs, x : _) -> xs ++ [x]
          _ -> []
      n = maximumSet $ fmap (length . snd) ss'
  in  fmap (second (\ s -> s ++ replicate (n - length s) empty)) ss'

emptyPosTrim :: SetX (Val, [WS]) -> SetX (Val, [WS])
emptyPosTrim ss =
  let used = foldSet (zipWith (||)) $ fmap g ss
      g (_, xs) = map (not . isEmpty) xs
  in  fmap (second $ compress used) ss

isectMany :: [WS] -> WS
isectMany [] = error "isectMany"
isectMany wss = foldl1 intersect wss

isectSetOfSeqs :: HasCallStack => SetX [WS] -> [WS]
isectSetOfSeqs ss =
  let n = minimumSet (fmap length ss)
      ss' = fmap (take n) ss
  in  foldSet xIntersect ss'

unionSetOfSeqs :: SetX [WS] -> [WS]
unionSetOfSeqs = foldSet xUnion

re :: [WS] -> [WS]
re = id -- filter (not . isEmpty)

squash :: [WS] -> [WS]
squash = filter (not . isEmpty)

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
oneE :: CExp -> Env -> SetX Env
oneE e rho = [ rho' | rho' <- dX e rho, not $ null $ squash $ dE e rho' ]

domV' :: Val -> SetX Val
domV' v@VFcn{} = domV v
domV' v@VTup{} = domV v
domV' _ = empty

chkClsd :: Maybe (Ident, CExp) -> Env -> Bool
chkClsd Nothing _ = True
chkClsd (Just (x, e)) rho =
  case lookupEnv x rho of
    v@VFcn{} -> chk v
    v@VTup{} -> chk v
    _ -> False
  where
    chk v =
      let xDom = domV' v
          eRng = unions $ dD e rho
      in  --trace ("\n=== " ++ show (ee, v, xDom == eRng)) $
        if xDom == eRng then   -- `lessEq` ??
          --trace ("\n=== " ++ show (ee, lookupEnv x rho, xDom == eRng)) $
          True
        else
          False

close :: OC -> SetX Fcn -> SetX Fcn
close Open fs = fs
close Closed fs =
  let r = [ f | f <- fs, forAll fs (\ f' -> dom f `isSubsetOf` dom f') ]
  in  --trace ("close " ++ show (fs, r))
      r

dX :: CExp -> Env -> SetX Env
dX e rho = mkSet $ dXL e rho

dXL :: CExp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dI e)
  in  map (foldr (\ (i,v) r -> extend r i v) rho) exts

dD :: CExp -> Env -> [WS]
dD e rho = xUnions [ dE e rho' | rho' <- dXL e rho ]

xUnions :: [[WS]] -> [WS]
xUnions [] = []
xUnions ss = foldr1 xUnion ss

xIntersect :: [WS] -> [WS] -> [WS]
xIntersect = zipWith intersect

xUnion :: [WS] -> [WS] -> [WS]
xUnion [] ys = ys
xUnion xs [] = xs
xUnion (x:xs) (y:ys) = union x y : xUnion xs ys

den :: Exp -> [WS]
den e = dD ({-redef $-} syntax "_" e) rho0

dene :: Exp -> [WS]
dene e = dD ({-redef $-} syntax "_" e) emptyEnv

dP :: Exp -> RVal
dP e =
  case squash $ den e of
    [s] | [v] <- toList s -> RVal v
        | otherwise       -> Wrong $ showListWith showPretty (toList s)
    vs                    -> Wrong $ show vs

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22,
           exp23,exp24,exp25,exp26,exp27,exp28,exp29,{-WRONG exp30,exp31,-}exp32,
           exp33, exp34, exp35,
           exp36, exp37, exp38, exp39, exp40, {- UNSURE exp41, exp43, exp44, -}
           exp45, exp46, exp47, exp48, {- UNSURE exp49, exp50, -}
           exp51, exp52,
           exp53, exp54,
           exp55, exp56, exp57, {- SLOW exp58,-} exp59, exp60,
           exp61, exp62
          ]

main :: IO ()
main = do
  putStrLn "Start"
  runExamples dP allExps

{-
g0 :: Exp
g0 = fun_c ("x" := 0 :| 1) "x"

g1 :: Exp
g1 = fun_c g1d "x"

g1d = "x" := (0 :| 1 :| 0) `wher` "y" := 2 :| 3
g1e = g1d >: "y"

g2 :: Exp
g2 = fun_c (2 :| 3 >: "x" := 0 :| 1) "x"

{-
(0,{[Nothing,           Just [|x->0,y->3|],Nothing,Nothing,Nothing,           Just [|x->0,y->3|]],
    [Just [|x->0,y->2|],Nothing,           Nothing,Nothing,Just [|x->0,y->2|],Nothing]})
(1,{[Nothing,Nothing,Nothing,           Just [|x->1,y->3|],Nothing,Nothing],
    [Nothing,Nothing,Just [|x->1,y->2|],Nothing,           Nothing,Nothing]})
-}

{-
if (x:= 1||2) { 1..x }
-}

g3 :: Exp
g3 = fun_c ("x" := cint)("x" === 1)
-}
