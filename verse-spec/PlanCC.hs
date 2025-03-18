{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main(main) where
import Control.Arrow(second)
import Control.Monad hiding (ap)
--import Data.Maybe
--import Data.Generics.Uniplate.Data(universe)
--import GHC.Stack
import Exp
import ExpSugar
import ValC
import SetX
import EnvC
import CExp
import Examples hiding ((===))
--import Debug.Trace

{-
implies :: Bool -> Bool -> Bool
implies x y = not x || y
-}

compress :: [Bool] -> [a] -> [a]
compress bs xs = [ x | (True, x) <- zip bs xs ]

-------------------------------------------

{-
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
-}

-------------------------------------------

aap :: W -> (Integer, W) -> Maybe W
aap (VInt k) (i, w) | i == k = Just w
aap _ _ = Nothing

applyf :: W -> WS -> [WS]
applyf (VTup ws) as = unionSetOfSeqs [ map (maybeToSet . aap  a) (zip [0..] ws) | a <- as ]
applyf (VFcn fs) as = unionSetOfSeqs [ map (maybeToSet . appM a) fs | a <- as ]
applyf _ _ = []

applys :: WS -> WS -> [WS]
applys fs as | isEmpty fs || isEmpty as = []                  -- avoid empty sets in foldSet
applys fs as = unionSetOfSeqs [ applyf f as | f <- fs ]

dE :: CExp -> Env -> [WS]
dE (CVar "_")          _rho  = [allWs]
dE (CVar x)             rho  = [sing $ lookupEnv x rho]
dE (CInt k)            _rho  = [sing $ VInt k]
dE (CPrim p)           _rho  = [sing $ dO p]
dE (CTup es)            rho  = map (fmap VTup . sequence) $ mapM (\ e -> dE e rho) es
dE (CApp e1 e2)         rho  = [ r | s1 <- dE e1 rho, s2 <- dE e2 rho, r <- applys s1 s2 ]
dE (COfType e1 e2)      rho  = dE (CApp e2 e1) rho
dE (CEqu e1 e2)         rho  = [ s1 `intersect` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CSeq CExi{} e)      rho  = dE e rho                                 -- just a speedup
dE (CSeq e1 e2)         rho  = [ if isEmpty s1 then empty else s2
                               | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CWhere e1 e2)       rho  = [ if isEmpty s2 then empty else s1
                               | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CExi _)            _rho  = [sing $ VInt 99999]
dE CFail               _rho  = []
dE (CChoice e1 e2)      rho  = dD e1 rho ++ dD e2 rho
dE (CUChoice e1 e2)     rho  = [ s1 `union` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CAll e)             rho  = [ fmap VTup $ sequence $ squash $ dD e rho ]
dE (CBlock e)           rho  = dD e rho
dE CDef{}              _rho  = error "CDef"
dE CFor{}              _rho  = error "CFor"
dE (CIf e1 e2 e3)       rho  =
  let rhos = oneE e1 rho
  in  if isEmpty rhos then
        squash $ dD e3 rho
      else
        -- XXX what's the right one
        squash $ unionSetOfSeqs [ squash $ dD e2 rho' | rho' <- rhos ]
        -- squash $ isectSetOfSeqs $ fmap (\ rho' -> squash $ dD e2 rho') rhos
dE (CLam q i e1 e2 me3) rho =
  let alts :: SetX (Val, SetX [Perhaps WS])
      alts =  [ (w, r) | w <- allWs, let r = useInput w, not (isEmpty r) ]
      useInput :: Val -> SetX [Perhaps WS]
      useInput w =       [ map (\ s -> if isEmpty s then No else Yes $ justOne $ dD e2 rho') w1s
                         | rho' <- dX e1 rho                    -- for each possible environment
                         , let w1s = dE e1 (extendEnv rho' i w)    -- evaluate e1, a sequence of choices
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
      vfcns = fmap VFcn $ sequence fcns
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

--dE e _ = error $ "unimplemented " ++ show e

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
  | f <- allFcns
  , domChk xyss f
  , forAll xyss ( \ (x, ys) -> isEmpty ys || app f x `member` ys)
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
{-
isYes :: Perhaps a -> Bool
isYes (Yes _) = True
isYes _ = False
-}

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

dD :: CExp -> Env -> [WS]
dD e rho = unionSetOfSeqs [ dE e rho' | rho' <- dX e rho ]

dXL :: CExp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dI e)
  in  map (foldr (\ (i,v) r -> extendEnv r i v) rho) exts

unionSetOfSeqs :: SetX [WS] -> [WS]
unionSetOfSeqs = foldSet unionSeqs

-- Pointwise union of sequences
unionSeqs :: [WS] -> [WS] -> [WS]
unionSeqs [] ys = ys
unionSeqs xs [] = xs
unionSeqs (x:xs) (y:ys) = union x y : unionSeqs xs ys

den :: Exp -> [WS]
den e = dD ({-redef $-} syntax "_" e) rho0

dene :: Exp -> [WS]
dene e = squash $ dD ({-redef $-} syntax "_" e) emptyEnv

dP :: Exp -> RVal
dP e =
  case squash $ den e of
    [s] | [v] <- toList s -> RVal v
        | otherwise       -> Wrong $ showListWith showPretty (toList s)
    vs                    -> Wrong $ show vs

allExps :: [Example]
allExps = [exp01, exp02, exp03, exp04,
           exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22,
           exp23,exp24,exp25,exp26,exp27,exp28,exp29,{-WRONG exp30,exp31,-}exp32,
           exp33, exp34, exp35,
           exp36, exp37, exp38, exp39, exp40, {- UNSURE exp41, exp43, exp44, -}
           exp45, exp46, exp47, exp48, {- UNSURE exp49, exp50, -}
           exp51, exp52,
           exp53, exp54,
           exp55, exp56, exp57, {- SLOW exp58,-} exp59, exp60,
           exp61, exp62, exp63
          ]

main :: IO ()
main = do
  putStrLn "Start"
  runExamples dP allExps
