{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods -Wno-x-partial #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main where
import Control.Arrow(second)
import Control.Monad hiding (ap)
--import Data.Maybe
import Data.Generics.Uniplate.Data(universe)
import GHC.Stack
import Exp
import ExpSugar
import ValC
import SetX
import EnvC
import CExp
import Examples hiding ((===))
import Debug.Trace

{-
implies :: Bool -> Bool -> Bool
implies x y = not x || y
-}

compress :: [Bool] -> [a] -> [a]
compress bs xs = [ x | (True, x) <- zip bs xs ]

-------------------------------------------

-- Reintroduce CDef for definitions only mentioned to the right.
-- Together with the direct semantics for CDef this is a big speedup.
redef :: CExp -> CExp
redef | ()/=() = id
      | otherwise = red []
  where
    red vs (CBlock b) = CBlock $ redb vs b
    red vs (CApp e1 e2) = CApp (red vs e1) (red (allVars e1 ++ vs) e2)
    red vs (CEqu e1 e2) = CEqu (red vs e1) (red (allVars e1 ++ vs) e2)
    red vs (CSeq e1 e2) = CSeq (red vs e1) (red (allVars e1 ++ vs) e2)
    red vs (CChoice b1 b2) = CChoice (redb vs b1) (redb vs b2)
    red vs (CUChoice b1 b2) = CUChoice (redb vs b1) (redb vs b2)
    --red vs (CTup ...)
    red vs (CIf b1 b2 b3) = CIf (redb vs b1) (redb (allVarsB b1 ++ vs) b2) (redb vs b3)
    red vs (CLam oc i b1 b2 me) = CLam oc i (redb vs b1) (redb (allVarsB b1 ++ vs) b2) me
    red _vs (CFor _ _) = undefined
    red vs (CAll b) = CAll (redb vs b)
    red _vs e = e

--    redb avs (CBlk es) | trace ("redb " ++ show (avs, es, length es)) False = undefined
    redb avs (CBlk aes) = CBlk $ loop avs aes
      where loop _ [] = []
            loop vs (CDef i (CExi x `CSeq` CEqu (CVar x') e) : es) 
              | x == x', x `notElem` vs' =
                  CDef x (red vs e) : CDef i (CVar x) : loop vs' es
                  where vs' = allVars e ++ vs
            loop vs (CExi x : CEqu (CVar x') e : es)
              | x == x', x `notElem` vs' =
                  CDef x (red vs e) : loop vs' es
                  where vs' = allVars e ++ vs
            loop vs (e : es) = red vs e : loop (allVars e ++ vs) es

{-
    red vs (CSeq (CExi x) (CEqu (CVar x') e))
          | x == x', x `notElem` vs, x `notElem` allVars e = CDef x (red vs e)
        red vs (CSeq (CExi x) (CSeq (CEqu (CVar x') e) e2))
          | x == x', x `notElem` vs, x `notElem` allVars e = CDef x (red vs e) `CSeq` red vs e2
        red vs (CSeq (CSeq e1 e2) e3) = red vs (CSeq e1 (CSeq e2 e3))
--        red vs (CSeq e1 (CSeq e2 e3)) = red vs (CSeq (CSeq e1 e2) e3)
        red vs (CSeq e1 e2) = CSeq (red vs e1) (red (allVars e1 ++ vs) e2)
--        red vs (CWhere e1 e2) = CWhere (red vs e1) (red (allVars e1 ++ vs) e2)
        red vs (CDef i e) = CDef i (red vs e)
        red vs (CEqu e1 e2) = CEqu (red vs e1) (red (allVars e1 ++ vs) e2)
        red vs (CApp e1 e2) = CApp (red vs e1) (red (allVars e1 ++ vs) e2)
        red _ (CLam q i e1 e2 me3) = CLam q i (red [] e1) (red [] e2) me3
        red _ e = e
-}

allVars :: CExp -> [Ident]
allVars e = [ i | CVar i <- universe e ]

allVarsB :: CBlk -> [Ident]
allVarsB = allVars . cexpb

-------------------------------------------

aap :: W -> (Integer, W) -> Maybe W
aap (VInt k) (i, w) | i == k = Just w
aap _ _ = Nothing

applyf :: W -> Ws -> WS
applyf (VTup ws) as = unionSetOfSeqs [ map (maybeToSet . aap  a) (zip [0..] ws) | a <- as ]
applyf (VFcn fs) as = unionSetOfSeqs [ map (maybeToSet . appM a) fs | a <- as ]
applyf _ _ = []

applys :: Ws -> Ws -> WS
applys fs as | isEmpty fs || isEmpty as = []                  -- avoid empty sets in foldSet
applys fs as = unionSetOfSeqs [ applyf f as | f <- fs ]

dE :: CExp -> Env -> WS
dE (CVar "_")          _rho  = [allWs]
dE (CVar x)             rho  = [sing $ lookupEnv x rho]
dE (CInt k)            _rho  = [sing $ VInt k]
dE (CPrim p)           _rho  = [sing $ dO p]
dE (CTup es)            rho  = map (fmap VTup . sequence) $ mapM (\ e -> dE e rho) es
dE (CApp e1 e2)         rho  = [ r | s1 <- dE e1 rho, s2 <- dE e2 rho, r <- applys s1 s2 ]
dE (COfType e1 e2)      rho  = dE (CApp e2 e1) rho
dE (CEqu e1 e2)         rho  = do -- [ s1 `intersect` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
  case e1 of
    CLam{} -> traceM (show (CEqu e1 e2))
    _ -> pure ()
  s1 <- dE e1 rho
  case e1 of
    CLam{} -> traceM (show (s1, rho))
    _ -> pure ()
  s2 <- dE e2 rho
  case e1 of
    CLam{} -> traceM (show s2)
    _ -> pure ()
  pure $ s1 `intersect` s2
dE (CSeq CExi{} e)      rho  = dE e rho                                 -- just a speedup
dE (CSeq e1 e2)         rho  = concat [ if isEmpty s1 then [empty] else dE e2 rho
                                      | s1 <- dE e1 rho ]
{-
dE (CWhere (CDef i e1) e2)rho= [ if isEmpty s2 then empty else s1
                               | s1 <- dE e1 rho,
                                 s2 <- dEs e2 [ extendEnv rho i v | v <- s1 ] ]
dE (CWhere e1 e2)       rho  = [ if isEmpty s2 then empty else s1
                               | s1 <- dE e1 rho, s2 <- dE e2 rho ]
-}
dE (CExi _)            _rho  = [sing $ VInt 99999]
dE CFail               _rho  = []
dE (CChoice e1 e2)      rho  = dB e1 rho ++ dB e2 rho
dE (CUChoice e1 e2)     rho  = [ s1 `union` s2 | s1 <- dB e1 rho, s2 <- dB e2 rho ]
dE (CAll e)             rho  = [ fmap VTup $ sequence $ squash $ dB e rho ]
dE (CBlock e)           rho  = dB e rho
dE e@(CDef _ _)        _rho  = error $ "CDef " ++ show e
--dE (CDef _ e)           rho  = dE e rho
dE CFor{}              _rho  = error "CFor"
dE CLHS                 rho  = [sing $ VEnv $ toListEnv rho]
dE (CIf e1 e2 e3)       rho  =
  let rhos = oneE e1 rho
  in  if isEmpty rhos then
        squash $ dB e3 rho
      else
        -- XXX what's the right one
        squash $ unionSetOfSeqs [ squash $ dB e2 rho' | rho' <- rhos ]
        -- squash $ isectSetOfSeqs $ fmap (\ rho' -> squash $ dD e2 rho') rhos
#if 1
dE e@(CLam q _i _e1 _e2 me3) rho =
    if anySet (all isEmpty) fs then [empty] else  -- Don't do this if the functions is decides.
    let res = if chkClsd me3 rho then [fcn] else [empty]
    in
--      let wr = if null me3 then "un" else "wr" in
--      trace ("=== dE " ++ wr ++ " enter e=" ++ show e ++ ", rho=" ++ show rho) $ 
--      trace ("=== dE " ++ wr ++ " enter e=" ++ show e ++ ", rho=" ++ show rho ++ ", fs=" ++ show fs) $ 
--      trace ("=== dE " ++ wr ++ " exit  e=" ++ show e ++ ", rho=" ++ show rho ++ ", res=" ++ show res) $
      res
  where
    fs :: SetX [SetX (W, W)]
    fs = [ map (dist v) r | v <- allWs, let r = dF e rho v, not (null r) ]
    dist v s = fmap (v,) s
    fcn = combine q $ filterSet (not . all isEmpty) fs
#else
dE (CLam q i b1 b2 me3) rho =
  let e1 = cexpb b1; e2 = cexpb b2 in
  let alts :: SetX (Val, SetX [Perhaps Ws])
      alts =  [ (w, r) | w <- allWs, let r = useInput w, not (isEmpty r) ]
      useInput :: Val -> SetX [Perhaps Ws]
      useInput w = [ map (\ s -> if isEmpty s then No else Yes $ justOne $ dD e2 rho') w1s
                   | rho' <- dX e1 rho                    -- for each possible environment
                   , let w1s = dE e1 (extendEnv rho' i w) -- evaluate e1, a sequence of choices
                   , not $ all isEmpty w1s                -- and use it, if it succeeds at any choice
                   ]
      justOne [x] = x
      justOne _ = error "multi-valued rhs"
      seqPerhapsToSet :: Ord a => [Perhaps a] -> SetX a
      seqPerhapsToSet xs = mkSet [ a | Yes a <- xs ]
      inOuts :: SetX (Val, WS)
      inOuts = [ (x, joins r) | (x, r) <- alts ]
      joins :: SetX [Perhaps Ws] -> WS
      joins ss =
        let ws1 :: SetX (SetX Ws)
            ws1 = fmap seqPerhapsToSet ss
            ws2 :: SetX Ws
            ws2 = join ws1
            ws :: Ws
            ws = foldSet intersect ws2
            comb :: [Perhaps Ws] -> [Perhaps Ws] -> [Perhaps Ws]
            comb = zipWith f
              where f No No = No
                    f _ _   = Yes ws
            emp No = empty
            emp (Yes s) = s
            emp _ = undefined -- make GHC happy
        in  if isEmpty ws then [empty]
            else fmap emp $ foldSet comb ss
      inOuts' :: SetX (Val, WS)
      inOuts' = emptyPosTrim $ timTrim inOuts
      inOutPairs :: SetX [(Val, Ws)]
      inOutPairs = fmap (\ (v, s) -> [ (v, x) | x <- s ] ) inOuts'
      fcnDescs :: [SetX (Val, Ws)]
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
#endif

-- For a function with domain e1 and range e2,
-- in an environment (which binds the function input),
-- compute the sequence of result.  The result has
-- a prefix of empty sets (possibly) followed by a non-empty set.
-- E.g.
--  F (x:=0|1)             (x)    [x->0]  =  [{0}]
--  F (x:=0|1)             (x)    [x->1]  =  [{},{1}]
--  F (x:=0|1)             (x||2) [x->0]  =  [{0,2}]
--  F (2|3; x:=0|1)        (x)    [x->1]  =  [{},{1}]    (trunc ,{},{1}])
--  F (x:=0|1 `where` 2|3) (x)    [x->1]  =  [{},{},{1}] (trunc ,{1}])
dF :: CExp -> Env -> W -> WS
dF (CLam _oc i e1 e2 _) rho v = dF' i e1 e2 rho v
dF _ _ _ = undefined

dF' :: Ident -> CBlk -> CBlk -> Env -> W -> WS
dF' i b1 b2 rho v = trunc ws
  where
    ss :: SetX [SetX W]
    ss = [ map (\ s -> if isEmpty s then empty else join [ once $ dB b2 w | w <- s ]) rho1s
         | rho' <- dXB b1 (extendEnv rho i v)     -- for each possible environment
         , let rho1s = dBEnv b1 rho'               -- evaluate e1, a sequence of choices
         , not $ all isEmpty rho1s                -- and use it, if it succeeds at any choice
         ]
    ws :: [SetX W]
    ws = unionSetOfSeqs' ss
    res = foldr1 intersect $ filter (not . isEmpty) ws
    once [x] = x
    once _   = empty  -- WRONG?
    trunc [] = []
    trunc (x:xs) | isEmpty x = x : trunc xs
                 | otherwise = [res]
--dF' _ e1 _ _ _ = error $ "dF' " ++ show e1

unVEnv :: Val -> Env
unVEnv (VEnv e) = fromListEnv e
unVEnv _ = undefined

combine :: OC -> SetX [SetX (W, W)] -> SetX W
combine q = join . fmap mk . sequence . map cross . groupByPos
  where
    -- deal with aperture
    mk :: [SetX (W, W)] -> SetX Val
    mk [] = empty
    mk ss =
      case q of
        -- create a single VFcn with the enumerated choices
        Closed -> sing $ VFcn $ map funFromSet ss
        -- union all the choices and pick all functions that agree with this
        -- XXX only produce functions where dom f = W?
        Open   -> mkSetUnsafe [ vFcn f | f <- allFcns, subFcn g f]
                  where g = funFromSet $ unions ss

funFromSet :: SetX (W, W) -> Fcn
funFromSet = mkFcn . toList

groupByPos :: SetX [SetX a] -> [ SetX (SetX a) ]
groupByPos s | isEmpty s = []
             | otherwise = fmap head nes : groupByPos (fmap tail es)
  where
    (es, nes) = partitionSet emptyHead s
    emptyHead (x:_) = isEmpty x
    emptyHead _ = undefined

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

{-
getFcn :: OC -> SetX (Val, Ws) -> SetX Fcn
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
  | VFcn fs <- allWs
  , f <- mkSetUnsafe fs
  , domChk xyss f
  , forAll xyss ( \ (x, ys) -> isEmpty ys || app f x `member` ys)
  ]
domChk :: SetX (Val, Ws) -> Fcn -> Bool
domChk xyss f = forAll xyss (\ (x, ys) -> if isEmpty ys then not (inDom x f) else inDom x f)
-}

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
timTrim :: SetX (Val, WS) -> SetX (Val, WS)
timTrim ss =
  let ss' = fmap (second trim) ss
      trim s =
        case span isEmpty s of
          (xs, x : _) -> xs ++ [x]
          _ -> []
      n = maximumSet $ fmap (length . snd) ss'
  in  fmap (second (\ s -> s ++ replicate (n - length s) empty)) ss'

emptyPosTrim :: SetX (Val, WS) -> SetX (Val, WS)
emptyPosTrim ss =
  let used = foldSet (zipWith (||)) $ fmap g ss
      g (_, xs) = map (not . isEmpty) xs
  in  fmap (second $ compress used) ss

squash :: WS -> WS
squash = filter (not . isEmpty)

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
oneE :: CBlk -> Env -> SetX Env
--oneE b rho = dbEnv b rho
oneE b rho = [ rho' | rho' <- dX e rho, not $ null $ squash $ dD e rho' ]
  where e = cexpb b

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
--      trace ("chkClsd v =" ++ show v ++", e=" ++ show e) $
      let xDom = domV' v
          eRng = unions $ dD e rho
      in
--        trace ("\n=== " ++ show (xDom, eRng, xDom == eRng)) $
        if xDom == eRng then   -- `lessEq` ??
--          trace ("\nchkClsd True " ++ show v) $
          True
        else
          False

dB :: CBlk -> Env -> WS
dB (CBlk es) rho = unionSetOfSeqs [ dE' es rho' | rho' <- dX (cexpb $ CBlk es) rho ]

dBEnv :: CBlk -> Env -> [SetX Env]
dBEnv b rho = map (fmap unVEnv) $ dB b rho

dE' :: [CExp] -> Env -> WS
dE' [] _rho = undefined
dE' [e] rho = dE e rho
dE' (CDef i e : es) rho = concat
  [ if isEmpty s then [empty] else dEs es [ extendEnv rho i v | v <- s ]
  | s <- dE e rho ]
dE' (e : es) rho = concat
  [ if isEmpty s then [empty] else dE' es rho
  | s <- dE e rho ]

dEs :: [CExp] -> SetX Env -> WS
dEs es rhos = unionSetOfSeqs' $ fmap (dE' es) rhos

dXB :: CBlk -> Env -> SetX Env
dXB b rho = dX (cexpb b) rho

dX :: CExp -> Env -> SetX Env
dX e rho = mkSetUnsafe $ dXL e rho

dD :: CExp -> Env -> WS
dD e rho = unionSetOfSeqs [ dE e rho' | rho' <- dX e rho ]

dXL :: CExp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dI e)
  in  map (foldr (\ (i,v) r -> extendEnv r i v) rho) exts

unionSetOfSeqs' :: SetX WS -> WS
unionSetOfSeqs' s | isEmpty s = []
                  | otherwise = unionSetOfSeqs s

unionSetOfSeqs :: HasCallStack => SetX WS -> WS
unionSetOfSeqs = foldSet unionSeqs

-- Pointwise union of sequences
unionSeqs :: WS -> WS -> WS
unionSeqs [] ys = ys
unionSeqs xs [] = xs
unionSeqs (x:xs) (y:ys) = union x y : unionSeqs xs ys

den :: Exp -> WS
den e = dD (redef $ syntax "_" e) rho0

dene :: Exp -> WS
dene e = dD (redef $ syntax "_" e) emptyEnv

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
           exp20, exp21, {- BUG (:=) exp22,-}
           exp23, exp24, exp25, exp26, exp27, exp28, exp29,{-WRONG exp30,exp31,-}exp32,
           exp33, exp34, {- SLOW exp35,-}
           -- XXX exp43 looks wrong
           exp36, exp37, exp38, {- SLOW exp39,-} {- UNSURE exp40,-} {- UNSURE exp41,-} exp43, {- UNSURE exp44, -}
           exp45, exp46, exp47, exp48, {- UNSURE exp49, exp50, -}
           {- SLOW exp51, exp52, -}
           {- NOT CHECKED exp53,-} {- DODGY circularity exp54,-}
           {- uses exp53 exp55,-} exp56, exp57, {- SLOW exp58,-} exp59, exp60,
           exp61, exp62, exp63, exp64, exp65, exp66, exp67 {- SLOW , exp68-}
          ]

main :: IO ()
main = do
  putStrLn "Start"
--  runExample dP exp68
  runExamples dP allExps

ds :: Exp -> CExp
ds = redef . syntax "_"

