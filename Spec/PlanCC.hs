{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods -Wno-x-partial #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}
module PlanCC(den, dene, ds, main, allExps, dP, edenSem, edenSemDS, CExp) where
--import Control.Arrow(second)
--import Control.Monad hiding (ap)
import Data.Maybe
import Data.Generics.Uniplate.Data(universe)
import GHC.Stack
import Exp
--import ExpSugar
import ValC
import SetX
import EnvC
import CExp
import Examples hiding ((===))
--import Debug.Trace
--import SExpC

--implies :: Bool -> Bool -> Bool
--implies x y = not x || y

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
    red vs (CLam oc i b1 b2 b3) = CLam oc i (redb vs b1) (redb (allVarsB b1 ++ vs) b2) (redb (allVarsB b1 ++ allVarsB b2 ++ vs) b3)
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

allVars :: CExp -> [Ident]
allVars e = [ i | CVar i <- universe e ]

allVarsB :: CBlk -> [Ident]
allVarsB = allVars . cexpb

-------------------------------------------

{-
aap :: W -> (Integer, W) -> Maybe W
aap (VInt k) (i, w) | i == k = Just w
aap _ _ = Nothing
-}

applyf :: W -> Ws -> WS
applyf (VFcn fs) as = unionSetOfSeqs [ map (maybeToSet . appM a) fs | a <- as ]
--applyf (VTup ws) as = unionSetOfSeqs [ map (maybeToSet . aap  a) (zip [0..] ws) | a <- as ]
applyf _ _ = []

applys :: Ws -> Ws -> WS
applys fs as | isEmpty fs || isEmpty as = []                  -- avoid empty sets in foldSet
applys fs as = unionSetOfSeqs [ applyf f as | f <- fs ]

applyo :: W -> W -> WS
applyo (VFcn fs) a = map (maybeToSet . appM a) fs
--applyo (VTup ws) a = map (maybeToSet . aap  a) (zip [0..] ws)
applyo _ _ = []

lookupVar :: Ident -> Env -> W
lookupVar x rho =
  case lookupEnvM x rho of
    Just v -> v
    Nothing ->
      case nameFcnM x of
        Just f -> VFcn [f]
        Nothing -> error $ "undefined variable " ++ show x

dE :: CExp -> Env -> WS
dE (CVar "_")          _rho  = [allWs]
dE (CVar x)             rho  = [sing $ lookupVar x rho]
dE (CInt k)            _rho  = [sing $ VInt k]
dE (CPrim p)           _rho  = [sing $ dO p]
dE (CTup es)            rho  = map (fmap mkTup . sequence) $ mapM (\ e -> dE e rho) es
dE (CApp e1 e2)         rho  = [ r | s1 <- dE e1 rho, s2 <- dE e2 rho, r <- applys s1 s2 ]
dE (COfType e1 e2)      rho  = dE (CApp e2 e1) rho
dE (CEqu e1 e2)         rho  = [ s1 `intersect` s2 | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (CSeq CExi{} e)      rho  = dE e rho                                 -- just a speedup
dE (CSeq e1 e2)         rho  = dE e1 rho `dseq` dE e2 rho
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
dE (CAll e)             rho  = [ fmap mkTup $ sequence $ squash $ dB e rho ]
dE (COne e)             rho  = take 1 $ squash $ dB e rho
dE (CBlock e)           rho  = dB e rho
dE e@(CDef _ _)        _rho  = error $ "error: dE CDef " ++ show e
--dE (CDef _ e)           rho  = dE e rho
dE CFor{}              _rho  = error "error: dE CFor"
dE CLHS                 rho  = [sing $ VEnv $ toListEnv rho]
dE (CIf e1 e2 e3)       rho  =
  let rhos = oneE e1 rho
  in  if isEmpty rhos then
        {-squash $ -} dB e3 rho
      else
        -- XXX what's the right one
        {-squash $ -} unionSetOfSeqs [ squash $ dB e2 rho' | rho' <- rhos ]
        -- squash $ isectSetOfSeqs $ fmap (\ rho' -> squash $ dD e2 rho') rhos
dE e@(CLam Closed i b1 b2@(CBlk [CDef _y _hx@(CApp (CVar _h) (CVar _x))]) b3) rho =
  -- Find a VFcn that is compatible with the lambda
  [ [ f
    | f <- allWs
    , forAll allWs $ \ v ->
        let b23 = CBlk [COne $ appCBlk b2 b3]
                  -- CBlk [CDef _y $ COne $ CBlk [_hx] ] `appCBlk` b3
        in  applyo f v =~= dB (b1 `appCBlk` b23) (extendEnv rho i v)
    ]
  | validFcn e rho
  ]
dE e _ = error $ "dE: not covered: " ++ show e

dseq :: WS -> WS -> WS
dseq ws1 ws2 = concat [ if isEmpty s1 then [] else ws2 | s1 <- ws1 ]


(=~=) :: WS -> WS -> Bool
(s1:ss1) =~= (s2:ss2) = s1 == s2 && ss1 =~= ss2
[]       =~= ss2      = all isEmpty ss2
ss1      =~= []       = all isEmpty ss1

--successful :: [SetX a] -> Bool
--successful = not . null . squash

appCBlk :: CBlk -> CBlk -> CBlk
appCBlk (CBlk es) (CBlk es') = CBlk (es ++ es')

validFcn :: CExp -> Env -> Bool
validFcn e@(CLam _ _i _e1 (CBlk [CDef _y (CApp (CVar _h) (CVar _x))]) _e2) rho =
  let r = validFcn' e rho
  in  r
validFcn _ _ = undefined

validFcn' :: CExp -> Env -> Bool
validFcn' (CLam _ i e1 (CBlk [CDef y (CApp (CVar h) (CVar x))]) e2) rho =
  forAll allWs $ \ vi ->
    let rhoss = dBEnv e1 (extendEnv rho i vi)  -- all the possible ways that e1 can succeed
    in  -- Condition (A), at most one domain match
        case squash rhoss of
          [] -> null $ squash $ applyo vh vi
          [rhos] ->                         -- a single match is ok
            case getSing (fmap (lookupEnv x) rhos) of  -- all xs must have the same value
              Just vx -> validB vx rhos
              Nothing -> error $ "validFcn: assumption failed: x=" ++ show (fmap (lookupEnv x) rhos)
          _      -> False
  where
    vh = lookupEnv h rho
    -- Condition (B) e2 returns the same singleton for every way that e1 succeeds
    -- NB, rhos is not empty
    validB vx rhos =
      case squash (applyo vh vx) of
        [] -> False -- XXX different for <decides>
        [(getSing -> Just vy)] ->  -- h[x] succeeds
          -- Check that all possible rhos produce the same singleton [ {r} ]
          case getSing $ fmap (\ r -> squash $ dB e2 (extendEnv r y vy)) rhos of
            Just [s]  -> isJust $ getSing s
            _ -> False
        r -> error $ "validB: impossible: " ++ show (vh, vx, r)
validFcn' _ _ = error "validFcn"

unVEnv :: Val -> Env
unVEnv (VEnv e) = fromListEnv e
unVEnv _ = undefined

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

squash :: [SetX a] -> [SetX a]
squash = filter (not . isEmpty)

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
oneE :: CBlk -> Env -> SetX Env
--oneE b rho = dbEnv b rho
oneE b rho = [ rho' | rho' <- dX e rho, not $ null $ squash $ dD e rho' ]
  where e = cexpb b

dB :: CBlk -> Env -> WS
dB (CBlk es) rho = unionSetOfSeqs [ dE' es rho' | rho' <- dX (cexpb $ CBlk es) rho ]

dBEnv :: CBlk -> Env -> [SetX Env]
dBEnv b rho = map (fmap unVEnv) $ dB b rho

dE' :: [CExp] -> Env -> WS
dE' [] _rho = undefined
dE' [CDef i e] rho = dE' [CDef i e, CVar i] rho
dE' [e] rho = dE e rho
dE' (CDef i e : es) rho = concat
  [ if isEmpty s then [empty] else dEs es [ extendEnv rho i v | v <- s ]
  | s <- dE e rho ]
dE' (e : es) rho = concat
  [ if isEmpty s then [empty] else dE' es rho
  | s <- dE e rho ]

dEs :: [CExp] -> SetX Env -> WS
dEs es rhos = unionSetOfSeqs' $ fmap (dE' es) rhos

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
           exp1, exp2, {- Open exp3,-} exp4, exp5, {- Open exp6,-} exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, {- BUG (:=) exp22,-}
           exp23, exp24, exp25, exp26, exp27, exp28, exp29,{-WRONG exp30,exp31,-}exp32,
           exp33, exp34, {- SLOW exp35,-}
           -- XXX exp43 looks wrong
           exp36, exp37, exp38, {- SLOW exp39,-} {- UNSURE exp40,-} {- UNSURE exp41,-} exp43, {- UNSURE exp44, -}
           exp45, exp46, exp47, exp48, exp48b, {- UNSURE exp49, exp50, -}
           {- SLOW exp51, exp52, -}
           {- NOT CHECKED exp53,-} {- DODGY circularity exp54,-}
           {- uses exp53 exp55,-} exp56, exp57, {- SLOW exp58,-} exp59, exp60,
           exp61, exp62, exp63, exp64, exp65, exp66, exp67, exp68, exp69,
           exp70, exp71
          ]


main :: IO ()
main = do
  putStrLn "Start"
--  runExamples dP allExps
--  print $ dene $ fun_c cint cint

{-
eint :: Exp
eint = --fun_c ("x" := 0:|||1) "x"
       fun_c ("x" := cint) "x"
f1 = fun_c ("f" := fun_c cint cint) ("f" :@ 1)

fsucc0 = fun_c 0 1
hfsucc0 = fun_c fsucc0 2

fcon = fun_c (0:|||1) 1
hfcon = fun_c fcon 2

fconc = fun_c (0:|1) 1

fid01 :: Exp
fid01 = fun_c ("x" := 0 :||| 1) "x"

fid01c :: Exp
fid01c = fun_c ("x" := 0 :| 1) "x"

hfid01 :: Exp
hfid01 = fun_c ("f" := fid01) ("f" :@ 1)
-}

ds :: Exp -> CExp
ds = redef . syntax "_"

edenSem :: CExp -> IO WS
edenSem e = return (dD e emptyEnv)

edenSemDS :: Exp -> CExp
edenSemDS = redef . syntax "_"
