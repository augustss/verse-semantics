{-# LANGUAGE MonadComprehensions #-}
module PlanA where
import GHC.Stack
import Exp
import ValC
import EnvC
import Map hiding (null)
import SetX

import ExpSugar

dE :: Exp -> Env -> WS
dE (Var "_")          _rho  = [allWs]
dE (Var x)             rho  = [sing $ lookupVar x rho]
dE (Int k)            _rho  = [sing $ VInt k]
dE (Prim p)           _rho  = [sing $ dO p]
dE (App e1 e2)         rho  = dE e1 rho `eapp` dE e2 rho
dE (Equ e1 e2)         rho  = dE e1 rho `isect` dE e2 rho
dE (Seq e1 e2)         rho  = dE e1 rho `dseq` dE e2 rho
dE (Def x e)           rho  = [sing $ lookupVar x rho] `isect` dE e rho
dE (Colon e)           rho  = dE e rho `eapp` [allWs]
dE Fail               _rho  = []
dE (Tup es)            rho  = map (fmap VTup . sequence) $ mapM (\ e -> dE e rho) es
dE (If e1 e2 e3)       rho  =
  let rhos = oneE e1 rho
  in  if isEmpty rhos then
        dD e3 rho
      else
        unionSetOfSeqs [ squash $ dD e2 rho' | rho' <- rhos ]
-- Fun
dE (Choice e1 e2)      rho  = dD e1 rho ++ dD e2 rho
dE (All e)             rho  = [ fmap mkTup $ sequence $ squash $ dD e rho ]
-- For
dE (Where e1 e2)       rho  = [ s1 | s1 <- ws1, s2 <- ws2, not (isEmpty s2) ]
  where ws1 = dE e1 rho; ws2 = dE e2 rho

dE (Def2 x y e)        rho  = [sing $ lookupVar x rho] `isect` [sing $ lookupVar y rho] `isect` dE e rho
-- OfType
dE (UChoice e1 e2)     rho  = [ s1 `union` s2 | s1 <- dD e1 rho, s2 <- dD e2 rho ]
dE (Block e)           rho  = dD e rho
dE (DefI x e)          rho  = [sing $ lookupVar x rho] `isect` dE e rho
dE (Exi _)            _rho  = [sing $ VInt 99999]

dD :: Exp -> Env -> WS
dD e rho = unionSetOfSeqs [ dE e rho' | rho' <- dX e rho ]

dX :: Exp -> Env -> SetX Env
dX e rho = mkSetUnsafe $ dXL e rho
dXL :: Exp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dIExp e)
  in  map (foldr (\ (i,v) r -> extendEnv r i v) rho) exts

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
oneE :: Exp -> Env -> SetX Env
oneE e rho = [ rho' | rho' <- dX e rho, not $ null $ squash $ dD e rho' ]



dseq :: WS -> WS -> WS
dseq ws1 ws2 = concat [ if isEmpty s1 then [] else ws2 | s1 <- ws1 ]

isect :: WS -> WS -> WS
isect ws1 ws2 = [ s1 `intersect` s2 | s1 <- ws1, s2 <- ws2 ]

eapp :: WS -> WS -> WS
eapp ws1 ws2 = [ r | s1 <- ws1, s2 <- ws2, r <- applyFsAs s1 s2 ]

lookupVar :: Ident -> Env -> W
lookupVar x rho =
  case lookupEnvM x rho of
    Just v -> v
    Nothing ->
      case nameFcnM x of
        Just f -> VFcn [f]
        Nothing -> error $ "undefined variable " ++ show x






------------------

applyFsAs :: Ws -> Ws -> WS
applyFsAs fs as | isEmpty fs || isEmpty as = []                  -- avoid empty sets in foldSet
applyFsAs fs as = unionSetOfSeqs [ apply f a | f <- fs, a <- as ]

apply :: W -> W -> WS
apply (VFcn fs) a = map (maybeToSet . appM a) fs
apply _ _ = []

unionSetOfSeqs :: HasCallStack => SetX WS -> WS
unionSetOfSeqs = foldSet unionSeqs

-- Pointwise union of sequences, padding the shorter
unionSeqs :: WS -> WS -> WS
unionSeqs [] ys = ys
unionSeqs xs [] = xs
unionSeqs (x:xs) (y:ys) = union x y : unionSeqs xs ys

squash :: [SetX a] -> [SetX a]
squash = filter (not . isEmpty)

dene :: Exp -> WS
dene e = dD e emptyEnv
