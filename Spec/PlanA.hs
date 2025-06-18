{-# LANGUAGE MonadComprehensions #-}
module PlanA where
import Data.List(dropWhileEnd)
import GHC.Stack
import Exp
import ValC
import EnvC
import Map hiding (null, empty)
import SetX
import Debug.Trace

import ExpSugar

dA :: Exp -> Env -> Val
dA (Var x)             rho  = lookupVar x rho
dA (Int k)            _rho  = VInt k
dA (Prim p)           _rho  = dO p
dA _                  _     = error "dA"

dE :: Exp -> Env -> WS
dE (Var "_")          _rho  = [allWs]
dE e@Var{}             rho  = [sing $ dA e rho]
dE e@Int{}             rho  = [sing $ dA e rho]
dE e@Prim{}            rho  = [sing $ dA e rho]
dE (App e1 e2)         rho  = dE e1 rho `eapp` dE e2 rho
dE (Equ e1 e2)         rho  = dE e1 rho `eequ` dE e2 rho
dE (Seq e1 e2)         rho  = dE e1 rho `eseq` dE e2 rho
dE (Def x e)           rho  = [sing $ dA (Var x) rho] `eequ` dE e rho
dE (Colon e)           rho  = dE e rho `eapp` [allWs]
dE Fail               _rho  = []
dE (Tup es)            rho  = map (fmap VTup . sequence) $ mapM (\ e -> dE e rho) es
dE (If e1 e2 e3)       rho  =
  let rhos = dC e1 rho
  in  if isEmpty rhos then
        dD e3 rho
      else
        unionHat [ dD e2 rho' | rho' <- rhos ]
-- Fun
dE (Choice e1 e2)      rho  = dD e1 rho ++ dD e2 rho
dE (All e)             rho  = [ fmap mkTup $ sequence $ squash $ dD e rho ]
-- For
dE (Where e1 e2)       rho  = [ s1 | s1 <- ws1, s2 <- ws2, not (isEmpty s2) ]
  where ws1 = dE e1 rho; ws2 = dE e2 rho

dE (Def2 x y e)        rho  = [sing $ lookupVar x rho] `eequ` [sing $ lookupVar y rho] `eequ` dE e rho
-- OfType
dE (UChoice e1 e2)     rho  = [ s1 `union` s2 | s1 <- dD e1 rho, s2 <- dD e2 rho ]
dE (Block e)           rho  = dD e rho
dE (DefI x e)          rho  = [sing $ lookupVar x rho] `eequ` dE e rho
dE (Exi _)            _rho  = [sing $ VInt 99999]
dE (Fun q e1 e2)       rho  = trim [
    [ f | f@VFcn{} <- allWs {-mkSetUnsafe [mkVFcn [(VInt 1,VInt 0),(VInt 2,VInt 0)]]-}
        , let rhos = dX e1 rho
        , forAll {-allInts-} allWs $ \ x ->
            forAll rhos $ \ rho' ->
              let d1 = dM e1 x rho' in
--              trace ("try f=" ++ show f ++ ", x=" ++ show x ++ ", d1=" ++ show d1) $
              (not $ null d1) `implies`
              (let fx = apply f x in
               not (null fx) &&
                ( -- trace ("fx=" ++ show fx ++ ", d2=" ++ show (d1 `eseq` dD e2 rho')) $
                fx `elemSeq` (d1 `eseq` dD e2 rho'))  )
        , (q == Closed) `implies`
          (forAll (domV f) $ \ x ->
             exists rhos $ \ rho' ->
               not $ null $ dM e1 x rho')
    ]
  ]

dD :: Exp -> Env -> WS
dD e rho = unionHat [ dE e rho' | rho' <- dX e rho ]

dX :: Exp -> Env -> SetX Env
dX e rho = mkSetUnsafe $ dXL e rho
dXL :: Exp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dIExp e)
  in  map (foldr (\ (i,v) r -> extendEnv r i v) rho) exts

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
dC :: Exp -> Env -> SetX Env
dC e rho = [ rho' | rho' <- dX e rho, not $ null $ squash $ dD e rho' ]



eseq :: WS -> WS -> WS
eseq ws1 ws2 = [ if isEmpty s1 then empty else s2 | s1 <- ws1, s2 <- ws2 ]

eequ :: WS -> WS -> WS
eequ ws1 ws2 = trim [ s1 `intersect` s2 | s1 <- ws1, s2 <- ws2 ]

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

elemSeq :: WS -> WS -> Bool
elemSeq [] _ = True
elemSeq _ [] = False
elemSeq (x:xs) (y:ys) = x `isSubsetOf` y && elemSeq xs ys

implies :: Bool -> Bool -> Bool
implies True b = b
implies _    _ = True

trim :: WS -> WS
trim = dropWhileEnd isEmpty

------------------

-- XXX temp
dM :: Exp -> Val -> Env -> WS
dM e u rho = [sing u] `eequ` dE e rho

------------------

applyFsAs :: SetX Val -> SetX Val -> WS
applyFsAs fs as = unionHat [ apply f a | f <- fs, a <- as ]

apply :: W -> W -> WS
apply (VFcn fs) a = trim $ map (maybeToSet . appM a) fs
apply _ _ = []

unionHat :: HasCallStack => SetX WS -> WS
unionHat s | isEmpty s = []
           | otherwise = foldSet unionSeqs s

-- Pointwise union of sequences, padding the shorter
unionSeqs :: WS -> WS -> WS
unionSeqs [] ys = ys
unionSeqs xs [] = xs
unionSeqs (x:xs) (y:ys) = union x y : unionSeqs xs ys

squash :: [SetX a] -> [SetX a]
squash = filter (not . isEmpty)

dene :: Exp -> WS
dene e = dD e emptyEnv
