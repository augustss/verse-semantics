{-# LANGUAGE CPP #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main {-PlanA-} where
import Control.Monad(zipWithM)
import Data.List(dropWhileEnd)
import GHC.Stack
import Exp
import ValC
import EnvC
import Map hiding (null, empty)
import SetX
import Debug.Trace

import ExpSugar

type VAL = SetX Val

dA :: Exp -> Env -> Val
dA (Var x)             rho  = lookupVar x rho
dA (Int k)            _rho  = VInt k
dA (Prim p)           _rho  = dO p
dA _                  _     = error "dA"

dE :: Exp -> Env -> [VAL]
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
        unionsHat [ dD e2 rho' | rho' <- rhos ]
-- Fun
dE (Choice e1 e2)      rho  = dD e1 rho ++ dD e2 rho
dE (All e)             rho  = [ fmap mkTup $ sequence $ squash $ dD e rho ]
-- For
dE (Where e1 e2)       rho  = [ if isEmpty s2 then empty else s1
                              | s1 <- dE e1 rho, s2 <- dE e2 rho ]
dE (Def2 x y e)        rho  = [sing $ lookupVar x rho] `eequ` [sing $ lookupVar y rho] `eequ` dE e rho
-- OfType
dE (UChoice e1 e2)     rho  = unionHat (dD e1 rho) (dD e2 rho)
dE (Block e)           rho  = dD e rho
dE (DefI x e)          rho  = [sing $ lookupVar x rho] `eequ` dE e rho
dE (Exi _)            _rho  = [sing $ VInt 99999]
dE (Fun q e1 e2)       rho  = {-trim-} [
    [ f | f@VFcn{} <- allWs {-mkSetUnsafe [mkVFcn [(VInt 1,VInt 0),(VInt 2,VInt 0)]]-}
        , let rhos = dX e1 rho
        , forAll allWs {-allInts-} $ \ x ->
            forAll rhos $ \ rho' ->
              let d1 = dM e1 x rho' in
              --trace ("try f=" ++ show f ++ ", x=" ++ show x ++ ", d1=" ++ show d1) $
              notFail d1 `implies`
              (let fx = apply f x in
               notFail fx &&
                ( --trace ("fx=" ++ show fx ++ ", d2=" ++ show (d1 `eseq` dD e2 rho')) $
                fx `elemSeq` (d1 `eseq` collapse (dD e2 rho')))  )
        , (q == Closed) `implies`
          (forAll (domV f) $ \ x ->
             exists rhos $ \ rho' ->
               notFail $ dM e1 x rho')
    ]
  ]
dE e _ = error $ "dE: " ++ show e

dD :: Exp -> Env -> [VAL]
dD e rho = unionsHat [ dE e rho' | rho' <- dX e rho ]

dX :: Exp -> Env -> SetX Env
dX e rho = mkSetUnsafe $ dXL e rho
dXL :: Exp -> Env -> [Env]
dXL e rho = 
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dIExp e)
  in  map (foldr (\ (i,v) r -> extendEnv r i v) rho) exts

-- Evaluate e with all possible local environments.
-- Return the environments that result in a non-empty sequence
dC :: Exp -> Env -> SetX Env
dC e rho = [ rho' | rho' <- dX e rho, notFail $ dD e rho' ]



eseq :: [VAL] -> [VAL] -> [VAL]
eseq ws1 ws2 = [ if isEmpty s1 then empty else s2 | s1 <- ws1, s2 <- ws2 ]

eequ :: [VAL] -> [VAL] -> [VAL]
eequ ws1 ws2 = {-trim-} [ s1 `intersect` s2 | s1 <- ws1, s2 <- ws2 ]

eapp :: [VAL] -> [VAL] -> [VAL]
eapp ws1 ws2 = [ r | s1 <- ws1, s2 <- ws2, r <- applyFsAs s1 s2 ]

lookupVar :: Ident -> Env -> W
lookupVar x rho =
  case lookupEnvM x rho of
    Just v -> v
    Nothing ->
      case nameFcnM x of
        Just f -> VFcn [f]
        Nothing -> error $ "undefined variable " ++ show x

elemSeq :: [VAL] -> [VAL] -> Bool
elemSeq [] _ = True
elemSeq _ [] = False
elemSeq (x:xs) (y:ys) = x `isSubsetOf` y && elemSeq xs ys

implies :: Bool -> Bool -> Bool
implies True b = b
implies _    _ = True

trim :: [VAL] -> [VAL]
trim = dropWhileEnd isEmpty

(=<=) :: Val -> [VAL] -> [VAL]
u =<= s = [sing u] `eequ` s

notFail :: [VAL] -> Bool
notFail = not . all isEmpty

collapse :: [VAL] -> [VAL]
#if 0
collapse ss = ss
#else
collapse [] = []
collapse ss = [ foldr1 union ss ]
#endif

samePos :: [VAL] -> [VAL] -> Bool
samePos []         [] = True
samePos (x:xs) (y:ys) = isEmpty x == isEmpty y && samePos xs ys
samePos _           _ = False

------------------

dM :: Exp -> Val -> Env -> [VAL]
#if 1
-- XXX temp
dM e u rho =
  dM' e u rho
{-
  let r = u =<= dE e rho
  in  if r == dM' e u rho then r else
      error $ show (e, u, r, dM' e u rho)
-}

dM' :: Exp -> Val -> Env -> [VAL]
dM' (Var "_")          u _rho  = [sing u]
dM' e@Var{}            u  rho  = u =<= [sing $ dA e rho]
dM' e@Int{}            u  rho  = u =<= [sing $ dA e rho]
dM' e@Prim{}           u  rho  = u =<= [sing $ dA e rho]
dM' (App e1 e2)        u  rho  = u =<= (dE e1 rho `eapp` dE e2 rho)
dM' (Equ e1 e2)        u  rho  = dM' e1 u rho `eequ` dM' e2 u rho
dM' (Seq e1 e2)        u  rho  = dE e1 rho `eseq` dM' e2 u rho
dM' (Def x e)          u  rho  = [sing $ dA (Var x) rho] `eequ` dM' e u rho
dM' (Colon e)          u  rho  = dE e rho `eapp` [sing u]
dM' Fail               _ _rho  = []
dM' (Tup es)           u  rho  =
  case u of
    VTup us | length us == length es ->
      map (fmap VTup . sequence) $ zipWithM (\ e u -> dM' e u rho) es us
    _ -> []
dM' (If e1 e2 e3)      u  rho  =
  let rhos = dC e1 rho
  in  if isEmpty rhos then
        dM' e3 u rho
      else
        unionsHat [ dL e2 u rho' | rho' <- rhos ]
-- Fun
dM' (Choice e1 e2)     u  rho  = dL e1 u rho ++ dL e2 u rho
dM' (All e)            u  rho  = u =<= [ fmap mkTup $ sequence $ squash $ dD e rho ]
-- For
dM' (Where e1 e2)      u  rho  = [ if isEmpty s2 then empty else s1
                                 | s1 <- dM e1 u rho, s2 <- dE e2 rho ]
dM' (Def2 x y e)       u  rho | lookupVar x rho == u
                              = [sing $ lookupVar y rho] `eequ` dM' e u rho
                             | otherwise = []
-- OfType
dM' (UChoice e1 e2)    u  rho  = unionHat (dL e1 u rho) (dL e2 u rho)
dM' (Block e)          u  rho  = dL e u rho
dM' (DefI x e)         u  rho  = [sing $ lookupVar x rho] `eequ` dM' e u rho
dM' (Exi _)            u _rho  = u =<= [sing $ VInt 99999]
dM' (Fun q e1 e2)      g  rho  = [
    [ f | f@VFcn{} <- allWs {-mkSetUnsafe [mkVFcn [(VInt 1,VInt 0),(VInt 2,VInt 0)]]-}
        , VFcn{} <- sing g
        , let rhos = dX e1 rho
        , forAll {-allInts-} allWs $ \ x ->
            forAll rhos $ \ rho' ->
              let d1 = dM' e1 x rho' in
              trace ("try f=" ++ show f ++ ", x=" ++ show x ++ ", d1=" ++ show d1 ++ ", g=" ++ show g) $
              notFail d1 `implies`
                (flip all d1 $ \ xs1 ->
                  forAll xs1 $ \ x1 ->
                  let gx1 = apply g x1 in
                  case squash gx1 of
                    [getSing -> Just y] ->
                      let fx = apply f x in
                      fx `elemSeq` (d1 `eseq` collapse (dM e2 y rho'))  -- XXX?
                      && samePos fx gx1
                    _ -> False
                )

        , (q == Closed) `implies`
          (forAll (domV f) $ \ x ->
             exists rhos $ \ rho' ->
               notFail $ dM' e1 x rho')
    ]
  ]

dM' e _ _ = error $ "dM': " ++ show e

dL :: Exp -> Val -> Env -> [VAL]
dL e u rho = unionsHat [ dM' e u rho' | rho' <- dX e rho ]
#endif

------------------

applyFsAs :: SetX Val -> SetX Val -> [VAL]
applyFsAs fs as = unionsHat [ apply f a | f <- fs, a <- as ]

apply :: W -> W -> [VAL]
apply (VFcn fs) a = trim $ map (maybeToSet . appM a) fs
apply _ _ = []

unionsHat :: HasCallStack => SetX [VAL] -> [VAL]
unionsHat s | isEmpty s = []
            | otherwise = foldSet unionHat s

-- Pointwise union of sequences, padding the shorter
unionHat :: [VAL] -> [VAL] -> [VAL]
unionHat [] ys = ys
unionHat xs [] = xs
unionHat (x:xs) (y:ys) = union x y : unionHat xs ys

squash :: [SetX a] -> [SetX a]
squash = filter (not . isEmpty)

dene :: Exp -> [VAL]
dene e = dD e emptyEnv

---- Can't trim M[e1|e2]
---- Get rid of trim?

t1or2 :: Exp
t1or2 = 1 :||| 2
f1,f2,f3,f4 :: Exp
-- f1 semantics withot and with collapse
f1 = fun_c ("x":=t1or2)       (1:|  2)   -- [{F[{},{1↦2,2↦2}], F[{1↦1},{2↦2}], F[{2↦1},{1↦2}], F[{1↦1,2↦1}]}]
                                         -- [{F[{1↦1,2↦1}], F[{1↦1,2↦2}], F[{1↦2,2↦1}], F[{1↦2,2↦2}]}]
f2 = fun_c ("x":=t1or2)       (1:|||2)   -- [{F[{1↦1,2↦1}], F[{1↦1,2↦2}], F[{1↦2,2↦1}], F[{1↦2,2↦2}]}]
f3 = fun_c ("x":=t1or2)("x"===(1:|  2))  -- [{F[{1↦1},{2↦2}]}]
f4 = fun_c ("x":=t1or2)("x"===(1:|||2))  -- [{F[{1↦1,2↦2}]}]

main :: IO ()
main = do
  --mapM_ (print . dene) [f1,f2,f3,f4]
  print $ dene $ fun_c (fun_c 0 1) 2


