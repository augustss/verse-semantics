{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main where
import Control.Arrow(first)
import Control.Applicative
import Control.Monad
--import Data.Maybe
import Exp hiding (dI)
import ValK
import SetX
import EnvK
import CExp
import Examples hiding ((===))
import Debug.Trace

-------------------------------------------

newtype Sem a = Sem { unSem :: SetX [a] }
  deriving Show

instance Functor Sem where
  fmap f (Sem s) = Sem (fmap (fmap f) s)

instance Applicative Sem where
  pure x = Sem (sing [x])
  (<*>) = ap

instance Monad Sem where
  s >>= k = joinSem (fmap k s)

instance Alternative Sem where
  empty = Sem empty -- $ sing []
  Sem x1 <|> Sem x2 = Sem [ s1 ++ s2 | s1 <- x1, s2 <- x2 ]

joinSem :: Sem (Sem a) -> Sem a
joinSem (Sem ss) = Sem $ joinSetSet (fmap (fmap unSem) ss)

joinSetSet :: SetX [SetX [a]] -> SetX [a]
joinSetSet set = [ s2 | s1 <- set, s2 <- flatten s1 ]

flatten :: [SetX [a]] -> SetX [a]
flatten [] = sing []
flatten (s:ss) = [ s1 ++ s2 | s1 <- s, s2 <- flatten ss ]

-------------------------------------------

type WS = Sem (Env, W)

dE :: CExp -> Sem (Env, W)
dE (CVar "_") = Sem [ [(emptyEnv,    v)] | v <- allWs ]
dE (CVar x)   = Sem [ [(singEnv x v, v)] | v <- allWs ]
dE (CInt k)   = pure (emptyEnv, VInt k)
dE (CPrim p)  = Sem $ mkSet [ [(emptyEnv, v)] | v <- dO p ]
dE (CTup es)  = fmapM (comb VTup) $ traverse dE es
dE (CApp f x) = Sem $ mkSet [ [(mkEnv [(f, fv), (x, xv)], yv)]
                            | fv <- allWsL
                            , Just xys <- [enumFcn fv]
                            , (xv, yv) <- xys
                            ]
--dE (COfType e1 e2) = undefined
{-
dE (CEqu e1 e2) = do
  (rho1, v1) <- dE e1
  (rho2, v2) <- dE e2
--  traceM $ "Equ 1 " ++ show (rho1, v1, rho2, v2)
  guard (v1 == v2)
--  traceM $ "Equ 2 " ++ show (rho1, rho2, combEnv rho1 rho2)
  rho <- combEnv rho1 rho2
--  traceM $ "Equ 3 " ++ show (rho, v2)
  pure (rho, v2)
-}
dE (CEqu (CVar x) e2) = Sem
  [ [ (rho', v2) | (rho2, v2) <- s2, Just rho' <- [unifyEnv (singEnv x v2) rho2] ]
  | s2 <- unSem (dE e2)
  ]
  
dE (CExi _ `CSeq` e) = dE e
dE (CSeq e1 e2) = Sem
  [ prod s1 s2
  | s1 <- unSem (dE e1)
  , s2 <- unSem (dE e2)
  ]
  where prod :: [(Env, W)] -> [(Env, W)] -> [(Env, W)]
        prod s1 s2 = [ (EPair rho1 rho2, v2)
                     | (rho1, _v1) <- s1
                     , (rho2, v2) <- s2
--                     , Just rho' <- [unifyEnv rho1 rho2]
                     ]
dE (CWhere e1 e2) = do
  (rho1, v1) <- dE e1
  (rho2, _v2) <- dE e2
  rho <- combEnv rho1 rho2
  pure (rho, v1)
dE (CExi _) = pure (emptyEnv, VInt 99999)
dE CFail = Sem $ sing []
--dE (CIf e1 e2 e3) = undefined
--dE (CLam q i e1 e2 me3) = undefined
dE (CChoice e1 e2) = dE e1 <|> dE e2
--dE (CUChoice e1 e2) = union (dE e1) (dE e2)
--dE (CAll e) rho = undefined
dE (CBlock e) = Sem
  [ [ (remVars is rho, v) | (rho, v) <- s ]
  | s <- unSem (dE e)
  ] where is = dI e
dE e = error $ "unimplemented " ++ show e

dD :: CExp -> WS
dD e = fmap (first (remVars xs)) $ dE e
  where xs = dI e

combEnv :: Env -> Env -> Sem Env   -- the list is always either empty or a singleton
combEnv r1 r2 =
  case unifyEnv r1 r2 of
    Nothing -> Sem empty
    Just x  -> pure x

comb :: ([a] -> a) -> [(Env, a)] -> Maybe (Env, a)
comb f xs = do
  let (rhos, ws) = unzip xs
  rho <- unifyEnvs rhos
  pure (rho, f ws)

fmapM :: (a -> Maybe b) -> Sem a -> Sem b
fmapM f (Sem x) = Sem (SetX.mapMaybe (mapM f) x)

unifyEnvs :: [Env] -> Maybe Env
unifyEnvs [] = Just emptyEnv
unifyEnvs [rho] = Just rho
unifyEnvs (rho:rhos) = unifyEnvs rhos >>= unifyEnv rho

-------------------------------------------

den :: Exp -> WS
den e = dD ({-redef $-} syntax "_" e)

dene :: Exp -> WS
dene e = dD ({-redef $-} syntax "_" e)

dP :: Exp -> RVal
dP e =
  case toList $ unSem $ den e of
    [[(_, v)]] -> RVal v
--        | otherwise       -> Wrong $ showListWith showPretty (toList s)
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

aa = (Var "x" `Equ` Int 1) `Choice` (Var "x" `Equ` Int 2)
bb = Var "x" `Equ` Var "y"
dx = Def "x" $ Colon $ Var "x"
dy = Def "y" $ Colon $ Var "y"
