{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main where
import Data.Ord
--import Control.Arrow(second)
--import Control.Monad hiding (ap)
import qualified Data.Map as M
--import Data.Maybe
--import Data.Generics.Uniplate.Data(universe)
--import Data.String
--import GHC.Stack
import Exp hiding (dI)
import ExpSugar
import ValLA
import SetX
import EnvLA
import CExp
--import Examples hiding ((===))
--import Debug.Trace

dE :: CExp -> Env -> SetX (Labelled W)
dE (CVar "_")      _rho = noLabel <$> allWs
dE (CVar "enum")   _rho = mkSet [ noLabel $ VFcn enum0_1, noLabel $ VFcn enum1_0 ]
dE (CVar x)         rho = sing $ noLabel $ lookupEnv x rho
dE (CInt i)        _rho = sing $ noLabel $ VInt i
dE (CApp e1 e2)     rho = [ labels (labelOf w1 ++ labelOf w2) r
                          | w1 <- dE e1 rho
                          , w2 <- dE e2 rho
                          , r <- applyS (unLabel w1) (unLabel w2)
                          ]
dE (CSeq CExi{} e)  rho = dE e rho
dE (CSeq e1 e2)     rho = [ labels (labelOf w1) w2 | w1 <- dE e1 rho, w2 <- dE e2 rho ]
dE (CEqu e1 e2)     rho = [ labels (labelOf w1 ++ labelOf w2) (noLabel wr)
                          | w1 <- dE e1 rho
                          , w2 <- dE e2 rho
                          , wr <- maybeToSet (isect (unLabel w1) (unLabel w2)) ]
dE (CChoice e1 e2)  rho = union (label L <$> dD e1 rho) (label R <$> dD e2 rho)
dE (CFail)         _rho = empty
--dE (CExi _)        _rho = fmap noLabel allWs
dE (CBlock e)       rho = dD e rho
dE (CUChoice e1 e2) rho = union (dE e1 rho) (dE e2 rho)
dE (CAll e)         rho = (noLabel . VTup) <$> (sequence $ sortLbl $ dD e rho)
                          -- Could make All of non-singletons be wrong
dE e _rho = error $ "unimplemented: " ++ show e

-- D, expression in a scope
dD :: CExp -> Env -> SetX (Labelled W)
dD e rho = [ r | rho' <- dX e rho, r <- dE e rho' ]

dX :: CExp -> Env -> SetX Env
dX e rho = mkSet $
  let exts = sequence $ map (\ x -> map (x,) allWsL) (dI e)
  in  map (foldr (\ (i,v) r -> extend r i v) rho) exts

applyS :: Val -> Val -> SetX (Labelled Val)
applyS f a = maybeToSet $ apply f a

isect :: Val -> Val -> Maybe Val
isect v1 v2 | v1 == v2  = Just v2
isect (VFcn f1) (VFcn f2) = VFcn <$> isectFcn f1 f2
-- XXX Should turn tuples into functions
isect _ _ = Nothing

sortLbl :: SetX (Labelled W) -> [SetX W]
sortLbl = map (fmap unLabel) . toListBy (comparing labelOf)

den :: Exp -> SetX (Labelled W)
den e = dD (syntax "_" e) emptyEnv

main :: IO ()
main = return ()

aa :: Exp
aa = "x" := (1 :| 2)

bb :: Exp
bb = All (1 :| 2)

cc :: Exp
cc = All (1 :|| 2)

dd :: Exp
dd = "x" ::: "any" :>
     "y" ::: "any" :>
     "x" := 1 :| 2 :>
     "x" === "y"

ee :: Exp
ee = "x" ::: "any" :> ("x"===0 :> 1) :| ("x"===1 :> 2) :> "x"===1

enum0_3 :: Fcn
enum0_3 = mkEnumFcn "enum0_3" [(VInt i, VInt i) | i <- [0.. maxVInt-1]]

enum3_0 :: Fcn
enum3_0 = mkEnumFcn "enum3_0" $ reverse [(VInt i, VInt i) | i <- [0.. maxVInt-1]]

enum0_1 :: Fcn
enum0_1 = mkEnumFcn "enum0_1" [(VInt i, VInt i) | i <- [0.. 1]]

enum1_0 :: Fcn
enum1_0 = mkEnumFcn "enum1_0" $ reverse [(VInt i, VInt i) | i <- [0.. 1]]

weird1 :: Fcn
weird1 = Fcn "weird1" $ M.fromList [ (VInt 0, Lbl [L] (VInt 0)), (VInt 1, Lbl [L] (VInt 1))
                                  , (VInt 2, Lbl [R] (VInt 2)), (VInt 3, Lbl [R] (VInt 3))]

weird2 :: Fcn
weird2 = Fcn "weird2" $ M.fromList [ (VInt 0, Lbl [L] (VInt 0)), (VInt 1, Lbl [L,R] (VInt 1))
                                  , (VInt 2, Lbl [L,R] (VInt 2)), (VInt 3, Lbl [R,R] (VInt 3))]

fint :: Fcn
fint = mkFcn "int" [(VInt i, VInt i) | i <- [0.. maxVInt-1]]

den1 :: Exp -> SetX (Labelled W)
den1 e = dD (syntax "_" e) rho1

rho1 :: Env
rho1 = mkEnv [
              ("enum0_3", VFcn enum0_3)
             ,("enum3_0", VFcn enum3_0)
             ,("enum0_1", VFcn enum0_1)
             ,("enum1_0", VFcn enum1_0)
             ,("int", VFcn fint)
             ]

