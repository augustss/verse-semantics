module Env where
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Val
import Exp
import Set

--------------------
---- Environment

type W = Val
type WS = Set W

type Env = M.Map Ident Val

lookupEnv :: Ident -> Env -> W
lookupEnv x rho = fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $ M.lookup x rho

extend :: Env -> Ident -> W -> Env
extend rho i w = M.insert i w rho

-- Initial environment
rho0 :: Env
rho0 = M.fromList $
  [ (n, dO o) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", VFcn fsucc), ("pred", VFcn fpred) ] ++
  [ ("false", VTup []) ]

--------------------
---- "Universal" set of values
-- This is a carefully selected set of values to make
-- the examples work.

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allWs :: WS
allWs = S.fromList $
  nonFcn ++
  [ dO o | o <- [Oint, Ogt, Oadd] ] ++
  map VFcn [ id0, fid1, id01, f01, const0, const1, const2, const3, fsucc,
             fsuccsucc, fpred, succ0, comp, ho1, ho2, ho3, ho4, ho5, ho6, ho7 ]
  where
    nonFcn =
      allInts ++
      --[VTup [x, y] | x <- allInts, y <- allInts]
      [VTup [VInt 2, VInt 1]]
    id0 = mkFcn "id0" [(VInt 0, VInt 0)]
    id01 = mkFcn "id01" [(VInt 0, VInt 0), (VInt 1, VInt 1)]
    f01 = mkFcn "f01" [(VInt 0, VInt 0), (VInt 1, VInt 2)]
    const0 = mkFcn "const0" [(x, VInt 0) | x <- allInts]
    const1 = mkFcn "const1" [(x, VInt 1) | x <- allInts]
    const2 = mkFcn "const2" [(x, VInt 2) | x <- allInts]
    const3 = mkFcn "const3" [(x, VInt 3) | x <- allInts]
    succ0 = mkFcn "succ0" [(VInt 0, VInt 1)]
    comp = mkFcn "comparable" [(w, w) | w <- nonFcn ]
    -- The function that accepts f:int->int as an argument and returns f[1]
    ho1 = mkFcn "ho1" [(VFcn fsucc, VInt 2), (VFcn fpred, VInt 0), (VFcn fint, VInt 1),
                       (VFcn fsuccsucc, VInt 3), --(VFcn comp, VInt 1),
                       (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                      ]
    ho2 = mkFcn "ho2" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                       (VFcn fsuccsucc, VInt 0), --(VFcn comp, VInt 2),
                       (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                      ]
    ho3 = mkFcn "ho3" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                       (VFcn fsuccsucc, VInt 0), --(VFcn comp, VInt 2),
                       (VFcn const0, VInt 1), (VFcn const1, VInt 2), (VFcn const2, VInt 3), (VFcn const3, VInt 0)
                      ]
    ho4 = mkFcn "ho4" $ [(VFcn fsucc, VInt 2), (VFcn succ0, VInt 2), (VFcn const1, VInt 2)]
                      ++ [ (VTup [VInt 1,i], VInt 2) | i <- allInts ]
    ho5 = mkFcn "ho5" [(VFcn succ0, VInt 2)]
    ho6 = mkFcn "ho6" [(VInt 0, VFcn fint)]
    ho7 = mkFcn "ho7" [(VInt 0, VFcn comp)]

fid1 :: Fcn Val Val
fid1 = mkFcn "id1" [(VInt 1, VInt 1)]

fint :: Fcn Val Val
fint = mkFcn "int" [(x, x) | x <- allInts ]

fsucc :: Fcn Val Val
fsucc = mkFcn "succ" [(x, vadd x (VInt 1)) | x <- allInts ]

fsuccsucc :: Fcn Val Val
fsuccsucc = mkFcn "succsucc" [(x, vadd x (VInt 2)) | x <- allInts ]

fpred :: Fcn Val Val
fpred = mkFcn "pred" [(x, vadd x (VInt 3)) | x <- allInts ]

getW :: String -> W
getW s = ([ w | w <- unSet allWs, show w == s ] ++ [error $ "undefined " ++ s]) !! 0

--------------------
---- Primitive functions

dO :: Op -> W
dO Oint = VFcn $ mkFcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = VFcn $ mkFcn "gt"  [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = VFcn $ mkFcn "add" [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

