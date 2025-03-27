{-# LANGUAGE MonadComprehensions #-}
module EnvC(
  W, Ws, WS,
  Env, lookupEnv, extendEnv, emptyEnv,
  rho0, allWs, allFcns,
  allWsL, allInts,
  findFcn,
  dO,
  ) where
import Data.List
import Data.Maybe
import Exp
import qualified Map as M
import SetX
import ValC

--------------------
---- Environment

type W = Val
type Ws = SetX W
type WS = [Ws]

newtype Env = Env { unEnv :: M.Map Ident Val }
  deriving (Eq, Ord)

instance Show Env where
  show (Env m) = "[|" ++ intercalate "," (map f (M.toList m)) ++ "|]"
    where f (i,v) = i ++ "->" ++ show v

lookupEnv :: Ident -> Env -> W
lookupEnv x rho = fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $ M.lookup x $ unEnv rho

extendEnv :: Env -> Ident -> W -> Env
extendEnv rho i w = Env $ M.insert i w $ unEnv rho

-- Initial environment
rho0 :: Env
rho0 = Env $ M.fromList $
  [ (n, dO o) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", vFcn fsucc), ("pred", vFcn fpred) ] ++
  [ ("false", VTup []) ]

emptyEnv :: Env
emptyEnv = Env M.empty

--------------------
---- "Universal" set of values
-- This is a carefully selected set of values to make
-- the examples work.

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allWs :: Ws
allWs = mkSetUnsafe allWsL
allWsL :: [W]
allWsL =
  nonFcn
--   ++ [ dO o | o <- [Oint, Ogt, Oadd] ]
   ++ map vFcn [ id0, fid1, id2, id3 , id01, f01, const0, const1, const2, const3, fsucc, -- succMod3,
             fsuccsucc, fpred, succ0, comp, ho1, ho2, ho3, ho4, ho5, ho6, ho7, id123, f0t12,
             const_12_1, id12 ]
  where
    nonFcn =
      allInts
      ++ [VTup [x, y] | x <- allInts, y <- allInts]
    id0 = mkFcn "id0" [(VInt 0, VInt 0)]
    id2 = mkFcn "id2" [(VInt 2, VInt 2)]
    id3 = mkFcn "id3" [(VInt 3, VInt 3)]
    id01 = mkFcn "id01" [(VInt 0, VInt 0), (VInt 1, VInt 1)]
    id123 = mkFcn "id123" [(VInt 1, VInt 1), (VInt 2, VInt 2), (VInt 3, VInt 3)]
    id12 = mkFcn "id123" [(VInt 1, VInt 1), (VInt 2, VInt 2)]
    f01 = mkFcn "f01" [(VInt 0, VInt 0), (VInt 1, VInt 2)]
    const0 = mkFcn "const0" [(x, VInt 0) | x <- allInts]
    const1 = mkFcn "const1" [(x, VInt 1) | x <- allInts]
    const2 = mkFcn "const2" [(x, VInt 2) | x <- allInts]
    const3 = mkFcn "const3" [(x, VInt 3) | x <- allInts]
    const_12_1 = mkFcn "const_12_1" [(VInt 1, VInt 1), (VInt 2, VInt 1)]
    succ0 = mkFcn "succ0" [(VInt 0, VInt 1)]
    comp = mkFcn "comparable" [(w, w) | w <- nonFcn ]
    -- The function that accepts f:int->int as an argument and returns f[1]
    ho1 = mkFcn "ho1" [(vFcn fsucc, VInt 2), (vFcn fpred, VInt 0), (vFcn fint, VInt 1),
                       (vFcn fsuccsucc, VInt 3), --(vFcn comp, VInt 1),
                       (vFcn const0, VInt 0), (vFcn const1, VInt 1), (vFcn const2, VInt 2), (vFcn const3, VInt 3)
                      ]
    ho2 = mkFcn "ho2" [(vFcn fsucc, VInt 3), (vFcn fpred, VInt 1), (vFcn fint, VInt 2),
                       (vFcn fsuccsucc, VInt 0), --(vFcn comp, VInt 2),
                       (vFcn const0, VInt 0), (vFcn const1, VInt 1), (vFcn const2, VInt 2), (vFcn const3, VInt 3)
                      ]
    ho3 = mkFcn "ho3" [(vFcn fsucc, VInt 3), (vFcn fpred, VInt 1), (vFcn fint, VInt 2),
                       (vFcn fsuccsucc, VInt 0), --(vFcn comp, VInt 2),
                       (vFcn const0, VInt 1), (vFcn const1, VInt 2), (vFcn const2, VInt 3), (vFcn const3, VInt 0)
                      ]
    ho4 = mkFcn "ho4" $ [(vFcn fsucc, VInt 2), (vFcn succ0, VInt 2), (vFcn const1, VInt 2)]
                      ++ [ (VTup [VInt 1,i], VInt 2) | i <- allInts ]
    ho5 = mkFcn "ho5" [(vFcn succ0, VInt 2)]
    ho6 = mkFcn "ho6" [(VInt 0, vFcn fint)]
    ho7 = mkFcn "ho7" [(VInt 0, vFcn comp)]
    f0t12 = mkFcn "f0t12" [(VInt 0, VTup [VInt 1, VInt 2])]
    succMod3 = mkFcn "succMod3" [(VInt 0, VInt 1),
                                 (VInt 1, VInt 2),
                                 (VInt 2, VInt 0),
                                 (VInt 3, VInt 1)]
{-
    gt0 = mkFcn "gt0" [(VTup [VInt 1, VInt 0], VInt 1),
                       (VTup [VInt 2, VInt 0], VInt 2),
                       (VTup [VInt 3, VInt 0], VInt 3)]
-}

allFcns :: SetX Fcn
allFcns = mkSetUnsafe allFcnsL

allFcnsL :: [Fcn]
allFcnsL = [ f | VFcn fs <- allWsL, f <- fs ]

fid1 :: Fcn
fid1 = mkFcn "id1" [(VInt 1, VInt 1)]

fint :: Fcn
fint = mkFcn "int" [(x, x) | x <- allInts ]

fsucc :: Fcn
fsucc = mkFcn "succ" [(x, vadd x (VInt 1)) | x <- allInts ]

fsuccsucc :: Fcn
fsuccsucc = mkFcn "succsucc" [(x, vadd x (VInt 2)) | x <- allInts ]

fpred :: Fcn
fpred = mkFcn "pred" [(x, vadd x (VInt 3)) | x <- allInts ]

findFcn :: [(Val, Val)] -> Fcn
findFcn fcn =
  let fm = M.fromList fcn in
  case find (eqFcnMap fm) allFcnsL of
    Just f -> f
    Nothing -> --error $ "Missing function " ++ show fcn
      mkFcn name fcn
      where name = "{" ++ intercalate "," (map showF fcn) ++ "}"
            showF (a,b) = show a ++ "\x21a6" ++ show b

{-
getW :: String -> W
getW s = ([ w | w <- allWsL, show w == s ] ++ [error $ "undefined " ++ s]) !! 0
-}
--------------------
---- Primitive functions

dO :: Op -> W
dO Oint = vFcn $ mkFcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = vFcn $ mkFcn "gt"  [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = vFcn $ mkFcn "add" [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

