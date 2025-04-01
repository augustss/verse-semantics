{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE MonadComprehensions #-}
module EnvC(
  W, Ws, WS,
  Env, lookupEnv, extendEnv, emptyEnv, toListEnv, fromListEnv,
  rho0, allWs,
  allWsL, allInts, allFcns,
  dO,
  maxVInt, vadd,
  mkFcn, mkVFcn, noFcn,
  ) where
import Data.Array
import Data.List hiding (intersect)
import Data.Maybe
import Exp
import qualified Map as M
import qualified Data.Map as DM
import SetX
import ValC

type W = Val
type Ws = SetX W
type WS = [Ws]

--------------------

maxVInt :: Integer
maxVInt = 3

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

--------------------
---- Primitive functions

dO :: Op -> W
dO Oint = vFcn $ mkFcn [ (x, x) | x <- allInts ]
dO Ogt  = vFcn $ mkFcn [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = vFcn $ mkFcn [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

--------------------
---- Functions

mkVFcn :: MappingV -> Val
mkVFcn = vFcn . mkFcn

allFcns :: [Fcn]
allFcns = setNos $ intFcns ++ pairFcns ++ hoFcns

allFcnsA :: Array Int Fcn
allFcnsA = array (0, length allFcns - 1) [ (n, f) | f@(Fcn n _ _) <- allFcns ]

allFcnsM :: DM.Map Mapping Fcn
allFcnsM = DM.fromList [ (xys, f) | f@(Fcn _ _ xys) <- allFcns ]

mkFcn :: MappingV -> Fcn
mkFcn xys =
  let xys' = mkMapping xys in
  case DM.lookup xys' allFcnsM of
    Nothing -> error $ "Function not in allFcns: " ++ showMapping' xys'
    Just f  -> f

noFcn :: Int -> Fcn
noFcn i = allFcnsA ! i

(↦) :: a -> b -> (a, b)
(↦) = (,)

-- Generate all function with the given domain
-- and a subset of the range.
-- mkAllFcn [0,1,2] [2,3] =
--  [ Fcn [(0, 2), (1, 2), (2, 2)]
--  , Fcn [(0, 2), (1, 2), (2, 3)]
--  , Fcn [(0, 2), (1, 3), (2, 2)]
--  , Fcn [(0, 2), (1, 3), (2, 3)]
--  , Fcn [(0, 3), (1, 2), (2, 2)]
--  , Fcn [(0, 3), (1, 2), (2, 3)]
--  , Fcn [(0, 3), (1, 3), (2, 2)]
--  , Fcn [(0, 3), (1, 3), (2, 3)]
--  ]
mkDomRng :: [Val] -> [Val] -> [Mapping]
mkDomRng fdom frng =
  let rs = sequence $ replicate (length fdom) frng
      fs = map (zip fdom) rs
  in  map mkMapping fs

intFcns :: [Mapping]
intFcns =
  let ss = subsequences allInts
      fs = concat [ mkDomRng d allInts | d <- ss ]
  in  fs

pairFcns :: [Mapping]
pairFcns = intToPairFcns ++ pairToIntFcns

pairToIntFcns :: [Mapping]
pairToIntFcns = -- [ mkMapping [(VTup [x,y], z) | x <- allInts, y <- allInts ] | z <- allInts ]
  [ mkMapping madd ]  -- just one for now

madd :: MappingV
madd = [ (VTup [x,y], vadd x y) | x <- allInts, y <- allInts ]

intToPairFcns :: [Mapping]
intToPairFcns = [ mkMapping [(z, VTup [x,y])] | x <- allInts, y <- allInts, z <- allInts ]

hoFcns :: [Mapping]
hoFcns = hos

-- Set name field for some known functions
setNos :: [Mapping] -> [Fcn]
setNos = zipWith set [0..]
  where set i m = Fcn i (lookup m knownFcns) m

knownFcns :: [(Mapping, String)]
knownFcns =
  [ (mkMapping m, s) | (m, s) <-
    [ (mint, "int")
    , (madd, "add")
    ]
  ]

type MappingV = [(Val, Val)]

emptym :: MappingV
emptym = []

mint :: MappingV
mint = [ (x, x) | x <- allInts ]

msucc :: MappingV
msucc = [ (x, vadd x (VInt 1)) | x <- allInts ]

mpred :: MappingV
mpred = [ (x, vadd x (last allInts)) | x <- allInts ]

--------------------
---- Environment

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
rho0 = fromListEnv $
  [ (n, dO o) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", mkVFcn msucc), ("pred", mkVFcn mpred) ] ++
  [ ("false", mkVFcn emptym) ]

emptyEnv :: Env
emptyEnv = Env M.empty

toListEnv :: Env -> [(String, Val)]
toListEnv (Env m) = M.toList m

fromListEnv :: [(String, Val)] -> Env
fromListEnv = Env . M.fromList

--------------------
---- "Universal" set of values
-- This is a carefully selected set of values to make
-- the examples work.

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allChoice2IntFcns :: [Val]
allChoice2IntFcns = map VFcn $ pick2 intVFcns
  where pick2 fs = [ [f1, f2]
                   | f1 <- fs
                   , let d1 = domFcn f1
                   , not (isEmpty d1)
                   , f2 <- fs
                   , let d2 = domFcn f2
                   , not (isEmpty d2)
                   , isEmpty (d1 `intersect` d2)
                   ]
        intVFcns = filter intDom allFcns
        intDom f = domFcn f `isSubsetOf` mkSet allInts

allWs :: Ws
allWs = mkSetUnsafe allWsL

allWsL :: [W]
allWsL =
  let
    nonFcn = allInts
--      ++ [VTup [x, y] | x <- allInts, y <- allInts]
  in nonFcn ++ map vFcn allFcns ++ allChoice2IntFcns

--------------

-- fun_c(fun_c(0){1}){2}
hos :: [Mapping]
hos = map mkMapping
 [ [ F[noFcn 2{-={0↦1}-}] ↦ VInt 2 ]
 , [ F[noFcn 15{-={0↦2,1↦2}-}] ↦ VInt 2 ]
 , [ F[noFcn 15{-={0↦2,1↦2}-}] ↦ VInt 2
   , F[noFcn 61{-={0↦2,1↦2,2↦0}-}] ↦ VInt 2
   , F[noFcn 62{-={0↦2,1↦2,2↦1}-}] ↦ VInt 2
   , F[noFcn 63{-={0↦2,1↦2,2↦2}-}] ↦ VInt 2
   ]
 ]
