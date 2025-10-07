{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MonadComprehensions #-}
#define HAS_HO 1
#define HAS_CHOICE 1
module EnvC(
  W, Ws, WS,
  Env, lookupEnv, lookupEnvM, extendEnv, emptyEnv, toListEnv, fromListEnv,
  rho0, allWs,
  allWsL, allInts, allIntsL, allFcns, allChoiceFcns, allFcnsL,
  dO,
  maxVInt, vadd,
  mkFcn, mkVFcn, noFcn, nameFcn, nameFcnM,
  mkIntFcn,
  MappingV,
  ) where
import Data.Array
import Data.List hiding (intersect)
import Data.Maybe
import Exp
import qualified Map as M
import qualified Data.Map as DM
import GHC.Stack
import SetX
import ValC

partitions :: [a] -> [([a],[a])]
partitions =
   foldr
      (\x -> concatMap (\(lxs,rxs) -> [(x:lxs,rxs), (lxs,x:rxs)]))
      [([],[])]

--------------------

type W = Val
type Ws = SetX W
type WS = [Ws]

--------------------

maxVInt :: Integer
maxVInt = 3

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

vneg :: Val -> Val
vneg (VInt x) = VInt ((-x) `mod` maxVInt)

--------------------

---- Primitive functions

dO :: Op -> W
dO Oint = vFcn $ mkFcn [ (x, x) | x <- allIntsL ]
dO Oneg = vFcn $ mkFcn [ (x, vneg x) | x <- allIntsL ]
dO Ogt  = vFcn $ mkFcn [ (VTup [x, y], x) | x <- allIntsL, y <- allIntsL, x > y ]
-- add is a single function, not many as in the doc.
dO Oadd = vFcn $ mkFcn [ (VTup [x, y], vadd x y) | x <- allIntsL, y <- allIntsL]
dO Oany = undefined -- Jeff: silencing warnings

--------------------
---- Functions

mkVFcn :: MappingV -> Val
mkVFcn = vFcn . mkFcn

allFcnsL :: [Fcn]
allFcnsL = setNos $ intFcns ++ pairFcns ++ extraFcns
#if HAS_HO
           ++ hoFcns
#endif

allFcns :: SetX Fcn
allFcns = mkSetUnsafe allFcnsL

numFcns :: Int
numFcns = length allFcnsL

allFcnsA :: Array Int Fcn
allFcnsA = array (0, numFcns - 1) [ (n, f) | f@(Fcn n _ _) <- allFcnsL ]

allFcnsM :: DM.Map Mapping Fcn
allFcnsM = DM.fromList [ (xys, f) | f@(Fcn _ _ xys) <- allFcnsL ]

allIntFcnsM :: DM.Map Mapping Fcn
allIntFcnsM = DM.fromList [ (xys, f) | f@(Fcn _ _ xys) <- setNos intFcns ]

mkFcn :: HasCallStack => MappingV -> Fcn
mkFcn xys =
  let xys' = mkMapping xys in
  case DM.lookup xys' allFcnsM of
    Nothing -> error $ "Function not in allFcns: " ++ showMapping' xys'
    Just f  -> f

mkIntFcn :: HasCallStack => MappingV -> Fcn
mkIntFcn xys =
  let xys' = mkMapping xys in
  case DM.lookup xys' allIntFcnsM of
    Nothing -> error $ "Function not in allIntFcns: " ++ showMapping' xys'
    Just f  -> f

noFcn :: HasCallStack => Int -> Fcn
noFcn i | i >= numFcns = error "noFcn"
noFcn i = allFcnsA ! i

nameFcn :: String -> Fcn
nameFcn s = fromMaybe (error $ "no function: " ++ s) $ nameFcnM s

nameFcnM :: String -> Maybe Fcn
nameFcnM s =
  case [ f | f@(Fcn _ (Just n) _) <- allFcnsL, s == n ] of
    [] -> Nothing
    f : _ -> Just f

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
  let ss = subsequences allIntsL
      fs = concat [ mkDomRng d allIntsL | d <- ss ]
  in  fs

pairFcns :: [Mapping]
pairFcns = intToPairFcns ++ pairToIntFcns

pairToIntFcns :: [Mapping]
pairToIntFcns = -- [ mkMapping [(VTup [x,y], z) | x <- allIntsL, y <- allIntsL ] | z <- allIntsL ]
  [ mkMapping madd ]  -- just one for now

madd :: MappingV
madd = [ (VTup [x,y], vadd x y) | x <- allIntsL, y <- allIntsL ]

intToPairFcns :: [Mapping]
intToPairFcns = [] -- [ mkMapping [(z, VTup [x,y])] | x <- allIntsL, y <- allIntsL, z <- allIntsL ]

hoFcns :: [Mapping]
hoFcns = hos

-- Set name field for some known functions
setNos :: [Mapping] -> [Fcn]
setNos = zipWith set [0..]
  where set i m = Fcn i (lookup m knownFcns) m

knownFcns :: [(Mapping, String)]
knownFcns =
  [ (mkMapping m, s) | (m, s) <-
    [ ([], "false")
    , (mint, "int")
    , (madd, "add")
    , (mcomparable, "comparable")
    , (msucc, "succ")
    , (mpred, "pred")
    , (mgt, "gt")
    , (mf012, "f012")
    , ([(VInt 0, VInt 0)], "id0")
    , ([(VInt 1, VInt 1)], "id1")
    , ([(VInt 2, VInt 2)], "id2")
    , ([(VInt 0, VInt 0), (VInt 1, VInt 1)], "id01")
    , ([(VInt 0, VTup [VInt 1, VInt 2])], "f0t12")
    , ([(x, VInt 0) | x <- allIntsL], "const0")
    , ([(VInt 0, VInt 1)], "succ0")
    , ([(x, x) | x <- [VInt 1, VInt 2]], "id12")
#if HAS_HO
    , (mho8, "ho8")
    , (mho1, "ho1")
    , (mho2, "ho2")
    , (mho3, "ho3")
#endif
    ]
  ]

type MappingV = [(Val, Val)]

emptym :: MappingV
emptym = []

mint :: MappingV
mint = [ (x, x) | x <- allIntsL ]

msucc :: MappingV
msucc = [ (x, vadd x (VInt 1)) | x <- allIntsL ]

mpred :: MappingV
mpred = [ (x, vadd x (last allIntsL)) | x <- allIntsL ]

mgt :: MappingV
mgt = [ (VTup [x, y], x) | x <- allIntsL, y <- allIntsL, x > y ]

--------------------
---- Environment

newtype Env = Env { unEnv :: M.Map Ident Val }
  deriving (Eq, Ord)

instance Show Env where
  show (Env m) = "[|" ++ intercalate "," (map f (M.toList m)) ++ "|]"
    where f (i,v) = i ++ "->" ++ show v

lookupEnv :: Ident -> Env -> W
lookupEnv x rho = fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $ lookupEnvM x rho

lookupEnvM :: Ident -> Env -> Maybe W
lookupEnvM x rho = M.lookup x $ unEnv rho

extendEnv :: Env -> Ident -> W -> Env
extendEnv rho i w = Env $ M.insert i w $ unEnv rho

-- Initial environment
rho0 :: Env
rho0 = fromListEnv $
  [ (n, dO o) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd), ("neg", Oneg) ] ] ++
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

allIntsL :: [Val]
allIntsL = [ VInt i | i <- [0 .. maxVInt - 1] ]

allInts :: SetX Val
allInts = mkSetUnsafe allIntsL

allTuples :: [Val]
allTuples = [VTup [x, y] | x <- allIntsL, y <- allIntsL]

allChoice2IntFcns :: [Val]
allChoice2IntFcns = map VFcn $ pick2 intVFcns
  where pick2 fs = [ [f1, f2]
                   | f1 <- fs
                   , let d1 = domFcn f1
--                   , not (isEmpty d1)
                   , f2 <- fs
                   , let d2 = domFcn f2
                   , not (isEmpty d2)
                   , isEmpty (d1 `intersect` d2)
                   ]
        intVFcns = filter intDom allFcnsL
        intDom f = domFcn f `isSubsetOf` mkSet allIntsL

someChoice3IntFcns :: [Val]
someChoice3IntFcns =
  [ VFcn [ id0, id1, id2 ]
  , VFcn [ id2, id1, id0 ]
  ]
  where
    id0 = mkFcn [VInt 0 ↦ VInt 0]
    id1 = mkFcn [VInt 1 ↦ VInt 1]
    id2 = mkFcn [VInt 2 ↦ VInt 2]

allChoiceFcnsL :: [Val]
allChoiceFcnsL =
  VFcn [] : map vFcn allFcnsL
#if HAS_CHOICE
            ++ allChoice2IntFcns ++ someChoice3IntFcns
#endif

allChoiceFcns :: Ws
allChoiceFcns = mkSetUnsafe allChoiceFcnsL

allWs :: Ws
allWs = mkSetUnsafe allWsL

allWsL :: [W]
allWsL = allIntsL
--          ++ allTuples
         ++ allChoiceFcnsL

--------------

extraFcns :: [Mapping]
extraFcns = map mkMapping [mcomparable, mgt]

-- Some function to make 'int' less lonely
mcomparable :: MappingV
mcomparable = [ (x, x) | x <- allIntsL ++ allTuples ]

--------------


hos :: [Mapping]
hos = map mkMapping $
 [ --[ F[noFcn 2{-={0↦1}-}] ↦ VInt 2 ]           -- fun_c(fun_c(0){1}){2}
   mf012
 , mfcon
 , [ F[noFcn 15{-={0↦2,1↦2}-}] ↦ VInt 2 ]
 , [ F[noFcn 15{-={0↦2,1↦2}-}] ↦ VInt 2
   , F[noFcn 61{-={0↦2,1↦2,2↦0}-}] ↦ VInt 2
   , F[noFcn 62{-={0↦2,1↦2,2↦1}-}] ↦ VInt 2
   , F[noFcn 63{-={0↦2,1↦2,2↦2}-}] ↦ VInt 2
   ]
 , mho1
 , mho2
 , mho3
 , mho6
 , mho7
 , mho8
 ]

-- fun(fun(0){1}){2}
mf012 :: MappingV
mf012 =
  [ F[                noFcn 2{-={0↦1}-}] ↦ VInt 2
  , F[noFcn 0{-={}-}, noFcn 2{-={0↦1}-}] ↦ VInt 2
  ]

-- fun(fun(0|||1){1}){2}
mfcon :: MappingV
mfcon =
  [ VTup [VInt 1, VInt 1]             ↦ VInt 2
  , F[noFcn 11]                       ↦ VInt 2
  , F[nameFcn "false", noFcn 11]      ↦ VInt 2
  , F[nameFcn "succ0", nameFcn "id1"] ↦ VInt 2
  , F[nameFcn "id1", nameFcn "succ0"] ↦ VInt 2
  ]

-- fun_c(fun_c(:int){:int}){f[1]}
mho1 :: MappingV
mho1 = concatMap fpad
   [ F[noFcn 37{-={0↦0,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 38{-={0↦0,1↦0,2↦1}-}] ↦ VInt 0
   , F[noFcn 39{-={0↦0,1↦0,2↦2}-}] ↦ VInt 0
   , F[noFcn 40{-={0↦0,1↦1,2↦0}-}] ↦ VInt 1
   , F[noFcn 41{-={0↦0,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 42{-={0↦0,1↦1,2↦2}-}] ↦ VInt 1
   , F[noFcn 43{-={0↦0,1↦2,2↦0}-}] ↦ VInt 2
   , F[noFcn 44{-={0↦0,1↦2,2↦1}-}] ↦ VInt 2
   , F[noFcn 45{-={0↦0,1↦2,2↦2}-}] ↦ VInt 2
   , F[noFcn 46{-={0↦1,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 47{-={0↦1,1↦0,2↦1}-}] ↦ VInt 0
   , F[noFcn 48{-={0↦1,1↦0,2↦2}-}] ↦ VInt 0
   , F[noFcn 49{-={0↦1,1↦1,2↦0}-}] ↦ VInt 1
   , F[noFcn 50{-={0↦1,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 51{-={0↦1,1↦1,2↦2}-}] ↦ VInt 1
   , F[noFcn 52{-={0↦1,1↦2,2↦0}-}] ↦ VInt 2
   , F[noFcn 53{-={0↦1,1↦2,2↦1}-}] ↦ VInt 2
   , F[noFcn 54{-={0↦1,1↦2,2↦2}-}] ↦ VInt 2
   , F[noFcn 55{-={0↦2,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 56{-={0↦2,1↦0,2↦1}-}] ↦ VInt 0
   , F[noFcn 57{-={0↦2,1↦0,2↦2}-}] ↦ VInt 0
   , F[noFcn 58{-={0↦2,1↦1,2↦0}-}] ↦ VInt 1
   , F[noFcn 59{-={0↦2,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 60{-={0↦2,1↦1,2↦2}-}] ↦ VInt 1
   , F[noFcn 61{-={0↦2,1↦2,2↦0}-}] ↦ VInt 2
   , F[noFcn 62{-={0↦2,1↦2,2↦1}-}] ↦ VInt 2
   , F[noFcn 63{-={0↦2,1↦2,2↦2}-}] ↦ VInt 2
   ]
 where fpad :: (Val, Val) -> [(Val, Val)]
       fpad arg@(F [af], r) =
         let xys :: MappingV
             xys = M.toList $ fcnMapping af
             ps :: [(MappingV, MappingV)]
             ps  = filter (not . null . snd) $ partitions xys
             fss :: [(Fcn, Fcn)]
             fss = map (\ (p1, p2) -> (mkIntFcn p1, mkIntFcn p2)) ps
         in  arg : [ (F [f1, f2], r) | (f1, f2) <- fss ]
       fpad _ = undefined

-- XXX check if this is correct
-- fun_c(fun_c(:succ){:int}){f[1]}
mho2 :: MappingV
mho2 =
   [ F[noFcn 37{-={0↦0,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 38{-={0↦0,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 39{-={0↦0,1↦0,2↦2}-}] ↦ VInt 2
   , F[noFcn 40{-={0↦0,1↦1,2↦0}-}] ↦ VInt 0
   , F[noFcn 41{-={0↦0,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 42{-={0↦0,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 43{-={0↦0,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 44{-={0↦0,1↦2,2↦1}-}] ↦ VInt 1
   , F[noFcn 45{-={0↦0,1↦2,2↦2}-}] ↦ VInt 2
   , F[noFcn 46{-={0↦1,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 47{-={0↦1,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 48{-={0↦1,1↦0,2↦2}-}] ↦ VInt 2
   , F[noFcn 49{-={0↦1,1↦1,2↦0}-}] ↦ VInt 0
   , F[noFcn 50{-={0↦1,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 51{-={0↦1,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 52{-={0↦1,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 53{-={0↦1,1↦2,2↦1}-}] ↦ VInt 1
   , F[noFcn 54{-={0↦1,1↦2,2↦2}-}] ↦ VInt 2
   , F[noFcn 55{-={0↦2,1↦0,2↦0}-}] ↦ VInt 0
   , F[noFcn 56{-={0↦2,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 57{-={0↦2,1↦0,2↦2}-}] ↦ VInt 2
   , F[noFcn 58{-={0↦2,1↦1,2↦0}-}] ↦ VInt 0
   , F[noFcn 59{-={0↦2,1↦1,2↦1}-}] ↦ VInt 1
   , F[noFcn 60{-={0↦2,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 61{-={0↦2,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 62{-={0↦2,1↦2,2↦1}-}] ↦ VInt 1
   , F[noFcn 63{-={0↦2,1↦2,2↦2}-}] ↦ VInt 2
   ]

mho3 :: MappingV
mho3 =
   [ F[noFcn 37{-={0↦0,1↦0,2↦0}-}] ↦ VInt 1
   , F[noFcn 38{-={0↦0,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 39{-={0↦0,1↦0,2↦2}-}] ↦ VInt 1
   , F[noFcn 40{-={0↦0,1↦1,2↦0}-}] ↦ VInt 2
   , F[noFcn 41{-={0↦0,1↦1,2↦1}-}] ↦ VInt 2
   , F[noFcn 42{-={0↦0,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 43{-={0↦0,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 44{-={0↦0,1↦2,2↦1}-}] ↦ VInt 0
   , F[noFcn 45{-={0↦0,1↦2,2↦2}-}] ↦ VInt 0
   , F[noFcn 46{-={0↦1,1↦0,2↦0}-}] ↦ VInt 1
   , F[noFcn 47{-={0↦1,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 48{-={0↦1,1↦0,2↦2}-}] ↦ VInt 1
   , F[noFcn 49{-={0↦1,1↦1,2↦0}-}] ↦ VInt 2
   , F[noFcn 50{-={0↦1,1↦1,2↦1}-}] ↦ VInt 2
   , F[noFcn 51{-={0↦1,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 52{-={0↦1,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 53{-={0↦1,1↦2,2↦1}-}] ↦ VInt 0
   , F[noFcn 54{-={0↦1,1↦2,2↦2}-}] ↦ VInt 0
   , F[noFcn 55{-={0↦2,1↦0,2↦0}-}] ↦ VInt 1
   , F[noFcn 56{-={0↦2,1↦0,2↦1}-}] ↦ VInt 1
   , F[noFcn 57{-={0↦2,1↦0,2↦2}-}] ↦ VInt 1
   , F[noFcn 58{-={0↦2,1↦1,2↦0}-}] ↦ VInt 2
   , F[noFcn 59{-={0↦2,1↦1,2↦1}-}] ↦ VInt 2
   , F[noFcn 60{-={0↦2,1↦1,2↦2}-}] ↦ VInt 2
   , F[noFcn 61{-={0↦2,1↦2,2↦0}-}] ↦ VInt 0
   , F[noFcn 62{-={0↦2,1↦2,2↦1}-}] ↦ VInt 0
   , F[noFcn 63{-={0↦2,1↦2,2↦2}-}] ↦ VInt 0
   ]

mho6 :: MappingV
mho6 = [VInt 0 ↦ F[noFcn 42{-={0↦0,1↦1,2↦2}-}]]

mho7 :: MappingV
mho7 = [F[noFcn 8{-={0↦0,1↦1}-}] ↦ VTup [VInt 1, VInt 0]]

-- fun(f:=fun(x:=0|||1){x}){f[1]}
mho8 :: MappingV
mho8 = [F[noFcn 8{-={0↦0,1↦1}-}] ↦ VInt 1]
