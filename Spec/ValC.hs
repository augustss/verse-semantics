{-# LANGUAGE PatternSynonyms #-}
module ValC(
  Val(..), pattern F,
  RVal(..),
  Fcn(..), vFcn, appM, domFcn, app, inDom, domV, eqFcnMap, isEmptyDom,
  subFcn,
  Mapping, mkMapping,
  fcnMapping,
  showMapping, showMapping',
  showPretty,
  showListWith,
  ) where
import Data.List
import Data.Maybe
import qualified Map as M
import SetX

data FunctionShow = JustNumber | NumberAndDefinition | HsSyntax
  deriving (Eq)

showFcnNo :: FunctionShow
showFcnNo = NumberAndDefinition

--------------------
---- Values

data Val = VInt Integer | VTup [Val] | VFcn [Fcn]
         | VEnv [(String, Val)]                    -- HACK for fast lambda evaluation
  deriving (Eq, Ord)

pattern F :: [Fcn] -> Val
pattern F fs = VFcn fs

vFcn :: Fcn -> Val
vFcn f = VFcn [f]

data RVal = RVal Val | Wrong String

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VTup vs) = showString "<" . foldr (.) id (intersperse (showString ",") (map (showsPrec p) vs)) . showString ">"
  showsPrec _ (VFcn fs) = showString "F" . showsPrec 0 fs
  showsPrec p (VEnv r) = showsPrec p r

instance Show RVal where
  showsPrec _ (RVal v) = showString (showPretty v)
  showsPrec _ (Wrong s) = showString $ "Wrong" ++ s

showPretty :: Val -> String
showPretty (VInt i) = show i
showPretty (VTup vs) = "<" ++ intercalate "," (map showPretty vs) ++ ">"
showPretty (VFcn [f]) = show f
showPretty (VFcn fs) = show fs
showPretty (VEnv _) = "<<VEnv>>"

showListWith :: (a -> String) -> [a] -> String
showListWith f xs = "[" ++ intercalate "," (map f xs) ++ "]"

--------------------
---- Functions as tables
-- All functions have a unique name

type FcnNo = Int                 -- unique function number

type Mapping = M.Map Val Val

data Fcn = Fcn FcnNo (Maybe String) Mapping    -- mapping from a to b

showMapping :: Mapping -> String
showMapping xys ="{" ++ intercalate "," (map showM $ M.toList xys) ++ "}"
  where showM (a,b) = show a ++ "\x21a6" ++ show b

showMapping' :: Mapping -> String
showMapping' xys ="[" ++ intercalate "\n," (map showM $ M.toList xys) ++ "]"
  where showM (a,b) = show a ++ " \x21a6 " ++ show b

eqFcnMap :: Mapping -> Fcn -> Bool
eqFcnMap fm (Fcn _ _ m) = m == fm

instance Eq Fcn where
  Fcn f _ _ == Fcn f' _ _  =  f == f'

instance Ord Fcn where
  Fcn f _ _ `compare` Fcn f' _ _  =  f `compare` f'

instance Show Fcn where
  show (Fcn f ms m) =
    case showFcnNo of
      HsSyntax -> "noFcn " ++ show f ++ "{-=" ++ showMapping m ++ "-}"
      _ -> case ms of
             Just s -> s
             _ | showFcnNo == JustNumber -> "f" ++ show f
               | otherwise -> "f" ++ show f ++ "=" ++ showMapping m

-- is f a subset of g, when the functions are viewed as sets of pairs
subFcn :: Fcn -> Fcn -> Bool
subFcn (Fcn _ _ f) (Fcn _ _ g) = M.isSubmapOf f g

-- Domain test
inDom :: Val -> Fcn -> Bool
inDom x (Fcn _ _ xys) = M.member x xys

domFcn :: Fcn -> SetX Val
domFcn (Fcn _ _ m) = mkSetUnsafe $ M.keys m

isEmptyDom :: Fcn -> Bool
isEmptyDom (Fcn _ _ m) = M.null m

-- Application when the argument is in the domain
app :: Fcn -> Val -> Val
app f@(Fcn _ _ xys) x =
  fromMaybe (error $ "ap: outside domain " ++ show f ++ " " ++ show x) $
  M.lookup x xys

appM :: Val -> Fcn -> Maybe Val
appM x (Fcn _ _ xys) = M.lookup x xys

domV :: Val -> SetX Val
domV (VFcn fs) = mkSetUnsafe (concatMap (\ (Fcn _ _ m) -> M.keys m) fs)
domV (VTup es) = mkSetUnsafe [ VInt (toInteger i) | i <- [0..length es-1] ]
domV v = error $ "domV: " ++ show v

fcnMapping :: Fcn -> Mapping
fcnMapping (Fcn _ _ xys) = xys

mkMapping :: [(Val, Val)] -> Mapping
mkMapping = mk M.empty
  where mk :: M.Map Val Val -> [(Val, Val)] -> M.Map Val Val
        mk m [] = m
        mk m ((x,y):xys) =
          case M.lookup x m of
            Just y' | y /= y' -> error $ "mkMapping: inconsistent " ++ show (x, (y, y'))
            _ -> mk (M.insert x y m) xys
