module ValC where
import qualified Data.Map as M
import Data.List
import Data.Maybe
import SetX

--------------------
---- Values

data Val = VInt Integer | VTup [Val] | VFcn [Fcn]
  deriving (Eq, Ord)

vFcn :: Fcn -> Val
vFcn f = VFcn [f]

data RVal = RVal Val | Wrong String

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VTup vs) = showString "<" . foldr (.) id (intersperse (showString ", ") (map (showsPrec p) vs)) . showString ">"
  showsPrec p (VFcn fs) = showString "F" . showsPrec p fs

instance Show RVal where
  showsPrec p (RVal v) = showsPrec p v
  showsPrec _ (Wrong s) = showString $ "Wrong" ++ s

--------------------
---- Functions as tables
-- All functions have a unique name

data Fcn = Fcn String (M.Map Val Val)    -- mapping from a to b

mkFcn :: String -> [(Val, Val)] -> Fcn
mkFcn s xys = Fcn s (M.fromList xys)

instance Eq Fcn where
  Fcn f _ == Fcn f' _  =  f == f'

instance Ord Fcn where
  Fcn f _ `compare` Fcn f' _  =  f `compare` f'

instance Show Fcn where
  show (Fcn s _) = s

-- Domain test
inDom :: Val -> Fcn -> Bool
inDom x (Fcn _ xys) = M.member x xys

dom :: Fcn -> SetX Val
dom (Fcn _ m) = mkSet $ M.keys m

-- Application when the argument is in the domain
ap :: Fcn -> Val -> Val
ap (Fcn f xys) x =
  fromMaybe (error $ "ap: outside domain " ++ f ++ " " ++ show x) $
  M.lookup x xys

inDomV :: Val -> Val -> Bool
inDomV x (VFcn fs) = any (inDom x) fs
inDomV (VInt x) (VTup vs) = 0 <= x && x < toInteger (length vs)
inDomV _ _ = False

domV :: Val -> SetX Val
domV (VFcn fs) = mkSet (concatMap (\ (Fcn _ m) -> M.keys m) fs)
domV (VTup es) = mkSet [ VInt (toInteger i) | i <- [0..length es-1] ]
domV v = error $ "domV: " ++ show v

apV :: Val -> Val -> [Val]
apV (VFcn fs) x = [ ap f x | f <- fs, inDom x f ]
apV (VTup vs) (VInt x) = [ vs !! fromInteger x ]
apV _ _ = error "apV outside domain"

function :: Val -> Bool
function (VFcn _) = True
function (VTup _) = True
function _ = False

--------------------

maxVInt :: Integer
maxVInt = 4

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

