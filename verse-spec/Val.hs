module Val where
import qualified Data.Map as M
import Data.Maybe
import Set

--------------------
---- Values

data Val = VInt Integer | VTup [Val] | VFcn (Fcn Val Val)
  deriving (Eq, Ord)

data RVal = RVal Val | Wrong String

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VTup vs) = showsPrec p vs
  showsPrec p (VFcn f) = showsPrec p f

instance Show RVal where
  showsPrec p (RVal v) = showsPrec p v
  showsPrec _ (Wrong s) = showString $ "Wrong" ++ s

--------------------
---- Functions as tables
-- All functions have a unique name

data Fcn a b = Fcn String (M.Map a b)    -- mapping from a to b

mkFcn :: (Ord a) => String -> [(a, b)] -> Fcn a b
mkFcn s xys = Fcn s (M.fromList xys)

instance Eq (Fcn a b) where
  Fcn f _ == Fcn f' _  =  f == f'

instance Ord (Fcn a b) where
  Fcn f _ `compare` Fcn f' _  =  f `compare` f'

instance Show (Fcn a b) where
  show (Fcn s _) = s

-- Domain test
inDom :: Ord a => a -> Fcn a b -> Bool
inDom x (Fcn _ xys) = M.member x xys

-- Application when the argument is in the domain
ap :: (Show a, Ord a) => Fcn a b -> a -> b
ap (Fcn f xys) x =
  fromMaybe (error $ "ap: outside domain " ++ f ++ " " ++ show x) $
  M.lookup x xys

inDomV :: Val -> Val -> Bool
inDomV x (VFcn f) = inDom x f
inDomV (VInt x) (VTup vs) = 0 <= x && x < toInteger (length vs)
inDomV _ _ = False

apV :: Val -> Val -> Val
apV (VFcn f) x = ap f x
apV (VTup vs) (VInt x) = vs !! fromInteger x
apV _ _ = error "apV outside domain"

function :: Val -> Bool
function (VFcn _) = True
function (VTup _) = True
function _ = False

domV :: Val -> Set Val
domV (VFcn (Fcn _ m)) = mkSet (M.keys m)
domV (VTup es) = mkSet [ VInt (toInteger i) | i <- [0..length es-1] ]
domV _ = error "domV"

--------------------

maxVInt :: Integer
maxVInt = 4

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

