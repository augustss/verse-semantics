module ValLA where
import Control.Arrow(second)
import qualified Data.Map as M
import Data.List as L
import Data.Ord
import SetX

--------------------
---- Values

data Labelled a = Lbl [Label] a
  deriving (Eq, Ord)

unLabel :: Labelled a -> a
unLabel (Lbl _ a) = a

labelOf :: Labelled a -> [Label]
labelOf (Lbl l _) = l

noLabel :: a -> Labelled a
noLabel a = Lbl [] a

label :: Label -> Labelled a -> Labelled a
label l (Lbl ls a) = Lbl (l:ls) a

labels :: [Label] -> Labelled a -> Labelled a
labels l (Lbl ls a) = Lbl (l ++ ls) a

instance Show a => Show (Labelled a) where
  showsPrec _ (Lbl ls a) = showString (concatMap show ls) . showsPrec 11 a

data Label = L | R
  deriving (Eq, Ord, Show)

data Val = VInt Integer | VTup [Val] | VFcn Fcn
  deriving (Eq, Ord)

vFcn :: Fcn -> Val
vFcn = VFcn

data RVal = RVal Val | Wrong String

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VTup vs) = showString "<" . foldr (.) id (intersperse (showString ",") (map (showsPrec p) vs)) . showString ">"
  showsPrec p (VFcn f) = showsPrec p f

instance Show RVal where
  showsPrec _ (RVal v) = showString (showPretty v)
  showsPrec _ (Wrong s) = showString $ "Wrong" ++ s

showPretty :: Val -> String
showPretty (VInt i) = show i
showPretty (VTup vs) = "<" ++ intercalate "," (map showPretty vs) ++ ">"
showPretty (VFcn f) = show f

showListWith :: (a -> String) -> [a] -> String
showListWith f xs = "[" ++ intercalate "," (map f xs) ++ "]"

--------------------
---- Functions as tables
-- All functions have a unique name

data Fcn = Fcn String (M.Map Val (Labelled Val))    -- mapping from a to b

mkFcn :: String -> [(Val, Val)] -> Fcn
mkFcn s xys = Fcn s (M.fromList $ map (second noLabel) xys)

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

pairs :: Fcn -> [(Val, Labelled Val)]
pairs (Fcn _ m) = M.toList m

-- Application when the argument is in the domain
apply :: Val -> Val -> Maybe (Labelled Val)
apply (VFcn (Fcn _ xys)) x = M.lookup x xys
apply (VTup vs) (VInt i) | 0 <= i' && i' <= len = Just $ Lbl ls (vs !! i')
  where ls = replicate i' R ++ if i' == len-1 then [] else [L]
        len = length vs
        i' = fromInteger i
apply _ _ = Nothing

inDomV :: Val -> Val -> Bool
inDomV x (VFcn f) = inDom x f
inDomV (VInt x) (VTup vs) = 0 <= x && x < toInteger (length vs)
inDomV _ _ = False

domV :: Val -> SetX Val
domV (VFcn (Fcn _ m)) = mkSet $ M.keys m
domV (VTup es) = mkSet [ VInt (toInteger i) | i <- [0..length es-1] ]
domV v = error $ "domV: " ++ show v

{-
apV :: Val -> Val -> [Val]
apV (VFcn f) x = [ ap f x | inDom x f ]
apV (VTup vs) (VInt x) = [ vs !! fromInteger x ]
apV _ _ = error "apV outside domain"
-}

function :: Val -> Bool
function (VFcn _) = True
function (VTup _) = True
function _ = False

mkEnumFcn :: String -> [(Val, Val)] -> Fcn
mkEnumFcn n = Fcn n . M.fromList . lab []
  where lab _ [] = []
        lab ls [(x, y)] = [(x, Lbl ls y)]
        lab ls ((x, y) : xys) = (x, Lbl (ls ++ [L]) y) : lab (ls ++ [R]) xys

isectFcn :: Fcn -> Fcn -> Maybe Fcn
isectFcn f1@(Fcn n1 xys1) f2@(Fcn n2 xys2) =
  let xs = M.keys xys1 `L.intersect` M.keys xys2
      xys1' = rest xys1
      xys2' = rest xys2
      rest m = [ Lbl l (x, y) | (x, Lbl l y) <- M.toList m, x `elem` xs ]
  in  if map unLabel xys1' /= map unLabel xys2' then
        Nothing  
      else
        let sortL :: [Labelled a] -> [[a]]
            sortL as = map (map unLabel) $ groupBy eq $ sortBy (comparing labelOf) as
              where eq x y = labelOf x == labelOf y
        in  case compat (sortL xys1') (sortL xys2') of
              Nothing -> Nothing
              Just axys ->
                let xys = M.fromList $ relabel axys in
                if xys == xys1 then Just f1
                else if xys == xys2 then Just f2
                else Just $ Fcn (n1 ++ "/\\" ++ n2) xys

compat :: Eq a => [[a]] -> [[a]] -> Maybe [[a]]
compat [] [] = Just []
compat ([]:xs) ys = compat xs ys
compat xs ([]:ys) = compat xs ys
compat (x:xs) (y:ys) | x `subset` y = (x :) <$> compat xs ((y \\ x):ys)
                     | y `subset` x = (y :) <$> compat ((x \\ y):xs) ys
                     | otherwise = Nothing
compat _ _ = error "compat"

subset :: Eq a => [a] -> [a] -> Bool
subset x y = null (x \\ y)

relabel :: [[(a, b)]] -> [(a, Labelled b)]
relabel = re []
  where re _ [] = []
        re l [xys] = map (second (Lbl l)) xys
        re l (xys:xyss) = map (second (Lbl l')) xys ++ re (l ++ [R]) xyss
          where l' = l ++ [L]

--------------------

maxVInt :: Integer
maxVInt = 4

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined
