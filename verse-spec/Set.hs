module Set where
import Data.List
import qualified Data.Set as S

--------------------
---- Sets

newtype Set a = Set { uSet :: S.Set a }
  deriving (Eq, Ord)

instance (Show a) => Show (Set a) where
  show s = "{" ++ intercalate "," (map show (unSet s)) ++ "}"

unSet :: Set a -> [a]
unSet = S.toList . uSet

mkSet :: (Ord a) => [a] -> Set a
mkSet = Set . S.fromList

sUnion :: (Ord a) => [Set a] -> Set a
sUnion = Set . S.unions . map uSet

isect :: Ord a => Set a -> Set a -> Set a
isect (Set x) (Set y) = Set $ S.intersection x y

xunion :: Ord a => Set a -> Set a -> Set a
xunion (Set x) (Set y) = Set $ S.union x y

sing :: a -> Set a
sing = Set . S.singleton

unSing :: Set a -> a
unSing s =
  case unSet s of
    [x] -> x
    _   -> error "unSing"

empty :: Set a
empty = Set $ S.empty

isEmpty :: Set a -> Bool
isEmpty = S.null . uSet

sIn :: Ord a => a -> Set a -> Bool
sIn x (Set s) = S.member x s

smap :: (Ord b) => (a -> b) -> Set a -> Set b
smap f = Set . S.map f . uSet

-- Check if a predicate holds for all values in the set
forAll :: Set a -> (a -> Bool) -> Bool
forAll = forAllL . unSet

forAllL :: [a] -> (a -> Bool) -> Bool
forAllL xs p = all p xs

exists :: Set a -> (a -> Bool) -> Bool
exists = existsL . unSet

existsL :: [a] -> (a -> Bool) -> Bool
existsL xs p = any p xs

lessEq :: (Ord a) => Set a -> Set a -> Bool
lessEq (Set x) (Set y) = S.isSubsetOf x y
