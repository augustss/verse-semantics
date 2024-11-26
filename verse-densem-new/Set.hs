module Set where
import qualified Data.Set as S

--------------------
---- Sets

type Set a = S.Set a

unSet :: Set a -> [a]
unSet = S.toList

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

sUnion :: (Ord a) => [Set a] -> Set a
sUnion = S.unions

isect :: Ord a => Set a -> Set a -> Set a
isect = S.intersection

sing :: a -> Set a
sing = S.singleton

unSing :: Set a -> a
unSing s =
  case unSet s of
    [x] -> x
    _   -> error "unSing"

empty :: Set a
empty = S.empty

isEmpty :: Set a -> Bool
isEmpty = S.null

sIn :: Ord a => a -> Set a -> Bool
sIn = S.member

-- Check if a predicate holds for all values in the set
forAll :: Set a -> (a -> Bool) -> Bool
forAll = forAllL . unSet

forAllL :: [a] -> (a -> Bool) -> Bool
forAllL xs p = all p xs

exists :: Set a -> (a -> Bool) -> Bool
exists = existsL . unSet

existsL :: [a] -> (a -> Bool) -> Bool
existsL xs p = any p xs

