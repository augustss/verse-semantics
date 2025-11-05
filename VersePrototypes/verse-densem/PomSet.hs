{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PomSet(module PomSet) where
import Control.Applicative
import Control.Monad
import qualified Set
import Set(Set)
import GHC.Stack

default ()

infixl 6 :\/
infixl 8 :++

data P a
  = Empty
  | Unit a
  | P a :++ P a
  | P a :\/ P a
  deriving (Show)

--instance (Show a, Ord a) => Show (P a) where
--  showsPrec p s = showsPrec p (canon $ absorbEmpty s)

instance (Ord a) => Eq (P a) where
  x == y  =  compare x y == EQ

instance (Ord a) => Ord (P a) where
  compare x y = compare (canon x) (canon y)

unit :: a -> P a
unit a = Unit a

none :: P a
none = Empty

-- Smart constructor for :++
infixl 8 +++
(+++) :: P a -> P a -> P a
Empty +++ y = y
x +++ Empty = x
x +++     y = x :++ y

-- Smart constructor for :\/
infixl 6 `union`
union :: P a -> P a -> P a
union Empty y = y
union x Empty = x
union x     y = x :\/ y

instance Functor P where
  fmap f s = s >>= pure . f

instance Applicative P where
  pure = Unit
  (<*>) = ap

instance Monad P where
  return          = pure
  Empty     >>= _ = Empty
  Unit x    >>= k = k x
  (s :++ t) >>= k = (s >>= k) +++ (t >>= k)
  (s :\/ t) >>= k = (s >>= k) `union` (t >>= k)

instance Alternative P where
  empty = Empty
  (<|>) = union

{-
cONC :: [P a] -> P a
cONC = foldr (+++) Empty
-}

canon :: P a -> Set [a]
canon Empty = Set.empty
canon p = canon' p
  where
    canon' Empty = error "canon' : Empty"
    canon' (Unit a) = Set.singleton [a]
    canon' (s :\/ t) = canon' s `Set.union` canon' t
    canon' (Unit a :++ s) = [ a : as | as <- canon' s ]
    canon' ((s :\/ t) :++ r) = canon' (s :++ r) `Set.union` canon' (t :++ r)
    canon' ((s :++ t) :++ r) = canon' (s :++ (t :++ r))
    canon' (Empty :++ _) = error "cannot happen"

uncanon :: (HasCallStack, Ord a) => Set [a] -> P a
uncanon s | Set.isEmpty s = Empty
uncanon s = -- Set.foldSet union [ foldr (+++) Empty (map unit xs) | xs <- s ]
  foldr1 (:\/) (map f (Set.toList s))
  where f [] = error "uncanon: [] sequence"
        f xs = foldl1 (:++) (map Unit xs)

-- Return all the leaves of a pomset
allLeaves :: P a -> [a]
allLeaves Empty = []
allLeaves (Unit a) = [a]
allLeaves (a :++ b) = allLeaves a ++ allLeaves b
allLeaves (a :\/ b) = allLeaves a ++ allLeaves b

mkPomSetList :: [a] -> P a
mkPomSetList = foldr (+++) Empty . map unit
