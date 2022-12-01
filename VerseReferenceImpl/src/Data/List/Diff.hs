module Data.List.Diff
  ( List
  , cons
  , fromList
  , toList
  ) where

newtype List a = List { getList :: [a] -> [a] }

instance Functor List where
  fmap f = fromList . fmap f . toList

instance Applicative List where
  pure = List . (:)
  f <*> x = fromList $ toList f <*> toList x

cons :: a -> List a -> List a
cons x xs = List $ \ ys -> x:getList xs ys

fromList :: [a] -> List a
fromList = List . (++)

toList :: List a -> [a]
toList = ($ []) . getList

instance Semigroup (List a) where
  xs <> ys = List $ getList xs . getList ys

instance Monoid (List a) where
  mempty = List id
