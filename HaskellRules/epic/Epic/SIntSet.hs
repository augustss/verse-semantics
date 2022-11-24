{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Epic.SIntSet (SIntSet, empty, member, insert, fromList, toList, toIntSet) where

import Data.Coerce
import qualified Data.IntSet as S
import Epic.Print (Pretty (..))

newtype SIntSet k = SIntSet S.IntSet
  deriving (Eq, Ord, Show)

instance forall k. (Pretty k, Coercible k Int) => Pretty (SIntSet k) where
  pPrintPrec l p (SIntSet m) = pPrintPrec l p $ map (coerce :: Int -> k) $ S.toList m

empty :: forall k v. (Coercible k Int) => SIntSet k
empty = coerce (S.empty :: S.IntSet)

member :: forall k v. (Coercible k Int) => k -> SIntSet k -> Bool
member = coerce (S.member :: Int -> S.IntSet -> Bool)

insert :: forall k v. (Coercible k Int) => k -> SIntSet k -> SIntSet k
insert = coerce (S.insert :: Int -> S.IntSet -> S.IntSet)

fromList :: forall k. (Coercible k Int) => [k] -> SIntSet k
fromList = coerce (S.fromList :: [Int] -> S.IntSet)

toList :: forall k. (Coercible k Int) => SIntSet k -> [k]
toList = coerce (S.toList :: S.IntSet -> [Int])

toIntSet :: forall k. (Coercible k Int) => SIntSet k -> S.IntSet
toIntSet = coerce
