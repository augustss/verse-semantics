{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Freshenable
  ( Freshenable (..)
  , Unit1 (..)
  , Identity1 (..)
  , Sum1 (..)
  ) where

import Data.Kind

class Freshenable (t :: ((Type -> Type) -> Type) -> Type) where
  freshen :: Applicative m => (forall f . Traversable f => g f -> m (h f)) -> t g -> m (t h)

data Unit1 (f :: (Type -> Type) -> Type) = Unit1

instance Freshenable Unit1 where
  freshen _ _ = pure Unit1

newtype Identity1 f (g :: (Type -> Type) -> Type) = Identity1
  { runIdentity1 :: g f
  }

instance Traversable f => Freshenable (Identity1 f) where
  freshen f = fmap Identity1 . f . runIdentity1

data Sum1 g h (f :: (Type -> Type) -> Type) = Sum1 (g f) (h f)

instance (Freshenable g, Freshenable h) => Freshenable (Sum1 g h) where
  freshen f (Sum1 x y) = Sum1 <$> freshen f x <*> freshen f y
