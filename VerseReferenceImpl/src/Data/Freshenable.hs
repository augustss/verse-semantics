{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Freshenable
  ( Freshenable (..)
  ) where

import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.Kind

class Freshenable a where
  type Elem a :: (Type -> Type) -> Type
  freshen :: Applicative m => (forall f . Traversable f => Elem a f -> m (Elem a f)) -> a -> m a

instance Freshenable a => Freshenable (Identity a) where
  type Elem (Identity a) = Elem a
  freshen f = fmap Identity . freshen f . runIdentity

instance Freshenable a => Freshenable [a] where
  type Elem [a] = Elem a
  freshen f = traverse $ freshen f

instance (Freshenable a, Freshenable b, Elem a ~ Elem b) => Freshenable (a, b) where
  type Elem (a, b) = Elem a
  freshen f (a, b) = (,) <$> freshen f a <*> freshen f b

instance (Freshenable a, Freshenable b, Elem a ~ Elem b) => Freshenable (Either a b) where
  type Elem (Either a b) = Elem a
  freshen f = \ case
    Left a -> Left <$> freshen f a
    Right b -> Right <$> freshen f b

instance Freshenable v => Freshenable (HashMap k v) where
  type Elem (HashMap k v) = Elem v
  freshen f = traverse $ freshen f
