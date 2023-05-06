{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Freshenable
  ( Freshenable (..)
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Functor.Identity
import Data.Void

class Freshenable a where
  type Elem a
  freshen :: Applicative m => (Elem a -> m (Elem a)) -> a -> m a

instance Freshenable (Identity a) where
  type Elem (Identity a) = a
  freshen f = fmap Identity . f . runIdentity

instance Freshenable a => Freshenable [a] where
  type Elem [a] = Elem a
  freshen = traverse . freshen

instance Freshenable () where
  type Elem () = Void
  freshen _ = pure

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
  freshen = traverse . freshen
