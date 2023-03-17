{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Freshenable
  ( Freshenable (..)
  , VarUnit (..)
  , VarIdentity (..)
  , VarSum (..)
  ) where

import Control.Monad.Var

import Data.Kind

class Freshenable f where
  freshen :: MonadVar m => f (Var m) -> m (f (Var m))

data VarUnit (var :: (Type -> Type) -> Type) = VarUnit

instance Freshenable VarUnit where
  freshen = pure

newtype VarIdentity f (var :: (Type -> Type) -> Type) = VarIdentity
  { runVarIdentity :: var f
  }

instance Traversable f => Freshenable (VarIdentity f) where
  freshen = fmap VarIdentity . freshenVar . runVarIdentity

data VarSum f g (var :: (Type -> Type) -> Type) = VarSum (f var) (g var)

instance (Freshenable f, Freshenable g) => Freshenable (VarSum f g) where
  freshen (VarSum x y) = VarSum <$> freshen x <*> freshen y
