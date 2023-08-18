{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Named
  ( Named (..)
  ) where

import Control.Monad.Verse

import Language.Verse.Pretty
import Language.Verse.Val

data Named m a
  = Val a
  | Ref (VarRef m (Val m)) deriving (Functor, Foldable, Traversable)

instance EqVarRef (VarRef m) => Unifiable (Named m)

instance EqVarRef (VarRef m) => Zippable (Named m) where
  zipMatch = curry $ \ case
    (Val x, Val y) -> Just [(x, y)]
    (Ref x, Ref y) | eqVarRef x y -> Just []
    _ -> Nothing

instance Freshenable a => Freshenable (Named m a) where
  type Elem (Named m a) = Elem a
  freshen f = \ case
    Val x -> Val <$> freshen f x
    x@Ref {} -> pure x

instance (MonadPretty a m, MonadVarRef m) => MonadPretty (Named m a) m where
  prettyM = \ case
    Val x -> prettyM x
    Ref x -> readVarRef x >>= freezeVar >>= \ case
      Nothing -> pure "_"
      Just x -> prettyM x
