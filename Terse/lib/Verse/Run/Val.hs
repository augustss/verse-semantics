{-# LANGUAGE LambdaCase #-}
module Verse.Run.Val
  ( Val (..)
  , Var
  , freshVar
  , newVar
  , readVar
  , unifyVar
  , freeze
  ) where

import Control.Monad

import Data.Function
import Data.Functor.Compose
import Data.Functor.Identity

import Fix
import Ref

import Verse.Monad (VerseT, Vars (..), ZipVars_ (..))
import Verse.Monad qualified as Monad

data Val f a
  = Int !Integer deriving Show

instance Vars (Val f a) m where
  vars _ = \ case
    x@Int {} -> pure x

instance ZipVars_ (Val f a) m where
  zipVars_ _ = curry $ \ case
    (Int x, Int y) -> guard $ x == y

type Var m = Fix (Compose (Monad.Var m) (Val (Monad.VarsRef m)))

freshVar :: MonadRef m => VerseT m (Var m)
freshVar = Fix . Compose <$> Monad.freshVar

newVar :: MonadRef m => Val (Monad.VarsRef m) (Var m) -> VerseT m (Var m)
newVar = fmap (Fix . Compose) . Monad.newVar

readVar :: MonadRef m => Var m -> VerseT m (Val (Monad.VarsRef m) (Var m))
readVar = Monad.readVar . getCompose . getFix

unifyVar :: MonadRef m => Var m -> Var m -> VerseT m ()
unifyVar = Monad.unifyVar `on` getCompose . getFix

freeze
  :: MonadRef m
  => Var m -> VerseT m (Fix (Val Identity))
freeze = readVar >=> fmap Fix . \ case
  Int x -> pure $ Int x
