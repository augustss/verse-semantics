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

import Control.Applicative
import Control.Monad

import Data.Function
import Data.Functor.Compose
import Data.Functor.Identity

import Fix
import Ref

import Verse.Monad (VerseT, Vars (..), ZipVars_ (..))
import Verse.Monad qualified as Monad

data Val f a
  = Int !Integer
  | Tup [a] deriving Show

instance Vars a m => Vars (Val f a) m where
  vars f = \ case
    x@Int {} -> pure x
    Tup x -> Tup <$> vars f x

instance ZipVars_ a m => ZipVars_ (Val f a) m where
  zipVars_ f = curry $ \ case
    (Int x, Int y) -> guard $ x == y
    (Tup x, Tup y) -> zipVars_ f x y
    _ -> empty

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
  Tup x -> Tup <$> traverse freeze x
