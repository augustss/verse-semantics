{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Run.Val
  ( Val (..)
  , Var
  , freshVar
  , newVar
  , readVar
  , unifyVar
  , freeze
  , newInteger
  , readInteger
  ) where

import Control.Applicative
import Control.Monad

import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Identity

import Prettyprinter

import Fix
import Ref

import Verse.Monad (VerseT, Vars (..), ZipVars_ (..))
import Verse.Monad qualified as Monad
import Verse.Run.S

data Val f m a
  = Int !Integer
  | Tup [a]
  | forall b . Vars b m => Lam b (b -> S m -> S m -> Var m -> VerseT m (Var m))

instance Vars a m => Vars (Val f m a) m where
  vars f = \ case
    x@Int {} -> pure x
    Tup x -> Tup <$> vars f x
    Lam x y -> vars f x <&> \ x -> Lam x y

instance ZipVars_ a m => ZipVars_ (Val f m a) m where
  zipVars_ f = curry $ \ case
    (Int x, Int y) -> guard $ x == y
    (Tup x, Tup y) -> zipVars_ f x y
    _ -> empty

instance Pretty a => Pretty (Val f m a) where
  pretty = \ case
    Int x -> pretty x
    Tup x -> tupled $ pretty <$> x
    Lam _ _ -> "fun"

type Var m = Fix (Compose (Monad.Var m) (Val (Monad.VarsRef m) m))

freshVar :: MonadRef m => VerseT m (Var m)
freshVar = Fix . Compose <$> Monad.freshVar

newVar :: MonadRef m => Val (Monad.VarsRef m) m (Var m) -> VerseT m (Var m)
newVar = fmap (Fix . Compose) . Monad.newVar

readVar :: MonadRef m => Var m -> VerseT m (Val (Monad.VarsRef m) m (Var m))
readVar = Monad.readVar . getCompose . getFix

unifyVar :: MonadRef m => Var m -> Var m -> VerseT m ()
unifyVar = Monad.unifyVar `on` getCompose . getFix

freeze
  :: MonadRef m
  => Var m -> VerseT m (Fix (Val Identity m))
freeze = readVar >=> fmap Fix . \ case
  Int x -> pure $ Int x
  Tup x -> Tup <$> traverse freeze x
  _ -> empty

newInteger :: MonadRef m => Integer -> VerseT m (Var m)
newInteger = newVar . Int

readInteger :: MonadRef m => Var m -> VerseT m Integer
readInteger = readVar >=> \ case
  Int x -> pure x
  _ -> empty
