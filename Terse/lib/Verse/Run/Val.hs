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
  , newInt
  , readInt
  , newChar
  , readChar
  , newTup
  , newPair
  , readPair
  , newString
  , readString
  , newLam
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

import Verse.Monad (VerseT, Vars (..), ZipVars_ (..), stuck)
import Verse.Monad qualified as Monad
import Verse.Run.S

data Val f m a
  = Int !Integer
  | Char {-# UNPACK #-} !Char
  | Ptr !(f a)
  | Tup [a]
  | forall b . Vars b m => Lam !b !(b -> S m -> S m -> Var m -> VerseT m (Var m))

instance Vars a m => Vars (Val f m a) m where
  vars f = \ case
    x@Int {} -> pure x
    x@Char {} -> pure x
    x@Ptr {} -> pure x
    Tup x -> Tup <$> vars f x
    Lam x y -> vars f x <&> \ x -> Lam x y

instance (Eq (f a), ZipVars_ a m) => ZipVars_ (Val f m a) m where
  zipVars_ f = curry $ \ case
    (Int x, Int y) -> guard $ x == y
    (Char x, Char y) -> guard $ x == y
    (Ptr x, Ptr y) -> guard $ x == y
    (Tup x, Tup y) -> zipVars_ f x y
    _ -> empty

instance (Pretty (f a), Pretty a) => Pretty (Val f m a) where
  pretty = \ case
    Int x -> pretty x
    Char x -> pretty x
    Ptr x -> pretty x
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
  Char x -> pure $ Char x
  Ptr x -> fmap (Ptr . Identity) . freeze =<< Monad.readVarsRef x
  Tup x -> Tup <$> traverse freeze x
  _ -> stuck

newInt :: MonadRef m => Integer -> VerseT m (Var m)
newInt = newVar . Int

readInt :: MonadRef m => Var m -> VerseT m Integer
readInt = readVar >=> \ case
  Int x -> pure x
  _ -> empty

newChar :: MonadRef m => Char -> VerseT m (Var m)
newChar = newVar . Char

readChar :: MonadRef m => Var m -> VerseT m Char
readChar= readVar >=> \ case
  Char x -> pure x
  _ -> empty

newTup :: MonadRef m => [Var m] -> VerseT m (Var m)
newTup = newVar . Tup

newPair :: MonadRef m => Var m -> Var m -> VerseT m (Var m)
newPair x y = newVar $ Tup [x, y]

readPair :: MonadRef m => Var m -> VerseT m (Var m, Var m)
readPair = readVar >=> \ case
  Tup [x1, x2] -> pure (x1, x2)
  _ -> empty

newString :: MonadRef m => String -> VerseT m (Var m)
newString = newTup <=< traverse newChar

readString :: MonadRef m => Var m -> VerseT m String
readString = readVar >=> \ case
  Tup xs -> traverse readChar xs
  _ -> empty

newLam
  :: (Vars a m, MonadRef m)
  => a -> (a -> S m -> S m -> Var m -> VerseT m (Var m)) -> VerseT m (Var m)
newLam x f = newVar $ Lam x f
