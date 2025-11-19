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
  , fork1
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
  {-# INLINABLE vars #-}
  vars f = \ case
    x@Int {} -> pure x
    x@Char {} -> pure x
    x@Ptr {} -> pure x
    Tup x -> Tup <$> vars f x
    Lam x y -> vars f x <&> \ x -> Lam x y

instance (Eq (f a), ZipVars_ a m) => ZipVars_ (Val f m a) m where
  {-# INLINABLE zipVars_ #-}
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
{-# INLINE freshVar #-}
freshVar = Fix . Compose <$> Monad.freshVar

newVar :: Val (Monad.VarsRef m) m (Var m) -> VerseT m (Var m)
{-# INLINE newVar #-}
newVar = fmap (Fix . Compose) . Monad.newVar

readVar :: MonadWeakRef m => Var m -> VerseT m (Val (Monad.VarsRef m) m (Var m))
{-# INLINE readVar #-}
readVar = Monad.readVar . getCompose . getFix

unifyVar :: MonadWeakRef m => Var m -> Var m -> VerseT m ()
{-# INLINE unifyVar #-}
unifyVar = Monad.unifyVar `on` getCompose . getFix

fork1 :: MonadWeakRef m => VerseT m (Var m) -> VerseT m (Var m)
{-# INLINE fork1 #-}
fork1 = fmap (Fix . Compose) . Monad.fork1 . fmap (getCompose . getFix)

freeze
  :: MonadWeakRef m
  => Var m -> VerseT m (Fix (Val Identity m))
{-# INLINABLE freeze #-}
freeze = readVar >=> fmap Fix . \ case
  Int x -> pure $ Int x
  Char x -> pure $ Char x
  Ptr x -> fmap (Ptr . Identity) . freeze =<< Monad.readVarsRef x
  Tup x -> Tup <$> traverse freeze x
  _ -> stuck

newInt :: Integer -> VerseT m (Var m)
{-# INLINE newInt #-}
newInt = newVar . Int

readInt :: MonadWeakRef m => Var m -> VerseT m Integer
{-# INLINE readInt #-}
readInt = readVar >=> \ case
  Int x -> pure x
  _ -> empty

newChar :: Char -> VerseT m (Var m)
{-# INLINE newChar #-}
newChar = newVar . Char

readChar :: MonadWeakRef m => Var m -> VerseT m Char
{-# INLINE readChar #-}
readChar = readVar >=> \ case
  Char x -> pure x
  _ -> empty

newTup :: [Var m] -> VerseT m (Var m)
{-# INLINE newTup #-}
newTup = newVar . Tup

newPair :: Var m -> Var m -> VerseT m (Var m)
{-# INLINE newPair #-}
newPair x y = newTup [x, y]

readPair :: MonadWeakRef m => Var m -> VerseT m (Var m, Var m)
{-# INLINE readPair #-}
readPair = readVar >=> \ case
  Tup [x1, x2] -> pure (x1, x2)
  _ -> empty

newString :: String -> VerseT m (Var m)
{-# INLINABLE newString #-}
newString = newTup <=< traverse newChar

readString :: MonadWeakRef m => Var m -> VerseT m String
{-# INLINABLE readString #-}
readString = readVar >=> \ case
  Tup xs -> traverse readChar xs
  _ -> empty

newLam
  :: Vars a m
  => a -> (a -> S m -> S m -> Var m -> VerseT m (Var m)) -> VerseT m (Var m)
{-# INLINE newLam #-}
newLam x f = newVar $ Lam x f
