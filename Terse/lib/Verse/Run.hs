{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module Verse.Run
  ( module Verse.Run.S
  , app
  , plus
  , minus
  , less
  ) where

import Control.Applicative
import Control.Monad

import Data.Functor

import Ref

import Verse.Monad
import Verse.Run.S
import Verse.Run.Val (newInteger, readInteger)
import Verse.Run.Val qualified as Val

app
  :: MonadRef m
  => Val.Var m -> S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
app var1 s1 s2 var2 = Val.readVar var1 >>= \ case
  Val.Int _ -> stuck
  Val.Lam x f -> f x s1 s2 var2
  Val.Tup vars -> do
    readChoiceFree s1
    var <- asum $ zip [0 ..] vars <&> \ (i, var1) -> do
      Val.unifyVar var2 <=< Val.newVar $ Val.Int i
      pure var1
    unifyS s1 s2
    pure var

plus
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
plus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $! x1 + x2

minus
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
minus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $! x1 - x2

less
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
less s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  guard $! x1 < x2
  pure var1
