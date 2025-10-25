module Verse.Run
  ( module Verse.Run.S
  , plus
  , minus
  , less
  ) where

import Control.Applicative
import Control.Monad

import Ref

import Verse.Monad
import Verse.Run.S
import Verse.Run.Val (newInteger, readInteger)
import Verse.Run.Val qualified as Val

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
