{-# LANGUAGE ImportQualifiedPost #-}
module Control.Monad.Verse
  ( MonadVerse (..)
  , VerseT
  , Verse.Label
  , runVerseT
  , freshen
  ) where

import Control.Monad.Trans.Verse (VerseT, runVerseT, freshen)
import Control.Monad.Trans.Verse qualified as Verse
import Control.Monad.Verse.Class
