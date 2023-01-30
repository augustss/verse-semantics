{-# LANGUAGE ImportQualifiedPost #-}
module Control.Monad.Verse
  ( MonadVerse (..)
  , VerseT
  , Verse.Label
  , runVerseT
  ) where

import Control.Monad.Trans.Verse (VerseT, runVerseT)
import Control.Monad.Trans.Verse qualified as Verse
import Control.Monad.Verse.Class
