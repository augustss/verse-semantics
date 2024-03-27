{-# LANGUAGE LambdaCase #-}
module Control.Monad.Extras
  ( module Control.Monad
  , whenM
  ) where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM m n = m >>= \ case
  False -> pure ()
  True -> n
