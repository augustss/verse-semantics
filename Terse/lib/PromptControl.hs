{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
module PromptControl
  ( MonadPromptControl (..)
  ) where

import Data.Kind

import GHC.Exts
import GHC.IO (IO (..))

class Monad m => MonadPromptControl m where
  type PromptTag m :: Type -> Type
  newPromptTag :: m (PromptTag m a)
  prompt :: PromptTag m a -> m a -> m a
  control0 :: PromptTag m a -> ((m b -> m a) -> m a) -> m b
  control :: PromptTag m a -> ((m b -> m a) -> m a) -> m b
  {-# INLINE control #-}
  control x f = control0 x $ \ k -> f $ prompt x . k

data PromptTagIO a = PromptTagIO (PromptTag# a)

instance MonadPromptControl IO where
  type PromptTag IO = PromptTagIO
  {-# INLINE newPromptTag #-}
  newPromptTag = IO $ \ s -> case newPromptTag# s of
    (# s, x #) -> (# s, PromptTagIO x #)
  {-# INLINE prompt #-}
  prompt (PromptTagIO x) (IO f) = IO $ prompt# x f
  {-# INLINE control0 #-}
  control0 (PromptTagIO x) f = IO $ control0# x $ \ k s ->
    case f $ \ (IO f) -> IO $ k f of
      IO f -> f s
