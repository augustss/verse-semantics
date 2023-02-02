{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Control.Monad.Unify
  ( MonadUnify (..)
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Var

import Data.Unifiable

class MonadVar m => MonadUnify m where
  unify :: Unifiable f => Var m f -> Var m f -> m ()
  default unify :: ( m ~ t n
                   , MonadUnifyTrans t n
                   , Unifiable f
                   ) => Var m f -> Var m f -> m ()
  unify x y = lift $ unify x y

type MonadUnifyTrans t n = (Var (t n) ~ Var n, MonadTrans t, MonadUnify n)

instance MonadUnify m => MonadUnify (ReaderT r m)

instance MonadUnify m => MonadUnify (CPS.WriterT w m)
