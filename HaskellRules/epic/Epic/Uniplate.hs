module Epic.Uniplate
  ( module Data.Generics.Uniplate.Data,
    RewriteOp (..),
    topDownRewriteM,
  )
where

import Data.Data (Data)
import Data.Generics.Uniplate.Data

data RewriteOp a
  = Descend
  | Redo a
  | Stop a
  deriving (Eq, Show)

topDownRewriteM :: (Monad m, Data on) => (on -> m (RewriteOp on)) -> on -> m on
topDownRewriteM f = g
  where
    g x = do
      r <- f x
      case r of
        Descend -> descendM (topDownRewriteM f) x
        Redo y -> topDownRewriteM f y
        Stop y -> pure y
