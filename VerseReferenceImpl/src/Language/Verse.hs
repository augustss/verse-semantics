{-# LANGUAGE LambdaCase #-}
module Language.Verse
  ( eval2
  , eval2'
  ) where

import Control.Monad ((<=<))
import Control.Monad.Abort
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Verse (VerseT, runVerseT)

import Data.ByteString (ByteString)
import Data.Functor

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Mode
import Language.Verse.Parse2 as P2
import Language.Verse.Rewrite
import Language.Verse.Val

eval2 :: ( MonadAbort Error m
         , MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => String -> ByteString -> m [FrozenVal]
eval2 path xs = do
  (e1, e2) <- liftEither $ runSupplyT $ do
    e <- rewrite =<< lift (P2.parse2 path xs)
    (,) <$> desugar Verification e <*> desugar Execution e
  whenNothingM_ (runVerseT $ Eval.eval Verification e1) $
    abort StuckError
  whenNothingM (runVerseT $ Eval.eval Execution e2) $
    abort StuckError

eval2' :: ( MonadAbort Error m
          , MonadFix m
          , MonadRef m
          , MonadSupply Label m
          , EqRef (Ref m)
          ) => String -> Mode -> ByteString -> VerseT m FrozenVal
eval2' path mode =
  Eval.eval mode <=<
  liftEither . (runSupplyT . (desugar mode <=< rewrite) <=< P2.parse2 path)

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM m n = m >>= \ case
  Nothing -> n
  Just x -> pure x

whenNothingM_ :: Monad m => m (Maybe a) -> m a -> m ()
whenNothingM_ m n = m >>= \ case
  Nothing -> void n
  Just _ -> pure ()
