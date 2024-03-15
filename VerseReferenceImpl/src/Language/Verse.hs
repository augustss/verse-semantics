{-# LANGUAGE LambdaCase #-}
module Language.Verse
  ( eval
  , eval'
  ) where

import Control.Monad
import Control.Monad.Wrong
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Verse

import Data.ByteString (ByteString)

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Mode
import Language.Verse.Parse2
import Language.Verse.Rewrite
import Language.Verse.Val

eval :: ( MonadWrong Error m
        , MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , EqRef (Ref m)
        ) => String -> ByteString -> m [[FrozenVal]]
eval path xs = do
  (e1, e2) <- liftEither $ runSupplyT $ do
    e <- rewrite =<< lift (parse2 path xs)
    (,) <$> desugar Verification e <*> desugar Execution e
  whenNothingM_ (runVerseT $ Eval.eval Verification e1) $
    wrong StuckError
  whenNothingM (runVerseT $ Eval.eval Execution e2) $
    wrong StuckError

eval' :: ( MonadWrong Error m
         , MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => String -> Mode -> ByteString -> VerseT m FrozenVal
eval' path mode =
  Eval.eval mode <=<
  liftEither . (runSupplyT . (desugar mode <=< rewrite) <=< parse2 path)

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM m n = m >>= \ case
  Nothing -> n
  Just x -> pure x

whenNothingM_ :: Monad m => m (Maybe a) -> m a -> m ()
whenNothingM_ m n = m >>= \ case
  Nothing -> void n
  Just _ -> pure ()
