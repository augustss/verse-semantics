{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse
  ( eval
  ) where

import Control.Monad ((<=<))
import Control.Monad.Abort
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Verse

import Data.ByteString (ByteString)

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Lexer
import Language.Verse.Parse
import Language.Verse.Val

eval :: ( MonadAbort Error m
        , MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , EqRef (Ref m)
        ) => ByteString -> VerseT m FrozenVal
eval = Eval.eval <=< liftEither . (desugar <=< runLexer parse)
