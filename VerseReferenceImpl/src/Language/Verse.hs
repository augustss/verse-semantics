{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse
  ( eval
  ) where

import Control.Monad ((<=<))
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Throw
import Control.Monad.Verse

import Data.ByteString (ByteString)
import Data.Fix

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Lexer
import Language.Verse.Parse
import Language.Verse.Val

eval :: ( MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , MonadThrow Error m
        , Eq (Ref m (Var m (Val m)))
        ) => ByteString -> VerseT m (Frozen (Val m))
eval = Eval.eval <=< liftEither . (desugar <=< runLexer parse)
