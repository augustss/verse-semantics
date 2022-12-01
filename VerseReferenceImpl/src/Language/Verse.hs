{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse
  ( eval
  ) where

import Control.Monad ((<=<))
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Ref

import Data.ByteString (ByteString)
import Data.Fix
import Data.Ref

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Lexer
import Language.Verse.Parse
import Language.Verse.Simplify
import Language.Verse.Val

eval :: ( MonadError Error m
        , MonadFix m
        , MonadRef m
        , EqRef (Ref m)
        ) => ByteString -> m [Fix Val]
eval = Eval.eval <=< liftEither . (simplify <=< desugar <=< runLexer parse)
