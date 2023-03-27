{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse
  ( eval
  ) where

import Control.Monad ((<=<))
import Control.Monad.Error.Class
import Control.Monad.Supply
import Control.Monad.Var
import Control.Monad.Verse.Class

import Data.ByteString (ByteString)
import Data.Fix

import Language.Verse.Desugar
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Lexer
import Language.Verse.Parse
import Language.Verse.Val

eval :: ( MonadError Error m
        , MonadSupply Label m
        , MonadVerse m
        , EqVarRef (VarRef m)
        ) => ByteString -> m (Fix (Val m))
eval = Eval.eval <=< liftEither . (desugar <=< runLexer parse)
