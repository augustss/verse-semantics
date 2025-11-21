-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Utils
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This is a compat module for the testsuite. It aggregates all the stuff that
-- TestSuite needs to write tests into a single module.
--
-----------------------------------------------------------------------------

module TestSuite.Utils
  ( Set
  , ENV
  , module Epic.Print
  , module FrontEnd.Expr
  , putExpr
  , annotate
  , liftIO
  ) where

import Set(Set)
import ENVP (ENV)
import Epic.Print
import FrontEnd.Expr

import Control.Monad.IO.Class

import qualified Hedgehog as H

-- | convience function for printing generated ASTs.
putExpr :: MonadIO io => SrcExpr -> io ()
putExpr = liftIO . putStrLn . show . pPrint

annotate :: (H.MonadTest m, Pretty a) => String -> a -> m ()
annotate str x = H.annotate $ str ++ show (pPrint x)
