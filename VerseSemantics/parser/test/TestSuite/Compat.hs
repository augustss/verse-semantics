-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Compat
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- The verse parser implements the grammar presented in the Verse Spec 0.15. The
-- grammar that the spec defines is closer to ShipVerse (the verse in UEFN) than
-- the verse the theory team is crafting (MaxVerse). The Parser.Verse.Compat
-- module extends the spec defined grammar with terms that exist in
-- MaxVerse. This module tests those extensions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestSuite.Compat
  ( unitTests
  ) where

import Utils

import Parser.Verse
import Parser.Compat

import Test.Tasty


-----------------------------------------------
--
--              Unit Tests
--
-----------------------------------------------

unitTests :: TestTree
unitTests = testGroup "parser/compat"
  [ exists
  , applications
  ]

exists :: TestTree
exists =
  let passes = prettyTest pList
  in testGroup "exists" $
  [ passes ("exists a b c { a = 1 }",  "[exists [a, b, c]{ a = 1 }]")
  , passes ("exists aC    { aC = 1 }", "[exists [aC]{ aC = 1 }]")
  ]

applications :: TestTree
applications =
  let passes = prettyTest pcExpr
      passes' = prettyTestEP (toSrcExpr <$> pcExpr)
  in testGroup "applications" $
  [ passes  ("   ((1,2) ||| (1,0))[0]  ", "((1, 2) ||| (1, 0))[0]")
  , passes' ("  ((1,2) ||| (1,0))[0]   ", "((1, 2) ||| (1, 0))[0]")
  , passes  ("  ((1,2) ||| (1,0))  [0]   ", "((1, 2) ||| (1, 0))[0]")
  , passes' ("  ((1,2) ||| (1,0))  [0]   ", "((1, 2) ||| (1, 0))[0]")
  ]
