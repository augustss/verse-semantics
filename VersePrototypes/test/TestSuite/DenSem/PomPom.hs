-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.DenSem.PomPom
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module defines semantic equivalences as property-based tests. We expect
-- that these equivalences hold in our denotational semantics. Right now
-- (Nov. 6th 2025) we only test the PomPom semantic function.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wall          #-}

module TestSuite.DenSem.PomPom
  ( properties
  ) where

import qualified PomPom (den)
import TestSuite.DenSem.Properties

import Test.Tasty
import Test.Tasty.Falsify (testProperty)


-----------------------------------------------
--
--              Properties
--
-----------------------------------------------

-- See Note [Reading the property-based test output] for how to read failures.
properties :: TestTree
properties = testGroup "PomPom.den: Semantic Identities"
  $ map ($ PomPom.den)
  [ testProperty "(e1 ||| e2) | e3 === (e1 | e3) ||| (e2 | e3)"
    . uChoiceDistributesOverChoice

  , testProperty "e | fail === e"
    . failIsChoiceUnit

  , testProperty "e1 | (e2 | e3) === (e1 | e2) | e3"
    . choiceIsAssociative

  , testProperty "e ||| e === e"
    . uChoiceIsIdempotent

  , testProperty "e1 ||| (e2 ||| e3) === (e1 ||| e2) ||| e3"
    . uChoiceIsAssociative

  , testProperty "k;e === e"
    . literalsAreSeqUnit

  , testProperty "(e1;e2);e3 === e1;(e2;e3)"
    . seqIsAssociative

  , testProperty "(e1 | e2); e3 === (e1; e3) | (e2; e3)"
    . choiceDistributesOverSequence
  ]
