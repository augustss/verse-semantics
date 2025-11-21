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

import qualified PomPom (denS, defaultConfig)
import TestSuite.DenSem.Properties

import Test.Tasty
import Test.Tasty.Hedgehog
import TestSuite.Utils

-----------------------------------------------
--
--              Properties
--
-----------------------------------------------

properties :: TestTree
properties =
  let go = (liftIO . fmap fst . PomPom.denS PomPom.defaultConfig False)
  in testGroup "PomPom.den: Semantic Identities"
  $ map ($ go)
  [ -- this does not hold in general so don't test it
    -- testProperty "(e1 ||| e2) | e3 === (e1 | e3) ||| (e2 | e3)"
    -- . uChoiceDistributesOverChoice
    testProperty "e | fail === e"
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

  , testProperty "fun( e1a | e1b ){e2}[a3] === fun(e1a){e2}[a3] | fun(e1b){e2}[a3]"
    . funDomainsDistributeOverChoice

  , testProperty "fun( e1a ||| e1b ){e2}[a3] === fun(e1a){e2}[a3] ||| fun(e1b){e2}[a3]"
    . funDomainsDistributeOverUChoice

  ]
