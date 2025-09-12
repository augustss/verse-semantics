-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Syntax
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- syntax.verse: Each of these tests come from the file
-- $ROOT/VersePrototypes/parser/test_data/syntax.verse
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestSuite.Epic.PomSet
  ( properties
  ) where

import Epic.PomSet

import Test.Tasty
import Test.Tasty.Falsify (testProperty, Gen, testFailed, assert)

import Test.Falsify.Property            (Property, gen)
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Generator as Gen
-- import Test.Falsify.Range               (Range)
import qualified Test.Falsify.Range     as Range

import Control.Monad
import qualified Data.List.NonEmpty as NE
--------------------------------------------------------------------------------
--
--
--------------------------------------------------------------------------------

-----------------------------------------------
--
--              Properties
--
-----------------------------------------------

properties :: TestTree
properties = testGroup "PomSet"
  [ testProperty "+++ left unit"      append_left_unit
  , testProperty "+++ right unit"     append_right_unit
  , testProperty "+++ is associative" append_assoc
  , testProperty "||| is commutative" union_commut
  , testProperty "||| is associative" union_assoc
  , testProperty "id = factor . distribute" factor_is_distrib_inverse
  ]

append_left_unit :: Property ()
append_left_unit = do
  ps <- gen pom
  r  <- gen $ genEmpty
  unless ((r +++ ps) == ps)
    $ testFailed "left unit"

append_right_unit :: Property ()
append_right_unit = do
  ps <- gen pom
  r  <- gen $ genEmpty
  unless ((ps +++ r) == ps)
    $ testFailed "right unit"

append_assoc :: Property ()
append_assoc = do
  a <- gen pom
  b <- gen pom
  c <- gen pom
  assert $
    P.eq
    P..$ ("lhs", toList $ (a +++ b) +++ c)
    P..$ ("rhs", toList $ (a +++ (b +++ c)))

union_commut :: Property ()
union_commut = do
  l <- gen pom
  r <- gen pom
  assert $
    P.eq
    P..$ ("lhs", l ||| r)
    P..$ ("rhs", r ||| l)

union_assoc :: Property ()
union_assoc = do
  a <- gen pom
  b <- gen pom
  c <- gen pom
  assert $
    P.eq
    P..$ ("lhs", toList $ (a ||| b) ||| c)
    P..$ ("rhs", toList $ (a ||| (b ||| c)))

factor_is_distrib_inverse :: Property ()
factor_is_distrib_inverse = do
  a <- gen pom
  b <- gen pom
  c <- gen pom
  assert $
    P.eq
    P..$ ("lhs", ((a ||| b) +++ c))
    P..$ ("rhs", factor $ distribute ((a ||| b) +++ c))

genEmpty :: Gen (Pom a)
genEmpty = pure Empty

genInt :: Gen Int
genInt = Gen.inRange (Range.between (-0xFF,0xFF))

pom :: Gen (Pom Int)
pom = pomSet (0, 512) genInt

pomSet :: (Word, Word) -> Gen a -> Gen (Pom a)
pomSet size genElem = do
  depth <- Gen.inRange (Range.between size)
  let genUnit  = Unit <$> genElem
      genEmpty = pure Empty
      composite n = Gen.oneof
        $ NE.fromList [ genUnit
                      , genEmpty
                      , PomAppend <$> go (n-1) <*> go (n-1)
                      , PomUnion  <$> go (n-1) <*> go (n-1)
                      ]
      go 0 = genEmpty
      go n = Gen.choose genUnit (composite n)
  go depth
