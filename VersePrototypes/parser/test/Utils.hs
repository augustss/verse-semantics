-----------------------------------------------------------------------------
-- |
-- Module    : Utils
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- Utility module for all parser tests. The purpose of this module is to isolate
-- the tests from the parser and its dependencies.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Utils
  ( makeTest
  , prettyTest, prettyTest_, prettyTestEP
  , roundTrip
  , broken
  , patName
  ) where

import Parser.Verse
import Language.Verse.Exp

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Control.Comonad
import Data.Text                 (Text,unpack)
import Data.Text.Encoding        (encodeUtf8)
import Prettyprinter             (Pretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter             (defaultLayoutOptions, layoutPretty)

import FrontEnd.Expr (SrcExpr)
import qualified Epic.Print as EP


dummy_file :: FilePath
dummy_file = "<parser test suite>"

verse_parser_ep :: Parser SrcExpr -> Text -> SrcExpr
verse_parser_ep p inp = parseDie p dummy_file (encodeUtf8 inp)

verse_parser :: Comonad w => Parser (w a) -> Text -> a
verse_parser p inp = parseNoLoc p dummy_file (encodeUtf8 inp)

-- only test if 'p' throws an exception or not
verse_parser_ :: Parser () -> Text -> ()
verse_parser_ p inp = parseDie p dummy_file (encodeUtf8 inp)

makeTest
  :: ( Eq      a
     , Show    a
     , Comonad w
     )
  => Parser (w a) -> (Text, a) -> TestTree
makeTest p (desc, result) = testCase cleaned_desc $ verse_parser p desc @=? result
  where
    escape_newlines = concatMap (\c -> if c == '\n' then "\\n" else [c])
    cleaned_desc    = escape_newlines $ unpack desc

-- the Epic.Print version of pretty test. Needed because the FrontEnd uses
-- pretty instead of prettyprinter.
prettyTestEP :: Parser SrcExpr -> (Text, Text) -> TestTree
prettyTestEP p (desc, result) =
  testCase cleaned_desc $ (render (verse_parser_ep p  desc)) @=? (unpack result)
  where
    render          = EP.render . EP.pPrint
    escape_newlines = concatMap (\c -> if c == '\n' then "\\n" else [c])
    cleaned_desc    = escape_newlines $ unpack desc

prettyTest
  :: ( Eq      a
     , Show    a
     , Pretty  a
     , Comonad w
     )
  => Parser (w a) -> (Text, Text) -> TestTree
prettyTest p (desc, result) =
  testCase cleaned_desc $ (render (verse_parser p desc)) @=? result
  where
    render          = renderStrict . layoutPretty defaultLayoutOptions . pretty
    escape_newlines = concatMap (\c -> if c == '\n' then "\\n" else [c])
    cleaned_desc    = escape_newlines $ unpack desc

prettyTest_ :: Parser () -> (Text, Text) -> TestTree
prettyTest_ p (desc, result) =
  testCase cleaned_desc $ (render (verse_parser_ p desc)) @=? result
  where
    render          = renderStrict . layoutPretty defaultLayoutOptions . pretty
    escape_newlines = concatMap (\c -> if c == '\n' then "\\n" else [c])
    cleaned_desc    = escape_newlines $ unpack desc

roundTrip
  :: ( Eq      a
     , Show    a
     , Pretty  a
     , Comonad w
     )
  => Parser (w a) -> Text -> TestTree
roundTrip p desc = prettyTest p (desc,desc)

broken :: Text -> TestTree -> TestTree
broken (unpack -> str) = expectFailBecause str


-----------------------------------------------
--
--                Helpers
--
-----------------------------------------------

-- just to avoid name clashes and a qualified import
patName :: IdentExp a -> Pat a
patName = Language.Verse.Exp.Name
