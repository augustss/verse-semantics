-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.VerseTest
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module defines tests for the parser to test .versetest files. See the
-- .versetest files in $ROOT/versetests. This module is purposefully messy to
-- read. Each test here encodes a failure the parser experienced when parsing a
-- versetest file.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestSuite.VerseTest
  ( unitTests
  ) where

import Utils

import Parser.Verse
import Language.Verse.Exp

import Test.Tasty


-----------------------------------------------
--
--              Unit Tests
--
-----------------------------------------------

unitTests :: TestTree
unitTests = testGroup "parser/test_data/all.verse"
  [ comments
  , sequences
  , tuples
  , options
  , comparisons
  ]

comments :: TestTree
comments =
  let passes            = prettyTest pList
      test_line_comment = prettyTest_ pLineCmt
      test_comment      = prettyTest pComment
      test_pSpace       = prettyTest pSpace
      test_pLine        = prettyTest_ pLine
  in testGroup "comments_and_spaces" $
  [ test_line_comment ("# Test line comments types \n", "()")
  , test_comment      ("# Test comment parser but with a line \n", "<comment>")
  , test_pSpace       ("# test space parser ", "<space>")
  , test_pLine        ("# test pLine, a line comment", "()")
  , passes            ("# Test types \n", "[]")
  ]

sequences :: TestTree
sequences =
  let passes  = makeTest pcExpr
      bPasses = makeTest $ lexeme (pcBraces pcExpr)
      passes' = prettyTest pcExpr
  in testGroup "sequences" $
  [ passes ("1;2" -- Test that 'l;r' is sequence
           , List [ L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 2, offset = 0})) (Int 1)
                  , L (Loc (Pos {line = 1, column = 3, offset = 0}) (Pos {line = 1, column = 4, offset = 0})) (Int 2)
                  ]

           )
  , passes ("1;", Int 1)          -- test that semicolon is still an expr terminator
  , passes ("1;\n", Int 1)        -- test that semicolon is still an expr terminator
  , passes ("x:=1; x"
           , List [ L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 5, offset = 0})) (InfixColonEqual (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 2, offset = 0})) (Pat (Name (IdentName "x")))) (L (Loc (Pos {line = 1, column = 4, offset = 0}) (Pos {line = 1, column = 5, offset = 0})) (Int 1)))
                  , L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (Pat (Name (IdentName "x")))
                  ]
           )

  , passes ("1=(1,2)"
           , List [ L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 2, offset = 0})) (Int 1) :=: L (Loc (Pos {line = 1, column = 3, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (Tuple [L (Loc (Pos {line = 1, column = 4, offset = 0}) (Pos {line = 1, column = 5, offset = 0})) (Int 1) , L (Loc (Pos {line = 1, column = 6, offset = 0}) (Pos {line = 1, column = 7, offset = 0})) (Int 2)]))
                  ]
           )

  , passes ("x:any; y:any; x=y; y=1"
           , List [ L (Loc (Pos {line = 1, column = 2, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) (ExpInfixColon (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 2, offset = 0})) (Pat (Name (IdentName "x")))) (L (Loc (Pos {line = 1, column = 3, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) (Pat (Name (IdentName "any")))))
                  , L (Loc (Pos {line = 1, column = 9, offset = 0}) (Pos {line = 1, column = 13, offset = 0})) (ExpInfixColon (L (Loc (Pos {line = 1, column = 8, offset = 0}) (Pos {line = 1, column = 9, offset = 0})) (Pat (Name (IdentName "y")))) (L (Loc (Pos {line = 1, column = 10, offset = 0}) (Pos {line = 1, column = 13, offset = 0})) (Pat (Name (IdentName "any")))))
                  , L (Loc (Pos {line = 1, column = 15, offset = 0}) (Pos {line = 1, column = 18, offset = 0})) (L (Loc (Pos {line = 1, column = 15, offset = 0}) (Pos {line = 1, column = 16, offset = 0})) (Pat (Name (IdentName "x"))) :=: L (Loc (Pos {line = 1, column = 17, offset = 0}) (Pos {line = 1, column = 18, offset = 0})) (Pat (Name (IdentName "y"))))
                  , L (Loc (Pos {line = 1, column = 20, offset = 0}) (Pos {line = 1, column = 23, offset = 0})) (L (Loc (Pos {line = 1, column = 20, offset = 0}) (Pos {line = 1, column = 21, offset = 0})) (Pat (Name (IdentName "y"))) :=: L (Loc (Pos {line = 1, column = 22, offset = 0}) (Pos {line = 1, column = 23, offset = 0})) (Int 1))
                  ]
           )

  , passes ("array{1;2;3}"
           , List [ L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 13, offset = 0})) (Inst (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) (Pat (Name (IdentName "array")))) (L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 13, offset = 0})) (List [L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (Int 1),L (Loc (Pos {line = 1, column = 9, offset = 0}) (Pos {line = 1, column = 10, offset = 0})) (Int 2),L (Loc (Pos {line = 1, column = 11, offset = 0}) (Pos {line = 1, column = 12, offset = 0})) (Int 3)])))]
            )

  , passes ("x:=(1,2); y:=(1,3); one{x=y}"
           , List [ L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 9, offset = 0})) (InfixColonEqual (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 2, offset = 0})) (Pat (Name (IdentName "x")))) (L (Loc (Pos {line = 1, column = 4, offset = 0}) (Pos {line = 1, column = 9, offset = 0})) (Tuple [L (Loc (Pos {line = 1, column = 5, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) (Int 1),L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (Int 2)])))
                  , L (Loc (Pos {line = 1, column = 11, offset = 0}) (Pos {line = 1, column = 19, offset = 0})) (InfixColonEqual (L (Loc (Pos {line = 1, column = 11, offset = 0}) (Pos {line = 1, column = 12, offset = 0})) (Pat (Name (IdentName "y")))) (L (Loc (Pos {line = 1, column = 14, offset = 0}) (Pos {line = 1, column = 19, offset = 0})) (Tuple [L (Loc (Pos {line = 1, column = 15, offset = 0}) (Pos {line = 1, column = 16, offset = 0})) (Int 1),L (Loc (Pos {line = 1, column = 17, offset = 0}) (Pos {line = 1, column = 18, offset = 0})) (Int 3)])))
                  ,L (Loc (Pos {line = 1, column = 25, offset = 0}) (Pos {line = 1, column = 29, offset = 0})) (One (L (Loc (Pos {line = 1, column = 25, offset = 0}) (Pos {line = 1, column = 29, offset = 0})) (L (Loc (Pos {line = 1, column = 25, offset = 0}) (Pos {line = 1, column = 26, offset = 0})) (Pat (Name (IdentName "x"))) :=: L (Loc (Pos {line = 1, column = 27, offset = 0}) (Pos {line = 1, column = 28, offset = 0})) (Pat (Name (IdentName "y"))))))
                  ]
           )

  , passes ("all{1..4}"
           , List [L (Loc (Pos {line = 1, column = 5, offset = 0}) (Pos {line = 1, column = 10, offset = 0})) (All (L (Loc (Pos {line = 1, column = 5, offset = 0}) (Pos {line = 1, column = 10, offset = 0})) (L (Loc (Pos {line = 1, column = 5, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) (Int 1) :..: L (Loc (Pos {line = 1, column = 8, offset = 0}) (Pos {line = 1, column = 9, offset = 0})) (Int 4))))]
           )

  , bPasses ("{ all{1..4} }"
           , List [ L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 12, offset = 0})) (All (L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 12, offset = 0})) (L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 8, offset = 0})) (Int 1) :..: L (Loc (Pos {line = 1, column = 10, offset = 0}) (Pos {line = 1, column = 11, offset = 0})) (Int 4))))
                  ]
           )

  , passes (" 3 ", List [L
                         (Loc
                          (Pos {line = 1, column = 2, offset = 0})
                          (Pos {line = 1, column = 3, offset = 0}))
                          (Int 3)])
  , passes' ("1|2;3|4",    "(1 | 2)\n(3 | 4)")
  , passes' ("1|2; 3|||4", "(1 | 2)\n(3 ||| 4)")
  , passes' ("1|||2; 3|4", "(1 ||| 2)\n(3 | 4)")
  , passes' ("1|||2",      "(1 ||| 2)")
  ]

tuples :: TestTree
tuples =
  let passes = prettyTest $ pcExpr
  in testGroup "tuples" $
  [ passes ("( (1, 2, 3) )"             , "(1, 2, 3)")
  , passes ("( (1, 2, 3)\n, (1, 1, 5) )", "((1, 2, 3), (1, 1, 5))")
  ]


options :: TestTree
options =
  let passes = prettyTest $ pcExpr
      passes' = makeTest  $ pcExpr
  in testGroup "options" $
  [ passes ("option{ }"        , "option {\n\n}")
  , passes ("option{ 2 }"      , "option {\n  2\n}")
  , passes ("a := option{ 2 }" , "a := option {\n  2\n}")
  , passes ("a := option{ }"   , "a := option {\n\n}")
  ] ++
  -- why does option parse to a list?
  -- FIXME: parse directly to Option
  [ passes' ("option{ 2 }"      , List [L (Loc (Pos {line = 1, column = 7, offset = 0}) (Pos {line = 1, column = 12, offset = 0})) (Option (Just (L (Loc (Pos {line = 1, column = 9, offset = 0}) (Pos {line = 1, column = 10, offset = 0})) (Int 2))))]
             )
  ]


comparisons :: TestTree
comparisons =
  let passes = prettyTest $ pcExpr
  in testGroup "comparisons" $
  [ passes ("3<>4", "(3 <> 4)")
  , passes ("3<>3", "(3 <> 3)")
  , passes ("3<>2", "(3 <> 2)")
  ]
