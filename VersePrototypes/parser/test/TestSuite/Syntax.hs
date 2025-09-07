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

module TestSuite.Syntax
  ( unitTests
  ) where

import Utils

import Parser.Verse
import Language.Verse.Exp

import Test.Tasty

--------------------------------------------------------------------------------
--
--
--------------------------------------------------------------------------------

-----------------------------------------------
--
--              Unit Tests
--
-----------------------------------------------

-- TODO: issue #87: Tests marked with 'broken' should pass but I've run out of
-- time. Tackle these as they arise.

unitTests :: TestTree
unitTests = testGroup "parser/test_data/syntax.verse"
  [ fixnums
  , chars
  , strings
  , identifiers
  , paths
  , indentation
  , indentation_and_comments
  , equal_sign
  , var_ref_set
  , return_break_yield
  , ats
  , field_accesses
  , macro_instantiation
  , extension_fields
  , enums
  , attributes
  ]


fixnums :: TestTree
fixnums =
  let passes = makeTest pNum
  in testGroup "fixnums" $
     passes <$>
     [ ("0"    , Int 0)
     , ("10"   , Int 10)
     , ("0o22" , Char '"') -- this is not octal, Verse Spec 0.15 defines this as
                           -- a char literal, but in the parser this is embedded
                           -- in pNum because the prefix is similar enough. See
                           -- 'Parser.Verse.pChar'
     , ("0u00021", Char32 '!') -- same as 0o22
     , ("0x23"   , Int 35)
     , ("0b101"  , Int 5)
     , ("0.0"    , Float 0.0)
     , ("3.14"   , Float 3.14)
     , ("4.2e+1" , Float 42)
     , ("4200e-2", Float 42)
     , ("1e6"    , Int 1000000)
     , ("10e-1"  , Float 1.0) -- treating everything negative as float, so this is 1.0 not Int 1
     , ("1.8mile" , Units
         (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 8, offset = 0}))
          (Float 1.8))
         (L (Loc (Pos {line = 1, column = 4, offset = 0}) (Pos {line = 1, column = 8, offset = 0}))
          "mile")
       )
     , ("20m/s"   , Units -- parser as '20m' / s, where s is an identifier
         (L (Loc (Pos {line = 1, column = 1, offset = 0}) (Pos {line = 1, column = 4, offset = 0}))
          (Int 20))
         (L (Loc (Pos {line = 1, column = 3, offset = 0}) (Pos {line = 1, column = 4, offset = 0})) "m")
       )
     ]

chars :: TestTree
chars =
  let passes = makeTest pChar
  in testGroup "chars" $
     passes <$>
     [ ("\'a\'", Char 'a') -- just an a character, in haskell 'a'
     ]

strings :: TestTree
strings =
  let passes = makeTest pString
  in testGroup "strings" $
     passes <$>
     [ ("\"hello world\"" , String "hello world" [])
     , ("\"This is not a {\n}multi line string\""
       , String "This is not a "
         [ (L (Loc (Pos {line = 2, column = 1, offset = 0}) (Pos {line = 2, column = 2, offset = 0}))
             (List [])
           , L (Loc (Pos {line = 2, column = 2, offset = 0}) (Pos {line = 2, column = 19, offset = 0}))
             "multi line string")]
       )
     , ("\"You have { count } points\""
       , String "You have "
         [(L (Loc (Pos {line = 1, column = 13, offset = 0}) (Pos {line = 1, column = 20, offset = 0}))
           (Pat (patName (IdentName "count")))
          , L (Loc (Pos {line = 1, column = 20, offset = 0}) (Pos {line = 1, column = 27, offset = 0}))
            " points")]
        )
     , ("\"abcåäö\"", String "abc\229\228\246" [] )
     , ("\"abc{42}def\"", String "abc"
         [(L (Loc (Pos {line = 1, column = 6, offset = 0}) (Pos {line = 1, column = 9, offset = 0}))
           (Int 42)
          , L (Loc (Pos {line = 1, column = 9, offset = 0}) (Pos {line = 1, column = 12, offset = 0}))
            "def")]
       )
     ]

identifiers :: TestTree
identifiers =
  let passes = makeTest pIdentT
  in testGroup "identifiers" $
     passes <$>
     [ ("apa1", "apa1")
     ]

paths :: TestTree
paths =
  let passes = makeTest pPath
  in testGroup "paths" $
     [ broken "unexpected lparen" $ passes ("(monkey:)apa3", IdentName "I'm broken")
     , passes ("/apa2", IdentPath
         (Path (L (Loc (Pos {line = 1, column = 2, offset = 0}) (Pos {line = 1, column = 6, offset = 0})) "apa2")
          [])
       )
     , passes ("/abc.def@whatever.com/apa4"
       , IdentPath
         (Path
          (L (Loc (Pos {line = 1, column = 2, offset = 0}) (Pos {line = 1, column = 22, offset = 0})) "abc.def@whatever.com")
          [ ( Nothing
            , L (Loc (Pos {line = 1, column = 23, offset = 0}) (Pos {line = 1, column = 27, offset = 0})) "apa4")
          ])
       )
     , passes ("/abc.def@whatever.com/(/here/be/path:)apa4"
       , IdentPath
         (Path
          (L (Loc (Pos {line = 1, column = 2, offset = 0}) (Pos {line = 1, column = 22, offset = 0})) "abc.def@whatever.com")
          [ ( Just (Path (L (Loc (Pos {line = 1, column = 25, offset = 0}) (Pos {line = 1, column = 29, offset = 0})) "here")
                   [( Nothing
                    , L (Loc (Pos {line = 1, column = 30, offset = 0}) (Pos {line = 1, column = 32, offset = 0})) "be"
                    )
                   , ( Nothing
                     , L (Loc (Pos {line = 1, column = 33, offset = 0}) (Pos {line = 1, column = 37, offset = 0})) "path")
                   ]
                  )
            , L (Loc (Pos {line = 1, column = 39, offset = 0}) (Pos {line = 1, column = 43, offset = 0})) "apa4"
            )
          ])
       )
     , passes ("/abc.def@whatever.com/(/here/be/path:)apa4/(/here/be/path:)apa4/(/here/be/path:)apa4"
       , IdentPath
         (Path
          (L (Loc (Pos {line = 1, column = 2, offset = 0}) (Pos {line = 1, column = 22, offset = 0})) "abc.def@whatever.com")
           [ ( Just (Path (L (Loc (Pos {line = 1, column = 25, offset = 0}) (Pos {line = 1, column = 29, offset = 0})) "here")
                     [ (Nothing,L (Loc (Pos {line = 1, column = 30, offset = 0}) (Pos {line = 1, column = 32, offset = 0})) "be")
                     , (Nothing,L (Loc (Pos {line = 1, column = 33, offset = 0}) (Pos {line = 1, column = 37, offset = 0})) "path")
                     ])
             , L (Loc (Pos {line = 1, column = 39, offset = 0}) (Pos {line = 1, column = 43, offset = 0})) "apa4"
             )
          , ( Just (Path (L (Loc (Pos {line = 1, column = 46, offset = 0}) (Pos {line = 1, column = 50, offset = 0})) "here")
                    [( Nothing
                     , L (Loc (Pos {line = 1, column = 51, offset = 0}) (Pos {line = 1, column = 53, offset = 0})) "be"
                     )
                    , ( Nothing
                      , L (Loc (Pos {line = 1, column = 54, offset = 0}) (Pos {line = 1, column = 58, offset = 0})) "path"
                      )
                    ])
            , L (Loc (Pos {line = 1, column = 60, offset = 0}) (Pos {line = 1, column = 64, offset = 0})) "apa4"
            )
          , ( Just (Path (L (Loc (Pos {line = 1, column = 67, offset = 0}) (Pos {line = 1, column = 71, offset = 0})) "here")
                    [( Nothing
                     , L (Loc (Pos {line = 1, column = 72, offset = 0}) (Pos {line = 1, column = 74, offset = 0})) "be"
                     )
                    , ( Nothing
                      , L (Loc (Pos {line = 1, column = 75, offset = 0}) (Pos {line = 1, column = 79, offset = 0})) "path"
                      )
                    ])
            , L (Loc (Pos {line = 1, column = 81, offset = 0}) (Pos {line = 1, column = 85, offset = 0})) "apa4"
            )
          ])
       )
     , broken "throws exception on lparen" $ passes ("(/mail@path/xx:)Value", IdentName "")
     , broken "throws exception on lparen" $ passes ("(/path/xx:)Value"     , IdentName "")
     , broken "throws exception on lparen" $ passes ("(/path:)Value"        , IdentName "")
     ]

indentation :: TestTree
indentation =
  let passes = prettyTest pcExpr
  in testGroup "indentation" $
     passes <$>
     [ ("A:\n  B:\n  C", "((A : B) : C)")
     ]

indentation_and_comments :: TestTree
indentation_and_comments =
  let passes = prettyTest pList
  in testGroup "indentation_and_comments" $
     [ broken "C is missed"
       $ passes ("A:\n  B\n\n  # Not more to the left\n  C", "A{\n  B\n  C\n}")
     , passes ("A{ B ; C }", "[A{ BC }]")
     -- A Comment with less indentation breaks the block
     , broken "C is missed even on newline"
       $ passes ("A:\n  B\n # Only B is part of A\n  C", "A{ B }; C") -- may fail from pretty
     , broken "misses D and E"
       $ passes ("A:\n       B\n       <# Confusing but will work #> C\n <# #> D   # D is only indented one space\n       E"
                , "A { B; C }; D ; E"
                )
     , broken "C is missed"
       $ passes ("A:\n   B\n   <# block comment\n#>C", "A{\n  B\n  C\n}")
     -- Indentation comments do end with blank lines. Should they?
     , broken "B is missed"
       $ passes ("A:\n  <#>\n    Some comment\n    That ends soon\n\n    B # Will be inside A"
       , "A { B }") -- this one is broken
     ]

equal_sign :: TestTree
equal_sign =
  let passes = prettyTest pList
  in testGroup "equal_sign" $
  [ passes ("A:\n  B:X =\n C", "[((A : B) : X) = C]")
  , broken "D is missed"
    $ passes ("A:\n  B:X =\n  C\n  D", "")
  , passes ("A:\n  B:X = C =\n D", "[((A : B) : X) = C = D]")
  , broken "E is missed" $ passes ("A:\n  B:X = C =\n  D\n  E", "")
  , passes ("A or B : X = C"           , "[(A or B : X) = C]")
  , broken "D is missed"
    $ passes ("A:\n  B =\n  C\n    D"    , "")
  , passes ("A { B {} }; C; D;"        , "[A{ B{  } }, C, D]")
  , broken "E is missed"
    $ passes ("A:\n  B = C =\n  D\n    E", "")
  , passes ("X = 1"                    ,  "[X = 1]")
  , passes ("X = \n  1"                ,  "[X = 1]")
  ]

var_ref_set :: TestTree
var_ref_set =
  let passes = prettyTest pExpr
  in testGroup "var_ref_set" $
  [ passes ("var X:int := 1"  , "(var X : int) := 1")
  , passes ("var X:int = 1"   , "(var X : int) = 1")
  , passes ("set X = 1"       , "set X = 1")
  , passes ("ref X:int := 1"  , "(ref X : int) := 1")
  , passes ("ref X:int = 1"   , "(ref X : int) = 1")
  , passes ("alias X:int := Y", "(alias X : int) := Y")
  , passes ("alias X:int = Y" , "(alias X : int) = Y")
  ] ++
  [ broken "produces redundant set" $ passes ("set X -= 1" , "set X -= 1")
  , broken "produces redundant set" $ passes ("set X += 1" , "set X += 1")
  , broken "produces redundant set" $ passes ("set X *= 1" , "set X *= 1")
  , broken "produces redundant set" $ passes ("set X /= 1" , "set X /= 1")
  ]

return_break_yield :: TestTree
return_break_yield =
  let passes = prettyTest pExpr
  in testGroup "var_ref_set" $
  passes <$>
  [ ("return", "return")
  , ("break" , "break")
  , ("yield" , "yield")
  ]

ats :: TestTree
ats =
  let passes = prettyTest pExpr
  in testGroup "ats" $
  passes <$>
  [ ("@editable\nX:int = 0"       , "@editable (X : int) = 0")
  , ("@editable @public X:int = 0", "@editable @public (X : int) = 0")
  , ("@editable; X:int = 0"       , "@editable (X : int) = 0")
  ]

field_accesses :: TestTree
field_accesses =
  let passes = prettyTest pExpr
  in testGroup "field_accesses" $
  passes <$>
  [ ("xxx.c"  , "xxx .c")
  , ("xxx .c" , "xxx .c")
  , ("xxx .<# the comment prevents the '.' from seeing the ' ' before 'c' #> c", "xxx .c")
  ]

macro_instantiation :: TestTree
macro_instantiation =
  let passes = prettyTest pExpr
  in testGroup "macro_instantiation" $
  passes <$>
  [ ("xxx . c", "xxx{\n  c\n}")
  , ("xxx. c" , "xxx{\n  c\n}")
  ]

extension_fields :: TestTree
extension_fields =
  let passes = prettyTest pExpr
  in testGroup "extension_fields" $
  passes <$>
  [ ("x.field", "x .field")
  , ("0.field", "0 .field")
  , ("1.field", "1 .field")
  , ("2.field", "2 .field")
  ]

-- The enum macro is special since it allows ',' in addition to ';' between its subexpressions.
enums :: TestTree
enums =
  let passes = prettyTest pExpr
  in testGroup "enums" $
  [ passes ("X := enum { A; B, C }"       , "X := enum{\n  A\n  B\n  C\n}")
  , broken "Unexpected A (65)"
    $ passes ("Y := enum:\n  A, B\n  C; D"  , "Y := enum{\n  A\n  B\n  C\n  D\n}")
  , broken "Unexpected A (65)"
    $ passes ("Z := enum:\n  A\n  B\n  C; D", "Z := enum{\n  A\n  B\n  C\n  D\n}")
  ]

attributes :: TestTree
attributes =
  let passes = prettyTest pExpr
  in testGroup "attributes" $
  [ broken "Unexpected A (65)"
    $ passes ("Z := enum<whatever>:\n  A\n  @deprecated B\n  @deprecated\n  C"
    , "Z := enum<whatever>{\n  A\n  @deprecated\n  B\n  @deprecated\n  C\n}")
  ]
