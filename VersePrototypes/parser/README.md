## The Verse Parser

### What

This is the verse parser as a library. It parses source verse concrete syntax to `Language.Verse`.

Clients are expected to write their own translation from `Exp SimpleName` to their own AST.

### Testing

We use [tasty](https://hackage.haskell.org/package/tasty) as the test framework
with [tasty-hunit](https://hackage.haskell.org/package/tasty-hunit) to test
specific cases for the parser.

The top level test runner is in `parser/test/Test.hs`. For the moment this also
includes all the tests defined for the parser.

#### Running the test suite

Do

```
$ cabal test verse-parser-test
```

and you should see the output of running all tests like so:

```
$ cabal test verse-parser-test
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - VersePrototypes-0.1.0.0 (test:verse-parser-test) (first run)
Preprocessing test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Building test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Running 1 test suites...
Test suite verse-parser-test: RUNNING...
Parser
  Unit Tests
    parser/test_data/syntax.verse
      fixnums
        0:                                                                                                         OK
        10:                                                                                                        OK
        0o22:                                                                                                      OK
        0u00021:                                                                                                   OK
        0x23:                                                                                                      OK
        0b101:                                                                                                     OK
        0.0:                                                                                                       OK
        3.14:                                                                                                      OK
        4.2e+1:                                                                                                    OK
        4200e-2:                                                                                                   OK
        1e6:                                                                                                       OK
        10e-1:                                                                                                     OK
        1.8mile:                                                                                                   OK
        20m/s:                                                                                                     OK
```

Each test has a name and belongs to a test group. In the above output the test
named `0` belongs to the groups `fixnum`, `parser/test_data/syntax.verse`, `Unit
Tests` and `Parser`.

To run a specific test use the `-p` tasty option:

```
$ cabal test verse-parser-test --test-options='-p 1.8mile'
```

Here I asked `tasty` to only run the test named `1.8mile`.

And you'll see:

```
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - VersePrototypes-0.1.0.0 (test:verse-parser-test) (first run)
Preprocessing test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Building test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Running 1 test suites...
Test suite verse-parser-test: RUNNING...
Parser
  Unit Tests
    parser/test_data/syntax.verse
      fixnums
        1.8mile: OK

All 1 tests passed (0.00s)
Test suite verse-parser-test: PASS
```

You can also only run groups of tests, for example running all tests in the
`fixnums` group:

```
$ cabal test verse-parser-test --test-options='-p fixnums'
```

which outputs:

```
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - VersePrototypes-0.1.0.0 (test:verse-parser-test) (first run)
Preprocessing test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Building test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Running 1 test suites...
Test suite verse-parser-test: RUNNING...
Parser
  Unit Tests
    parser/test_data/syntax.verse
      fixnums
        0:       OK
        10:      OK
        0o22:    OK
        0u00021: OK
        0x23:    OK
        0b101:   OK
        0.0:     OK
        3.14:    OK
        4.2e+1:  OK
        4200e-2: OK
        1e6:     OK
        10e-1:   OK
        1.8mile: OK
        20m/s:   OK

All 14 tests passed (0.00s)
Test suite verse-parser-test: PASS
```

or you can fuzzy match and run every test that contains the string `10`:

```
$ cabal test verse-parser-test --test-options='-p 10'
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - VersePrototypes-0.1.0.0 (test:verse-parser-test) (first run)
Preprocessing test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Building test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Running 1 test suites...
Test suite verse-parser-test: RUNNING...
Parser
  Unit Tests
    parser/test_data/syntax.verse
      fixnums
        10:    OK
        0b101: OK
        10e-1: OK

All 3 tests passed (0.00s)
```

#### Adding Tests

To add a test:

Define a test group to put the test or create a new test group. For example if
you want to add a test to parse a `fixnum` then add it to the `fixnum` group. If
the group does not exists then create it following the examples in
`parser/test/Test.hs`:

```
fixnums :: TestTree
fixnums =
let passes = make_test pNum
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
   ...
   , -- your new test here as an element of this list
```

The general strategy is to use a helper function like `make_test` to create a
test that `tasty-hunit` understands. Here is the type of `make_test`:

```
 make_test
 :: ( Eq      a
    , Show    a
    , Comonad w
    )
 => Parser (w a) -> (Text, a) -> TestTree
 ```

`make_test` takes a parser, and then a tuple where `fst` is the name of the test
and `a` is the expected output. `TestTree` is a type from `tasty` so `make_test`
is an adapter around `tasty`. When you add new tests please try to stick to
using these adapters instead of coding directly to `tasty`. Now you should be
able to read the `fixnums` code and make sense of it:

- We define a function `passes` that uses the `pNum` parser.
- `passes` then takes a tuple of `(name, expected_result)` and creates a `tasty` test.
- We then fmap over the list to generate all the `fixnum` tests displayed in the
  test suite output.

#### Marking a test broken

Use the `broken` helper to mark a test broken:

```
broken :: Text -> TestTree -> TestTree
broken (unpack -> str) = expectFailBecause str
```

`broken` wraps a `tasty` test with a description that will be included in the
output of the test when the test suite runs. For example here is the output of
the `equal_sign` parse tests:

```
$ cabal test verse-parser-test --test-options='-p equal_sign'
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - VersePrototypes-0.1.0.0 (test:verse-parser-test) (first run)
Preprocessing test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Building test suite 'verse-parser-test' for VersePrototypes-0.1.0.0...
Running 1 test suites...
Test suite verse-parser-test: RUNNING...
Parser
  Unit Tests
    parser/test_data/syntax.verse
      equal_sign
        A:\n  B:X =\n C:           FAIL (expected: C gets missed)
          parser/test/Test.hs:70:
          expected: "A{\n  (B : X) = \n}"
           but got: "A{\n  (B : X) = C\n}" (expected failure)
        A:\n  B:X =\n  C\n  D:     OK
        A:\n  B:X = C =\n D:       FAIL (expected: exception on newline)
          Exception: "<parser test suite>" (line 1, column 3):
          unexpected "\n"
          expecting "<#", "<#>" or '('
          CallStack (from HasCallStack):
            error, called at parser/Parser/Verse.hs:146:17 in VersePrototypes-0.1.0.0-inplace-verse-parser:Parser.Verse (expected failure)
        A:\n  B:X = C =\n  D\n  E: OK
        A or B : X = C:            OK
        A:\n  B =\n  C\n    D:     OK
        A { B {} }; C; D;:         OK
        A:\n  B = C =\n  D\n    E: FAIL (expected: should newlines be preserved?)
          parser/test/Test.hs:70:
          expected: "A{\n  B = C = D\n  E\n}"
           but got: "A { B = (C = D); E}" (expected failure)
        X = 1:                     OK
        X = \n  1:                 OK

All 10 tests passed (0.01s)
Test suite verse-parser-test: PASS
```

The description following `FAIL` is the description supplied to `broken`. For
example, the message `C gets missed` in the test `A:\n B:X =\n C` comes from
`broken`. Here is the definition of the that `A:\n B:X =\n C`:

```
equal_sign :: TestTree
equal_sign =
  let passes = pretty_test pExpr
  in testGroup "equal_sign" $
  [ broken "C gets missed" $ passes ("A:\n  B:X =\n C", "A{\n  (B : X) = C\n}")
...
```

So to mark a test broken we just wrap the test with `broken`.

One can mix `passes` and `broken` like this:

```
equal_sign :: TestTree
equal_sign =
  let passes = pretty_test pExpr
  in testGroup "equal_sign" $
  [ broken "C gets missed"        $ passes ("A:\n  B:X =\n C", "A{\n  (B : X) = C\n}")
  , passes ("A:\n  B:X =\n  C\n  D", "A{\n  (B : X) = \n  C\n  D\n}")
  , broken "exception on newline" $ passes ("A:\n  B:X = C =\n D", "")
  , passes ("A:\n  B:X = C =\n  D\n  E", "A{\n  (B : X) = C = D\n  E\n}")
  ...
```

or keep them separate like this:

```
var_ref_set :: TestTree
var_ref_set =
  let passes = pretty_test pExpr
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
```
