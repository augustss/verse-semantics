# README

## TLDR
To run the interactive system

```sh
$ cabal run verse -- --rules=POPL
```

To run some tests
```sh
$ cabal build
$ make testfast
```

## Contents
Doing a complete build, `cabal build` will generate the following binaries:

* `verse` An interactive system for parsing, desugaring, and evaluation.
* `tester` A test running taking a `.versetest` file as input.
* `qctest` A QuickCheck test for rule properties.
* `timtest` Parse Tim S's test files.

There can be invoked like `cabal run verse`.
For each of the three first programs you can specify which rule system to use,
e.g. `cabal run verse -- --rules=PLDI` or `cabal run qctest -- --rules=POPL`.

## Directories
The Haskell code is split into a number of directories corresponding to
different parts of the system.

* `densem/` Haskell code for the denotational semantics
* `epic/` Haskell code for general utilities (should be its own cabal project)
* `frontend/` Haskell code for the parser and desugaring
* `qctest/` Haskell code for the QuickCheck tester.
* `rules/` Haskell code for the various rewrite systems.
* `tester/` Haskell code for ingesting and running a test file.
* `timtest/` Haskell code for parsing Tim's test files.
* `trs/` Haskell generic (i.e., not Verse) code for rewrite rules.
* `verse/` Haskell code for the interactive system.
* `versetests/` Verse code for the various tests.
