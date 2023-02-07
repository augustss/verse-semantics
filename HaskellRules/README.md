# README

## TLDR

There are three (well, four) programs here
* `verse` An interactive system for parsing, desugaring, and evaluation.
* `tester` A test running taking a `.versetest` file as input.
* `qctest` A QuickCheck test for rule properties.
* `timtest` Parse Tim S's test files.

All programs have `--help` support.
`cabal build` will build all three.

They can be invoked in several ways:
* Via Cabal
  ```sh
  $ cabal run tester -- <flags>
  ```
  (or `cabal run qctest`, `cabal run verse`)

* Build a local binary you can invoke (`tester` and `qctest` only)
  ```sh
  $ make bin/tester
  $ bin/tester <flags>
  ```
* Use the Makefile (specific to each one .. see below)


## `verse`: running the interactive system

```sh
$ cabal run verse -- --rules=POPL
```

## `tester`: running tests

To run `tester` via the makefile:
```sh
$ make testfast
```
This runs tester program (built in `tester/`) passing it the name of
the test spec file (e.g. `versetests/tricky.versetest`), plus sundry
other flags.


## `qctest`: Quickcheck

Todo.

## Specifying rules

The rewrite rules are expressed an embeded DSL in Haskell, compiled
into `tester`, `qctest` etc.
* The rules themselves are in `rules/Rules`, in files `Core.hs`, `PLDI.hs` etc.
* `tester` uses `Rules.System.allSystems` for the rows of the table with `--summary`,
  and to look up the name of the rules system with `--rules=<name>`.
* In `TRSSystem`:
  * `rules :: Rules t` are used to make progress
  * `confluenceRules :: Rules t` are the structural rules used during confluence checking

* Rules are writtn in the list monad.  E.g.
  ```haskell
  type ERule = Rule Expr
  type Rule a = RuleEnv a -> a -> [(String, a)]

  rulesPrimOps :: ERule
  rulesPrimOps _ lhs =
    "P-ADD" `name`
    do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
       pure (Int (k1+k2))
  ```
* Contexts in rules use an auxiliary function to split an expression into a
  context and a hole, in all possible ways.  E.g. for context X we have
  ```
  type Context = Expr -> Expr
  execX :: Expr -> [(Context, Expr)]
  ```
  Matching checks the thing in the hole against what you are looking for:
  ```haskell
  rulesFail :: ERule
  rulesFail _ lhs =
    "FAIL" `name`
    do (_cx, Fail) <- execX1 lhs
       pure Fail
       do (ctx, Var x :=: Val v) <- execX lhs
        ...
  ```
  (The "1" in `execX1` means never return an empty context.)

## Directories
The Haskell code is split into a number of directories corresponding to
different parts of the system.

* `densem/` Haskell code for the denotational semantics
* `epic/` Haskell code for general utilities (should be its own cabal project)
* `frontend/` Haskell code for the parser and desugaring
* `qctest/` Haskell code for the QuickCheck tester.
* `rules/Rules` Haskell code for the various rewrite systems.
* `tester/` Haskell code for ingesting and running a test file.
* `timtest/` Haskell code for parsing Tim's test files.
* `trs/` Haskell generic (i.e., not Verse) code for rewrite rules.
* `verse/` Haskell code for the interactive system.

* `versetests/` Verse code for the various tests.
  * `tricky.versetest`: some tricky examples

## Ranjit Notes

To run a single test with a `--trace`

```
cabal run -- tester -r PLDIT --only-test Koen5 --trace versetests/tricky.versetest
```

To get a summary for all the rules

```
cabal run -- tester -r PLDIT --summary versetests/tricky.versetest
```


(x = y; x); (y = x); STUFF

(x = y; y); (y = y); STUFF

    --> (x = y; y); (y = x); STUFF

    --> (x = x; x); (y = x); STUFF