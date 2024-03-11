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

## Running the Tim Test Suite through TRSVerifier


To run all tests:

```
make testtimverify
```

Or run by hand

```
cabal run tester -- --tim-verify --prelude=verifyprelude versetests/TimTests.verse
```

To examine a single test where 252 is the line number of the test

```
cabal run tester -- --tim-verify versetests/TimTests.verse --only-test=L252 --verbose --prelude=verifyprelude
```

## JUNK

verify:
  check<suc>:
    exi x, f, g:
      x=1
      x=2
      f = (verify{ check<suc> { 1 = (x=1) } } ;; \i. uni r. assume{r = (x=1)}; r)
      g = (verify{ check<suc> { exi z. z = f[];  z = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> selectively subst x=1 in f's `verify`

verify:
  check<suc>:
    exi x, f, g:
      x=1
      x=2
      f = (verify{ check<suc> { 1 = (1=1) } } ;; \i. uni r. assume{r = (x=1)}; r)
      g = (verify{ check<suc> { exi z. z = f[];  z = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> elim f's `verify`

verify:
  check<suc>:
    exi x, f, g:
      x=1
      x=2
      f = (\i. uni r. assume{r = (x=1)}; r)
      g = (verify{ check<suc> { exi z. z = f[];  z = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> subst f and app-beta inside `g`s verify and eliminate f

verify:
  check<suc>:
    exi x, g:
      x=1
      x=2
      g = (verify{ check<suc> { uni r, exi z. assume{r = (x=1)}; z = r;  z = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> selectively subst x=2 inside g's verify

verify:
  check<suc>:
    exi x, g:
      x=1
      x=2
      g = (verify{ check<suc> { uni r, exi z. assume{r = (2=1)}; z = r;  z = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> subst z=r and eliminate z

verify:
  check<suc>:
    exi x, g:
      x=1
      x=2
      g = (verify{ check<suc> { uni r. assume{r = (2=1)}; r = (1=2) } } ;; \i. uni r. assume{r = (1=2)}; r)
      g[]

--> use assume {r = (2=1)} to "prove" the g's verify

verify:
  check<suc>:
    exi x, g:
      x=1
      x=2
      g = (\i. uni r. assume{r = (1=2)}; r)
      g[]

--> app-beta and eliminate g

verify:
  check<suc>:
    exi x:
      x=1
      x=2
      uni r. assume{r = (1=2)}; r

--> 1=2 is fail

verify:
  check<suc>:
    exi x:
      x=1
      x=2
      uni r. assume{fail}; r

--> YIKES, prove outer check/verify with `assume{fail}`
