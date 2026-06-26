# What you can do here

This directory contains two runnable programs:
* The `repl`, a read-eval-print loop for Essential Verse
* The `tester`, which reads a file of tests, runs them, and prints a summary.

To run any of this code you need `ghc` and `cabal` installed. To do that:
* Install `ghcup` from `https://www.haskell.org/ghcup/install/`
* Run `ghcup install ghc`
* Run `ghcup install cabal`
* Try a quick test: run `ghci` to get a REPL

# The REPL

To run the `repl`, say
```
bash$ cabal run repl
```
Now you get an interactive REPL, where you can type in Essential Verse expresions and have them evaluated. For example:
```
bash$$ cabal run repl
Verse read-eval-print loop.
Use :help for help, and :quit to quit.
> x:int; x=3
x := :int; x = 3

================ Essential ===================
x := :int; x = 3

================ Reduced ===================
3
```

Useful flags:
* `:set match-first`: run all matching rewrite rules before any evaluation rules.
* `:set trace`: show a trace of the reduction steps.
* `:set verbosity=N`: set the verbosity level of the trace.  N=0 shows only the steps, but no intermediates.  N=4 shows all intermediates. N=1,2,3 are in between (details in flux).

# The tester

To run the tester:
```
bash$ cabal run tester -- tests/tests.versetest
```
This runs the batch tester, on input file `tests/tests.versetest`, and prints a summary.
You can find more test files in the `test/` subdirectory.

You can give command-line flags before or after the test-file name, thus:
```
bash$ cabal run tester -- tests/tests.versetest --only=Splice6
```
Useful flags:
* `--match-first`: run all matching rewrite rules before any evaluation rules.
* `--only=Splice6`: run only the test called `Splice6`
* `--only=Splice*`: run only the tests whose names start with "`Splice`"
* `--trace`: show a trace of the rewrites
* `--verbosity=N`: as for the REPL

# Finding your way around the source code:

* `VerseSemantics.cabal`: the Cabal package description.
* `repl/` the `Main.hs` for the REPL.
* `tester/` the `Main.hs` for the tester.
* `tests/`: lots of test cases to exercise the tester.
* `reduction/Red.hs`: the main rewrite rules, including data type declarations for terms and expressions.
* `Core/`
  * `Core.hs`: data type declarations for literals, primops etc.  (The `Expr` type itself is mostly past history; ignore it.)
  * `Bind.hs`: fresh names etc
  * `Solver.hs`: a baby Z3: solves constraints for the verifier
  * `Traced.hs`: infrastructure for managing reduction traces
* `FrontEnd/`: code for munging Source Verse, especially
   * `Expr.hs`: data type declaration
   * `Desugar.hs`: superficial desugaring into Essental Verse
* `parser/` parsing goop
* `Epic/` some utility modules.

# Git instructions

There are two repositories:
* The "private repo", which has all the team's work; not secret, but full of old cruft and work in progress.
* The "public repo", `https://github.com/augustss/verse-semantics`, which exposes a (hopefully) tidy subset of the private repo.

The public repo is acutally a [git "subtree"](https://www.atlassian.com/git/tutorials/git-subtree) of the private repo.
Specifically, the public repo exposes the `VerseSemantics/` directory of the private repo.

The following instructions are for the Epic team only, not users of the public repo.

## Push to private repo

If you make changes in the `VerseSemantics/` directory of the private repo, and do a
```
  git push
```
the change will go (only) to the private repo (i.e. `verse-paper`), just as before.


# Public repo

## Initialize it
git remote add verse-semantics git@github.com:augustss/verse-semantics.git
git fetch verse-semantics

## Push to public repo
To push the changes to the public repo you need to do the following
```
  cd <root of private repo>
  git subtree push --prefix=VerseSemantics verse-semantics main
```


## Pull from public repo
To pull changes from the public repo you need to do the following
```
  cd <root of private repo>
  git subtree pull --prefix=VerseSemantics verse-semantics main --squash
```
