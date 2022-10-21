# README

## Build

```sh
$ stack build
$ cabal v2-build
```

## Run

```sh
$ stack run
$ cabal v2-run
```

## Test

(or just leave out the arguments/options)

```
$ stack test --fast --test-arguments -rewrite
$ cabal v2-test --test-options -rewrite
```

or to use the plain `Makefile`

```sh
$ make test
$ make testr # popl-rewrite-rules
$ make testt # tim-parser
$ make testf # fresh-rewrite-rules
```
