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

## TODO: Fresh Rules

- [ ] `isFresh :: Expr -> Bool`
- [ ] `DsFresh :: Expr -> {e:Expr|isFresh e}`
- [ ] `rulesSubstitution`
  - [ ] `App-Ctx` (`A`)
  - [ ] `Exp-Ctx` (`E`)
- [ ] `rulesConjunction`
- [ ] `rulesGarbageCollection`
