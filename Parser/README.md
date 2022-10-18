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


x := 1; x

```haskell
def x, $1 in
  $1 = (def $1 in $1 = x; $1 = 1; $1);
  x

-->

def x, $1 in
  $1 = (def $1 in $1 = x; x = 1; x);
  x



```