## REPL

### How to Build

`cabal build verse-repl`

### How to Run

```bash
$ cabal run verse-repl
Verse parse, desugar, and evaluation testing.
Use :help for help, and :quit to quit.
>
```

### Useful commands

#### `:help`

Shows the help menu.

#### `:show`

Shows the last expression:

```
:show 1+2
1 + 2
InfixOp (Lit 1) + (Lit 2)
> :show
InfixOp (Lit 1) + (Lit 2)
```


#### `:set` and `:unset`

`:set [FLAG]` enables global flags for the repl, `:unset [FLAG]` disables
them. Calling `:set` or `:unset` with no arguments lists the flags:

```
> :set
  verify       off
  trace-eval   on
  ds-uniform   off
  steps        1000
> :unset
  verify       off
  trace-eval   on
  ds-uniform   off
  steps        1000
```

Call `:set` with an argument to enable a flag:

```
> :set verify
> :set
  verify       on -- now verify is enabled
  trace-eval   on
  ds-uniform   off
  steps        1000
```

Call `:unset` with an argument to disable a flag:

```
> :unset trace-eval
> :set
  verify       on
  trace-eval   off  -- trace-eval is disabled
  ds-uniform   off
  steps        1000
```

#### `:print`

Pretty prints an expression, with no argument `:print` pretty prints the last
expression.

```
> :print 1+3
1 + 3
1 + 3
>
> :print x:=1|2
x := 1 | 2
x := 1 | 2
>
> :print
x := 1 | 2
```

#### Desugaring and Converting expressions

The repl provides several commands to translate the input expression into
different verse's. Each of the semantics correlate to a figure in the
`Spec/verse-spec.pdf` paper. If you don't have the `pdf` you have to build it by
going to `Spec` and running `make`. If you see that the Figure listings have
changed please update this readme.

These are:

- `:essential`: Desugars Source Verse to Essential Verse. (Fig. 4 and 5)
- `:mini`: Desugars Source Verse to Essential Verse to Mini Verse. (Fig. 6)
- `:src-core`: Convert an expression to `SrcCore`. `SrcCore` is `Core` but still with `x:=t` terms.
- `:core`: Convert an expression to `Core` (Fig. 8). Verification and evaluation expect `Core`.

Each command optionally takes an argument expression. If no expression is
provided then the last expression is used.

For example:

```
> :essential x:=1| 2+2
x := 1 | 2 + 2

================ Essential ===================
operator'+' :=
  (\p. exists x y. (x, y) = p; isInt$[x]; isInt$[y]; intAdd$[x, y]);
x := 1 | operator'+'[2, 2]

> y:int
y : int
> :core

================ Essential ===================
int := (\y. isInt$[y]; y); y := :int

================ Mini ===================
exists int;
int = (\y. isInt$[y]; y);
exists $x1;
exists y;
y = int[$x1]

================ SrcCore ===================
verify()
  {check<succeeds>
     {exists int;
      int = (\y. isInt$[y]; y);
      exists $x1;
      exists y;
      y = int[$x1]}}

================ Prep'd Core ===================
CHECK<decides>{
  verify(;){
    CHECK<succeeds>{
      ∃int $x1 y. (int = (\y. isInt$[y]; y)); (y = int[$x1]); y
    }
  }
}
> :print
y : int
> :show
InfixOp (Variable y) : (Variable int)
```

Notice that `:core` used the last input: `y:int` as its argument because no
argument was provided and that can pretty print and show the last input after
the conversion to `:core`.


#### Running Denotational Semantics

The repl implements multiple denotational semantics. Each of the semantics
correlate to a figure built in the `verse-spec.pdf`. The commands are:

- `:densem`: Implements the Essential Verse `E-LS` (jeff: could be out of date?)
- `:dls-densem`: Implements the `D-LS`  semantics (Fig. 18)
- `:sls-densem`: Implements the Essential Verse `S-LS` semantics (Fig. 22)
