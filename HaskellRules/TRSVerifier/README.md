# Direct Rewrite

- [x] Extend `Core`
  - `assume`
  - `succeeds`

- [x] Tim's examples

- [ ] Rules


## Core Extension


## Idea

* Use `assert` and `assume` to express the obligations directly in the source.

* Function definition desugars two way:
  - **def-site**
      + `assume` input does not fail to verify
      + `assert` that output does not fail

  - **use-site**
      + verify `assert` that actual input does not fail and
      + `assume` that output does not fail


## Examples

### Example 0

  forall x. int[x] => forall y.  int[y] => forall z. int[z] => x=z => succeeds{ exists a b. a=x; b=a; b=y}

-->

  \x y z. assume { int[x] }; assume { int[y] }; assume { int[z] }; assume { x=z }; succeeds { exi a,b. a=x; b=a; b=y }

-->

  \x y z. assume { int[x] }; assume { int[y] }; assume { int[z] }; assume { x=z }; succeeds { exi a,b. a=x; b=x; b=y }

-->

  \x y z. assume { int[x] }; assume { int[y] }; assume { int[z] }; assume { x=z }; succeeds { exi a,b. a=x; b=x; x=y }

--> [elim a, b]

  \x y z. assume { int[x] }; assume { int[y] }; assume { int[z] }; assume { x=z }; succeeds { x=y }

STUCK. cannot prove succeeds { x=y } from the assumptions.


### Example 1

    # definition of `int` using a primitive type test `isInt`
    int = \v. isInt(v); v

    # a "primitive" successor function for `int` values
    succ = \v. assert{int[v]}; exi r. assume{int(s)}; r

    # call `succ` on `x`
    incr(x:int):int := x + 1

    incr = \v. exi x,r. assert { x = int(v) }; assume { int(r) }; r

Desugar to three representations

    incr = \v. exi x. assume {x = int(v)} ; assert { exi r. r = succ(x); int(s); r }  ... (**def-site** goal)

    incr = \v. exi x. assert {x = int(v)} ; assume { exi r. r = succ(x); int(s); r }  ... (**use-site** concrete)

    incr = \v. exi x. assert {x = int(v)} ; assume { exi r. int(r) ; r }              ... (**use-site** abstract)

Rewriting goal:

**Step 1.** *Eliminate* (prove) all the asserts
**Step 2.** *Ingest* using either `concrete` or `abstract` version, depending on type signature

Lets try **step 1** on **def-site** goal for `incr`

    int  = (\v. isInt(v); v);
    succ = (\v. assert{int[v]}; exi r. assume{int(s)}; r);

      \v. exi x. assume {x = int(v)} ; assert { exi r. r = succ(x); int(r); r }

      -->> [simplify `int(v)`]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r. r = succ(x); int(r); r }

      -->> [subst `x = v`]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r. r = succ(v); int(r); r }

      -->> [simplify `int(r)`]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r. r = succ(v); isInt(r); r }

      -->> [apply `succ(v)`]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r. r = ( exi w, s. w = v; assert{int(w)}; assume{int(s)}; s) ; isInt(r); r }

      -->> [subst w and elim w ]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r. r = ( exi s. assert{int(v)}; assume{int(s)}; s) ; isInt(r); r }

      -->> [float]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi r, s. assert{int(v)}; assume{int(s)}; r = s ; isInt(r); r }

      -->> [subst r=s, elim r]

      \v. exi x. assume {isInt(v); x = v} ; assert { exi s. assert{int(v)}; assume{int(s)}; isInt(s); s }

      -->> [float+distribute+meet]

      \v. exi x. assume {isInt(v)}; assume{x = v} ; exi s. assert{int(v)}; assume{int(s)}; assert{isInt(s)}; s

      -->> [apply-int + elim]

      \v. exi x. assume {isInt(v)}; assume{x = v} ; exi s. assert{isInt(v)}; v; assume{isInt(s)}; s; assert{isInt(s)}; s

      -->> [val-elim]

      \v. exi x. assume {isInt(v)}; assume{x = v} ; exi s. assert{isInt(v)}; assume{isInt(s)}; assert{isInt(s)}; s

      -->> [prove v]

      \v. exi x. assume {isInt(v)}; assume{x = v} ; exi s. isInt(v); assume{isInt(s)}; assert{isInt(s)}; s

      -->> [prove s]

      \v. exi x. assume {isInt(v)}; assume{x = v} ; exi s. isInt(v); assume{isInt(s)}; isInt(s); s

      --->> QED

### Example 2

```
f(x := 3) := x = 3; 3
```

desugars to **def-site** goal

    f = \x. assume{x = 3}; assert{ exi r. r = (x = 3; 3); r }

    -->> [float + distribute]

    f = \x. assume{x = 3}; assert{ exi r. x = 3; r = 3; r }

    -->> [subst-r + elim]

    f = \x. assume{x = 3}; assert{ x = 3; 3 }

    -->> [float+distribute]

    f = \x. assume{x = 3}; assert{ x = 3 } ; assert { 3 }

    -->> [cond-val]

    f = \x. assume{x = 3}; assert{ x = 3 } ; 3

    ---> [subst x = 3 under assume]

    f = \x. assume{x = 3}; assert{ 3 = 3 } ; 3

    QED.


### Example 3

Given

    FOO := \x. (x = 666 | x = 42); x

    f(x:FOO):FOO := 708 - x

Desugars to

    f = \v. exi x. assume{x = FOO(v)}; assert{exi r. r = 708 - x; FOO(r)}

    -->> [apply foo]

    f = \v. exi x. assume{x = ((v = 666 | v = 42); v) }; assert{exi r. r = 708 - x; FOO(r)}

    ---> [semi-float]

    f = \v. exi x. assume{(v = 666 | v = 42); x = v }; assert{exi r. r = 708 - x; FOO(r)}

    -->> [choice]

    f = \v. exi x. assume{v = 666; x = v }; assert{exi r. r = 708 - x; FOO(r)}
                 | assume{v = 42 ; x = v }; assert{exi r. r = 708 - x; FOO(r)}

    -->> [subst v]

    f = \v. exi x. assume{v = 666; x = 666 }; assert{exi r. r = 708 - x; FOO(r)}
                 | assume{v = 42 ; x = 42 } ; assert{exi r. r = 708 - x; FOO(r)}

    -->> [subst x]

    f = \v. exi x. assume{v = 666; x = 666 }; assert{exi r. r = 708 - 666; FOO(r)}
                 | assume{v = 42 ; x = 42 } ; assert{exi r. r = 708 - 42 ; FOO(r)}

    -->> [apply -]

    f = \v. exi x. assume{v = 666; x = 666 }; assert{exi r. r = 42 ; FOO(r)}
                 | assume{v = 42 ; x = 42 } ; assert{exi r. r = 666; FOO(r)}

    -->> [subst r + elim]

    f = \v. exi x. assume{v = 666; x = 666 }; assert{FOO(42)}
                 | assume{v = 42 ; x = 42 } ; assert{FOO(666)}

    -->> [apply FOO]

    f = \v. exi x. assume{v = 666; x = 666 }; assert{42}
                 | assume{v = 42 ; x = 42 } ; assert{666}

    --->> [cond-val]

    f = \v. exi x. assume{v = 666; x = 666 }; 42
                 | assume{v = 42 ; x = 42 } ; 666

    QED.

### Example 4

Given

    nat = \x. int(x); 0<=x; x
    dec = \x. assume{int(x)}; assert{exi r. int(r)}
    add = \x y. assume{int(x)}; assume{int(y)}; assert{exi r. int(r)}

Verify

    sum(x:any): int:= if nat(x) then add x (sum (sub x 1)) else 0

Desugar env

    nat = \x. int(x); 0<=x; x
    dec = \x. assert{int(x)}; assume{exi r. int(r)}
    add = \x y. assert{int(x)}; assert{int(y)}; assume{exi r. int(r)}
    sum = \x. assume{exi r. int(s)}

Verify

    \x. assert{exi r. r = if nat(x) then add x (sum (dec x)) else 0; int(s) }

    ---> [cond-if]

    \x. assert{exi r. r = ((assume {nat(x)}; add x (sum (dec x))) | 0) ; int(s) }

    ---> [choice]

    \x. assert{exi r. r = (assume {nat(x)}; add x (sum (dec x))) ; int(s) }     ... (a)
      | assert{exi r. r = 0 ; int(s) }                                          ... (b)

Case (b)

    \x. ... | assert{exi r. r = 0 ; int(s) }                                    ... (b)

    ---> [subst + elim r]

    \x. ... | assert{ int[0] }

    ---> [apply]

    \x. ... | assert{ 0 }

    ---> [cond-val]

    \x. ... | 0

Case (a)

    \x. assert{exi r. r = (assume {nat(x)}; add x (sum (dec x))) ; int(s) }     ... (a)

    ---> [float + distribute]

    \x. assume {nat(x)}; exi r. r = assert{add x (sum (dec x))} ; assert{int(s)}

    ---> [apply nat, distrib]

    \x. assume {int(x)}; assume{0<=x}; exi r. r = assert { add x (sum (dec x)) } ; assert { int(s) }

    ---> [ANF]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r0, r1.
            r0 = assert{ dec x };
            r1 = assert{ sum r0 };
            r = assert{ add x r1 };
            assert { int(s) }

    ---> [apply dec]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r0, r1.
            r0 = assert{ assert{ int(x) }; assume {exi s0. int(s0) } };
            r1 = assert{ sum r0 };
            r = assert{ add x r1 };
            assert { int(s) }

    ---> [float]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r0, r1.
            assert{ int(x) }
            r0 = assume {exi s0. int(s0)};
            r1 = assert{ sum r0 };
            r = assert{ add x r1 };
            assert { int(s) }

    ---> [prove int(x); val-elim]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r0, r1.
            r0 = assume {exi s0. int(s0)};
            r1 = assert{ sum r0 };
            r = assert{ add x r1 };
            assert { int(s) }

    --->> [float]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r0, r1, s0.
            assume{isInt(s0)};
            r0 = s0;
            r1 = assert{ sum r0 };
            r = assert{ add x r1 };
            assert { int(s) }

    ---> [subst r0; elim]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r1, s0.
            assume{isInt(s0)};
            r1 = assert{ sum s0 };
            r = assert{ add x r1 };
            assert { int(s) }

    ---> [apply sum]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r1, s0.
            assume{isInt(s0)};
            r1 = assert{ assume {exi s1. int[s1] } }
            r = assert{ add x r1 };
            assert { int(s) }

    --->> [float]

    \x. assume {int(x)}; assume{0<=x};
          exi r, r1, s0, s1.
            assume{isInt(s0)};
            assume{isInt(s1)};
            r1 = s1;
            r = assert{ add x r1 };
            assert { int(s) }

    --->> [subst r1; elim]

    \x. assume {int(x)}; assume{0<=x};
          exi r, s0, s1.
            assume{isInt(s0)};
            assume{isInt(s1)};
            r = assert{ add x s1 };
            assert { int(s) }

    ---> [apply add]

    \x. assume {int(x)}; assume{0<=x};
          exi r, s0, s1.
            assume{isInt(s0)};
            assume{isInt(s1)};
            r = assert{ assert {int(x)}; assert{int(s1)}; assume {exi s. int[s]}} ;
            assert { int(s) }

    ---> [float]

    \x. assume {int(x)}; assume{0<=x};
          exi r, s0, s1, s.
            assume{isInt(s0)};
            assume{isInt(s1)};
            assert{int(x)};
            assert{int(s1)};
            assume{int(s)};
            r = s ;
            assert { int(s) }

    --->> [prove x, prove s1, elim x, elim s1, elim s0]

    \x. assume {int(x)}; assume{0<=x};
          exi r, s.
            assume{int(s)};
            r = s ;
            assert { int(s) }

    ---> [subst r; elim]

    \x. assume {int(x)}; assume{0<=x};
          exi s.
            assume{int(s)};
            assert{int(s)}

    ---> [prove s]

    \x. assume {int(x)}; assume{0<=x};
          exi s. assume{int(s)}; int(s)

    QED.

## New Rules

    assert{exi x. e} ---> exi x. assert{e}        [assert-exi]
    assume{exi x. e} ---> exi x. assume{e}        [assume-exi]
    assert{e1;e2}    ---> assert{e1}; assert{e2}  [assert-seq]
    assume{e1;e2}    ---> assume{e1}; assume{e2}  [assume-seq]
    assert{v}        ---> v                       [assert-val]

*cond-meet*

    assert{assert{e}} ---> assert{e}              [assert-assert]
    assert{assume{e}} ---> assume{e}              [assert-assume]
    assume{assert{e}} ---> assume{e}              [assume-assert]

*cond-val*

    assume{v} ---> v

*cond-if*

    if e1 then e2 else e3   --->   (assume{e1}; e2) | e3

*prove*

    E[assert{f(x)}] ---> E[assume{f(x)}]  if  ctx(E) |- f(x)

    E[assert{e}] ---> E[assume{e}]        if  ctx(E) |- e

E ::= HOLE | exi x. E | E; e | e ; E | x = E | assume{ E }

X ::= ...
    | assume { X }      // allow substitutions using equalities under `assume` (but NOT `assert`)





## Examples

```
f(x := 3) := x = 3; 3
```

Foo = exists x, y, z. x = INT, y = z; z = add x 1; y
Foo(X:int):int =
     Y := Z
     Z := X + 1
     Y
Foo(42) + 68

-- 5 (Phil's Bar)
Bar(Y:int):int =
  for (Z:int = Y; next Z = Z + 1):
    if (Z = 666):
      return Y + Z
