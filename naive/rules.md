## Ranjit's NAIVE modifications

Key idea "distinguish" between *flexible* and *rigid* variable occurrences

- **Flexible** variable is exists-bound at the "same" level as occurrence
- **Rigid** variable is non-flexible i.e. either lambda-bound at same level or exists bound at outer level

For example, in

```
exists a, b. a = <10>; b = <10>; one { a = b; 5 }
```

(reading from left-to-right) the first occurrence of `a` and `b` is *flexible* but the second (inside the `one {..}`)
is *rigid*.

## Invariant

Add extra (temp/local) variables to ensure

1. Every equation is of the form `x = e` where `x` is *flexible*.
2. Every tuple is of the form `<x,...>` where `xi` is *flexible*.

Lets split cases on the equation's RHS: either a value `v` or an `e`.

The `x=e` constraints we have to wait until they are `v`.

The `x=v` constraints are one of

- **definition**  `x = hnf`   (written `x is hnf`)
- **alias**   	`x = y` where `y` is flexible
- **test**    	`x = a` where `a` is rigid

## Unification: Aliases

*Aliases* are boring (?). Lets just replace with some common variable so

```
(UNI-ALIAS) exists x, y. x = y; e   	--> 	exists z. e{z / x, y}
```

## Unification: Definitions and Tests

Assuming we've gotten rid of `aliases` with `(uni-alias)`
I'd like to crunch unifications down to the following:

- def-def :   `x = h1; x = h2`  -->  ???
- def-test:   `x = h1; x = a`   -->  ???
- test-test:  `x = a1; x = a2`  -->  ???


(UNI-FLEX)
E[x=h1; x=h2; e]  	--> E[cs; x=h; e]   	if unify(h1, h2) = (cs, h)
                  	--> E[fail]         	if unify(h1, h2) = fail

(UNI-FLEX-TEST)
E[x = h; x = a; e]	--> E[cs; x = h''; e]   if E |- a is h' and unify(h, h') = (cs, h'')
E[x = h; x = a; e]	--> E[fail; e]      	if E |- a is h' and unify(h, h') = fail

(UNI-TEST)
E[x = a1; x = a2; e]  --> E[cs; x = h; e]   if E |- ai is hi and unify(h1, h2) = (cs, h)
E[x = a1; x = a2; e]  --> E[fail; e]    	if E |- ai is hi and unify(h1, h2) = fail


E[x = a1; x = a2; e]  --> E[x = LAM; e] 	if E |- a1 is LAM   	E |- a2 is LAM
                  	--> E[x = k; e]   	if E |- a1 is k     	E |- a2 is k
                  	--> E[c..;x = <w>; e] if E |- a1 is <u1..uk>  E |- a2 is <v1..vk>   <u1..uk> ~ <v1..vk> ==> c.. / <w1..wk>
                  	--> E[fail]       	if E |- a1 is h1    	E |- a2 is h2     	(and not above)


### Unification: Def-Def

```
unify(h1, h2) = (constraints, h) | fail

<> ~ <> => 0, <>

<y,ys> ~ <z, zs> =>

(UNI-FLEX)	x = <y1...yk> ; x = <z1...zk>   --> ci...ck; x = <w1...wk>

where

  ci, wi = yi=zi, yi  IF yi, zi flex

  ci, wi = yi=zi, yi  IF yi flex, zi rigid

  ci, wi = yi=zi, zi  IF zi flex, yi rigid

  ci, wi = (exists ti. ti = yi; ti = zi), ti  IF yi, zi rigid
```

Why not

```
(UNI-RIGID) 	E[x = a] --> E[x = h]   	if x is flex, a is rigid, E |- a is h
```

and the ACTUAL work is done by

(UNI-FLEX)

## Examples

### Yikes1

```
exists x y . x=<1,2>; y=<1,3>; one{ x = y ; 10 }

--> [desugar]

exists x y . x=<1,2>; y=<1,3>; one{ exists z. z = x; z = y ; 10 }

--> [UNI-RIGID] x 2

exists x y . x=<1,2>; y=<1,3>; one{ exists z. z = <1,2>; z = <1,3> ; 10 }

--> [UNI-FLEX] x 2

exists x y . x=<1,2>; y=<1,3>; one{ exists z. fail ; 10 }

--> [FAIL]

fail
```


### Yikes2

```
exists x y . x = <1,2>; y = x; \w. <x, y>

--> [UNI-ALIAS]

exists z. z = <1,2>; \w. <z, z>
```

### RJ1

```
exists ID. ID = (\a.a); exists x y . x = ID; one{ x = y; x(10) }

--> [desugar]

exists ID. ID = (\a.a); exists x y . x = ID; one{ exists t. t = x; t = y; x(10) }

--> [float-exists]

exists ID,x,y. ID = (\a.a); x = ID; one{ exists t. t = x; t = y; x(10) }

--> [uni-alias] (x, ID)

exists y, z. z = (\a.a); one{ exists t. t = z; t = y; z(10) }

--> [APP] + [BETA]

exists y, z. z = (\a.a); one{ exists t. t = z; t = y; 10 }
```

### RJ2

```
exists ID. ID = (\a.a); exists x y . x = ID; y = x; < y >

--> [float]

exists ID,x,y. ID = (\a.a); x = ID; y = x; < y >

--> [alias] x, Id

exists z, y. z = (\a.a);  y = z; < y >

--> [alias] y, z

exists t. t = (\a.a); < t >
```

### RJ3

```
exists a b c d x . x = <a, b>; x = <c, d>; x

--> [UNIFY-FLEX]

	exists a b c d x . x = <a, b>; a = c; b = d; x  ... (1)
	--> [alias] a, c
	exists b d x y. x = <y, b>; b = d; x
	--> [alias] b, d
	exists x y z. x = <y, z>; x

OR

	exists a b c d x . x = <c, d>; a = c; b = d; x  ... (2)
	--> [alias] a, c
	exists b d x y . x = <y, d>; b = d; x
	--> [alias] b, d
	exists x y z. x = <y, z>; x
```

### Wombat1a

exists x. x = <1>; one { exists y . x = y; < x > } }

--> [desugar]

exists x. x = <1>; one { exists y . y = x; < x > } }

--> [one]

exists x. x = <1>; exists y . y = x; < x >

--> [float-ex]

exists x, y.  x = <1>; y = x; < x >

--> [alias] x, y

exists z. z = <1>; < z >


### Wombat1b

exists x. x = (\a.a); one { exists y . x = y; < x > }

--> [desugar]

exists x. x = (\a.a); one { exists y . y = x; < x > }

--> [one]

exists x. x = (\a.a); exists y . y = x; < x >

--> [float-ex]

exists x, y. x = (\a.a); y = x; < x >

--> [alias] x, y

exists z. z = (\a.a); < z >

### Wombat2

exists x, y. x = (\a.a); one { x = y; < x > }

--> [desugar]

exists x, y. x = (\a.a); one { exists t. t = x; t = y; < x > }

[STUCK]

exists y . one { y = (\a.a); < \a.a > }
