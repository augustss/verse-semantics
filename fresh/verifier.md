# Fresh-Style Verifier

## Terminology

- `WRONG` adverse event at run-time
- `FAIL` plain old `fail` could be swallowed up by branches etc. NBD
- `OK` should be verified

## Stuck v. Divergence (which are both “going wrong”?)

`stuck`   = "deadlock", no more rules fire = *safety*
`diverge` = "livelock", infinite sequence rules = *liveness*

```
f(x:int):int => y:=f(y)     # stuck
f(x:int):int=f(x) => f(0)   # diverges
x := x+1                    # stuck
f := x => f (x+1); f 2      # diverge
x=y+1; y=x+1                # stuck
x = x*x                     # solutions 0 or 1
(x, r) = f(x, y)            # This may well have a solution in Haskell
x = (if y =1  then 1 else 2); y = if (x = 1 then 1 else 2); x
```

## Modularity vs Stuck

What is a modular signature for `f` that lets us verify `g` doesn't get stuck?

```
f(z:int):int = ...
g(x:int):int = ex y:int. x := f(y); y
```

## Examples

```
# OK
if (1 + “hello”; 1=2) then 1 else 2

# OK
if (loop();1=2) then 1 else 2

# OK
(x='d';  if int[x] then x+1 else 3)
=> if int['d'] then 'd' + 1 else 3
```

# version 1
f(x:int):int=x+1 # implicitly <succeeds>.
f[3]      # OK => 4
f(3)      # OK => 4
f[false]  # WRONG
f(false)  # WRONG

# version 2
f(x:int):int=x+1

# alternatively
IsEven(i:int)<decides>:int=int[i/2]

IsEven[4]       # OK
IsEven[3]       # FAIL, but not wrong
IsEven[“hello”] # WRONG

f(x:=3) => x+1  # OK
f(x:int) => x+1 # OK

IsTwo(x:int)<decides>:int=x=2   #OK
IsTwo(x:any)<decides>:int=x=2   #WRONG  [Tim]  x:any => x=2 evaluates (to enforce parametricity)???

# OK
power2(N:int, E:nat) : int = if (E1 := nat[E-1]) then (N * power2(N, E1)) else 1


if (E1 := nat[INT]) then (INT *# power2(N,E1)) else 1

NAT -# INT -> INT

E1 → NAT

INT *# power2(INT, NAT)

INT

To do this, start with N:int, E:nat as assumptions, and try to evaluate
int[  if (E1 := nat[E-1]) then (N * power2(N,E1)) else 1 ]

Some axiom concludes that E-1 : int based on operator’-’(x:int,y:int):int.
e.g.  as well having (x=e) as an expression, add assume{e:type} as an expression

Next inference is nat[E-1] is <decides> because E-1 is an integer, therefore E-1>=0 is <decides>.


Now we know the condition if the “if” is decidable.
That’s key, as otherwise we’re stuck.
For example, if we compared two functions in the if-condition, we’d be stuck.

Now we apply something like the choice “forking” axiom to break down our original problem into two independent problems and verify each.
int[  N * power2(N,E1)]
int[  1 ]

int[1] is trivial.

Now we verify int[  N * power2(N,E1)].
We infer power2(N,E1) : int from the definition of power2, and check its parameters.
Now we conclude int[power2(N,E1)] succeeds.

A new axiom concludes that nat[E-1] <decides>.

int +# int -> int
nat +# nat -> nat

operator’+’(x:int,y:int) := “once x and y are known to be int, we return int; once x and y are exactly known, we refine our return to their exact sum”.

Other example (Koen):

# OK
power3(N:int, E:nat) : int = if (E > 0) then (N * power3(N,E-1)) else 1

pos(x:int)<decides>:int := 0 < int[x]
power4(N:int, E:nat) : int = if (pos[E]) then (N * power3(N,E-1)) else 1


e.g.   X[ if(e) then e1 else e2 ]  →  X[ succeeds{e}; e1 ]  |    X[ fails{e}; e2 ]
Then we need things like
    succeeds{ x>3 } … succeeds{ x<2 } ….    will fail

What are our constraints:
- Equality constraints x=y, which we already have
- For all effects x, x{expr} tests whether expr has effects x
- assume{expr} assumes that expr succeeds, allowing further inference based on those assumptions
- f[x]=y is a newly important special case of equality constraints reflecting that f is a function
from x to y; x is in the domain of f; and y is the result of calling f with x.
Given assume{int[x]}, we can conclude int[x].
---