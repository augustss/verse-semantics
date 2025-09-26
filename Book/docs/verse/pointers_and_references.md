# Pointers and References


Verse is a multiparadigm programming language. In addition to supporting functional and logic programming, Verse is also great for writing imperative code. Let's jump right in with our first mutable variable:

```verse
var M:int = 1410

```

Mutable variables are declared with `var`. They must be given a type (`int` in this case) and must be given an initial value (`1410` in this case). Reading a mutable variable doesn't require any special syntax; just mention its name:

```verse
V := M + 1

```

This expression defines a new immutable variable called `V` that gets `M`'s current value plus one (so `1411`). To assign to a mutable variable, we use `set`:

```verse
set M = 2 + 2

```

From this point forward, reading `M` gives `4`. Let's put these pieces together to see how they interact:

```verse
var M:int = 1410
V := M + 1
set M = 2 + 2
V = 1411          # succeeds, since V is an immutable variable and keeps its value
M = 4             # succeeds, since we changed M's value to 4

```

It's possible to use mutable variables almost anywhere that immutable variables are allowed. For example, a mutable variable can be global or local to a function.

Unlike immutable variable operations, which execute by equation solving, mutable variable operations execute in order just as you would expect from other imperative languages. But it's possible to mix both immutable variables, specified by out-of-order equations, and mutable variables, specified by reads and writes in a certain order. Consider this fun example:

```verse
A:int             # declare an immutable variable, but don't specify its value yet
var B:int = C     # define a mutable variable, and specify its value to be C, which we haven't even declared yet!
A = B             # define A to get the value read from B, but we can't read from B until we know what C is!
set B = 5         # change the value of B - but this has to happen after we read B on the previous line
C:int = 6         # declare C and give it a value...

```

Verse executes this program by *suspending* the execution of `var B:int = C` until it knows what `C` is. But we don't find out what `C` is until the end\! So, this program executes like so:

1. Create a placeholder for `A` due to `A:int`

2. Suspend the creation of `B:int` and assigning it the initial value `C` because we don't know what `C` is yet.

3. Suspend the reading of `B` and the unification of that value with `A` (i.e. `A = B`) because the creation of `B` is suspended.

4. Suspend `set B = 5` because there is a suspended *read effect* (the read of `B` in `A = B`). Verse ensures that imperative effects execute in program order by suspending read/write effects anytime prior read/writes are suspended.

5. Create `C` and unify it with `6`. This sets off a chain of events that leads to the program running for real:

   1. Resume the `var B:int = C` suspension, so now `B` exists and is set to `6`.

   2. Resume the `A = B` suspension, so we read `B` (which yields `6`) and set `A` to `6`.

   3. Resume the `set B = 5` suspension, so now `B` is set to `5`.

Note that if we're not careful with using leniency and effects, we could easily create a program that gets *stuck*. The Verse checker will reject any program that it suspects might get stuck. If the Verse checker accepts your program, then this is a proof that it won't get stuck. Here's an example of a program that would get stuck and so would get rejected during checking:

```verse
A:int
var B:int = A
A = B

```

This program gets stuck because:

- We must execute `var B:int = A` before we execute `A = B` because we cannot read from `B` until we create it.

- We must execute `A = B` before we execute `var B:int = A` because we cannot create `B` until we have a value for `A`.

This cyclic dependency means that the program cannot make any progress, so this will be rejected by the checker.

Now that we have a feeling for the basics of mutability, let's consider a couple additional features.

Verse supports `ref`s, which allow you to carry around a reference to a variable. For example:

```verse
var V:int = 13
ref R:int = V ref
set R = 14
V = 14

```

When we say `V ref`, we are asking `V` to give us a reference to itself rather than reading its value. Then the `ref R:int` *aliases* the variable `V`, so assigning to `R` causes `V`'s value to change.

Verse also supports pointer syntax, which allows us to be somewhat more precise about what points at what:

```verse
var V:int = 13
P:^int = V ref
set P^ = 14
V = 14
```
