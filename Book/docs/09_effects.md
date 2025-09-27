# Effects

This document provides a gentle introduction to the Verse effect
system, starting with a simple “Hello, World!” function:

```verse
Return Type   --------------------\
An Effect     -------------\      |
Argument List --------\    |      |
                      |    |      |
                      V    V      V
          ThisIsHello( )<writes>:void=
               set MyString = "Hello, world!"
               Print(MyString)
```

`ThisIsHello` writes to a variable and prints its value. The `<writes>`
effect specifier indicates that the function writes to the heap.  Just
as functions have return types, they also have *effects*, explicitly
declared between `<angle_brackets>` after the argument list.  Effect
specifiers can also annotate data structures.

### What’s an Effect?

An effect represents an observable interaction with program state or
the environment.  While types describe what data a function uses and
returns, effects describe what the function does.  Fundamental effects
include:

* Reading or writing mutable memory  
* Allocating observably unique values  
* Suspending execution  
* Producing multiple results  
* Failing

Effect specifiers document these behaviors, enabling the compiler to
reason about and enforce restrictions—similar to how type systems
enforce type safety.

### Why Use Effects?

Making side effects explicit helps manage complexity.  For example, a
function annotated `<computes>` is pure: it cannot read from memory,
print output, or even query the current time.  This benefits both the
programmer and the compiler:

* **For developers:** clearer understanding of what a function may
    do

* **For the compiler:** enables compile-time detection of
   side-effect bugs, enforcement of invariants, and more aggressive
   optimizations

## Families and Specifiers

Effects are grouped into ***effect families***, each tracking a
specific kind of behavior. Each family contains ***fundamental
effects***, and ***effect specifiers*** that declare which effects a
function may perform.

### Effect Families

The six effect families are:

* **Cardinality**: Whether and how a function returns  
* **Heap**: Access to mutable memory
* **Suspension**: Whether a function may suspend execution  
* **Divergence**: Whether a function may run forever  
* **Prediction**: Where a function runs
* **Internal**: Reserved for internal use

Some effects, like `fails`, have no specifier. Some specifiers imply
multiple effects, for instance `<transacts>` implies `reads`,
`writes`, and `allocates`. Note also that `<transacts>` belongs to two
families.

|Fundamental Effect|Effect Specifier|Effect Family|Effects implied by Specifier within its family | Notes |
| -----          | -----------    | -------     | -----               | ----               |
| **succeeds**   |                | Cardinality |                     | *No specifier*     |
| **fails**      |                | Cardinality |                     | *No specifier*     |
|                | `<decides>`    | Cardinality | `{succeeds, fails}` |                    |
|                | `<ambiguates>` | Cardinality |                     | *Planned*          |
|                | `<abstracts>`  | Cardinality |                     | *Planned*          |
|                | `<iterates>`   | Cardinality |                     | *Planned*          |
| **reads**      | `<reads>`      | Heap        | `{reads}`           |                    |
| **writes**     | `<writes>`     | Heap        | `{writes}`          |                    |
| **allocates**  | `<allocates>`  | Heap        | `{allocates}`       |                    |
|                | `<transacts>`  | Heap        | `{reads, writes, allocates}` |           |
|                | `<computes>`   | Heap        | `{}`                |                    |
| **suspends**   | `<suspends>`   | Suspension  | `{suspends}`        |                    |
| **diverges**   |                | Divergence  | `{diverges}`        | *No specifier*     |
|                | `<converges>`  | Divergence  | `{}`                |                    |
| **dictates**   |                | Prediction  | `{dictates}`        | *No specifier*     |
|                | `<predicts>`   | Prediction  | `{}`                |                    |
| **no_rollback**|                | Internal    | `{no_rollback}`     | *To be deprecated* |
|                | `<transacts>`  | Internal    | `{}`                |                    |

There is another planned specifier, `<interacts>`, that is expected to
be used for code that has external effects, such as network
communication or interaction with a user.  Planned specifiers appear
in design documents but are not yet available.[^0] They are discussed
here for completeness.

[^0]: `<ambiguates>` and `<abstracts>` are key to the planned logic
 features of Verse, they denote functions that may return
 different values due to use of the choice
 operator. `<diverges>` (and `<converges>`) will indicate
 whether a function may not terminate or if it is provably
 guaranteed to return a value in a finite number of steps.

### Effect Specifiers

Think of effect specifiers as setting bits in a bit vector: one bit
per fundamental effect. Without any annotation, a function such as
`ThisIsHelloV1` has the following bits set:[^1]

[^1]:
  We only show shipping effects available to users.

```
ThisIsHelloV1( ):void= ...
```

| dictates | suspends | reads | writes | allocates | succeeds | fails |
| :---:    | :---:    | :---: | :---:  | :---:     | :---:    | :---: |
| ✔️  ️    | ❌      | ✔️    | ✔️     | ✔️        | ✔️      | ❌    |

This means the function allows `diverges`, `reads`, `writes`,
`allocates` and `succeeds`. It is *almost* like writing
`<diverges><transacts>` -- except we lack a way to specify that the
function "may not fail".[^2]

[^2]: A specifier such as `<fails>` would have dubious use as a function
that always `fails` never returns a value and can not have any observable
side effects -- these will be undone by failure. The `<succeeds>` specifier
is implicit and does not need to be written out.

Annotating a function only affects the bits in that specifier's
family. So `<reads>`, sets the `reads` bit within the Heap family and
clears the others bits. For example: with annotations `<reads>` and
`<predicts>` has the following bits set:

```verse
ThisIsHelloV2( )<reads><predicts>:void= ... 
```

yields:

| dicates |  suspends | reads | writes | allocates | succeeds | fails |
| :---:   |  :---:    | :---: | :---:  | :---:     | :---:    | :---: |
| ❌      | ❌       | ✔️    | ❌️     | ❌️       | ✔️       | ❌    |

Specifying `<reads><predicts>` clears the `writes` and `allocates`
bits, set the `predicts` bit and leaves everything else unchanged.

### Composing Effects

Effects are generally *not* hidden, they are sticky. If a function `F`
calls `G` and `H`, then `F` must declare all effects of both.
Exceptions include `fails`—if caught in a control structure like `if`,
the enclosing function may omit that effect:

```
Test(X:float)<computes>:string=   # if hides the decides
    if (X>0) then {"Yes"} else {"No"}
```

Furthermore, `suspends` is hidden by a `spawn` statement and
`predicts` is hidden by a `dictates` statement.

Effect over-specification is allowed and sometimes desirable, the
following version of the `Test` function declares that the function
may read the mutable heap even though its implementation does not:

```
Test(X:float)<reads>:string= if (IsPositive(X)) then {"Yes"} else {"No"}
```

This can future-proof APIs and avoid breaking changes later.

!!! note

    **Backwards compatibility** The effects of a function are part of
    what is checked for backwards compatibility. When updating a
    function that is part of a published API, the new version can have
    "fewer bits" but not more.  So, a function that was marked as
    `<reads>` in a previous version cannot evolve to `<computes>`.

## Cardinality Family

A function that may fail must be annotated as `<decides>`. Functions
that are not annotated are guaranteed to succeed. The `IsPositive`
function tests the value of its argument, it succeeds if it is
strictly larger than `0` and fails otherwise.

```
IsPositive(X: float)<decides>:float= X > 0.0
```

The `<decides>` specifier is mutually exclusive with `<suspends>`.
Furthermore, `<decides>` cannot be used on a constructor, creation of
a struct or object must succeed.

Besides `if`, another way to hide possible failure is to use
`option`. The expression `option(IsPositive(X))` will either return an
`option(float)`, if `X` is larger than `0` or `false` if isn't.

## Heap Family

Effects in this family pertain to interactions with the mutable heap.

### Determinism with `<computes>`

A function annotated as `<computes>` does not access the mutable heap
which implies that the function is *deterministic*, a fancy way of
saying that if you call the function with the same value, it will
return the same result over and over again.

```verse
Succ(i:int)<computes>:int=      # Deterministic
    return i + 1

```

The call `Succ(2)` will always return `3`. Examples of functions that
do not have that property include functions whose result depend on the
value of a mutable variable or on user input.

### Accessing mutable data with `<reads>`

A function with the `reads` effect is allowed to read the mutable
heap, or more plainly, read from a field marked as `var`. Consider a
`monster` class that consists of an immutable field and a mutable `Health`
and a `greeter` class with one one mutable field and one immutable
field:

```verse
monster := class:
    Name:string               # immutable
    var Health:float= 100.0   # mutable

greeter := class:
  var Goblin:monster = monster{Name:="Boblin"}   # mutable
 OtherGoblin := monster{Name:="Joblin"}         # immutable
```

Accessing a `Goblin` reads the mutable heap, as does any read of
`OtherGoblin.Health`.  The following three functions illustrate which
specifiers are needed:

```verse
Greet(Greeting:string)<reads>:string = Greeting + Goblin.Name

GreetNR(Greeting:string)<computes>:string = Greeting + OtherGoblin.Name

GetHealth()<reads>:float = OtherGoblin.Health 
```

`Greet` is `<reads>` because it accesses a mutable variable.
`GreetsNR` only accesses immutable fields through an immutable
variable.  `GetHealth` is `<reads>` because it accesses a a mutable
field.

### Updating the heap with `<writes>`

Using `set` in a function requires the `<writes>` and `<reads>`
specifier.[^4] The following updates an object's
`Health` field, so it must have both annotations:

[^4]:
 The `reads` effect is need due to another planned feature
 of the language: live variables.

```verse
ResetHealth()<writes><reads>:void = set OtherGoblin.Health = 0.0
```

Verse does not, currently, have a notion of *local* variables that
can be updated without heap effects.

### Heap Effects on Declarations

Data structures can be annotated with heap effects specifiers, these
apply to the constructor of the data structure, be that a class,
struct or interface. The following limits a class's constructor to
have the `<allocates>` effects:

```verse
npc := class<computes>:
    Name: string = "Default"
```

What benefit is there in limiting a class constructor? It allows us to
use this class in pure functions. Generally speaking, it is good
practice to keep the set of effects as small as possible.

### Allocation with `<allocates>`

The `allocates` effect has a name that suggests it captures memory
allocation in the mutable heap, but that’s not quite right. This
specifier is applied to expressions that, when evaluated multiple
times, return different but isomorphic values. In plain English this
means you should slap `<allocates>` on functions that create values
that either have a `var` in them or that are `<unique>`.

Next, class `joe` is annotated `<computes>` to indicate that its
constructor does not access the mutable heap -- the class has no
mutable fields. Thus the `MakeJoe` function can also be `<computes>`
as it creates an immutable value.

```verse
joe := class<computes>:
 Name:string = "Joe"

MakeJoe()<computes>:joe= joe{}      # No allocates effect

```

Class `jil` is unique and class `jon` has a mutable field, so
functions that create objects of either class will have to be marked
as `<allocates>`:

```verse
jil := class<unique><allocates>:
 Name:string = "Jil"

MakeJil()<allocates>:jil= jil{}     # Allocates because unique

jon := class<allocates>:
 var Name:string = "Jon"

MakeJon()<allocates>:jon= jon{}     # Allocates because jon has a var
```

Finally a function that uses a mutable variable needs the `<reads>`
and `<writes>` effects as well as the `<allocates>`.  This can be
expressed by `<transacts>` or by omitting the heap annotation
altogether as it is the default.

```verse
UseVar()<transacts>:void=        
 var X = 3
 set X = 4
```

## Predicts Family

Even though all Verse code runs authoritatively on Epic’s servers,
some functions can be run predictively on the client to reduce
latency.  Functions with the `dictates` effect, which is the default,
only run the server. The `<predicts>` specifier retracts `dictates` to
allow functions to run directly on the user’s machine. To be clear,
`<predicts>` functions *also* run on the server, but the client has a
chance to run them first, and automatically synchronize after the
server runs them.

```verse
OnBegin()<suspends>:void =
  MyPlayer.JumpedEvent().Subscribe(HandleJumpButtonPress)

HandleJumpButtonPress()<predicts>:void = # gets called on clients as soon as the player jumps
  JumpAnimation.Start()
```

A `dictates{}` statement is used to force code to run on the server.

## Suspends Family

The `suspends` effect indicate that a function may suspend execution.

```verse
OnSimulate<override>()<suspends>:void =
        Print("Starting Entity Lifetime Test")
        Sleep(0.0)
        for (LifetimeRecord : EntityLifetimes):
            spawn{ ProcessLifetimeRecord(LifetimeRecord)}
```

When within a `spawn{}` statement, the suspends effect is suppressed.

## Internal Family

Epic internal effects are not available to users. The `no_rollback`
effect is linked to operations that, in an older implementation of the
language, could not be run within a transaction. As this
implementation is retired, the effect will be deprecated.

The `<transact>` and `<computes>` specifiers retract the `no_rollback` effect.

## Diverges Family

This planned family is expected to deal with termination. It is
expected, for instance, that constructors will be checked for
convergence.

As of this writing some Epic authored, native, functions are annotated
as `<converges>`; the specifier retracts heap effects and is not
available in user code.
