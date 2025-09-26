# Classes, Structs, and Interfaces


Verse uses the same definition syntax for reasoning about functions, classes, structs, modules, and many other constructs. In all of those cases, Verse is defining an immutable variable. Consider this:

```verse
c<public> := class:
    A<public>:int
    B<public>:int

```

This defines a public class named `c`, and it has two public fields, both of which are integers. We can create instances of this class by saying:

```verse
O := c{A := 1, B := 2}

```

Note that *instance* here really just means an immutable record, passed by value. It has no identity like how a Java object might have identity. Two instances are equal simply if their fields are equal. We'll get to objects that have identity, and supporting mutable state, later.

Classes exist both as operations executed by the checker (the checker executes the `class:` construct) and as operations executed at runtime (the running program executes `class:` as well and the program gets to pass `c` around as a value). However, the `class` construct can only be used in places that have a path. For example, `c` above has a path if it appears inside a module body (toplevel of a `.verse` file counts), so the checker will accept it. Verse will accept classes created as a result of some computation (more on that later), but only in cases where a path can be trivially ascribed to the class based on where the `class` construct appears syntactically. This restriction exists in part so that classes in Verse are nominally, not structurally, typed.

Classes can have one supertype and multiple super*interfaces*.

Verse classes like `c` above are immutable and pure; allocating instances of them is not observable. They just carry around immutable data that can be accessed. But it's also possible to create a Verse class that has observable allocation:

```verse
d<public> := class<unique>:
    C<public>:int

```

Because we have marked `d` as `<unique>`, instances know that they are distinct from one another. In other words:

```verse
Inst1 := d{C := 1}
Inst2 := d{C := 1}
Inst1 = Inst1        # Unique instances are always equal to themselves.
Inst1 <> Inst2       # The instances are not equal for the same reason that two Java object instances are not equal.

```

Additionally, we can put mutable variables in a class:

```verse
e<public> := class:
    var D<public>:int
Inst := e{D := 3}
set Inst.D = 4

```

This allocates a new instance of `e` and initially sets the mutable field `D` to `3`, but later sets it to `4`.

Verse classes also allow subtyping:

```verse
f<public> := class(e):
    var E<public>:float
Inst2 := f:                   # Multiline syntax for instantiation.
    D := 10                   # Initialize field D inherited from e.
    E := 11                   # Initialize field E from f.

```

Classes allow functions in addition to data members. Functions and data members operate on the same principle, since functions in Verse are just first-class values. Hence, any value, including a function, can be a member of a class.

```verse
my_counter := class:
    var Counter<private>:int
    AddOne():void =
        set Counter += 1
    PrintCounter():void =
        Print(Counter)
```
