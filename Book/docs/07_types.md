# Types

Every value has a type, and understanding the type system is fundamental to mastering any language. Types aren't merely labels - they form a rich hierarchy that governs how values flow through your program, what operations are permitted, and how the compiler reasons about your code. The type system combines static verification with practical flexibility, catching errors at compile time while still allowing sophisticated patterns of code reuse and abstraction.

At the apex of this hierarchy sits `any`, the universal supertype from which all other types descend. At the opposite extreme lies `void`, the empty type that contains no values at all. Between these extremes exists a carefully designed lattice of types, each with its own capabilities and constraints. 

## Understanding Subtyping

Subtyping is the foundation of the type hierarchy. When we say that type A is a subtype of type B, we mean that every value of type A can be used wherever a value of type B is expected. This relationship creates a natural ordering among types, from the most specific to the most general.

Consider the relationship between `nat` (natural numbers; not a valid type in Verse but handy for our examples) and `int` (integers). Every natural number is an integer, but not every integer is a natural number. Therefore, `nat` is a subtype of `int`. This means you can pass a `nat` to any function expecting an `int`, but not vice versa:

<!--NoCompile-->
```verse
ProcessInteger(X:int):void = Print("Integer: {X}")
ProcessNatural(X:nat):void = Print("Natural: {X}")

MyNat:nat = 42
MyInt:int = -10

ProcessInteger(MyNat)  # Works - nat is a subtype of int
ProcessNatural(MyInt)  # Error - int is not a subtype of nat
```

The subtyping relationship extends to composite types in sophisticated ways. Arrays and tuples follow covariant subtyping rules for their elements. This means that `[]nat` would be a subtype of `[]int` if `nat` was a subtype of `int`. Similarly, `tuple(nat, nat)` would be a subtype of `tuple(int, int)`. This covariance allows collections of more specific types to be used where collections of more general types are expected.

Maps exhibit more complex subtyping behavior. A map type `[K1]V1` is a subtype of `[K2]V2` when `K2` is a subtype of `K1` (contravariant in keys) and `V1` is a subtype of `V2` (covariant in values). The contravariance in keys might seem counterintuitive at first, but it ensures type safety: if you can look up values using a more general key type, you must be able to handle more specific key types as well.

Classes and interfaces introduce nominal subtyping through inheritance. When a class inherits from another class or implements an interface, it explicitly declares a subtyping relationship:

```verse
vehicle := class:
    Speed:float = 0.0

car := class(vehicle):  # car is a subtype of vehicle
    NumDoors:int = 4

sports_car := class(car):  # sports_car is a subtype of car (and vehicle)
    Turbo:logic = true
```

This inheritance hierarchy means that a `sports_car` can be used anywhere a `car` or `vehicle` is expected, but not the reverse. The subtype inherits all fields and methods from its supertypes while potentially adding new ones or overriding existing ones.

## Casting and Conversion

All type conversions must be explicit, a design choice that eliminates entire categories of bugs while making the programmer's intent clear. Converting between numeric types illustrates this principle clearly. To convert an integer to a float, you multiply by 1.0:

<!--verse
F():void={
-->
```verse
MyInt:int = 42
MyFloat:float = MyInt * 1.0  # Explicit conversion to float
```
<!--verse
}
-->

The reverse conversion, from float to integer, requires choosing a rounding strategy:

<!--verse
F()<decides>:void={
-->
```verse
MyFloat:float = 3.7
Option1:int = Floor[MyFloat]  # Results in 3
Option2:int = Ceil[MyFloat]   # Results in 4
Option3:int = Round[MyFloat]  # Results in 4 (rounds to nearest)
```
<!--verse
}
-->

These conversion functions are failable - they have the `<decides>` effect and will fail if passed non-finite values like `NaN` or `Inf`. This forces you to handle edge cases explicitly:

```verse
SafeConvert(Value:float)<decides>:int =
    Value <> NaN
    Value <> Inf
    Floor[Value]
```

String conversions follow similar principles. The `ToString()` function converts various types to their string representations, while string interpolation provides a convenient syntax for embedding values in strings:

<!--verse
F():void={
-->
```verse
Score:int = 1500
Message:string = "Your score: {Score}"  # Implicit ToString() call
```
<!--verse
}
-->

Type casting becomes particularly interesting with parametric types. When you have a generic function, you can constrain type parameters to ensure certain operations are available:

<!--NoCompile-->
```verse
DoIt(Value:t where t:subtype(any)):void =
```

The constraint `t:subtype(any)` is redundant since all types are subtypes of `any` (inface the above would behave identifcally if one had written `t:type`), but it makes the type parameter explicit and allows for more specific constraints in real code.

## Where Clauses

Where clauses are the mechanism for constraining type parameters in generic code. They appear after type parameters and specify requirements that types must satisfy to be valid arguments. This creates a powerful system for writing generic code that is both flexible and type-safe.

<!--verse
using { /Verse.org/VerseCLR }
-->
```verse
# Simple subtype constraint
Process(Value:t where t:subtype(comparable)):void =
    if (Value = Value):  # We know t supports equality
        Print("Value equals itself")
```

Using the same type in multiple constraints is not yet supported, when implemented, it will allow to write code such as:

<!--NoCompile-->
```verse
# Multiple constraints on the same type
Transform(Input:t where t:subtype(comparable), t:subtype(printable)):t = # Not supported, yet 
    Print("Processing: {Input}")
    Input
```

Where clauses become more powerful when working with multiple type parameters:

```verse
# Independent constraints on different parameters
Combine(A:t1, B:t2 where t1:type, t2:type):tuple(t1, t2) =
    (A, B)

# Related constraints
Convert(From:t1, Converter:type{_(t1):t2} where t1:type, t2:type):t2 =
    Converter(From)
```

Where clauses can express sophisticated relationships between types:

```verse
# Constraint that ensures compatible types for an operation
Merge(Container1:[]t, Container2:[]t where t:subtype(comparable)):[]t =
    var Result:[]t = Container1
    for (Element : Container2, not Contains(Result, Element)):
        set Result += array{Element}
    Result

# Function type constraints
ApplyTwice(F:type{_(:t):t}, Value:t where t:type):t =
    F(F(Value))
```

Where clauses enable sophisticated generic programming patterns:

```verse
MapFunction(F:type{_(a):b}, Container:[]a where a:type, b:type):[]b =
    for (Element : Container):
        F(Element)
```

<!--  THIS DON'T WORK  ... sadly

Verse's type inference works with where clauses to deduce type parameters:

```verse
# Type parameters can often be inferred
AutoProcess(Items:[]t where t:subtype(comparable)):void =
    for (Item : Items):
        Print("Item: {Item}")

# Called without explicit type arguments
MyInts:[]int = array{1, 2, 3}
AutoProcess(MyInts)  # t is inferred as int

# Explicit type arguments when needed
AutoProcess([]nat)(MyNaturals)  # Explicitly specify t as nat
```

### Practical Applications

Where clauses are essential for writing reusable, type-safe code:

```verse
# Generic sorting with comparison constraint
Sort(Items:[]t where t:subtype(comparable)):[]t =
    # Implementation can use < and > because t is comparable
    var Sorted:[]t = array{}
    for (Item : Items):
        set Sorted = InsertInOrder(Sorted, Item)
    Sorted

# Generic caching with hashable constraint
cache(key_type, value_type where key_type:subtype(hashable)) := class:
    Storage:[key_type]value_type = map{}

    Get(Key:key_type):?value_type =
        if (Value := Storage[Key]):
            option{Value}
        else:
            false

    Put(Key:key_type, Value:value_type):void =
        set Storage[Key] = Value

# Builder pattern with type safety
builder(t where t:type) := class:
    var Current:?t = false

    With(Modifier:type{_(t):t}):builder(t) =
        if (C := Current?):
            set Current = option{Modifier(C)}
        self

    Build()<decides>:t =
        Current?
```

Where clauses thus provide the foundation for Verse's generic programming capabilities, allowing you to write code that is both highly reusable and completely type-safe. They enable you to express precise requirements about types while maintaining the flexibility to work with any types that meet those requirements.

-->

## Comparable and Equality

The `comparable` type represents a special subset of types that support equality comparison. Not all types can be compared for equality - this is a deliberate design choice that prevents meaningless comparisons and ensures that equality has well-defined semantics.

A type is comparable if its values can be meaningfully tested for equality. The basic scalar types are all comparable: `int`, `float`, `rational`, `logic`, `char`, and `char32`. Compound types are comparable if all their components are comparable. This means arrays of integers are comparable, tuples of floats and strings are comparable, and maps with comparable keys and values are comparable.

The equality operators `=` and `<>` are defined in terms of the comparable type:

<!--NoCompile-->
```verse
operator'='(X:t, Y:t where t:subtype(comparable))<decides>:t
operator'<>'(X:t, Y:t where t:subtype(comparable))<decides>:t
```

This signature reveals something subtle: both operands must be of the same type. This prevents nonsensical comparisons while allowing flexibility within type hierarchies:

<!--NoCompile-->
```verse
0 = 0        # Succeeds - both are int
0.0 = 0.0    # Succeeds - both are float
0 = 0.0      # Fails - int and float don't share a subtype relationship
```

Classes require special handling for comparability. By default, class instances are not comparable because there's no universal way to define equality for user-defined types. However, you can make a class comparable using the `unique` specifier:

<!--verse
entity := class<unique>:
    ID:int
    Name:string

F()<decides>:void={
Player1 := entity{ID := 1, Name := "Alice"}
Player2 := entity{ID := 1, Name := "Alice"}
Player3 := Player1

Player1 = Player2  # Fails - different instances
Player1 = Player3  # Succeeds - same instance
}<#
-->
```verse
entity := class<unique>:
    ID:int
    Name:string

Player1 := entity{ID := 1, Name := "Alice"}
Player2 := entity{ID := 1, Name := "Alice"}
Player3 := Player1

Player1 = Player2  # Fails - different instances
Player1 = Player3  # Succeeds - same instance
```
<!--verse
#>
-->

With the `unique` specifier, instances are only equal to themselves (identity equality), not to other instances with the same field values (structural equality). This provides a clear, predictable semantics for class equality.

The comparable type also constrains what can be used as map keys. Map keys must be comparable so the map can determine whether a key already exists. However, not all comparable types can be map keys - currently, `float`, `option`, and classes (even with `unique`) cannot be used as map keys. This restriction exists because these types either have special values (like `NaN` for floats) or reference semantics that complicate map implementation.

There is currently no way to make a class comparable by writing a comparison method. 

<!--TODO the above is right, right? It seems like a major limitation. People will invent their own solutions. -->


## Type Hierarchies

The type system forms a graph rather than a simple tree. This means types can have multiple supertypes, though multiple inheritance is currently limited to interfaces. Understanding these relationships helps you design flexible, reusable code.

At the top of the hierarchy, `any` serves as the universal supertype. Every type is a subtype of `any`, which means a value of any type can be assigned to a variable of type `any`. However, once a value is typed as `any`, you lose access to type-specific operations:

<!--verse
using { /Verse.org/VerseCLR }
ProcessValue(Value:any):void =
    # Can't do much with Value here - it could be anything
    Print("Got a value")  # About all we can do
F():void={
MyInt:int = 42
ProcessValue(MyInt)  # Works, but loses type information
}<#
-->
```verse
ProcessValue(Value:any):void =
    # Can't do much with Value here - it could be anything
    Print("Got a value")  # About all we can do

MyInt:int = 42
ProcessValue(MyInt)  # Works, but loses type information
```
<!--verse
#>
-->

The `void` type occupies the opposite position - it's the empty type with no values. Functions with `void` return type don't produce a value (though they actually return `false` for consistency). The `void` type is useful for marking functions that exist for their side effects:

<!--NoCompile-->
```verse
LogEvent(Event:string):void =
    WriteToFile(Event)
    UpdateCounter()
    # No explicit return needed, ignored if provided
```

Between these extremes, types form natural groupings. The numeric types (`int`, `float`, `rational`) share common arithmetic operations but don't form a single hierarchy - they're siblings rather than ancestors and descendants. The container types (arrays, maps, tuples, options) each have their own subtyping rules based on their element types.

Understanding variance is crucial for working with generic containers. Arrays and options are covariant in their element type - if A is a subtype of B, then `[]A` is a subtype of `[]B` and `?A` is a subtype of `?B`. This allows natural code like (assuming that Verse had a `nat` type):

```verse
ProcessNumbers(Numbers:[]int):void =
    for (N : Numbers):
        Print("{N}")

NaturalNumbers:[]nat = array{1, 2, 3}
ProcessNumbers(NaturalNumbers)  # Works due to covariance
```

Functions exhibit more complex variance. They're contravariant in their parameter types and covariant in their return types. A function type `(T1)->R1` is a subtype of `(T2)->R2` if T2 is a subtype of T1 (contravariance) and R1 is a subtype of R2 (covariance). This ensures that function subtyping preserves type safety:

```verse
function_type1 := type{_(:any):int}
function_type2 := type{_(:int):any}

# function_type1 is a subtype of function_type2
# It accepts more general input (any vs int)
# And returns more specific output (int vs any)
```

## Aliases and `type{}`

Verse's type system includes several advanced features that enable sophisticated programming patterns. Type aliases allow you to give meaningful names to complex types:

<!--verse
entity:=struct{}
-->
```verse
coordinate := tuple(float, float, float)
entity_map := [string]entity
update_handler := type{_(:float):void}
```

These aliases improve code readability and make refactoring easier. They're particularly valuable for function types, which can become syntactically complex.

<!-- TODO NOT IMPLEMENTED YET

The type construct provides runtime type information, enabling a form of reflection:

```verse
InspectType(Value:int):void =
    T := type{Value}
    # T now holds the type 'int'
```

This capability is useful for debugging and for writing generic code that needs to reason about types at runtime.
-->

Parametric types with multiple constraints allow specification of generic function requirements:

<!--verse
ArrayContains(A:[]t, B:t where t:subtype(comparable))<transacts><decides>:[]t = false
-->
```verse
Merge(A:[]t, B:[]t where t:subtype(comparable))<decides>:[]t =
    var Result:[]t = A
    for (Element : B, not ArrayContains(Result, Element)):
        set Result += array{Element}
    Result
```

This function requires that the element type be comparable (so we can check for duplicates) while maintaining type safety throughout.

<!-- Nah, don't dream

Another powerful pattern involves using parametric types with constraints to write generic but type-safe code:

```verse
FindMax(Values:[]t where t:subtype(comparable))<decides>:t =
    var Max:?t = false
    for (Value : Values):
        if (not Max?):
            set Max = option{Value}
        else if (Value > Max?):
            set Max = option{Value}
    Max?
```

This function works for any array of comparable values while maintaining full type safety. The constraint ensures that the greater-than operator is available, and the return type matches the element type of the input array.

-->

When designing class hierarchies, consider carefully whether to use inheritance or composition. Inheritance creates tight coupling but allows polymorphism, while composition provides flexibility but requires explicit delegation:

<!--verse
player := class{}
-->
```verse
# Inheritance approach
enhanced_player := class(player):
    PowerLevel:float = 1.0

# Composition approach
player_enhancement := struct:
    BasePlayer:player
    PowerLevel:float = 1.0
```

The inheritance approach allows an `enhanced_player` to be used anywhere a `player` is expected, while the composition approach keeps the types separate but requires accessing the nested `BasePlayer` field.
