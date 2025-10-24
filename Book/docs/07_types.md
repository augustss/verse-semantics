# Types

Every value has a type, and understanding the type system is fundamental to mastering any language. Types aren't merely labels - they form a rich hierarchy that governs how values flow through your program, what operations are permitted, and how the compiler reasons about your code. The type system combines static verification with practical flexibility, catching errors at compile time while still allowing sophisticated patterns of code reuse and abstraction.

At the apex of this hierarchy sits `any`, the universal supertype from which all other types descend. Another universal supertype is `void`, which accepts all values—every type is a subtype of `void`. At the opposite extreme lies `false`, the empty type that contains no values at all (the uninhabited or bottom type). Between these extremes exists a carefully designed lattice of types, each with its own capabilities and constraints.

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

## Numeric and String Conversions

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

These conversion functions are failable - they have the `<decides>` effect and will fail if passed non-finite values like `NaN` (Not a Number) or `Inf` (Infinity). These special float values represent undefined or infinite mathematical results. The explicit failure forces you to handle edge cases:

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

## The `any` Type

At the apex of Verse's type hierarchy sits `any`, the universal supertype that can hold a value of any type. Every type in Verse is a subtype of `any`, making it the most permissive type in the system. While this flexibility is powerful, it comes with significant trade-offs in type safety and capabilities.

### Understanding `any`

The `any` type serves as an escape hatch when you genuinely need to work with values of unknown or varying types. When you assign a value to an `any` variable, the value retains its runtime type information, but you lose compile-time knowledge of what operations are valid:

<!--verse
using { /Verse.org/VerseCLR }
-->
```verse
# Can hold any value
Value1:any = 42
Value2:any = "hello"
Value3:any = array{1, 2, 3}
Value4:any = player{Name := "Alice"}

# But you can't do much with them
# Value1 + 1         # Error: can't perform arithmetic on any
# Value2.Length      # Error: any doesn't have a Length field
# Value3[0]          # Error: can't index into any
```

Once a value is typed as `any`, you've effectively told the compiler "I don't know what this is," and the compiler responds by preventing most operations. This is by design—without knowing the actual type, the compiler cannot verify that operations are safe.

### Explicit Coercion to `any`

You can explicitly coerce any value to `any` using function call syntax:

```verse
# Explicit coercion
IntValue:int = 42
AnyValue:any = any(IntValue)

# Also works in expressions
ProcessValue(any(array{1, 2, 3}))
```

This explicit form makes it clear when you're intentionally widening a type to `any`, though assignment to an `any` variable performs this coercion implicitly.

### Implicit Coercion to `any`

Verse automatically coerces values to `any` in several contexts where types would otherwise be incompatible. Understanding these rules is crucial for working effectively with heterogeneous data.

**Mixed-type arrays** automatically become `[]any`:

```verse
# Array with different element types
MixedArray := array{42, "hello", true, 3.14}
# Type is []any

# Explicitly typed
Numbers:[]any = array{1, 2.0, "three"}
```

**Mixed-value maps** coerce their values to `any` while preserving key types:

```verse
# Map with different value types
MixedMap := map{0 => "zero", 1 => 1, 2 => 2.0}
# Type is [int]any

ConfigMap:[string]any = map{
    "count" => 42,
    "name" => "Player",
    "active" => true
}
```

**Conditional expressions** with disjoint branch types produce `any`:

```verse
# If branches return different types
GetValue(UseString:logic):any =
    if (UseString?):
        "text result"
    else:
        42

# Result type must be any since string and int don't share a common subtype
Result := GetValue(true)
```

**Logical OR with disjoint types** coerces to `any`:

```verse
# Returns either int or string
GetEither(Flag:logic, IntVal:int, StrVal:string):any =
    if (Flag?):
        option{IntVal}
    else:
        false
    or StrVal
```

**Container type coercions** allow more specific types to be used as more general ones:

```verse
# ?t coerces to ?any
OptionalInt:?int = option{42}
OptionalAny:?any = OptionalInt

# []t coerces to []any
IntArray:[]int = array{1, 2, 3}
AnyArray:[]any = IntArray

# [K]t coerces to [K]any
StringToInt:[string]int = map{"one" => 1}
StringToAny:[string]any = StringToInt

# tuple(t, u) coerces to tuple(any, any)
SpecificTuple:tuple(int, string) = (42, "hello")
AnyTuple:tuple(any, any) = SpecificTuple
```

These implicit coercions make working with heterogeneous data more ergonomic, automatically widening types when necessary.

### Limitations of `any`

The `any` type has important restrictions that reflect its role as a truly generic container:

**Not comparable**: You cannot use equality operators with `any`:

<!--NoCompile-->
```verse
# Error: any is not comparable
Value1:any = 42
Value2:any = 42
Value1 = Value2  # Compilation error
```

This restriction exists because equality comparison requires knowing the actual types being compared. Without type information, the compiler cannot generate correct comparison code.

**Cannot be a map key**: Because `any` is not comparable, it cannot be used as a map key type:

<!--NoCompile-->
```verse
# Error: any cannot be a map key
BadMap:[any]int = map{}  # Compilation error
```

Map operations require key comparison to determine if a key already exists, which is impossible without comparability.

**Limited operations**: Besides assignment and passing to functions, `any` values support very few operations directly. You cannot:

- Perform arithmetic or logical operations
- Access fields or methods
- Index into collections
- Call as a function

To perform any meaningful operations on an `any` value, you must first narrow it to a more specific type through casting or pattern matching.

### Working with `any`

Despite its limitations, `any` serves important purposes:

**Generic containers and configuration**:

```verse
# Configuration that holds various types
Config:[string]any = map{
    "window_width" => 1920,
    "window_height" => 1080,
    "fullscreen" => true,
    "title" => "My Game"
}
```

**Interfacing with dynamic systems**:

```verse
# JSON-like data structures
JsonValue := struct:
    Data:any

ParsedData := JsonValue{Data := map{
    "name" => "Player",
    "score" => 1500,
    "items" => array{"sword", "shield"}
}}
```

**Type erasure for storage**:

```verse
# Store different types in same collection
EventData:[]any = array{
    player{Name := "Alice"},
    vector3{X := 0.0, Y := 0.0, Z := 0.0},
    42,
    "message"
}
```

When using `any`, prefer to narrow back to specific types as quickly as possible through type checking or by maintaining separate type information alongside the `any` values.

## Class and Interface Casting

Verse provides two distinct casting mechanisms for classes and interfaces: fallible casts for runtime type checking, and infallible casts for compile-time verified conversions. Understanding when and how to use each is essential for working with inheritance hierarchies and polymorphic code.

### Fallible Casts: Runtime Type Checking

Fallible casts use square bracket syntax `TargetType[value]` to perform runtime type checks. These casts return an optional value (`?TargetType`), succeeding only if the value is actually of the target type or a subtype:

```verse
# Define a class hierarchy
component := class<castable>:
    Name:string = "Component"

physics_component := class<castable>(component):
    Velocity:float = 0.0

render_component := class<castable>(component):
    Material:string = "default"

# Runtime type checking with fallible casts
ProcessComponent(Comp:component):void =
    if (PhysicsComp := physics_component[Comp]):
        # Successfully cast - PhysicsComp is physics_component
        Print("Physics velocity: {PhysicsComp.Velocity}")
    else if (RenderComp := render_component[Comp]):
        # Different type - RenderComp is render_component
        Print("Render material: {RenderComp.Material}")
    else:
        # Neither type matched
        Print("Unknown component type")
```

The cast expression evaluates to `false` if the runtime type doesn't match, allowing you to use it directly in conditionals. The optional binding pattern `(Variable := Expression)` both performs the cast and binds the result to a variable when successful.

**Identity preservation**: For classes marked `<unique>`, fallible casts preserve identity—a successful cast returns the same instance, not a copy:

```verse
entity := class<unique><castable>:
    ID:int

player := class<unique>(entity):
    Name:string

# Create an instance
P := player{ID := 1, Name := "Alice"}

# Cast to base type
if (E := entity[P]):
    E = P  # True - same instance
```

### Fallible Cast Restrictions

Fallible casts work **only with class and interface types**. You cannot dynamically cast from or to primitive types, structs, arrays, or other value types:

<!--NoCompile-->
```verse
component := class<castable>{}

# Error: cannot cast from primitives
Comp := component[42]          # int to class - not allowed
Comp := component[3.14]        # float to class - not allowed
Comp := component["text"]      # string to class - not allowed
Comp := component[array{1,2}]  # array to class - not allowed

# Error: cannot cast to non-class types
Value := int[component{}]      # class to int - not allowed
Value := logic[component{}]    # class to logic - not allowed
Value := (?int)[component{}]   # class to option - not allowed
```

The restriction exists because fallible casts rely on runtime type information that only classes and interfaces maintain. Value types like integers and structs don't have runtime type tags.

### Infallible Casts: Compile-Time Verification

Infallible casts use parenthesis syntax `TargetType(value)` for conversions that the compiler can verify will always succeed. These casts require the source type to be a compile-time subtype of the target type:

```verse
# Upcasting: always safe, always succeeds
Base:component = physics_component{Velocity := 10.0}
BaseAgain:component = component(physics_component{Velocity := 5.0})
```

Infallible casts never fail at runtime because the type relationship is verified at compile time. If the compiler cannot prove the cast will succeed, it produces a compilation error:

<!--NoCompile-->
```verse
# Error: cannot cast parent to child (not a subtype)
Derived := physics_component(component{})  # Compilation error

# Error: unrelated types
Other := render_component(physics_component{})  # Compilation error
```

**Casting to `void`**: Any type can be infallibly cast to `void`, which discards the value:

```verse
void(42)           # Discard an integer
void("result")     # Discard a string
void(component{})  # Discard an object
```

This is occasionally useful when you need to call a function for its side effects but want to explicitly ignore its return value.

### When to Use Each Cast Type

Use **fallible casts** when:

- You need to test if a value is of a specific type at runtime
- Working with polymorphic collections where elements might be different types
- Implementing type-specific behavior based on actual runtime types
- Building systems with dynamic type dispatch

```verse
# Process different component types differently
for (Comp : AllComponents):
    if (Physics := physics_component[Comp]):
        UpdatePhysics(Physics)
    else if (Render := render_component[Comp]):
        UpdateRendering(Render)
```

Use **infallible casts** when:

- Explicitly documenting an upcast in the type hierarchy
- Converting a value to a less specific type for API compatibility
- Making type relationships explicit in the code

```verse
# Make the upcast explicit for clarity
StoreComponent(component(physics_component{}))

# Interface implementation
drawable := interface<castable>:
    Draw():void

sprite := class(drawable):
    Draw<override>():void = Print("Drawing sprite")

# Explicit upcast to interface
DrawableObject:drawable = drawable(sprite{})
```

### Dynamic Type-Based Casting

Types in Verse are first-class values, which means you can store types in variables and use them dynamically for casting. This enables powerful patterns for runtime polymorphism:

```verse
# Type hierarchy
component := class<castable>{}
physics_component := class<castable>(component){}
render_component := class<castable>(component){}

# Store types as values
ComponentType:castable_subtype(component) = physics_component

# Cast using the stored type
TestComponent(Comp:component, ExpectedType:castable_subtype(component)):logic =
    if (Specific := ExpectedType[Comp]):
        true  # Component matches expected type
    else:
        false

# Use with different types
P := physics_component{}
TestComponent(P, physics_component)  # true
TestComponent(P, render_component)   # false
```

This pattern is particularly powerful when the type to check isn't known until runtime:

```verse
# Select type based on configuration
GetComponentType(Config:string):castable_subtype(component) =
    if (Config = "physics"):
        physics_component
    else if (Config = "render"):
        render_component
    else:
        component

# Use the dynamically selected type
RequiredType := GetComponentType(LoadedConfig)
for (Comp : Components):
    if (Specific := RequiredType[Comp]):
        # Process components of the required type
        ProcessSpecific(Specific)
```

This bridges compile-time type safety with runtime flexibility, allowing type decisions to be made based on program state while maintaining type correctness.

### Casting Best Practices

When working with casts:

**Prefer static types**: Use specific types when possible rather than casting from generic types. This catches errors at compile time.

**Use type hierarchies carefully**: Design class hierarchies so that you rarely need to downcast. Frequent downcasting often indicates a design issue.

**Check before casting**: When using fallible casts, always handle the failure case:

```verse
if (Specific := specific_type[value]):
    # Success path
    UseSpecific(Specific)
else:
    # Failure path - value wasn't the expected type
    HandleUnexpectedType()
```

**Document cast rationale**: When casts are necessary, comment why they're safe or what invariant ensures they'll succeed:

```verse
# Safe cast: this function only called with physics components
ProcessPhysics(Comp:component):void =
    if (Physics := physics_component[Comp]):
        # Invariant: caller guarantees this is a physics component
        UpdateVelocity(Physics)
```

Casting is a powerful feature but represents a departure from compile-time verification. Use it judiciously and with clear understanding of the runtime type relationships in your code.

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

## Refinement Types: Value-Level Constraints

While `where` clauses constrain type parameters in generic code, **refinement types** use `where` to constrain the *values* a type can hold. This creates subtypes that only accept values satisfying specific conditions, enabling domain-specific constraints enforced by the type system.

### Basic Syntax

A refinement type defines a constrained subtype using value predicates:

```verse
# Percentages: floats between 0.0 and 1.0
percent := type{_X:float where 0.0 <= _X, _X <= 1.0}

# Valid assignments
Opacity:percent = 0.5
Alpha:percent = 1.0

# Invalid: out of range (runtime check fails)
# BadPercent:percent = 1.5  # Fails at assignment
```

**Syntax structure:**

```verse
TypeName := type{_Variable:BaseType where Constraint1, Constraint2, ...}
```

- `_Variable` is a placeholder for the value being constrained
- `BaseType` is `int` or `float`
- Constraints are comparison expressions using `<=`, `<`, `>=`, `>`, or `=`

### Integer Refinement Types

Integer refinements restrict int values to specific ranges:

```verse
# Age between 0 and 120
age := type{_X:int where 0 <= _X, _X <= 120}

ValidAge:age = 25
# InvalidAge:age = 150  # Fails constraint

# Positive integers
positive_int := type{_X:int where _X > 0}

Count:positive_int = 42
# Zero:positive_int = 0  # Fails: not positive

# Range with single bound
small_int := type{_X:int where _X < 100}
```

### Float Refinement Types

Float refinements handle continuous ranges with IEEE 754 semantics:

```verse
# Unit interval [0.0, 1.0]
normalized := type{_X:float where 0.0 <= _X, _X <= 1.0}

# Positive floats
positive := type{_X:float where _X > 0.0}

# Temperature in Celsius above absolute zero
celsius := type{_X:float where _X >= -273.15}
```

**Finite Floats (Excluding Infinity):**

```verse
# Finite values only (no ±Inf)
finite := type{_X:float where -Inf < _X, _X < Inf}

# Maximum and minimum finite IEEE 754 doubles
MaxFinite:finite = 1.7976931348623157e+308
MinFinite:finite = -1.7976931348623157e+308

# Invalid: infinities excluded
# Infinite:finite = Inf  # Fails constraint
```

**Infinity Types:**

```verse
# Only positive infinity
infinity_type := type{_X:float where 1.7976931348623157e+308 < _X}

PosInf:infinity_type = Inf  # Valid
# Finite:infinity_type = 100.0  # Fails: not infinite

# Only negative infinity
neg_infinity := type{_X:float where -1.7976931348623157e+308 > _X}

NegInf:neg_infinity = -Inf  # Valid
```

### IEEE 754 Edge Cases

**Negative Zero:**

IEEE 754 distinguishes between `+0.0` and `-0.0`. Refinement types respect this:

```verse
# Negative values (excludes both zeros)
negative := type{_X:float where _X < 0.0}

negative[-1.0]          # Valid
negative[-0.5]          # Valid
negative[0.0 / -1.0]    # Fails: produces -0.0, not truly negative
```

The expression `0.0 / -1.0` produces `-0.0`, which is **not** less than `0.0` in IEEE 754 semantics, so it fails the constraint.

**Positive vs Zero:**

```verse
# Positive (excludes zero)
positive := type{_X:float where _X > -0.0}

positive[1.0]   # Valid
positive[0.1]   # Valid
positive[0.0]   # Fails: zero not considered positive
```

**Floating-Point Precision:**

Constraints respect exact IEEE 754 representations:

```verse
# Values strictly less than 0.1
small_float := type{_X:float where _X < 0.1}

# Valid: largest float before 0.1
Tiny:small_float = 0.09999999999999999167332731531132594682276248931884765625

# Invalid: 0.1's actual representation is slightly above 0.1
# NotSmall:small_float = 0.1000000000000000055511151231257827021181583404541015625
```

The decimal `0.1` cannot be represented exactly in binary floating-point, so the actual stored value is slightly above the mathematical 0.1.

### Constraint Expression Restrictions

Refinement type constraints have strict limitations on what expressions are allowed:

**Only Literal Values:**

Constraints must use literal numbers, not variables or expressions:

```verse
# Valid: literal float
bounded := type{_X:float where _X < 100.0}

# Invalid: cannot use variables
Limit:float = 100.0
# bad_type := type{_X:float where _X < Limit}  # ERROR 3502

# Invalid: cannot use function calls
GetMax():float = 100.0
# bad_type := type{_X:float where _X < GetMax()}  # ERROR 3502

# Invalid: cannot use qualified names
config := module{Max:float = 100.0}
# bad_type := type{_X:float where _X < (config:)Max}  # ERROR 3502
```

This ensures constraints are statically known at compile time.

**Float Literals Required for Float Types:**

When constraining floats, bounds must be float literals (with decimal point):

```verse
# Invalid: integer literal in float constraint
# bad_float := type{_X:float where _X <= 142}  # ERROR 3502

# Valid: float literal
good_float := type{_X:float where _X <= 142.0}
```

**NaN Not Allowed:**

`NaN` (Not a Number) cannot appear in constraints:

```verse
# Invalid: NaN in constraint
# nan_type := type{_X:float where _X <= NaN}      # ERROR 3502
# nan_type := type{_X:float where NaN <= _X}      # ERROR 3502
# nan_type := type{_X:float where 0.0/0.0 <= _X}  # ERROR 3502
```

Since `NaN` comparisons are always false, such constraints would be meaningless.

**Allowed Literal Forms:**

- Float literals: `1.0`, `3.14`, `-2.5`, `1.7976931348623157e+308`
- Integer literals: `0`, `42`, `-100` (for int refinements)
- Special float values: `Inf`, `-Inf`

### Runtime Checking with Fallible Casts

Refinement types are checked at assignment and through fallible casts:

```verse
percent := type{_X:float where 0.0 <= _X, _X <= 1.0}

# Direct assignment (compile-time known)
Valid:percent = 0.5  # OK

# Runtime check with fallible cast
UserInput:float = GetInputFromUser()
if (Value := percent[UserInput]):
    # UserInput was in [0.0, 1.0]
    ProcessPercent(Value)
else:
    # Out of range
    ShowError()
```

The cast `percent[UserInput]` returns `?percent`—succeeding if the value satisfies the constraint, failing otherwise.

### Using Refinement Types in Functions

Refinement types work as parameter and return types:

```verse
finite := type{_X:float where -Inf < _X, _X < Inf}

# Parameter with constraint
Half(X:finite):float = X / 2.0

Half(100.0)  # Returns 50.0
Half(1.0)    # Returns 0.5

# Cannot pass infinity
# Half(Inf)  # ERROR 3509: Inf not in finite
```

**Coercion and Negation:**

```verse
percent := type{_X:float where 0.0 <= _X, _X <= 1.0}
negative_percent := type{_X:float where _X <= 0.0, _X >= -1.0}

MakePercent():percent = 0.5

# Negation preserves constraint compatibility
NegValue:negative_percent = -MakePercent()  # -0.5 valid

# Multiple negations
NegValue2:negative_percent = ---0.7  # Triple negation = -0.7
```

### Overloading Restrictions

Overlapping refinement types cannot be used for function overloading—they're ambiguous:

```verse
percent := type{_X:float where 0.0 <= _X, _X <= 1.0}
not_infinity := type{_X:float where Inf > _X}

# ERROR 3532: Cannot distinguish - percent ⊂ not_infinity
# F(X:percent):float = 0.0
# F(X:not_infinity):float = X

# Calling F(0.5) would be ambiguous - which overload?
```

However, **disjoint** refinement types can overload:

```verse
positive := type{_X:float where _X > 0.0}
negative := type{_X:float where _X < 0.0}

# Valid: ranges don't overlap (zero excluded from both)
F(X:positive):float = X
F(X:negative):float = X + 1.0

F(1.0)   # Returns 1.0 (positive overload)
F(-1.0)  # Returns 0.0 (negative overload)
# F(0.0)  # Would fail - neither overload matches
```

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

### Comparable as a Generic Constraint

The `comparable` type is commonly used as a constraint in generic functions to ensure operations like equality testing are available:

```verse
Find(Items:[]t, Target:t where t:subtype(comparable))<decides>:int =
    for (Item:Items, Index->Item):
        if (Item = Target):
            return Index
    -1  # Not found

# Works with any comparable type
Position := Find[array{"apple", "banana", "cherry"}, "banana"]  # Returns 1
```

### Array-Tuple Comparison

A notable feature of Verse's equality system is that arrays and tuples of comparable elements can be compared with each other:

```verse
# Arrays can equal tuples
array{1, 2, 3} = (1, 2, 3)       # Succeeds
(4, 5, 6) = array{4, 5, 6}       # Succeeds - bidirectional

# Inequality also works
array{1, 2, 3} <> (1, 2, 4)      # Succeeds - different values
```

This comparison works structurally - the sequences must have the same length and corresponding elements must be equal. This feature allows functions expecting arrays to accept tuples, increasing flexibility.

### Overload Distinctness with Comparable

You cannot create overloads where one parameter is a specific comparable type and another is the general `comparable` type, as this creates ambiguity:

```verse
# Not allowed - ambiguous overloads
F(X:int):void = {}
F(X:comparable):void = {}  # ERROR: int is already comparable

# Not allowed with unique classes either
unique_class := class<unique>{}
G(X:unique_class):void = {}
G(X:comparable):void = {}  # ERROR: unique_class is comparable
```

However, you can overload with non-comparable types:

```verse
# This is allowed
regular_class := class{}  # Not comparable
H(X:regular_class):void = {}
H(X:comparable):void = {}  # OK: no ambiguity
```

### Dynamic Comparable Values

When working with heterogeneous collections, you may need to box comparable values into the `comparable` type explicitly. These boxed values maintain their equality semantics:

```verse
AsComparable(X:comparable):comparable = X

# Boxed values compare correctly with both boxed and unboxed
array{AsComparable(1)} = array{1}              # Succeeds
array{AsComparable(1)} = array{AsComparable(1)} # Succeeds
array{AsComparable(1)} <> array{2}             # Succeeds
```

This allows you to create collections that mix different comparable types by boxing them all to `comparable`.

### Map Keys and Comparable

Map keys must be comparable types. Most comparable types can be used as map keys, including:

- All numeric types: `int`, `float`, `rational`
- Character types: `char`, `char32`
- Text: `string`
- Enumerations
- `<unique>` classes
- Optionals of comparable types: `?t` where `t` is comparable
- Arrays of comparable types: `[]t` where `t` is comparable
- Tuples of comparable types
- Maps with comparable keys and values: `[k]v`
- Structs with comparable fields

Note that while `float` can be used as a map key, floating-point special values have specific equality semantics (see [Map documentation](02_builtins.md#floating-point-keys) for details on `NaN` and zero handling).

There is currently no way to make a regular class comparable by writing a custom comparison method. Only the `<unique>` specifier enables class comparability through identity equality.

<!--TODO the above is right, right? It seems like a major limitation. People will invent their own solutions. -->

## Generators: Lazy Sequences

Generators represent lazy sequences that produce values on demand rather than storing all elements in memory. Unlike arrays which materialize all elements upfront, generators compute each value only when requested during iteration. This makes them memory-efficient for large or infinite sequences, and essential for scenarios where you're processing streaming data or expensive computations.

### Basic Syntax and Type

Generators use the parametric type `generator(t)` where `t` is the element type:

```verse
# Generator of integers
IntSequence:generator(int) = MakeIntegerSequence()

# Generator of custom classes
entity := class:
    ID:int

EntityStream:generator(entity) = GetAllEntities()
```

**Important syntax restrictions:**

```verse
# Correct: Use parentheses
ValidGenerator:generator(int) = GetSequence()

# Wrong: Square brackets are invalid
# BadGenerator:generator[int] = GetSequence()  # ERROR 3511

# Wrong: Curly braces are invalid
# BadGenerator:generator{int} = GetSequence()  # ERROR 3506
```

Element types must be valid Verse types, not literals or expressions:

```verse
# Valid
generator(int)
generator(string)
generator(my_class)

# Invalid: Cannot use literals
# generator(1)        # ERROR 3547
# generator("text")   # ERROR 3547
```

Constrained types work as element types:

```verse
# Valid: Constrained element type
PositiveInts:generator(type{X:int where X > 0, X < 10}) = GetConstrainedSequence()
```

### Using Generators in For Loops

The primary way to consume generators is through `for` expressions:

```verse
# Direct iteration
ProcessStream()<transacts>:void =
    for (Item : GetIntegerSequence()):
        Print("{Item}")

# Store in variable first
ProcessWithVariable()<transacts>:void =
    Sequence := GetIntegerSequence()
    for (Item : Sequence):
        Print("{Item}")
```

Generators work with arrow syntax in loops, showing that domain and range are identical:

```verse
DoubleCheck():logic =
    for (Index->Value : GetFloatSequence()):
        # Index and Value are the same
        Index = Value
```

**Multiple generators in one loop:**

```verse
ProcessPairs()<transacts>:void =
    var Total:float = 0.0
    for (A : GetFloatSequence(), B : GetFloatSequence()):
        set Total += A + B
```

**Combining generators with conditions:**

```verse
FilteredSum()<transacts>:float =
    var Total:float = 0.0
    for (
        A : GetFloatSequence(),
        B : array{1.0, 2.0, 4.0, 8.0},
        A <> 4.0,
        B <> 4.0
    ):
        set Total += A + B
    Total
```

### Generators as Function Parameters and Returns

Generators are first-class types and can be used anywhere types appear:

**As parameters:**

```verse
SumSequence(Values:generator(float)):float =
    var Total:float = 0.0
    for (Value : Values):
        set Total += Value
    Total

Result := SumSequence(GetFloatSequence())
```

**As return values:**

```verse
MakeSequence()<transacts>:generator(int) =
    GetIntegerSequence()

# Use returned generator
for (Item : MakeSequence()):
    ProcessItem(Item)
```

**As class fields:**

```verse
stream_processor := class:
    Source:generator(int)

    Process():int =
        var Product:int = 1
        for (Value : Source):
            set Product *= Value
        Product

Processor := stream_processor{Source := GetIntegerSequence()}
Result := Processor.Process()
```

### Type Conversion and Restrictions

Generators have strict type conversion rules to maintain safety:

**Cannot convert arrays to generators:**

```verse
Numbers := array{1, 2, 3}
# Seq:generator(int) = Numbers  # ERROR 3509
```

**Cannot convert between incompatible element types:**

```verse
IntSeq := GetIntegerSequence()
# FloatSeq:generator(float) = IntSeq  # ERROR 3509
```

**Cannot index generators like arrays:**

```verse
Seq := GetIntegerSequence()
# Value := Seq[0]  # ERROR 3509
# Generators don't support random access
```

**Converting generators to arrays:**

Use a `for` expression to materialize the sequence:

```verse
GeneratorToArray(Gen:generator(t) where t:type):[]t =
    for (Item : Gen):
        Item

Numbers := GeneratorToArray(GetIntegerSequence())
# Numbers is now array{1, 2, 3, 4}
```

### Generator Covariance with Class Hierarchies

Generators are **covariant** in their element type when the element type has subtyping relationships:

```verse
animal := class:
    Name:string

dog := class(animal):
    Breed:string

# Covariant: generator(dog) is a subtype of generator(animal)
DogStream:generator(dog) = GetDogSequence()
AnimalStream:generator(animal) = DogStream  # OK - covariance

# Cannot upcast: generator(animal) is NOT a subtype of generator(dog)
GeneralStream:generator(animal) = GetAnimalSequence()
# SpecificStream:generator(dog) = GeneralStream  # ERROR 3509
```

This covariance enables flexible APIs:

```verse
# Function accepting generator of base type
ProcessAnimals(Animals:generator(animal)):void =
    for (A : Animals):
        Print(A.Name)

# Can pass generator of derived type
ProcessAnimals(GetDogSequence())  # OK due to covariance
```

### Type Joining with Generators

When conditionally selecting between generators, Verse finds the least common supertype:

```verse
base := class:
    ID:int

child1 := class(base):
    Extra1:string

child2 := class(base):
    Extra2:int

# Conditional selection finds common supertype
GetStream(UseFirst:logic):generator(base) =
    if (UseFirst?):
        GetChild1Sequence()  # Returns generator(child1)
    else:
        GetChild2Sequence()  # Returns generator(child2)
    # Result type: generator(base)
```

Similar to effect joining, the compiler computes the least upper bound (join) of the generator element types.

### Parametric Functions with Generators

Generators work naturally with parametric functions:

```verse
# Generic function accepting any generator
GetFirstValue(Seq:generator(t) where t:type)<decides>:t =
    for (Value : Seq):
        return Value
    false?  # Fails if sequence is empty

# Works with any element type
FirstInt := GetFirstValue[GetIntegerSequence()]?
FirstFloat := GetFirstValue[GetFloatSequence()]?
FirstEntity := GetFirstValue[GetEntitySequence()]?
```

### Optional Generators

Generators can be optional, allowing you to represent "maybe a sequence":

```verse
# Optional generator
MaybeGetSequence()<decides>:?generator(int) =
    if (SomeCondition?):
        option{GetIntegerSequence()}
    else:
        false

# Check if sequence exists
if (Seq := MaybeGetSequence[]):
    for (Item : Seq?):
        ProcessItem(Item)
```

### Mutable Generator Variables

Generator variables can be mutable, allowing you to swap between different sequences:

```verse
ProcessDynamicStream()<transacts>:void =
    var CurrentStream:generator(float) = GetFloatSequence()

    # Process first sequence
    SumSequence(CurrentStream)

    # Switch to different sequence
    set CurrentStream = GetAnotherFloatSequence()

    # Process second sequence
    SumSequence(CurrentStream)
```

### Generators in Parametric Types

Generators can be used as type arguments to other parametric types:

```verse
# Wrapper class parameterized over generator
stream_wrapper(element_type:type) := class:
    Source:generator(element_type)

    Process():[]element_type =
        for (Item : Source):
            Item

IntWrapper := stream_wrapper(int){Source := GetIntegerSequence()}
IntArray := IntWrapper.Process()
```

### Design Patterns and Best Practices

**Lazy evaluation:**

Generators defer computation until values are needed:

```verse
# Expensive computation only happens during iteration
ExpensiveSequence():generator(int) =
    # Setup happens once
    GenerateExpensiveData()

# Computation deferred until loop executes
for (Value : ExpensiveSequence()):
    if (Value > 100):
        break  # Can stop early, avoiding unnecessary work
```

**Stream processing:**

Chain operations without materializing intermediate collections:

```verse
# Transform one generator into another
FilterSequence(
    Source:generator(int),
    Predicate(:int)<decides>:void
):[]int =
    for (Value : Source, Predicate[Value]):
        Value

Positives := FilterSequence(
    GetIntegerSequence(),
    (X:int) => X > 0
)
```

**Infinite sequences:**

Generators can represent infinite sequences safely:

```verse
# Infinite counter
Counter():generator(int) = GenerateInfiniteInts()

# Process only what you need
FirstTen:[]int =
    var Count:int = 0
    for (Value : Counter(), Count < 10):
        set Count += 1
        Value
```

**Resource management:**

Generators can encapsulate resource lifecycle:

```verse
# Generator that manages file reading
ReadFileLines(Path:string):generator(string) =
    OpenAndStreamFile(Path)

# Resource cleanup happens when iteration completes
for (Line : ReadFileLines("data.txt")):
    ProcessLine(Line)
# File automatically closed when generator exhausted
```

### Constraints and Limitations

**No random access:**

Generators don't support indexing or random access operations. They're strictly sequential:

```verse
Seq := GetSequence()
# Value := Seq[5]  # ERROR: No indexing
# Length := Seq.Length  # ERROR: No length property
```

**No reusability:**

Most generators can only be iterated once. After consuming a generator, it's exhausted:

```verse
Seq := GetSequence()

# First iteration works
for (Item : Seq):
    ProcessItem(Item)

# Second iteration may be empty or error
for (Item : Seq):
    # Might not execute - generator already consumed
```

**Cannot cast from `any`:**

While you can cast a generator to `any`, you cannot cast an `any` value back to a generator:

```verse
Seq:generator(int) = GetSequence()
GenericValue:any = Seq  # OK - upcast to any

# Invalid: Cannot cast any to generator
# Recovered:generator(int) = any[GenericValue]  # ERROR 3509
```

**Version gating:**

Generators were introduced in Fortnite version 29.30. Code using generators won't compile for earlier versions:

```verse
# Requires UploadedAtFNVersion >= 2930
MyFunction():generator(int) = GetSequence()
```

### Summary

Generators provide:

- **Memory efficiency**: Process large sequences without materializing them
- **Lazy evaluation**: Compute values only when needed
- **Composability**: Chain operations naturally with `for` expressions
- **Type safety**: Covariant in element type with strict conversion rules
- **Integration**: First-class types usable anywhere types appear

Key rules to remember:

- Syntax: `generator(element_type)` with parentheses
- Primary usage: `for (Item : Generator)`
- Covariant in element type (child generator → parent generator)
- Cannot convert arrays to generators or vice versa directly
- Use `for` expressions to materialize generators into arrays
- Cannot index or randomly access generator elements
- Most generators are single-use (consumed after iteration)

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

The `void` type is another universal supertype alongside `any`. **Every type is a subtype of `void`**, meaning `void` accepts all values. This is fundamentally different from `false`, the true empty/bottom type.

### Understanding void as Universal Supertype

Unlike `any`, which erases type information, `void` serves as a "discard" type indicating that a value's specific type doesn't matter:

```verse
# void accepts any value
X:void = 42              # int → void ✓
Y:void = 3.14            # float → void ✓
Z:void = "hello"         # string → void ✓
W:void = array{1, 2}     # []int → void ✓
```

**void as return type:**

Functions with `void` return type can return any value, which is then discarded by the type system:

```verse
LogEvent(Message:string):void =
    WriteToFile(Message)
    UpdateCounter()
    42                   # Returns int, but typed as void

F():void = 1             # Valid - returns int, typed as void
F()                      # Result is void
```

Despite being typed as `void`, these functions still produce their computed values—the values are simply not accessible through the type system. This ensures side effects and computations occur even when the return value is discarded:

```verse
MakePair(X:string, Y:string):void = (X, Y)

# Function computes the pair even though return type is void
MakePair("hello", "world")  # Still creates ("hello", "world")
```

**void as parameter type:**

Functions with `void` parameters accept any argument type:

```verse
Discard(X:void):int = 42

Discard(0)               # int → void ✓
Discard(1.5)             # float → void ✓
Discard("test")          # string → void ✓
Discard[array{1, 2}]     # []int → void ✓

# Multiple void parameters
Process(X:void, Y:void):int = 100
Process(42, "hello")     # Different types OK
```

**void in class fields:**

Class fields can be typed as `void`, accepting any initialization value:

```verse
config := class:
    Setting:void = array{1, 2}  # Default with array

# Can initialize with different type
Instance := config{Setting := "custom"}
```

**void with generic types:**

`void` works in generic contexts as a universal supertype:

```verse
# Optional void
X:?int = option{42}
Y:?void = X              # ?int → ?void ✓

# Array of void
Numbers:[]int = array{1, 2, 3}
AnyArray:[]void = Numbers  # []int → []void ✓

# Map to void
IntMap:[string]int = map{"a" => 1}
VoidMap:[string]void = IntMap  # [string]int → [string]void ✓
```

**Function type variance with void:**

In function types, `void` participates in variance:

```verse
IntIdentity(X:int):int = X

# Contravariant return: supertype in return position
F:int->void = IntIdentity  # int->int → int->void ✓
# void is supertype of int, so this works

AcceptVoid(X:void):int = 19

# Contravariant parameter: supertype in parameter position
G:int->int = AcceptVoid    # void->int → int->int ✓
# Can use function accepting void where function accepting int expected
```

However, `void` in parameter position does NOT allow conversion the other way:

```verse
IntFunction(X:int):int = X
# F:void->int = IntFunction  # ERROR 3509
# Cannot convert int parameter to void parameter in function type
```

### void vs any

While both `void` and `any` are universal supertypes, they serve different purposes:

**`any`**: Preserves the value for later type-checking

```verse
X:any = 42
# Can later check: if (Y := int[X])
```

**`void`**: Explicitly discards type information

```verse
X:void = 42
# Cannot recover the int-ness of X
```

### void vs false

The `false` type is the empty/bottom type (uninhabited type) with no values. It's the opposite of `void`:

- **`void`**: Universal supertype - all types are subtypes of void, contains all values
- **`false`**: Bottom type - subtype of all types, contains zero values

```verse
# void accepts everything
X:void = anything

# false produces nothing (only appears as result of failable operations)
if (Y := SomethingThatFails?):
    # Y is the successful type
else:
    # Else branch has type false - unreachable if we don't handle it
```

### Practical Uses

**Ignoring return values:**

```verse
ProcessData():int =
    # Complex computation
    42

IgnoreResult():void = ProcessData()  # Compute but discard result
```

**Generic discard function:**

```verse
AcceptAnything(X:void):void = {}

AcceptAnything(42)
AcceptAnything("test")
AcceptAnything(array{1, 2, 3})
```

**Marking side-effect-only functions:**

```verse
LogMessage(Msg:string):void =
    Print(Msg)
    WriteToFile(Msg)
    # void signals: call for effects, not return value
```

Between the universal supertypes (`any`, `void`) and the bottom type (`false`), types form natural groupings. The numeric types (`int`, `float`, `rational`) share common arithmetic operations but don't form a single hierarchy - they're siblings rather than ancestors and descendants. The container types (arrays, maps, tuples, options) each have their own subtyping rules based on their element types.

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

## Type Aliases

Type aliases allow you to create alternative names for types, making complex type signatures more readable and maintainable. They're particularly valuable for function types, parametric types, and frequently-used type combinations.

### Basic Syntax

A type alias is created using simple assignment syntax at module scope:

<!--verse
entity:=struct{}
-->
```verse
# Simple type aliases
coordinate := tuple(float, float, float)
entity_map := [string]entity
player_id := int

# Function type aliases
update_handler := type{_(:float):void}
validator := int -> logic
transformer := type{_(:string):int}
```

Type aliases are compile-time only - they create no runtime overhead and are purely for programmer convenience and code clarity.

### What Type Aliases Are

**Type aliases are alternative names, not new types.** They don't create distinct types like `newtype` in some languages. Values of the alias and the original type are completely interchangeable:

```verse
player_id := int
game_id := int

ProcessPlayer(ID:player_id):void = {}
ProcessGame(ID:game_id):void = {}

PID:player_id = 42
GID:game_id = 42

# These all work - aliases are just names
ProcessPlayer(PID)      # OK
ProcessPlayer(GID)      # OK - game_id is also int
ProcessPlayer(42)       # OK - int literal works too
ProcessGame(PID)        # OK - player_id is also int
```

### Module Scope Requirement

**Type aliases can ONLY be defined at module scope.** They cannot be defined inside classes, functions, or any nested scope:

```verse
# VALID: Module scope
coordinate := tuple(float, float)

MyFunction():void =
    # INVALID: Cannot define in function (ERROR 3502)
    # local_alias := int
    {}

my_class := class:
    # INVALID: Cannot define in class (ERROR 3502)
    # member_alias := string
    {}
```

This restriction ensures type aliases have consistent visibility and prevents scope-dependent type interpretations.

### Forward Reference Restrictions

Type aliases must be defined **before** they are used. Forward references are not allowed:

```verse
# VALID: Definition before use
IntArray := []int
IntMap := [string]IntArray    # OK - IntArray already defined

# INVALID: Forward reference (ERROR 3502)
# ForwardMap := [string]ForwardArray
# ForwardArray := []int          # Too late!
```

This applies to all type constructs that reference aliases:

```verse
# All require the alias to be defined first
A := int

B := ?A           # Optional of alias
C := []A          # Array of alias
D := [A]string    # Map with alias key
E := tuple(A, string)  # Tuple with alias
F := A -> logic   # Function type with alias
G := type{_(:A):void}  # Function type with alias
```

### What You Can Alias

Type aliases work with all Verse types:

#### Primitive Types

```verse
player_health := int
damage_multiplier := float
is_active := logic
display_name := string
```

#### Optional Types

```verse
OptionalInt := ?int
MaybeString := ?string

# Nested optionals
DoubleOptional := ?OptionalInt

# Can chain: value of type DoubleOptional needs `??` to unwrap
ProcessDouble(Value:DoubleOptional)<transacts><decides>:int = Value??
```

**Circular optional references are prohibited:**

```verse
# INVALID: Circular references (ERROR 3502)
# O1 := ?O2
# O2 := ?O1
```

#### Array and Map Types

```verse
# Arrays
IntArray := []int
StringList := []string

# Maps
PlayerScores := [string]int
EntityRegistry := [int]entity

# Using aliases in other aliases
NestedArray := []IntArray      # Array of arrays
ScoreMap := [string]PlayerScores  # Map of maps
```

#### Tuple Types

```verse
Point2D := tuple(float, float)
Point3D := tuple(float, float, float)
NamedPair := tuple(string, int)

ProcessPoint(P:Point2D):float =
    X := P(0)
    Y := P(1)
    X + Y
```

#### Function Types

```verse
# Using type{} syntax
Handler := type{_(:string, :int):void}
Predicate := type{_(:int)<decides>:logic}
Generator := type{_()<suspends>:int}

# Using arrow syntax
Transformer := int -> string
BiFunction := tuple(int, int) -> float

CallHandler(H:Handler, Name:string, Count:int):void = H(Name, Count)
```

#### Class, Struct, Enum, and Interface Types

```verse
player := class:
    Name:string
    Health:int

PlayerAlias := player       # Alias a class
Enemy := player             # Another alias for same class

# Both are interchangeable
ProcessEntity(E:PlayerAlias):void = {}
Entity:Enemy = player{Name := "Test", Health := 100}
ProcessEntity(Entity)       # OK - same underlying type
```

**Aliases of nominal types work seamlessly:**

```verse
color := enum{Red, Green, Blue}
ColorType := color

# Can use enum values from either name
MyColor:ColorType = color.Red
OtherColor:color = ColorType.Green

# Values are equal
MyColor = color.Red          # true
```

#### Subtype Constraints

```verse
AnyClass := subtype(class)
ComparableType := subtype(comparable)
EntitySubtype := subtype(entity)

AcceptClass(C:AnyClass):void = {}
AcceptComparable(C:ComparableType):void = {}
```

### Parametric Type Aliases

Type aliases can be parametric, creating reusable generic type patterns:

```verse
# Parametric collection aliases
Pair(t:type) := tuple(t, t)
Triple(t:type) := tuple(t, t, t)
Registry(k:type, v:type) := [k]v

# Use with concrete types
IntPair := Pair(int)
Point3D := Triple(float)
PlayerRegistry := Registry(string, player)

# Parametric function type aliases
Transformer(input:type, output:type) := input -> output
Predicate(t:type) := t -> logic
BinaryOp(t:type) := type{_(:t, :t):t}

# Use in function signatures
Map(Values:[]t, Transform:Transformer(t, u) where t:type, u:type):[]u =
    for (V : Values):
        Transform(V)

Filter(Values:[]t, Test:Predicate(t) where t:type):[]t =
    for (V : Values, Test(V)?):
        V
```

**Parametric aliases can have constraints:**

```verse
ComparablePair(t:subtype(comparable)) := tuple(t, t)
SortableArray(t:subtype(comparable)) := []t

# Constraint is enforced when using the alias
Scores:ComparablePair(int) = (100, 200)    # OK - int is comparable
# Invalid:ComparablePair(player) = ...     # ERROR if player not comparable
```

### Access Control and Visibility

Type aliases can have access specifiers that control their visibility across modules:

```verse
# Public alias - accessible from other modules
PublicAlias<public> := int

# Internal alias - only accessible within defining module
InternalAlias<internal> := string

# Protected/private also work
ProtectedAlias<protected> := float
```

**Critical restriction: Type aliases cannot be more public than the types they alias:**

```verse
PrivateClass := class{}      # No specifier = internal scope

# INVALID: Public alias to internal type (ERROR 3593)
# PublicToPrivate<public> := PrivateClass

# VALID: Same or less visibility
InternalToInternal<internal> := PrivateClass
InternalAlias := PrivateClass  # Defaults to internal
```

This restriction applies to all type constructs:

```verse
PrivateType := class{}

# All INVALID - trying to make internal type public (ERROR 3593)
# Pub1<public> := ?PrivateType           # Optional
# Pub2<public> := []PrivateType          # Array
# Pub3<public> := [int]PrivateType       # Map value
# Pub4<public> := [PrivateType]int       # Map key
# Pub5<public> := tuple(int, PrivateType)  # Tuple
# Pub6<public> := PrivateType -> int     # Function parameter
# Pub7<public> := int -> PrivateType     # Function return
# Pub8<public> := type{_():PrivateType}  # Function type
```

### Restrictions: What Aliases Are NOT

#### Cannot Be Called as Macros or Functions

Type aliases are names, not executable constructs:

```verse
OptInt := ?int

# INVALID: Cannot call as macro (ERROR 3545)
# Value := OptInt{}

# INVALID: Cannot call as function (ERROR 3552)
# Result := OptInt(42)
```

#### Cannot Have Circular References

```verse
# INVALID: Circular optional references (ERROR 3502)
# A := ?B
# B := ?A

# INVALID: Self-referential (ERROR 3502)
# C := ?C
```

#### Cannot Be Used in Type Expressions

Type aliases are not first-class values:

```verse
MyInt := int

# INVALID: Cannot use in expressions
# T := type{MyInt}      # type{} is only for function signatures
# X := MyInt + 1        # Aliases are types, not values
```

## Metatypes: subtype, concrete_subtype and castable_subtype

Verse provides advanced type constructors that allow you to work with types as values, enabling powerful patterns for runtime polymorphism and generic instantiation. These metatypes—`subtype`, `concrete_subtype`, and `castable_subtype`—bridge the gap between compile-time type safety and runtime flexibility.

### subtype: General Runtime Type Values

The `subtype(T)` type constructor represents runtime type values that are subtypes of `T`. Unlike `concrete_subtype` and `castable_subtype`, which are specialized for classes and interfaces, `subtype(T)` works with **any type** in Verse, including primitives, enums, collections, and function types.

```verse
C0 := class {}
C1 := class(C0) {}

C2 := class:
    var m0:subtype(C0)  # Can hold C0, C1, or any subtype of C0
    var m1:subtype(C2)  # Can hold C2 or any subtype of C2

    # Assign class types
    f0():void = set m0 = C0
    f1():void = set m0 = C1  # C1 is subtype of C0

    # Accept as parameter
    f3(classArg:subtype(C0)):void = set m0 = classArg
```

The key capability of `subtype(T)` is holding type values at runtime while maintaining type safety through the subtype relationship.

**Works with Any Type:**

Unlike the other metatypes, `subtype(T)` accepts any type as its parameter:

```verse
# Primitives
IntType:subtype(int) = int
LogicType:subtype(logic) = logic
FloatType:subtype(float) = float

# Enums
my_enum := enum { A, B, C }
EnumType:subtype(my_enum) = my_enum

# Collections
ArrayType:subtype([]int) = []int
OptionType:subtype(?string) = ?string

# Function types
FuncType:subtype(type{_():void}) = type{_():void}

# Classes and interfaces
ClassType:subtype(my_class) = my_class
InterfaceType:subtype(my_interface) = my_interface
```

This universality makes `subtype(T)` the most flexible of the metatypes, suitable for any scenario where you need to store or pass type values.

**Subtyping Relationship:**

The `subtype` constructor preserves the subtyping relationship: `subtype(T) <: subtype(U)` if and only if `T <: U`. This means you can assign a more specific subtype to a less specific one:

```verse
super_class := class{}
sub_class := class(super_class) {}

# Covariance: sub_class <: super_class
SubtypeVar:subtype(sub_class) = sub_class
SupertypeVar:subtype(super_class) = SubtypeVar  # Valid

# Reverse fails - super_class is not <: sub_class
# SubtypeVar2:subtype(sub_class) = super_class  # Error 3509
```

This also applies to interfaces:

```verse
super_interface := interface{}
sub_interface := interface(super_interface) {}

class_impl := class(sub_interface) {}

# Covariance through interface hierarchy
SpecificType:subtype(sub_interface) = class_impl
GeneralType:subtype(super_interface) = SpecificType  # Valid
```

**Using with Interfaces:**

When working with interfaces, `subtype(T)` can hold any class that implements the interface:

```verse
printable := interface:
    Print():void

document := class(printable):
    Print<override>():void = {}

# Can hold any type implementing printable
DocumentType:subtype(printable) = document
```

**Relationship to `type`:**

Both `subtype(T)` and `castable_subtype(T)` are subtypes of `type`, meaning they can be used where `type` is expected:

```verse
C := class:
    f(c:subtype(C)):type = return(c)  # Valid: subtype(C) <: type

T := interface {}
g(x:subtype(T)):type = x  # Valid: subtype(T) <: type
```

**Restrictions:**

While `subtype(T)` is flexible, it has important restrictions:

1. **Cannot use as value:** `subtype(T)` is a type constructor, not a value. You cannot use `subtype(T)` itself as a value:

   ```verse
   # Error 3509: subtype(C) is not a value
   # C := class:
   #     var m:subtype(C)
   #     f():void = set m = subtype(C)  # Error

   # Correct: assign a type value, not the constructor
   f():void = set m = C  # OK
   ```

2. **Exactly one argument:** `subtype` requires exactly one type argument (error 3547):

   ```verse
   # Error 3547: wrong number of arguments
   # C := class { m0:subtype() }
   # C := class { m0:subtype(C, C) }
   ```

3. **Cannot use with attributes:** `subtype` cannot be used with classes that inherit from `attribute` (error 3502):

   ```verse
   # Error 3502: attribute classes not allowed
   # C := class(attribute) { m0:subtype(C) }
   ```

4. **Reserved keyword:** `subtype` is a reserved keyword and cannot be redefined (error 3514):

   ```verse
   # Error 3514: reserved keyword
   # subtype := class {}
   # D := class { subtype():void = {} }
   # D := class { f(subtype:int):void = {} }
   ```

### concrete_subtype: Types as First-Class Values

The `concrete_subtype(t)` type constructor creates a type that represents concrete (instantiable) subclasses of `t`. A concrete class is one that can be instantiated directly—it has the `<concrete>` specifier and provides default values for all fields:

```verse
# Abstract base class
entity := class<abstract>:
    Name:string
    GetDescription():string

# Concrete implementations
player := class<concrete>(entity):
    Name<override>:string = "Player"
    GetDescription<override>():string = "A player character"

enemy := class<concrete>(entity):
    Name<override>:string = "Enemy"
    GetDescription<override>():string = "An enemy creature"

# Class that stores a type and can instantiate it
spawner := class:
    EntityType:concrete_subtype(entity)

    Spawn():entity =
        # Instantiate using the stored type
        EntityType{}

# Use it
PlayerSpawner := spawner{EntityType := player}
NewEntity := PlayerSpawner.Spawn()  # Creates a player instance
```

The key feature of `concrete_subtype` is that it ensures the stored type can be instantiated. Without this constraint, you couldn't safely call `EntityType{}` because abstract classes cannot be instantiated.

### Requirements for concrete_subtype

A type can be used with `concrete_subtype` only if it's a class or interface type. Additionally, the actual type value assigned must be a concrete class—one marked with `<concrete>` and having all fields with defaults:

```verse
# Valid: concrete class with all defaults
config := class<concrete>:
    MaxPlayers:int = 8
    TimeLimit:float = 300.0

ConfigType:concrete_subtype(config) = config  # Valid

# Invalid: abstract class cannot be concrete_subtype
abstract_base := class<abstract>:
    Value:int

# This would be an error:
# BaseType:concrete_subtype(abstract_base) = abstract_base
```

When you have a `concrete_subtype`, you can instantiate it with the empty archetype `{}`, but you cannot provide field initializers—the concrete class must provide all necessary defaults:

```verse
entity_base := class<abstract>:
    Health:int

warrior := class<concrete>(entity_base):
    Health<override>:int = 100

EntityType:concrete_subtype(entity_base) = warrior

# Valid: empty archetype uses defaults
Instance := EntityType{}

# Invalid: cannot initialize fields through metatype
# Instance := EntityType{Health := 150}
```

### castable_subtype: Runtime Type Queries

The `castable_subtype(t)` type constructor represents types that are subtypes of `t` and marked with the `<castable>` specifier. This enables runtime type queries and dynamic casting, which is essential for component systems and polymorphic hierarchies:

```verse
# Castable base class
component := class<abstract><castable>:
    Owner:entity

# Castable subtypes
physics_component := class<castable>(component):
    Velocity:vector3

render_component := class<castable>(component):
    Material:string

# Function accepting castable subtype
ProcessComponent(CompType:castable_subtype(component), Comp:component):void =
    # Can use CompType to perform type-safe casts
    if (Specific := CompType[Comp]):
        # Comp is now known to be of type CompType
```

### The final_super Specifier and Runtime Type Queries

The `castable_subtype` works with the `<final_super>` specifier and `GetCastableFinalSuperClass` function to enable sophisticated runtime type queries. This combination provides a powerful mechanism for component systems and polymorphic architectures.

#### The final_super Specifier

The `<final_super>` specifier marks classes as stable anchor points in inheritance hierarchies. These "final super classes" act as canonical representatives for families of related types:

```verse
component := class<castable>:
    Owner:entity

# Stable anchor for the physics component family
physics_component := class<final_super>(component):
    Velocity:vector3

# Specific implementations inherit from the anchor
rigid_body := class(physics_component):
    Mass:float

soft_body := class(physics_component):
    SpringConstant:float
```

By marking `physics_component` as `<final_super>`, you declare it as the canonical representative for all physics-related components. Even though `rigid_body` and `soft_body` are distinct types, they both belong to the "physics_component family" anchored at `physics_component`.

#### GetCastableFinalSuperClass Function

The `GetCastableFinalSuperClass` function queries the type hierarchy to find the `<final_super>` class between a base type and a derived type. Two variants exist:

```verse
# Takes an instance
GetCastableFinalSuperClass[BaseType, instance]:<decides>castable_subtype(BaseType)

# Takes a type
GetCastableFinalSuperClassFromType[BaseType, Type]:<decides>castable_subtype(BaseType)
```

Both return a `castable_subtype` representing the most specific `<final_super>` class that:

1. Directly inherits from the specified base type
2. Is in the inheritance chain of the instance/type

The function fails if no appropriate `<final_super>` class exists.

#### How It Works: Inheritance Chains

Consider this hierarchy:

```verse
component := class<castable>:
    ID:int

# Direct final_super subclass of component
physics_component := class<final_super>(component):
    Velocity:vector3

# Descendants of physics_component
rigid_body := class(physics_component):
    Mass:float

character_body := class(rigid_body):
    Health:int
```

Query results:

```verse
# All instances in the physics_component family return physics_component
Body := character_body{ID := 1, Velocity := vector3{}, Mass := 10.0, Health := 100}

if (Family := GetCastableFinalSuperClass[component, Body]):
    # Family = physics_component (the final_super anchor)
    # Even though Body is character_body, the family anchor is physics_component
```

The function "walks up" the inheritance chain from `character_body` → `rigid_body` → `physics_component` and stops at `physics_component` because:

1. It has `<final_super>`
2. It directly inherits from the queried base (`component`)

#### When Queries Succeed and Fail

**Succeeds when:**

- A `<final_super>` class directly inherits from the base type
- The instance/type inherits from that `<final_super>` class

```verse
base := class<castable>:
    Value:int

anchor := class<final_super>(base):
    Extra:string

derived := class(anchor):
    More:string

# Valid: anchor is final_super of base, derived inherits from anchor
GetCastableFinalSuperClass[base, derived{}]  # Returns anchor
GetCastableFinalSuperClass[base, anchor{}]   # Returns anchor
```

**Fails when:**

- No `<final_super>` class exists between base and instance
- The queried type itself is the instance type (cannot query from same level)
- Instance is not a subtype of the base

```verse
base := class<castable>:
    Value:int

# No final_super marker
middle := class(base):
    Extra:string

derived := class(middle):
    More:string

# Fails: no final_super between base and derived
GetCastableFinalSuperClass[base, derived{}]  # Fails

# Fails: cannot query from same type
GetCastableFinalSuperClass[derived, derived{}]  # Fails
```

#### Multiple Final Supers in Hierarchy

You can have multiple `<final_super>` classes at different levels. The function returns the one directly inheriting from the queried base:

```verse
base := class<castable>:
    ID:int

first_anchor := class<final_super>(base):
    Category:string

second_anchor := class<final_super>(first_anchor):
    Subcategory:string

leaf := class(second_anchor):
    Specific:string

# Query from base returns first_anchor
GetCastableFinalSuperClass[base, leaf{}]  # Returns first_anchor

# Query from first_anchor returns second_anchor
GetCastableFinalSuperClass[first_anchor, leaf{}]  # Returns second_anchor
```

This layered approach allows hierarchical categorization where different levels represent different granularities of type families.

#### Type Safety Constraints

The instance/type must be a subtype of the base type:

```verse
unrelated := class:
    Value:int

component := class<castable>:
    ID:int

# Type error: unrelated is not a subtype of component
# GetCastableFinalSuperClass[component, unrelated{}]  # ERROR 3509
```

The return type is `castable_subtype(BaseType)`, ensuring type-safe operations:

```verse
if (Family := GetCastableFinalSuperClass[component, instance]):
    # Family has type castable_subtype(component)
    # Can use Family[...] for type-safe casting to family members
```

#### Variant: GetCastableFinalSuperClassFromType

The type-based variant works identically but takes a type instead of instance:

```verse
# Same behavior, different syntax
TypeFamily := GetCastableFinalSuperClassFromType[component, rigid_body]
InstanceFamily := GetCastableFinalSuperClass[component, rigid_body{}]

# Both return the same castable_subtype
```

This is useful when working with type values directly rather than instances.

### Choosing Between Metatypes

The three metatype constructors serve different purposes. Understanding when to use each is key to effective Verse programming:

| Feature | `subtype(T)` | `concrete_subtype(T)` | `castable_subtype(T)` |
|---------|--------------|----------------------|---------------------|
| **Works with** | Any type | Classes/interfaces | Classes/interfaces with `<castable>` |
| **Can hold** | Any subtype of T | Only `<concrete>` classes | Only `<castable>` types |
| **Instantiation** | No | Yes with `{}` | No |
| **Runtime casting** | No | No | Yes with `Type[value]` |
| **Primitives** | ✓ (int, float, etc.) | ✗ | ✗ |
| **Enums** | ✓ | ✗ | ✗ |
| **Collections** | ✓ (arrays, maps) | ✗ | ✗ |
| **Function types** | ✓ | ✗ | ✗ |
| **Use case** | Store/pass type values | Factory pattern, instantiation | Component systems, dynamic casting |

**When to use `subtype(T)`:**

- Need to work with primitive types, enums, or collections
- Want to store or pass type values without instantiation
- Building type registries or metadata systems
- Working with any type in the Verse type system

**When to use `concrete_subtype(T)`:**

- Need to instantiate types at runtime (factory pattern)
- Working with configurable object creation
- The types are all concrete classes with complete defaults
- Type is determined dynamically but instantiation is needed

**When to use `castable_subtype(T)`:**

- Need runtime type queries and casting
- Building component-based architectures
- Types are marked with `<castable>` specifier
- Need to perform safe downcasts at runtime

**Common Pattern Combinations:**

```verse
# Registry using subtype for flexibility
type_registry := class:
    var AllTypes:[]subtype(my_interface) = array{}  # Any subtype

# Factory using concrete_subtype for instantiation
entity_factory := class:
    EntityType:concrete_subtype(entity)
    CreateEntity():entity = EntityType{}  # Can instantiate

# Component system using castable_subtype for casting
component_system := class:
    GetComponent(Entity:entity, CompType:castable_subtype(component)):<decides>component =
        if (Comp := CompType[Entity.GetComponent()]):
            Comp  # Type-safe cast
```

All three metatypes are subtypes of `type`, so they can be used interchangeably where `type` is expected, but each provides distinct capabilities for its specific use case.

## classifiable_subset

Building on the concept of runtime type queries introduced by `castable_subtype`, Verse provides `classifiable_subset`—a sophisticated mechanism for maintaining sets of runtime types. Where `castable_subtype` represents a single type value, `classifiable_subset` represents a collection of types, tracking which classes are present in a system and supporting queries based on type hierarchies.

This feature is particularly valuable for component-based architectures, where you need to track which component types an entity possesses, query for specific capabilities, or filter operations based on type compatibility. Rather than maintaining separate boolean flags or type tags, `classifiable_subset` provides a type-safe, hierarchy-aware registry of runtime types.

### The classifiable_subset Family

Three related types work together to provide both immutable and mutable type sets:

**`classifiable_subset(t)`** represents an immutable set of runtime types, where `t` must be a `<castable>` base type. Once created, the set cannot be modified, making it suitable for configuration, capability descriptions, or any scenario where the type set should remain stable.

**`classifiable_subset_var(t)`** provides a mutable variant with `Read()` and `Write()` operations, enabling dynamic type sets that change during program execution. This is essential for runtime systems where component types are added or removed as entities evolve.

**`classifiable_subset_key(t)`** represents keys used to identify specific instances when adding them to a mutable set. These keys enable removal of specific instances later, supporting lifecycle management of registered types.

### Construction and Type Safety

Unlike ordinary classes, `classifiable_subset` types cannot be directly instantiated. You must use the constructor functions `MakeClassifiableSubset()` and `MakeClassifiableSubsetVar()`:

```verse
# Immutable set, initially empty
EmptySet:classifiable_subset(component) = MakeClassifiableSubset()

# Immutable set with initial instances
InitialSet:classifiable_subset(component) =
    MakeClassifiableSubset(array{physics_component{}, render_component{}})

# Mutable set
var DynamicSet:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
```

The base type `t` must be `<castable>`, ensuring runtime type queries are possible. This restriction is enforced at compile time:

```verse
# Valid: component is castable
component := class<castable>:
    Owner:entity

ComponentSet:classifiable_subset(component) = MakeClassifiableSubset()

# Invalid: non-castable types cannot be used
regular_class := class:
    Value:int

# This would be an error:
# BadSet:classifiable_subset(regular_class) = MakeClassifiableSubset()
```

You cannot subclass these types or create instances through ordinary construction syntax. This ensures that all sets use the proper internal representation for efficient type queries.

### Type Hierarchy Semantics

The crucial insight of `classifiable_subset` is that it tracks runtime types, not individual instances. When you add an instance to the set, the system records that instance's actual runtime type. More importantly, type queries respect the inheritance hierarchy:

```verse
# Define a type hierarchy
component := class<castable>:
    Owner:entity

physics_component := class<castable>(component):
    Velocity:vector3

rigid_body_component := class<castable>(physics_component):
    Mass:float

# Add a rigid body instance
Set:classifiable_subset(component) =
    MakeClassifiableSubset(array{rigid_body_component{}})

# Query results respect hierarchy
Set.Contains[component]             # true - rigid_body is a component
Set.Contains[physics_component]     # true - rigid_body is a physics_component
Set.Contains[rigid_body_component]  # true - directly present
```

This hierarchy awareness makes `classifiable_subset` fundamentally different from a simple set of type tags. The `Contains` operation asks "does this set contain any type that is-a T?" rather than "does this set contain exactly T?".

When you add instances of different types, each distinct runtime type is tracked separately:

```verse
# Add multiple different types
var Set:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
Key1 := Set.Add(physics_component{})
Key2 := Set.Add(render_component{})
Key3 := Set.Add(audio_component{})

Set.Contains[component]          # true - all three are components
Set.Contains[physics_component]  # true - physics_component present
Set.Contains[render_component]   # true - render_component present
```

The set remembers each distinct type that was added. When you remove an instance by its key, that specific type is removed only if it was the last instance of that type:

```verse
# Add multiple instances of same type
var Set:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
Key1 := Set.Add(physics_component{})
Key2 := Set.Add(physics_component{})

Set.Contains[physics_component]  # true

Set.Remove[Key1]
Set.Contains[physics_component]  # still true - Key2 remains

Set.Remove[Key2]
Set.Contains[physics_component]  # false - last instance removed
```

### Core Operations

The `classifiable_subset` types provide several operations for querying and manipulating type sets:

**Contains** checks whether any type in the set matches or is a subtype of the queried type:

```verse
Set:classifiable_subset(component) =
    MakeClassifiableSubset(array{physics_component{}})

if Set.Contains[component]:
    # Physics component is present (and is a component)

if Set.Contains[render_component]:
    # No render component present
```

**ContainsAll** verifies that all types in an array are present in the set:

```verse
if Set.ContainsAll[array{physics_component, render_component}]:
    # Both physics and render components are present
```

**ContainsAny** checks whether at least one type from an array is present:

```verse
if Set.ContainsAny[array{physics_component, audio_component}]:
    # Either physics or audio component (or both) is present
```

**Add** (mutable sets only) adds an instance and returns a key for later removal:

```verse
var Set:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
Key := Set.Add(physics_component{})
# Can later remove using Key
```

**Remove** (mutable sets only) removes a previously added instance by its key:

```verse
if Set.Remove[Key]:
    # Successfully removed
else:
    # Key was not present (already removed or never added)
```

**FilterByType** creates a new set containing only types that are compatible (assignable to or from) the specified type:

```verse
# Hierarchy with sibling types
component := class<castable>{}
physics_component := class<castable>(component){}
render_component := class<castable>(component){}
audio_component := class<castable>(component){}

Set:classifiable_subset(component) = MakeClassifiableSubset(array{
    physics_component{}, render_component{}, audio_component{}})

# Filter to physics-related types
PhysicsSet := Set.FilterByType(physics_component)
PhysicsSet.Contains[physics_component]  # true
PhysicsSet.Contains[render_component]   # false - unrelated sibling
PhysicsSet.Contains[component]          # true - base type compatible
```

The filtering respects both upward and downward compatibility in the type hierarchy, keeping types that could be assigned to or from the filter type.

**Union** combines two sets using the `+` operator:

```verse
Set1:classifiable_subset(component) =
    MakeClassifiableSubset(array{physics_component{}})
Set2:classifiable_subset(component) =
    MakeClassifiableSubset(array{render_component{}})

Combined := Set1 + Set2
Combined.Contains[physics_component]  # true
Combined.Contains[render_component]   # true
```

For mutable sets, the Read/Write operations enable copying and updating:

```verse
var Set1:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
Set1.Add(physics_component{})

var Set2:classifiable_subset_var(component) = MakeClassifiableSubsetVar()
Set2.Write(Set1.Read())  # Copy Set1's contents to Set2
```

### Design Considerations

Several important constraints govern `classifiable_subset` usage:

The base type must be `<castable>` to enable runtime type queries. This requirement ensures that type checks can be performed efficiently.

You cannot subclass `classifiable_subset` types or create instances except through the designated constructor functions. This restriction maintains internal invariants required for correct type tracking.

Keys from one set cannot be used with a different set—they're bound to the specific set instance where the element was added.

The type parameter must be consistent across operations. You cannot add a `physics_component` to a `classifiable_subset(render_component)` even if both inherit from `component`:

```verse
render_set:classifiable_subset(render_component) = MakeClassifiableSubset()
physics_comp:physics_component = physics_component{}

# This would be a type error - physics_component is not a render_component
# render_set.Add(physics_comp)
```

Mutable sets require careful lifetime management. Keys become invalid when their corresponding instances are removed, and attempting to remove an already-removed key returns false.

Performance characteristics matter for large type sets. While `Contains` queries are efficient due to the internal representation, operations like `FilterByType` may need to examine each type in the set.

When designing systems with `classifiable_subset`, consider whether immutable or mutable sets better fit your needs. Immutable sets provide stronger guarantees and work well for configuration, while mutable sets support dynamic systems where component types change frequently.

The hierarchy-aware semantics mean that adding a derived type makes queries for base types succeed. This is usually desirable but requires awareness—if you only want exact type matches, `classifiable_subset` may not be the right tool.
