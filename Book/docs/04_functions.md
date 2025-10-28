# Functions

Functions are reusable code blocks that perform actions and produce outputs based on inputs. Think of them as abstractions for behaviors, much like ordering food from a menu at a restaurant. When you order, you tell the waiter what you want from the menu, such as `OrderFood("Ramen")`. You don't need to know how the kitchen prepares your dish, but you expect to receive food after ordering. This abstraction is what makes functions powerful - you define the instructions once and reuse them in different contexts throughout your code.

Verse takes a distinctive approach to functions by supporting three programming paradigms simultaneously: functional programming, imperative programming, and logic programming. This multi-paradigm approach makes functions particularly versatile.

<!-- TODO: We say "logic" a lot but we currently don't have it. Should that be toned down?  My current idea is to have some subsection early on that will explain the difference between MaxVerse and ShipVerse. -->

## Parameters and Arguments

Functions can accept any number of parameters, from none at all to as many as needed. The syntax follows a straightforward pattern where each parameter has an identifier and a type, separated by commas:

```verse
ProcessData(Name:string, Age:int, Score:float):string =
    "{Name} is {Age} years old with a score of {Score}"
```

For functions with many parameters or optional configuration, Verse supports named and default parameters. These are covered in detail in [Named and Default Parameters](#named-and-default-parameters) below.

## Named and Default Parameters

Named and default parameters are powerful features that make function APIs more flexible and ergonomic. They allow you to:

- Specify arguments by name rather than position
- Provide default values for optional parameters
- Call functions with only the arguments you need
- Add new optional parameters without breaking existing code

Named parameters are declared with a `?` prefix:

```verse
# Function with named parameters
Greet(?Name:string, ?Greeting:string):string =
    "{Greeting}, {Name}!"

# Call with named arguments using :=
Greet(?Name := "Alice", ?Greeting := "Hello")  # Returns "Hello, Alice!"
```

Named parameters with default values make them truly optional:

```verse
# Named parameters with defaults
Log(Message:string, ?Level:int = 1, ?Color:string = "white"):string =
    "[Level {Level}] {Message} ({Color})"

# Call with all defaults
Log("Starting")  # Returns "[Level 1] Starting (white)"

# Call with some arguments
Log("Warning", ?Level := 2)  # Returns "[Level 2] Warning (white)"

# Call with arguments in any order
Log("Error", ?Color := "red", ?Level := 3)  # Returns "[Level 3] Error (red)"
```

When declaring functions with named parameters, several rules ensure consistency:

**Once named, always named**: When you introduce a named parameter, all subsequent parameters must also be named:

```verse
# Valid: positional followed by named
Process(Required:int, ?Optional:string):void = {}

# Invalid: named followed by positional
# Invalid(? Named:int, Positional:string):void = {}  # ERROR
```

**No `?` in function body**: Named parameters are referenced without the `?` prefix inside the function:

```verse
Calculate(?Amount:float, ?Rate:float):float =
    Amount * Rate  # Not ?Amount * ?Rate
```

**No duplicate names**: Parameter names must be unique within a function:

```verse
# Invalid: duplicate parameter name
# Duplicate(?Value:int, ?Value:int):void = {}  # ERROR
```

### Calling with Named Arguments

When calling functions with named parameters, you must use the `?Name:=Value` syntax:

**Required named parameters must be specified**:

```verse
Format(?Text:string, ?Width:int):string =
    # Implementation

# Must provide all named parameters
Format(?Text := "Hello", ?Width := 10)

# Cannot omit required named parameters
# Format(?Text := "Hello")  # ERROR: missing ?Width
```

**Named arguments must match parameter names**:

```verse
Connect(?Host:string, ?Port:int):void = {}

# Correct
Connect(?Host := "localhost", ?Port := 8080)

# Wrong name
# Connect(?Server := "localhost", ?Port := 8080)  # ERROR
```

**Positional arguments must come first**:

```verse
Configure(Required:int, ?Option1:string, ?Option2:bool):void = {}

# Valid
Configure(42, ?Option1 := "test", ?Option2 := true)

# Invalid: named arg before positional
# Configure(?Option1 := "test", 42, ?Option2 := true)  # ERROR
```

**Named arguments can appear in any order**:

```verse
Setup(?Width:int, ?Height:int, ?Depth:int):void = {}

# These are equivalent
Setup(?Width := 10, ?Height := 20, ?Depth := 30)
Setup(?Depth := 30, ?Width := 10, ?Height := 20)
Setup(?Height := 20, ?Depth := 30, ?Width := 10)
```

**Cannot specify the same argument twice**:

```verse
# Invalid: duplicate named argument
# Setup(?Width := 10, ?Height := 20, ?Width := 15)  # ERROR
```

### Default Parameter Values

Default values make parameters truly optional by providing a value when none is supplied:

**Syntax**: Defaults are specified with `= value` after the type:

```verse
CreateWindow(?Title:string = "Untitled", ?Width:int = 800, ?Height:int = 600):window =
    # Implementation
```

**Calling with defaults**: Omit any parameter to use its default:

```verse
# Use all defaults
CreateWindow()

# Override some defaults
CreateWindow(?Title := "My Window")

# Override in any combination
CreateWindow(?Width := 1920, ?Height := 1080)
CreateWindow(?Height := 1080, ?Title := "Game")
```

### Default Value Scoping

Default values are evaluated in the function's defining scope, not at the call site. This allows defaults to reference:

**Module-level definitions**:

```verse
DefaultTimeout:int = 30

Connect(?Host:string, ?Timeout:int = DefaultTimeout):void =
    # Uses DefaultTimeout from module scope
```

**Class or interface members**:

```verse
game_config := class:
    DefaultLives:int = 3

    StartGame(?Lives:int = DefaultLives):void =
        # Uses DefaultLives from class

Player := game_config{}
Player.StartGame()  # Uses DefaultLives value (3)
```

**Earlier parameters in the same function**:

```verse
CreateRange(?Start:int, ?End:int = Start + 10):[]int =
    # End defaults to Start + 10
    # Implementation

CreateRange(?Start := 5)  # Creates range from 5 to 15
```

**Expressions and function calls**:

```verse
GetDefaultSize()<computes>:int = 100

Allocate(?Size:int = GetDefaultSize() * 2):void =
    # Default calls function and uses expression

# Defaults can be literals, expressions, constructors
Configure(?Name:string = "default",
          ?Size:int = 10 + 20,
          ?Items:[]int = array{1, 2, 3},
          ?Map:[int]string = map{0 => "zero"}):void = {}
```

### Default Values with Inheritance

Default values work with overridden members in class hierarchies:

```verse
base_game := class:
    DefaultSpeed:float = 1.0

    Move(?Speed:float = DefaultSpeed):void =
        # Uses DefaultSpeed from current instance

fast_game := class(base_game):
    DefaultSpeed<override>:float = 2.0

BaseInstance := base_game{}
BaseInstance.Move()  # Uses 1.0

FastInstance := fast_game{}
FastInstance.Move()  # Uses 2.0 (overridden value)
```

### Type System Interactions

Named and default parameters interact with Verse's type system in specific ways:

**Subtyping with defaults**: A function with default parameters is a subtype of the same function without those parameters:

```verse
ProcessData(?Required:int, ?Optional:int = 0):int =
    Required + Optional

# Can assign to type without optional parameter
F1:type{_(?Required:int):int} = ProcessData
F1(?Required := 5)  # Returns 5 (uses default)

# Can assign to type with optional parameter
F2:type{_(?Required:int, ?Optional:int):int} = ProcessData
F2(?Required := 5, ?Optional := 3)  # Returns 8

# Can even assign to type with no parameters (all have defaults)
DefaultAll(?A:int = 1, ?B:int = 2):int = A + B
F3:type{_():int} = DefaultAll
F3()  # Returns 3
```

**Parameter names must match**: Function types preserve named parameter names:

```verse
Calculate(?Amount:float, ?Rate:float):float = Amount * Rate

# Valid: names match
F1:type{_(?Amount:float, ?Rate:float):float} = Calculate

# Invalid: different names
# F2:type{_(?Value:float, ?Factor:float):float} = Calculate  # ERROR
```

**Defaults are not part of the type**: The type signature doesn't include default values:

```verse
F1(?X:int = 1):int = X
F2(?X:int = 999):int = X

# Same type despite different defaults
Type1:type{_(?X:int):int} = F1
Type1 = F2  # Valid - same type
```

### Overload Resolution

Named parameters participate in function overload resolution:

```verse
Process(Value:int):string = "One parameter"
Process(Value:int, ?Option:string):string = "Two parameters"
Process(Value:int, ?Option1:string, ?Option2:bool):string = "Three parameters"

Process(42)                                    # Calls first overload
Process(42, ?Option := "test")                 # Calls second overload
Process(42, ?Option1 := "test", ?Option2 := true)  # Calls third overload
```

The compiler selects the overload that matches the provided arguments. Named parameters make overload resolution more precise since names must match exactly.

#### Named Parameter Distinctness

Named parameters have specific rules for overload distinctness that differ from positional parameters. Two function signatures are considered **indistinct** (cannot overload) if they could be called with the same arguments.

**Parameter order doesn't matter for named parameters:**

Named parameters are matched by name, not position, so reordering doesn't create distinctness:

```verse
# ERROR 3532: Not distinct - same parameters, different order
# F(?Y:int, ?X:int):int = X + Y
# F(?X:int, ?Y:int):int = X - Y  # ERROR
```

**Defaults don't create distinctness:**

The presence or absence of default values doesn't make signatures distinct if the parameter names are the same:

```verse
# ERROR 3532: Same parameter name with/without default
# F(?X:int=42):int = X
# F(?X:int):int = X  # ERROR

# ERROR 3532: Different defaults don't help
# F(?X:int=42):int = X
# F(?X:int=100):int = X  # ERROR
```

**The all-defaults rule:**

If all parameters in both overloads have default values, the signatures are indistinct because both can be called with no arguments:

```verse
# ERROR 3532: Both can be called as F()
# F(?X:int=42):int = X
# F(?Y:int=42):int = Y  # ERROR

# ERROR 3532: Even with multiple parameters
# F(?X:int=1, ?Y:int=2):int = X + Y
# F(?A:int=3, ?B:int=4):int = A + B  # ERROR
```

This applies even if the parameter types differ:

```verse
# ERROR 3532: Both callable with no args
# F(?X:int=42):int = X
# F(?X:float=3.14):float = X  # ERROR
```

**Different parameter names ARE distinct:**

Functions with completely different named parameter names can overload:

```verse
# Valid: Different names
F(?X:int):int = X
F(?Y:int):int = Y  # OK - distinct parameter names

# Valid: Different type and name
F(?X:int):int = X
F(?Y:comparable):comparable = Y  # OK
```

**Named vs positional parameters are distinct:**

A named parameter is distinct from a positional parameter, even with the same name and type:

```verse
# Valid: Named vs positional
F(?X:int):int = X
F(X:int):int = X  # OK

# Valid: Even with defaults
F(?X:int=42):int = X
F(X:int):int = X  # OK
```

**At least one required parameter must differ:**

If the set of required (no default) named parameters differs, the overloads are distinct:

```verse
# Valid: First requires ?Y, second doesn't
F(?Y:int, ?X:int=42):int = X
F(?X:int):int = X  # OK - different required parameter set

# Valid: Completely different required parameters
F(?Y:int, ?Z:int=42):int = Y
F(?X:int):int = X  # OK

# ERROR 3532: Same required parameters
# F(?Y:int, ?X:int=42):int = X
# F(?Y:int, ?X:int):int = X  # ERROR - ?Y required in both
```

**Positional parameters create distinctness:**

Different positional parameter types make signatures distinct, even if named parameters are the same:

```verse
# Valid: Different positional parameter types
F(Arg:float, ?X:int):int = X
F(Arg:int, ?X:int):int = X  # OK
```

**Superset of calls:**

If one signature can handle all the calls that another can, they're indistinct:

```verse
# ERROR 3532: First can handle all calls to second
# F(?Y:int=42, ?X:int=42):int = X
# F(?X:int):int = X  # ERROR - can call first as F(?X := 10)

# ERROR 3532: Same parameter names with/without defaults
# F(?Y:int=42, ?X:int):int = X + Y
# F(?X:int, ?Y:int):int = X - Y  # ERROR
```

### First-Class Functions

Functions with named parameters can be stored in variables and passed as arguments:

```verse
Compute(Base:int, ?Multiplier:int = 2, ?Offset:int = 0):int =
    Base * Multiplier + Offset

# Store in variable
MyCompute := Compute

# Call through variable
MyCompute(10, ?Multiplier := 3, ?Offset := 5)  # Returns 35

# Pass as parameter
ApplyFunction(F(?Base:int, ?Multiplier:int, ?Offset:int):int, Value:int):int =
    F(?Base := Value, ?Multiplier := 2, ?Offset := 10)

ApplyFunction(Compute, 5)  # Returns 20 (5 * 2 + 10)
```

### Tuple Arguments and Named Parameters

Tuples can be used to provide positional arguments. However, you cannot mix a pre-constructed tuple variable with additional named arguments:

```verse
Calculate(A:int, B:int, ?C:int = 0):int = A + B + C

# Valid: tuple provides positional arguments
Args:tuple(int, int) = (1, 2)
Calculate(Args)  # Returns 3

# Valid: all arguments provided directly
Calculate(1, 2, ?C := 5)  # Returns 8

# Invalid: cannot mix tuple variable with named arguments
# Calculate(Args, ?C := 5)  # ERROR
```

Named parameters **can** appear inside tuple parameter structures when destructuring—see [Named Parameters in Tuple Destructuring](#named-parameters-in-tuple-destructuring) below for details.

### Tuple Parameter Destructuring

Functions can destructure tuple parameters directly in the parameter list, allowing you to extract tuple elements inline without manual indexing:

```verse
# Destructure tuple parameter in place
Func(A:int, (B:int, C:int), D:int):int =
    A + B + C + D

# All these calling forms work:
Func(1, (2, 3), 4)        # Direct tuple literal - returns 10

X := (2, 3)
Func(1, X, 4)             # Tuple variable - returns 10

Y := (1, (2, 3), 4)
Func(Y)                   # Entire argument list as tuple - returns 10
```

The parameter `(B:int, C:int)` destructures the tuple, giving direct access to `B` and `C` instead of requiring `Tuple(0)` and `Tuple(1)` indexing.

#### Nested Tuple Destructuring

Tuples can be destructured to arbitrary depth:

```verse
# Simple nesting
H(A:int, (B:int, (C:int, D:int)), E:int):int =
    A + B + C + D + E

H(1, (2, (3, 4)), 5)              # Returns 15
T := (2, (3, 4))
H(1, T, 5)                        # Returns 15
T2 := (1, (2, (3, 4)), 5)
H(T2)                             # Returns 15

# Complex multi-level nesting
I(A:int, (B:int, (C:int, D:int)), E:int, (F:int, G:int, (H:int, I:int))):int =
    A + B + C + D + E + F + G + H + I

# Call with inline literals
I(1, (2, (3, 4)), 5, (6, 7, (8, 9)))  # Returns 45

# Call with mixed variables
T3 := (2, (3, 4))
T4 := (6, 7, (8, 9))
I(1, T3, 5, T4)                       # Returns 45

# Call with entire signature as tuple
T5 := (1, (2, (3, 4)), 5, (6, 7, (8, 9)))
I(T5)                                 # Returns 45
```

#### Mixed Destructured and Non-Destructured Forms

You can mix destructured tuple parameters with regular tuple parameters that aren't destructured:

```verse
# Destructured form - access elements directly
F(A:int, (B:int, C:int), D:int):int =
    A + B + C + D

# Non-destructured form - use tuple indexing
G(A:int, T:tuple(int, int), D:int):int =
    A + T(0) + T(1) + D

# Both work identically
F(1, (2, 3), 4)  # Returns 10
G(1, (2, 3), 4)  # Returns 10
```

Choose destructured form when you need direct access to individual elements, and non-destructured when you need to pass the tuple as a whole to other functions.

#### Named Parameters in Tuple Destructuring

Tuple parameters can contain named/optional parameters, allowing for flexible APIs that combine structural decomposition with optional values:

```verse
# Named parameter inside nested tuple
SumValues(A:int, (X:int, (Y:int, (?Z:int = 0)))):int =
    A + X + Y + Z

# Can provide Z explicitly
SumValues(1, (2, (3, (?Z := 4))))  # Returns 10

# Can omit Z to use default
SumValues((1, (2, (3))))           # Returns 6

# Pre-constructed tuple also works
T := (3)
SumValues((1, (2, T)))             # Returns 6
```

**Multiple Named Parameters in Tuples:**

A tuple can contain multiple named parameters, and they can be specified in any order:

```verse
ProcessData(Base:int, (Items:[]int, ?Scale:int = 1, ?Offset:int = 0)):int =
    if (First := Items[0]):
        First * Scale + Offset + Base
    else:
        Base

Data := array{100, 200}

# All these are valid
ProcessData(10, Data)                              # Uses defaults: 110
ProcessData(10, (Data, ?Scale := 2))              # 210
ProcessData(10, (Data, ?Offset := 5))             # 115
ProcessData(10, (Data, ?Scale := 2, ?Offset := 5)) # 215
ProcessData(10, (Data, ?Offset := 5, ?Scale := 2)) # 215 (order doesn't matter)
```

**Limitation:**

When a tuple parameter contains **only** named parameters (no positional parameters), you must provide an empty tuple `()` even when using all defaults:

```verse
# Tuple with only named parameters
Configure(Base:int, (?Width:int = 10, ?Height:int = 20)):int =
    Base + Width + Height

# Must provide empty tuple when using all defaults
Configure(5, ())  # Returns 35

# Cannot omit the tuple entirely
# Configure(5)  # ERROR - tuple parameter required
```

This is a known limitation in the current implementation. When the tuple contains at least one positional parameter, this restriction doesn't apply.

#### Tuples Containing Other Types

Tuple parameters can contain arrays, other complex types, and be further nested:

```verse
# Tuple containing array
ProcessData(X:tuple([]int, int))<decides>:int =
    X(0)[0] + X(0)[1] + X(1)

ProcessData[(array{1, 2}), 3]  # Returns 6
T := ((3, 4), 5)
ProcessData[T]                  # Returns 12

# Equivalent flattened destructured form
ProcessFlattened(A:[]int, B:int)<decides>:int =
    A[0] + A[1] + B

ProcessFlattened[(array{1, 2}), 3]  # Returns 6
ProcessFlattened[T]                  # Returns 12
```

#### Restrictions

**Cannot use refined types (where clauses) in tuple parameters:**

Refined types with `where` clauses are not allowed in destructured tuple parameters:

```verse
# ERROR 3624: Refined types not supported in tuple destructuring
# H(A:int, ((B:int where B > 0), C:int), D:int):int =
#     A + B + C + D
```

This restriction applies to the types within the tuple destructuring. Regular parameter refinements outside tuples work normally.

### Automatic Tuple Flattening and Unflattening

Verse provides automatic conversion between tuples and multiple arguments at function call sites, enabling flexible calling conventions without explicit packing or unpacking.

**Direction 1: Tuple to Multiple Arguments (Flattening):**

A function expecting multiple parameters can be called with a single tuple:

```verse
# Function with multiple parameters
Add(X:int, Y:int):int = X + Y

# Can call with individual arguments
Add(3, 5)  # Returns 8

# Can also call with a tuple
Args := (3, 5)
Add(Args)  # Returns 8 - tuple automatically flattened
```

The tuple is automatically unpacked into the function's parameters.

**Direction 2: Multiple Arguments to Tuple (Unflattening):**

A function expecting a single tuple parameter can be called with flattened arguments:

```verse
# Function with single tuple parameter
ProcessPair(P:tuple(int, int)):int = P(0) + P(1)

# Can call with a tuple
Pair := (3, 5)
ProcessPair(Pair)  # Returns 8

# Can also call with flattened arguments
ProcessPair(3, 5)  # Returns 8 - args automatically packed into tuple
```

The individual arguments are automatically packed into the tuple parameter.

**Bidirectional Flexibility:**

This works in both directions, making function calls more convenient:

```verse
# Multi-parameter function
Multiply(A:int, B:int, C:int):int = A * B * C

# Tuple-parameter function
Sum(Values:tuple(int, int, int)):int = Values(0) + Values(1) + Values(2)

# Can use tuples for both
Args := (2, 3, 4)
Multiply(Args)  # 2 * 3 * 4 = 24 (flattening)
Sum(2, 3, 4)    # 2 + 3 + 4 = 9 (unflattening)
```

**Works with Tuple Parameter Destructuring:**

This flattening also works when the function uses tuple destructuring:

```verse
# Function destructuring tuple parameter
Compute(X:int, (Y:int, Z:int)):int = X + Y + Z

# All these work:
Compute(1, (2, 3))     # Standard: explicit tuple
Compute(1, 2, 3)       # Flattened: all separate args
Args := (1, (2, 3))
Compute(Args)          # Packed: entire signature as tuple
```

**Empty Tuples:**

Empty tuples work with the same flattening behavior:

```verse
# Function with empty tuple parameter
GetConstant(X:tuple()):int = 42

# Both forms equivalent
GetConstant(())   # Explicit empty tuple
GetConstant()     # No arguments - automatically creates empty tuple
```

### Named Parameters with Effects

Named and default parameters work with all function effects (see [Effects](10_effects.md) for details on effect specifiers):

```verse
# Failable function with default parameters
Validate(Value:int, ?Min:int = 0, ?Max:int = 100)<decides>:int =
    Value >= Min
    Value <= Max
    Value

# Async function with default timeout
WaitForEvent(?Timeout:float = 5.0)<suspends>:void =
    # Implementation

# Calling with named parameters
if (Result := Validate[42, ?Max := 50]):
    # Validation succeeded

spawn:
    WaitForEvent(?Timeout := 10.0)
```

**With parametric types**:

```verse
FindFirst(Items:[]t, ?Default:t where t:type)<decides>:t =
    if (First := Items[0]):
        First
    else if (Default?):
        Default?
    else:
        false

FindFirst[array{1, 2, 3}]           # Returns option{1}
FindFirst[array{}, ?Default := 42]  # Returns option{42}
```

### Evaluation Order

Arguments are evaluated in a specific order to maintain predictable behavior:

1. **Positional arguments**: Left to right in the call
2. **Named arguments**: Left to right as encountered in the call
3. **Default values**: Filled in for omitted parameters, left to right in parameter order

If named arguments appear in a different order than parameters, the compiler uses temporary variables to preserve the evaluation order you specified:

```verse
# Parameters are: A, B, C, D
Process(A:int, ?B:int, ?C:int, ?D:int):string =
    "{A}, {B}, {C}, {D}"

# Call with reordered named args
Process(1, ?D := 4, ?B := 2, ?C := 3)

# Evaluation order: 1, 4, 2, 3 (as written)
# But passed to function in parameter order: 1, 2, 3, 4
```

This ensures that side effects in argument expressions happen in the order you write them, not in parameter order.

## Extension Methods

Extension methods allow you to add new methods to existing types without modifying their original definitions. This powerful feature enables you to extend any type in Verse—including built-in types like `int`, `string`, arrays, and maps—with custom functionality while maintaining clean separation between different concerns.

Extension methods are particularly valuable when:

- You want to add domain-specific operations to built-in types
- You need to extend types from libraries you don't control
- You're building fluent or builder-style APIs
- You want to organize related functionality separately from type definitions

Extension methods use a special syntax where the extended type appears in parentheses before the method name:

```verse
# Extend int with a custom method
(Value:int).Double()<computes>:int = Value * 2

# Call the extension method using dot notation
X := 5
Y := X.Double()  # Returns 10

# Can also call on literals
Z := 7.Double()  # Returns 14
```

The type in parentheses can be any Verse type: primitives, tuples, classes, interfaces, arrays, maps, or structs.

### Extending Different Types

**Primitives:**

```verse
(N:int).IsEven()<computes>:logic = N % 2 = 0
(S:string).FirstChar()<decides>:char = S[0]?

42.IsEven()           # Returns true
"Hello".FirstChar()   # Returns 'H'
```

**Tuples:**

```verse
# Extend a specific tuple type
(Point:tuple(int, int)).Distance()<computes>:float =
    Sqrt(Point(0) * Point(0) + Point(1) * Point(1))

(3, 4).Distance()  # Returns 5.0
```

**Arrays:**

```verse
(Numbers:[]int).Sum()<computes>:int =
    var Total:int = 0
    for (N:Numbers):
        set Total += N
    Total

array{1, 2, 3, 4, 5}.Sum()  # Returns 15
```

**Maps:**

```verse
(M:[int]string).Keys()<computes>:[]int =
    for (Key->_:M):
        Key

map{1=>"a", 2=>"b", 3=>"c"}.Keys()  # Returns array{1, 2, 3}
```

**Classes:**

```verse
player := class:
    Name:string
    var Score:int

# Add method to existing class
(P:player).AddScore(Points:int):void =
    set P.Score += Points

Player1 := player{Name := "Alice", Score := 100}
Player1.AddScore(50)  # Score becomes 150
```

### Extension Methods with Additional Parameters

Extension methods can accept additional parameters beyond the extended type:

```verse
(Numbers:[]int).Scale(Factor:int):[]int =
    for (N:Numbers):
        N * Factor

array{1, 2, 3}.Scale(10)  # Returns array{10, 20, 30}
```

Multiple parameters work naturally:

```verse
(Base:int).InRange(Min:int, Max:int)<decides>:logic =
    Base >= Min
    Base <= Max

if (50.InRange(0, 100)):
    # Value is in range
```

### Extension Methods with Named and Default Parameters

Extension methods support all parameter features including named and default parameters:

```verse
(Text:string).Pad(?Left:int = 0, ?Right:int = 0):string =
    # Implementation to pad string with spaces

"Hello".Pad(?Left := 5)         # "     Hello"
"Hello".Pad(?Right := 5)        # "Hello     "
"Hello".Pad(?Left := 2, ?Right := 3)  # "  Hello   "
```

### Extension Methods with Tuple Parameters

Extension methods can combine with all tuple parameter features, including destructuring and named parameters in tuples:

```verse
# Extension with destructured tuple
(Base:int).AddPair((X:int, Y:int)):int =
    Base + X + Y

10.AddPair((5, 3))  # Returns 18

# Extension with nested tuple destructuring
(A:int).SumNested(B:int, (C:int, (D:int, E:int))):int =
    A + B + C + D + E

1.SumNested(2, (3, (4, 5)))  # Returns 15

# Extension with named parameters in tuples
(Base:int).Configure(Settings:([]int, ?Scale:int = 1, ?Offset:int = 0)):int =
    if (First := Settings(0)[0]):
        First * Settings(1) + Settings(2) + Base
    else:
        Base

Data := array{100}
10.Configure((Data, ?Scale := 2))              # Returns 210
10.Configure((Data, ?Scale := 2, ?Offset := 5)) # Returns 215
```

Extension methods work identically to regular functions for tuple parameters—all the same rules and patterns apply.

### Overloading Extension Methods

You can define multiple extension methods with the same name for different types:

```verse
# Overloaded Extension method for different types
(N:int).Format():string = "{N}"
(F:float).Format():string = "{F:.2f}"
(B:logic).Format():string = if (B?) {"true"} else {"false"}

42.Format()     # Returns "42"
3.14.Format()   # Returns "3.14"
true.Format()   # Returns "true"
```

The compiler selects the appropriate overload based on the receiver type.

### Extension Methods on the Empty Tuple

The empty tuple `tuple()` represents the unit type and can have extension methods:

```verse
(Unit:tuple()).GetMagicNumber():int = 42

().GetMagicNumber()  # Returns 42
```

This can be useful for creating namespace-like groupings of functions.

### Restrictions and Rules

**Cannot add fields**: Extension methods can only add methods, not data members:

```verse
# Invalid: Cannot add extension fields
# (N:int).StoredValue:int = N  # ERROR
```

**Must be called**: Extension methods cannot be referenced as first-class values without calling them:

```verse
(N:int).Double():int = N * 2

# Valid: calling the method
X := 5.Double()

# Invalid: referencing without calling
# F := 5.Double  # ERROR
```

**No direct field accessor syntax**: You cannot directly define field accessors like `.Length`:

```verse
# Invalid: Cannot define field accessor operators
# operator'.CustomField'(N:int):int = N  # ERROR
```

**Short tuple syntax limited**: In current Verse, you cannot use the short form for tuple parameters:

```verse
# Invalid in current Verse
# ().Extension():int = 0  # ERROR
# (A:int, B:int).Extension():int = 0  # ERROR

# Valid: Use full tuple type
(AB:tuple(int, int)).Extension():int = AB(0) + AB(1)
```

### Conflicts with Class Methods

Extension methods cannot have the same signature as methods defined directly in classes or interfaces:

```verse
player := class:
    Health():int = 100

# Invalid: Conflicts with class method
# (P:player).Health():int = 50  # ERROR
```

This prevents ambiguity and ensures that class methods always take precedence.

### Scope and Visibility

Extension methods are scoped like regular functions. They're only visible where they're defined or imported:

```verse
# In module A
utils := module:
    (S:string).Reverse<public>():string =
        # Implementation

# In module B
using { utils }

"Hello".Reverse()  # Available after importing
```

### Extension Methods in Class Scope

Extension methods can be defined inside classes and access class members:

```verse
game_manager := class:
    Multiplier:int = 10

    (Score:int).ScaledScore():int =
        Score * Multiplier  # Accesses class field

    ProcessScore(Value:int):int =
        Value.ScaledScore()  # Uses extension method

GM := game_manager{}
GM.ProcessScore(5)  # Returns 50
```

This creates a lexical closure where the extension method can reference the enclosing class's members.

### Extension Methods on Special Types

**Arrays and Maps**: Extension methods work on collection types:

```verse
(Items:[]t).First<public>()<decides>:t where t:type =
    Items[0]?

array{1, 2, 3}.First()  # Returns option{1}
```

Note that arrays `[]t` and maps `[k]v` are different types for extension method purposes, but you cannot define conflicting overloads:

```verse
# Valid: Different signatures
(A:[]int).Process():void = {}
(M:[int]string).Process():void = {}

# Invalid: Same name, would conflict
# (A:[]int).Convert():[]int = {}
# (M:[int]int).Convert():[]int = {}  # ERROR
```

**Built-in Special Cases**: Some built-in properties like `Length` on arrays and maps have special handling, but extension methods still work. You can even define extension methods with names that match fields on some types:

```verse
has_length := class{Length:int = 10}
no_length := class{}

(:no_length).Length():int = 20

has_length{}.Length     # Field: 10
no_length{}.Length()    # Extension method: 20
```

### Extension Methods with Effects

Extension methods support all function effects:

**With `<decides>` (failable):**

```verse
(Numbers:[]int).GetAt(Index:int)<decides>:int =
    Numbers[Index]?

if (Value := array{10, 20, 30}.GetAt(1)):
    # Value is 20
```

**With `<transacts>` (side effects):**

```verse
player := class:
    var Score:int

(P:player).ResetScore()<transacts>:void =
    set P.Score = 0
```

**With `<suspends>` (async):**

```verse
(Delay:float).Wait()<suspends>:void =
    Sleep(Delay)

spawn:
    5.0.Wait()  # Wait 5 seconds
```

### Extension Methods with Parametric Types

Extension methods can use type parameters:

```verse
(Items:[]t).FilterNonEmpty()<computes>:[]t where t:subtype(comparable) =
    for (Item:Items, Item <> false):
        Item

array{option{1}, false, option{2}}.FilterNonEmpty()  # Returns array with non-false values
```

### Tuple Argument Conversion

When an extension method has multiple parameters, you can pass a tuple to provide all arguments at once:

```verse
point := class:
    X:int
    Y:int

(P:point).Translate(DX:int, DY:int):point =
    point{X := P.X + DX, Y := P.Y + DY}

Origin := point{X := 0, Y := 0}
Delta := (5, 10)
NewPoint := Origin.Translate(Delta)  # Tuple expands to two arguments
```

This works when the tuple type matches the parameter list.

## Function Types and Lambdas

Functions in Verse are first-class values - they can be stored in variables, passed as parameters, returned from other functions, and created anonymously. This enables powerful functional programming patterns including higher-order functions, callbacks, and composable operations.

### Lambda Expressions

Lambda expressions create anonymous functions using the `=>` operator:

```verse
# Simple lambda
Square := (X:int) => X * X
Square(5)  # Returns 25

# Multiple parameters
Add := (X:int, Y:int) => X + Y
Add(3, 4)  # Returns 7

# No parameters
GetFortyTwo := () => 42
GetFortyTwo()  # Returns 42
```

Lambdas can have blocks with multiple statements:

```verse
# Lambda with block
ComplexCalculation := (X:int, Y:int) =>
{
    Temp := X * 2
    Result := Temp + Y
    Result * Result
}

ComplexCalculation(3, 4)  # Returns 100
```

Using explicit `return`:

```verse
# Lambda with explicit return
Process := (X:int) => return(X * X + 10)
Process(5)  # Returns 35
```

Lambdas cannot have explicit return type annotations. The return type is inferred from the body:

```verse
# Invalid: Cannot specify return type in lambda
# Bad := (X:int):int => X * X  # ERROR

# Correct: Type is inferred
Good := (X:int) => X * X
```

### Closures

Lambdas capture variables from their enclosing scope, creating closures:

```verse
MakeMultiplier(Factor:int):type{_(:int):int} =
    # Lambda captures Factor from outer scope
    (X:int) => X * Factor

Double := MakeMultiplier(2)
Triple := MakeMultiplier(3)

Double(5)  # Returns 10
Triple(5)  # Returns 15
```

The captured variables are bound when the lambda is created, not when it's called.

### Function Type Syntax with `->`

The arrow operator `->` declares function types explicitly:

```verse
# Simple function type
F:int->int = (X:int) => X + 1

# Function with multiple parameters
# Note: tuple() constructor is required
G:tuple(int, int)->string = (X:int, Y:int) => "{X}, {Y}"

# Function as return type
MakeAdder:int->int->int = (X:int) => (Y:int) => X + Y
AddFive := MakeAdder(5)
AddFive(3)  # Returns 8
```

**Important**: Use `tuple()` for multiple parameters. Without it, `(int, int)` is a tuple value, not a type:

```verse
# Wrong: This is a tuple value type
# Bad:[int, int]->string = ...  # ERROR

# Correct: Use tuple() constructor
Good:tuple(int, int)->string = (X:int, Y:int) => "{X}, {Y}"
```

### Functions as Parameters

Functions accepting other functions enable higher-order programming:

```verse
# Map function
Map(Items:[]int, Transform(:int):int):[]int =
    for (Item:Items):
        Transform(Item)

Numbers := array{1, 2, 3, 4, 5}
Doubled := Map(Numbers, (X:int) => X * 2)
# Returns array{2, 4, 6, 8, 10}

# Filter function
Filter(Items:[]int, Predicate(:int)<decides>:void):[]int =
    for (Item:Items, Predicate[Item]):
        Item

Evens := Filter(Numbers, (X:int) => X % 2 = 0)
# Returns array{2, 4}
```

### Lambda Precedence and Parentheses

The `:=` operator has higher precedence than `=>`, so you must use parentheses when assigning lambdas:

```verse
# Wrong: Parsed as (G := I:int) => I*I
# G := I:int => I*I  # ERROR

# Correct: Use parentheses
G := (I:int => I*I)

# Also correct: Type annotation forces correct parsing
H:int->int = I:int => I*I
```

### Function Variance

Function types follow specific subtyping rules based on **variance**:

**Parameters are contravariant**: A function accepting more general types can substitute for one accepting specific types.

**Returns are covariant**: A function returning more specific types can substitute for one returning general types.

```verse
animal := class:
    Name:string

dog := class(animal):
    Breed:string

# Functions with different parameter/return types
F1(X:animal):dog = dog{Name := X.Name, Breed := "Unknown"}
F2(X:dog):animal = X  # Returns supertype
F3(X:dog):dog = X

# Function type accepting dog, returning animal
ProcessDog:type{_(:dog):animal}

# Valid: F1 accepts animal (more general), returns dog (more specific)
ProcessDog = F1  # OK: tuple(animal)->dog <: tuple(dog)->animal

# Valid: F3 accepts dog, returns dog (more specific than animal)
ProcessDog = F3  # OK: tuple(dog)->dog <: tuple(dog)->animal

# Invalid: F2 returns animal but parameter is not contravariant enough
# ProcessDog = F2  # ERROR: tuple(dog)->animal </: tuple(dog)->animal
#                  # (same parameters, same return - no variance issue here)

# Invalid: Would require accepting animal where dog expected
ProcessAnimal:type{_(:animal):animal}
# ProcessAnimal = F3  # ERROR: tuple(dog)->dog </: tuple(animal)->animal
```

### Effects and Function Types

Effects are part of the function type and must match **exactly** - effects are **invariant**:

```verse
Pure():int = 42
Transactional()<transacts>:int = 42
Suspendable()<suspends>:int = 42

# Functions expecting specific effects
UsePure(F():int):int = F()
UseTransactional(F()<transacts>:int):int = F()
UseSuspendable(F()<suspends>:int):int = spawn{F()}

UsePure(Pure)  # OK
UseTransactional(Transactional)  # OK
UseSuspendable(Suspendable)  # OK

# Invalid: Effects must match exactly
# UsePure(Transactional)  # ERROR: ()<transacts>:int </: ():int
# UseTransactional(Pure)  # ERROR: ():int </: ()<transacts>:int
```

There is no subtyping relationship between functions with different effects, even if one seems "weaker" than another.

### Type Joining of Functions

When you assign different functions conditionally, Verse finds the least upper bound (join) of their types:

**Join with compatible return types**:

```verse
base := class{Value:int}
derived := class(base){Extra:string}

F1():base = base{Value := 1}
F2():derived = derived{Value := 2, Extra := "test"}

# Join: ()->base (common supertype)
G := if(true?) {F1} else {F2}
G().Value  # Can access base members
```

**Cannot join incompatible types**:

```verse
ReturnInt():int = 1
ReturnFloat():float = 1.0

# BP VM: ERROR - cannot join int and float
# Verse VM: Joins to numeric supertype
Conditional := if(true?) {ReturnInt} else {ReturnFloat}
```

**Cannot join different parameter types without common subtype**:

```verse
class_a := class{}
class_b := class{}

TakeA(X:class_a):int = 1
TakeB(X:class_b):int = 2

# ERROR: Cannot find common subtype for parameters
# Joined := if(true?) {TakeA} else {TakeB}
```

### Mutable and Optional Functions

Unlike regular variables, function members in classes can be mutable or optional using special syntax:

**Mutable function members** (declared without a body):

```verse
callback_handler := class:
    # Mutable function member (no default)
    OnUpdate(DeltaTime:float):void

    # Must be initialized when constructing
Handler := callback_handler{
    OnUpdate := (DT:float) => Print("Update: {DT}")
}

# Can be reassigned
set Handler.OnUpdate = (DT:float) => Print("New: {DT}")
Handler.OnUpdate(0.016)
```

**Optional function members** (using `?` suffix):

```verse
event_system := class:
    # Optional function with default
    OnEvent?(EventType:string):void = Print("Default: {EventType}")

    Trigger(EventType:string):void =
        # Check if function exists before calling
        if (Handler := OnEvent?):
            Handler(EventType)

System := event_system{}
System.Trigger("test")  # Uses default

# Override with custom handler
System2 := event_system{OnEvent := (E:string) => Print("Custom: {E}")}
System2.Trigger("test")  # Uses custom
```

**Mutable optional functions** (both `?` and no body):

```verse
flexible_system := class:
    # Mutable and optional
    Handler?(EventType:string):void

System := flexible_system{Handler := (E:string) => Print(E)}

# Can reassign
set System.Handler = (E:string) => Print("Changed: {E}")

# Can check existence
if (H := System.Handler?):
    H("event")
```

### Overriding Functions in Constructors

You can override function implementations when constructing instances:

```verse
processor := class:
    Process(X:int):string = "Default: {X}"

# Override in constructor
Custom := processor{
    Process(X:int):string = {"Custom: {X}"}
}

Custom.Process(42)  # Returns "Custom: 42"
```

This works with lambdas too:

```verse
Custom2 := processor{
    Process := (X:int) => "Lambda: {X}"
}
```

**Cannot override `<final>` functions**:

```verse
locked := class<final>:
    Process():void = {}

# ERROR: Cannot override in final class
# Instance := locked{Process := () => {}}
```

### Higher-Order Function Patterns

**Map-Filter-Reduce**:

```verse
# Generic map
Map(Items:[]t, F(:t):u where t:type, u:type):[]u =
    for (Item:Items):
        F(Item)

# Generic filter
Filter(Items:[]t, Pred(:t)<decides>:void where t:type):[]t =
    for (Item:Items, Pred[Item]):
        Item

# Generic fold/reduce
Fold(Items:[]t, Initial:u, F(:u, :t):u where t:type, u:type):u =
    var Acc:u = Initial
    for (Item:Items):
        set Acc = F(Acc, Item)
    Acc

# Usage
Numbers := array{1, 2, 3, 4, 5}
Squared := Map[Numbers, (X:int) => X * X]
Evens := Filter[Numbers, (X:int) => X % 2 = 0]
Sum := Fold[Numbers, 0, (Acc:int, X:int) => Acc + X]
```

**Function composition**:

```verse
Compose(F(:b):c, G(:a):b where a:type, b:type, c:type):type{_(:a):c} =
    (X:a) => F(G(X))

Add1 := (X:int) => X + 1
Double := (X:int) => X * 2

# Compose: first doubles, then adds 1
DoubleThenIncrement := Compose[Add1, Double]
DoubleThenIncrement(5)  # Returns 11 (5*2 + 1)
```

**Partial application**:

```verse
Partial(F(:a, :b):c, X:a where a:type, b:type, c:type):type{_(:b):c} =
    (Y:b) => F(X, Y)

Add := (X:int, Y:int) => X + Y
Add5 := Partial[Add, 5]
Add5(3)  # Returns 8
```

### Using `type{}` Syntax

The `type{_(...):...}` syntax declares function types with full detail. This is Verse's primary mechanism for creating precise function type signatures that include parameter types, return types, and effects.

#### Basic Function Type Declarations

The `type{}` construct uses an underscore `_` as a placeholder for the function name, emphasizing that it describes a signature, not a specific function:

```verse
# Function type variable
Handler:type{_(:string, :int)<decides>:void}

Handler = (Name:string, Count:int) =>
    Print("{Name}: {Count}")
    Count > 0  # Decides effect

# Function accepting function parameter
Process(F:type{_(:int):int}, Value:int):int =
    F(Value)

Process((X:int) => X * 2, 5)  # Returns 10
```

#### Important: Function Types Only

The `type{}` construct **exclusively declares function type signatures**. It cannot be used for general type expressions or to extract types from values:

```verse
# VALID: Function signatures
ValidType1 := type{_():int}
ValidType2 := type{_(:string, :int):float}
ValidType3 := type{_()<transacts><decides>:void}

# INVALID: Not function signatures (all produce errors)
# InvalidType1 := type{}                    # ERROR 3544: empty
# InvalidType2 := type{x:int}               # ERROR 3552: field
# InvalidType3 := type{1}                   # ERROR 3552: literal
# InvalidType4 := type{GetValue()}          # ERROR 3502: function call
# InvalidType5 := type{int}                 # ERROR 3552: type reference
# InvalidType6 := type{array}               # ERROR 3502: type constructor
```

#### Function Declarations Without Bodies

Within `type{}`, function declarations must have return types but **cannot have bodies**:

```verse
# VALID: Declaration with return type, no body
C := class:
    Handler:type{_():void}

# INVALID: Declaration with body (ERROR 3552)
# C := class:
#     Handler:type{_():void={}}
```

This restriction ensures that `type{}` describes signatures, not implementations.

#### Function Types as Class Members

Function types work as field types in classes:

```verse
Add(X:int, Y:int):int = X + Y
Multiply(X:int, Y:int):int = X * Y

calculator := class:
    Operation:type{_(:int, :int):int}

# Create instances with different operations
Adder := calculator{Operation := Add}
Multiplier := calculator{Operation := Multiply}

Adder.Operation(5, 3)      # Returns 8
Multiplier.Operation(5, 3) # Returns 15
```

#### Function Types as Local Variables

Function types can be used for local variables, enabling conditional function selection:

```verse
ProcessA():int = 10
ProcessB():int = 20

SelectFunction(UseA:logic):int =
    # Choose function based on condition
    Fn:type{_():int} =
        if (UseA?):
            ProcessA
        else:
            ProcessB
    Fn()

SelectFunction(true)   # Returns 10
SelectFunction(false)  # Returns 20
```

#### Optional Function Types

Combine `type{}` with `?` to create optional function types:

```verse
DefaultHandler():int = -1
CustomHandler():int = 42

Process(Handler:?type{_():int}):int =
    # Use handler if provided, otherwise use default
    Handler?() or DefaultHandler()

Process(false)                   # Returns -1 (no handler)
Process(option{CustomHandler})   # Returns 42 (custom handler)
```

#### Mutable Function Variables

Function variables can be mutable, allowing runtime reassignment:

```verse
ModeA():int = 1
ModeB():int = 2

RunWithModes():int =
    var CurrentMode:type{_():int} = ModeA
    Result1 := CurrentMode()     # Calls ModeA, returns 1

    set CurrentMode = ModeB
    Result2 := CurrentMode()     # Calls ModeB, returns 2

    Result1 + Result2            # Returns 3
```

#### Arrays of Functions

Create arrays of functions sharing the same signature:

```verse
GetZero():int = 0
GetOne():int = 1
GetTwo():int = 2

SumFunctions(Functions:[]type{_():int}):int =
    var Result:int = 0
    for (Fn : Functions):
        set Result += Fn()
    Result

SumFunctions(array{GetZero, GetOne, GetTwo})  # Returns 3
```

#### Effect Specifications in Function Types

Function types can include effect specifications, ensuring type safety with effects:

```verse
PureCompute(X:int)<computes>:int = X + 1
MayFail(X:int)<computes><decides>:int =
    if (X > 0):
        X
    else:
        X  # Would fail if negative

# Type with computes effect
F1:type{_(:int)<computes>:int} = PureCompute

# Type with computes and decides effects
F2:type{_(:int)<computes><decides>:int} = MayFail

# Can assign less-effectful to more-effectful type
F3:type{_(:int)<transacts>:int} = PureCompute  # OK
```

See the Effects chapter for complete details on effect subtyping.

## Nested Functions

Nested functions (also called local functions) are functions defined inside other functions. They provide encapsulation, enable closures over local variables, and help organize complex logic within a function's scope. Unlike lambdas, nested functions have names and can be recursive.

A nested function is declared just like a top-level function, but inside another function's body:

<!--verse
F():void={
-->
```verse
Outer(X:int):int =
    # Nested function definition
    Inner(Y:int):int = Y * 2

    # Call nested function
    Inner(X)

Outer(5)  # Returns 10
```
<!--verse
}
-->

Nested functions are only visible within their enclosing function's scope. They cannot be accessed from outside.

### Capturing Variables from Outer Scope

Nested functions can capture (close over) variables from any enclosing scope, just like lambdas:

<!--verse
F():void={
-->
```verse
MakeGreeter(Name:string):type{_():string} =
    # Greeting captures Name from outer scope
    Greeting():string = "Hello, {Name}!"

    # Return the nested function
    Greeting

SayHello := MakeGreeter("Alice")
SayHello()  # Returns "Hello, Alice!"

SayHi := MakeGreeter("Bob")
SayHi()  # Returns "Hello, Bob!"
```
<!--verse
}
-->

Each call to `MakeGreeter` creates a new closure with its own captured `Name` value.

### Multiple Nesting Levels

Nested functions can access variables from any level of the enclosing scope chain:

<!--verse
F():void={
-->
```verse
Outer(A:int):int =
    Middle(B:int):int =
        Inner(C:int):int =
            # Inner can access A, B, and C
            A + B + C
        Inner(B + 1)
    Middle(A + 1)

Outer(1)  # Returns 1 + 2 + 3 = 6
```
<!--verse
}
-->

The inner-most function has access to all variables from all enclosing scopes.

### Recursive Nested Functions

Nested functions can call themselves recursively:

<!--verse
F():void={
-->
```verse
Factorial(N:int):int =
    # Recursive nested function
    FactorialHelper(X:int):int =
        if (X <= 1):
            1
        else:
            X * FactorialHelper(X - 1)  # Recursive call

    FactorialHelper(N)

Factorial(5)  # Returns 120
```
<!--verse
}
-->

The nested function can reference itself by name within its body.

### Overloading in Nested Functions

Nested functions support overloading by parameter types:

<!--verse
F():void={
-->
```verse
Process(X:int):string =
    # Overloaded nested functions
    Format(Value:int):string = "Int: {Value}"
    Format(Value:float):string = "Float: {Value}"

    # Calls appropriate overload
    IntResult := Format(42)       # Calls int version
    FloatResult := Format(3.14)   # Calls float version

    "{IntResult}, {FloatResult}"

Process(1)  # Returns "Int: 42, Float: 3.14"
```
<!--verse
}
-->

Overload resolution works the same as for top-level functions.

### Closures with Mutable State

Nested functions can capture `var` variables and mutate them, creating stateful closures:

<!--verse
F():void={
-->
```verse
MakeCounter(Initial:int):tuple(type{_():int}, type{_():void}) =
    var Count:int = Initial

    # Getter captures Count
    GetCount():int = Count

    # Incrementer mutates captured Count
    Increment():void = set Count = Count + 1

    (GetCount, Increment)

Counter := MakeCounter(0)
GetValue := Counter(0)
IncrementValue := Counter(1)

GetValue()        # Returns 0
IncrementValue()  # Increments count
GetValue()        # Returns 1
IncrementValue()  # Increments count
GetValue()        # Returns 2
```
<!--verse
}
-->

This pattern creates a closure that maintains private mutable state.

### Accessing Fields Through Captured References

Nested functions can access class instance fields through captured references:

<!--verse
player_data := class:
    Name:string = "Player"
    var Score:int = 0
F():void={
-->
```verse
ProcessPlayer(Player:player_data):string =
    # Nested function accesses Player's fields
    GetInfo():string = "Player: {Player.Name}, Score: {Player.Score}"

    GetInfo()

P := player_data{Name := "Alice", Score := 100}
ProcessPlayer(P)  # Returns "Player: Alice, Score: 100"
```
<!--verse
}
-->

The nested function captures the class instance and can access its members.

### Nested Functions in Class Methods

When nested functions are defined inside class methods, they can access `Self`:

<!--verse
game_object := class:
    Health:int = 100

    ProcessDamage(Damage:int):string =
        # Nested function can access Self
        GetStatus():string = "Health: {Self.Health}"

        GetStatus()
F():void={
-->
```verse
Obj := game_object{}
Obj.ProcessDamage(10)  # Returns "Health: 100"
```
<!--verse
}
-->

The nested function inherits access to `Self` from the enclosing method.

### Type Parameters in Nested Functions

Nested functions can use type parameters from their enclosing function:

<!--verse
F():void={
-->
```verse
Transform<T, U>(X:T, Converter:type{_(:T):U} where T:type, U:type):U =
    # Nested function uses T and U from outer scope
    Apply(Value:T):U = Converter(Value)

    Apply(X)

# Call with type parameters inferred
Result := Transform(42, (N:int) => "{N}")  # Returns "42"
```
<!--verse
}
-->

The nested function `Apply` can reference the type parameters `T` and `U` from `Transform`.

### Default Values with Captured Variables

Default parameter values in nested functions can reference captured variables:

<!--verse
F():void={
-->
```verse
Configure(BaseValue:int):int =
    # Default references captured BaseValue
    ApplyMultiplier(?Multiplier:int = BaseValue):int =
        Multiplier * 2

    ApplyMultiplier()  # Uses BaseValue as default

Configure(5)  # Returns 10 (5 * 2)
```
<!--verse
}
-->

The default value is evaluated using the captured variable's value.

### Block Scopes with Nested Functions

Nested functions can be defined in block scopes and capture from multiple levels:

<!--verse
F():void={
-->
```verse
Process(X:int):int =
    Y := 10

    Outer():int = X + Y

    block:
        Z := 5
        # Inner captures X, Y, and Z
        Inner():int = X + Y + Z

        Outer() + Inner()

Process(1)  # Returns (1 + 10) + (1 + 10 + 5) = 27
```
<!--verse
}
-->

Functions in inner blocks can access variables from all enclosing scopes.

## Restrictions on Nested Functions

Nested functions have several important restrictions that distinguish them from top-level functions:

Nested functions **cannot** have access specifiers like `<public>`, `<internal>`, or `<private>`:

Nested functions are always private to their enclosing function.

You cannot define classes inside functions (nested or otherwise):

```verse
# ERROR 3502: Cannot define classes in local scope
F():void =
    my_class := class {}  # ERROR

# Correct: Define classes at module level
my_class := class {}

F():void =
    Instance := my_class{}  # OK - can use class
```

Classes must be defined at module or package scope.

Nested functions cannot reference variables or other nested functions defined later in the same scope:

```verse
# ERROR 3506: G used before defined
F():void =
    X := G()     # ERROR: G not yet defined
    G():int = 42

# Correct: Define before use
F():void =
    G():int = 42
    X := G()     # OK: G is defined
```

This also means **mutually recursive nested functions are not allowed**:

```verse
# ERROR 3506: G and H cannot be mutually recursive
F():int =
    G(N:int):int =
        if (N = 0):
            0
        else:
            H(N - 1)  # ERROR: H not yet defined

    H(N:int):int = G(N)  # Can't call G from before its definition completes

    G(5)
```

Only single-function recursion is supported (a function calling itself).

The `(super:)` syntax for calling parent class methods **cannot** be used in nested functions:

```verse
# ERROR 3612: super not allowed in nested function
base_class := class:
    F(X:int):int = X

derived_class := class(base_class):
    F<override>(X:int):int =
        G():int =
            (super:)F(X)  # ERROR: super not allowed here
        G()

# Correct: Use super directly in the overriding method
derived_class := class(base_class):
    F<override>(X:int):int =
        BaseResult := (super:)F(X)  # OK
        G():int = BaseResult * 2
        G()
```

Super calls must appear directly in the overriding method body.

## Parametric Functions

Parametric functions (also called generic functions) allow you to write code that works with multiple types while maintaining complete type safety. Rather than writing separate functions for each type, you define a single function with type parameters that adapt to whatever types you use them with.

A parametric function declares type parameters using a `where` clause that specifies constraints on those types:

```verse
# Simple identity function - works with any type
Identity(X:t where t:type):t = X

# Usage - type parameter inferred automatically
Identity(42)        # t inferred as int, returns 42
Identity("hello")   # t inferred as string, returns "hello"
Identity(true)      # t inferred as logic, returns true
```

The `where t:type` clause declares `t` as a type parameter with the constraint `type`, meaning it can be any Verse type. The function signature `(X:t):t` means "takes a value of type `t` and returns a value of that same type `t`."

```verse
FunctionName(Parameters where TypeParameter:Constraint, ...):ReturnType = Body
```

- **Type parameters** appear in the `where` clause
- **Constraints** specify requirements (e.g., `type`, `subtype(comparable)`)
- **Multiple type parameters** are comma-separated in the `where` clause

Verse automatically infers type parameters from the arguments you pass, eliminating the need for explicit type annotations in most cases:

```verse
# Function with two type parameters
Pair(X:t, Y:u where t:type, u:type):tuple(t, u) = (X, Y)

# All type parameters inferred
Pair(1, "one")        # t = int, u = string, returns (1, "one")
Pair(true, 3.14)      # t = logic, u = float, returns (true, 3.14)
```

**Inference with collections:**

```verse
# Generic first element function
First(Items:[]t where t:type)<decides>:t = Items[0]?

Numbers := array{1, 2, 3}
First(Numbers)  # t inferred as int from []int
```

**Tuple conversion:**

When you pass multiple values to a parametric function expecting a single type parameter, Verse can infer either a tuple or an array:

```verse
# Returns the argument unchanged
Identity(X:t where t:type):t = X

# Passing multiple values creates a tuple
Result1:tuple(int, int) = Identity(1, 2)  # t = tuple(int, int)

# Can also be treated as an array
Result2:[]int = Identity(1, 2)  # t = []int via conversion
```

### Type Constraints

Type constraints restrict which types can be used with type parameters, enabling operations that require specific capabilities.

**The `type` constraint:**

The most permissive constraint - accepts any Verse type:

```verse
# Works with absolutely any type
Store(Value:t where t:type):t = Value
```

**The `subtype` constraint:**

Restricts to types that are subtypes of a specified type:

```verse
vehicle := class:
    Speed:float = 0.0

car := class(vehicle):
    NumDoors:int = 4

# Only accepts vehicle or its subtypes
ProcessVehicle(V:t where t:subtype(vehicle)):t =
    # Can access Speed because we know V is a vehicle
    Print("Speed: {V.Speed}")
    V

# Valid calls
ProcessVehicle(vehicle{})      # t = vehicle
ProcessVehicle(car{})           # t = car (subtype of vehicle)

# Invalid - int is not a subtype of vehicle
# ProcessVehicle(42)  # ERROR 3509
```

**Key insight:** The function returns type `t`, not the base type. This preserves the specific type:

```verse
MyCar := car{NumDoors := 4, Speed := 60.0}
Result := ProcessVehicle(MyCar)  # Result has type car, not vehicle
Result.NumDoors  # Can access car-specific fields
```

**The `subtype(comparable)` constraint:**

Enables equality comparisons:

```verse
# Can use = and <> operators on t
FindInArray(Items:[]t, Target:t where t:subtype(comparable))<decides>:int =
    for (Index -> Item : Items):
        if (Item = Target):  # Comparison allowed due to comparable constraint
            return Index
    -1
```

**Related type parameters:**

Type parameters can reference each other in constraints:

```verse
# u must be a subtype of t
Convert(Base:t, Derived:u where t:type, u:subtype(t)):t =
    Base

# This ensures type safety across related types
```

**Type parameters in function types:**

```verse
# F is a function from t to u
Apply(F:type{_(:t):u}, X:t where t:type, u:type):u = F(X)

# Usage
Double := (N:int) => N * 2
Apply(Double, 21)  # t = int, u = int, returns 42
```

### Higher-Order Parametric Functions

Parametric functions can accept other functions as parameters, with type parameters flowing through the composition:

```verse
# Map over an array with a transforming function
Map(Items:[]t, Transform(:t):u where t:type, u:type):[]u =
    for (Item : Items):
        Transform(Item)

Numbers := array{1, 2, 3}
Strings := Map[Numbers, (N:int) => "{N}"]  # []string
```

**Function composition:**

```verse
# Compose two functions: (F ∘ G)(x) = F(G(x))
Compose(F(:b):c, G(:a):b where a:type, b:type, c:type):type{_(:a):c} =
    (X:a) => F(G(X))

Add1 := (X:int) => X + 1
Double := (X:int) => X * 2
DoubleThenAdd := Compose[Add1, Double]
DoubleThenAdd(5)  # Returns 11 (5 * 2 + 1)
```

**Partial application:**

```verse
# Fix the first argument of a two-argument function
Partial(F(:a, :b):c, X:a where a:type, b:type, c:type):type{_(:b):c} =
    (Y:b) => F(X, Y)

Add := (X:int, Y:int) => X + Y
Add5 := Partial[Add, 5]
Add5(3)  # Returns 8
```

### Member Access on Parametric Types

When using subtype constraints, you can access members that exist on the base type:

```verse
entity := class:
    Name:string = "Entity"
    Health:int = 100

player := class(entity):
    Score:int = 0

# Can access entity members through type parameter
GetInfo(E:t where t:subtype(entity)):tuple(t, string, int) =
    (E, E.Name, E.Health)  # Can access Name and Health

P := player{Name := "Alice", Health := 100, Score := 1500}
Info := GetInfo(P)  # Returns (player instance, "Alice", 100)
# Info(0) has type player, not entity - type preserved!
```

**Method calls work too:**

```verse
entity := class:
    GetStatus():string = "Active"

# Call methods on parametrically-typed values
CheckStatus(E:t where t:subtype(entity)):string =
    E.GetStatus()  # Method call through type parameter
```

### Effects and Parametric Functions

Parametric functions can have effects like `<decides>`, `<transacts>`, and `<suspends>`:

**Failable parametric functions:**

```verse
# Identity that can fail
FallableIdentity(X:t where t:type)<decides>:t = X

# Called with square bracket syntax
Result := FallableIdentity[42]  # Succeeds with 42
```

### Type Parameters in Extension Methods

Extension methods support type parameters:

```verse
# Generic map for arrays
(Items:[]t).Map(Transform(:t):u where t:type, u:type):[]u =
    for (Item : Items):
        Transform(Item)

# Usage
array{1, 2, 3}.Map((N:int) => N * 2)  # Returns array{2, 4, 6}
```

**Constrained extension methods:**

```verse
# Only works with comparable types
(Items:[]t).Contains(Target:t where t:subtype(comparable))<decides>:logic =
    for (Item : Items):
        if (Item = Target):
            return true
    false
```

### Polarity and Variance

Type parameters must be used consistently according to variance rules. This ensures type safety when functions are used as values or passed as arguments.

**Covariant positions** (safe for return types):

- Function return types
- Tuple/array element types (as return)
- Map value types (as return)

**Contravariant positions** (safe for parameter types):

- Function parameter types
- Map key types

**The polarity check:**

Verse validates that type parameters appear only in positions compatible with their intended use:

```verse
# Valid: t appears covariantly (return type)
GetValue(X:t where t:type):t = X

# Valid: t appears contravariantly (parameter)
Consume(X:t where t:type):void = {}

# Valid: t appears in both positions (through function parameter and return)
Apply(F:type{_(:t):t}, X:t where t:type):t = F(X)
```

**Invariant types cause errors:**

```verse
# ERROR 3502: Cannot return type that's invariant in t
# c(t:type) := class{var X:t}  # Mutable field makes c invariant in t
# MakeContainer(X:t where t:type):c(t) = c(t){X := X}
```

The error occurs because `c(t)` contains a mutable field of type `t`, making it invariant - neither covariant nor contravariant. Returning such a type from a parametric function is unsafe.

**Map polarity:**

Maps are contravariant in keys and covariant in values:

```verse
# Valid: contravariant key, covariant value
ProcessMap(M:[t]u where t:subtype(comparable), u:type):[t]u = M
```

### Type Parameters in Nested Functions

Nested functions can reference type parameters from their enclosing function:

```verse
Transform(X:t, Converter:type{_(:t):u} where t:type, u:type):u =
    # Nested function uses t and u from outer scope
    Apply(Value:t):u = Converter(Value)

    Apply(X)

Result := Transform(42, (N:int) => "{N}")  # Returns "42"
```

The nested function `Apply` inherits the type parameters `t` and `u` from `Transform`.

<!-- ### Practical Patterns

**Option mapping:**

```verse
# Map a function over an optional value
OptionMap(F(:t):u, X:?t where t:type, u:type):?u =
    if (Value := X?):
        option{F(Value)}
    else:
        false

MaybeNumber:?int = option{42}
MaybeString := OptionMap[(N:int) => "{N}", MaybeNumber]  # option{"42"}
```

**Array utilities:**

```verse
# Generic filter
Filter(Items:[]t, Predicate(:t)<decides>:void where t:type):[]t =
    for (Item : Items, Predicate[Item]):
        Item

# Generic fold/reduce
Fold(Items:[]t, Initial:u, Combine(:u, :t):u where t:type, u:type):u =
    var Acc:u = Initial
    for (Item : Items):
        set Acc = Combine(Acc, Item)
    Acc

Numbers := array{1, 2, 3, 4, 5}
Evens := Filter[Numbers, (N:int) => N % 2 = 0]
Sum := Fold[Numbers, 0, (Acc:int, N:int) => Acc + N]
```

**Generic containers:**

```verse
# Stack data structure
stack(element_type:type) := class:
    var Items:[]element_type = array{}

    Push(Item:element_type):void =
        set Items += array{Item}

    Pop()<decides>:element_type =
        Last := Items[Items.Length - 1]?
        set Items = Items.Slice(0, Items.Length - 1)
        Last

# Usage
IntStack := stack(int){}
IntStack.Push(42)
```

**Type-safe builders:**

```verse
# Builder pattern with parametric validation
builder(t where t:type) := class:
    var Current:?t = false

    With(Modifier:type{_(t):t}):builder(t) =
        if (Value := Current?):
            set Current = option{Modifier(Value)}
        Self

    Build()<decides>:t = Current?
```
-->

### Restrictions

**Cannot reference super-qualified functions:**

```verse
# ERROR 3502: Cannot store reference to super: qualified method
# base := class:
#     F():void = {}
# derived := class(base):
#     F<override>():void =
#         SuperF := (super:)F  # ERROR
```

You can call `(super:)F` directly but cannot store a reference to it.

**Type parameters must satisfy polarity:**

Type parameters appearing in mutable fields or other invariant positions cannot be used as return types from parametric functions.

**No undefined type parameters:**

Every type parameter declared in the `where` clause must be used in the function signature (parameters or constrained return positions).

### Best Practices

**Use descriptive type parameter names when clarity helps:**

```verse
# Generic: uses conventional t, u
Map(Items:[]t, F(:t):u where t:type, u:type):[]u = ...

# Domain-specific: uses meaningful names
ProcessRecords(Records:[]record_type,
               Handler:type{_(:record_type):result_type}
               where record_type:type, result_type:type):[]result_type = ...
```

**Prefer constraints that enable operations:**

```verse
# Too permissive - can't do much with t:type
Process(X:t where t:type):void = {}

# Better - constraint enables comparison
FindDuplicate(Items:[]t where t:subtype(comparable)):?t =
    # Can use = operator due to comparable constraint
```

**Type parameters for reusability, not premature generalization:**

Write parametric functions when you have actual use cases for multiple types, not speculatively.

**Let inference work:**

```verse
# Users rarely need to specify type parameters explicitly
Identity(42)  # Inference works
# Identity(int)(42)  # Unnecessary explicit type argument
```

## Return Values

Functions return the value of the last executed expression, which often eliminates the need for explicit return statements. This design choice leads to more concise code:

```verse
GetStatus(Score:int):string =
    if (Score >= 90):
        "Excellent"
    else if (Score >= 70):
        "Good"
    else:
        "Needs Improvement"
```

However, when you need to exit a function early or when the control flow becomes complex, explicit return statements provide clarity:

```verse
FindFirstNegative(Numbers:[]int):?int =
    for (Number : Numbers):
        if (Number < 0):
            return option{Number}
    false  # No negative found
```

Functions with void return types are special - they always return the value `false`, regardless of what expressions appear in their body. This consistent behavior simplifies the handling of void functions in logical contexts.

## Function Overloading

Function overloading allows you to define multiple functions with the same name but different parameter types. The compiler selects the correct version based on the types of the arguments provided at the call site.

Define multiple functions with the same name but different parameter types:

```verse
# Overload by parameter type
Process(Value:int):string = "Integer: {Value}"
Process(Value:float):string = "Float: {Value}"
Process(Value:string):string = "String: {Value}"

# Calls select the appropriate overload
Process(42)        # Returns "Integer: 42"
Process(3.14)      # Returns "Float: 3.14"
Process("hello")   # Returns "String: hello"
```

The compiler determines which overload to call based on the argument types. Each overload must have a distinct parameter type signature.

### Overloading Across Scopes

Overloading works across different scopes, allowing you to extend existing functions:

**Nested functions can overload:**

```verse
f():void = {}

g(f(x:int):void):void =
    f()      # Calls outer f
    f(42)    # Calls nested f
```

**Class methods can overload:**

```verse
f():void = {}

C := class:
    f(x:int):void = {}

    CallBoth():void =
        f()      # Calls module-level f
        f(10)    # Calls class method f
```

**Across modules:**

```verse
# Module A
vmodule(A):
    f<public>():void = {}

# Module B
vmodule(B):
    using{A}
    f(x:int):void = {}

    g():void =
        f()      # Calls A.f
        f(0)     # Calls B.f
```

### Cannot Capture Overloaded Functions

You cannot take a reference to an overloaded function name:

```verse
# ERROR 3502: Cannot capture overloaded function
f(x:int):void = {}
f(x:float):void = {}

# Error: which f?
# g:void = f
```

This restriction exists because the compiler cannot determine which overload you mean without seeing the call site with arguments.

### Effects and Overloading

You can overload functions with different effects, but only if the parameter types are also different:

**Valid: Different types, different effects:**

```verse
Process(x:float):float = x
Process(x:int)<transacts><decides>:int = x = 1

Process(3.0)   # Returns 3.0 (non-failable)
Process[1]     # Returns option{1} (failable)
```

**Invalid: Same types, different effects:**

```verse
# ERROR 3532: Same parameter type
f(x:int):void = {}
f(x:int)<transacts><decides>:void = {}  # ERROR
```

Effects alone don't create distinctness - you need different parameter types.

### Overloaded Class Methods

Classes can have overloaded methods, and subclasses can override them:

```verse
base_class := class:
    Process(x:int):int = x
    Process(x:float):float = x

derived_class := class(base_class):
    Y:int = 10
    Z:float = 5.0

    Process<override>(x:int):int = x + Y
    Process<override>(x:float):float = x + Z

B := base_class{}
B.Process(2)     # Returns 2
B.Process(2.0)   # Returns 2.0

D := derived_class{}
D.Process(2)     # Returns 12 (2 + 10)
D.Process(2.0)   # Returns 7.0 (2.0 + 5.0)
```

### Adding Overloads in Subclasses

Subclasses can add new overloads to methods:

```verse
C0 := class:
    f(x:int):int = x

C1 := class(C0):
    # Add new overload for float
    f(x:float):float = x

C0{}.f(5)     # OK - int overload
C1{}.f(5)     # OK - inherited int overload
C1{}.f(5.0)   # OK - new float overload
```

**Important:** When a subclass defines a method that shares a name with a parent method, it must either:

1. Provide a **distinct parameter type** (different from all parent overloads)
2. **Override exactly one** parent overload using `<override>`

```verse
C := class{}
D := class(C){}

# Parent class with overloads
E := class:
    f(c:C):C = c
    f(e:E):E = e

# Valid: Overrides one parent overload
F := class(E):
    f<override>(c:C):D = D{}

# ERROR 3532: D is subtype of C, overlaps but doesn't override
# G := class(E):
#     f(d:D):D = d  # ERROR - ambiguous with f(c:C)
```

### Interfaces with Overloaded Methods

Interfaces can declare overloaded methods:

```verse
formatter := interface:
    Format(x:int):string = "{x}"
    Format(x:float):string = "{x}"

entity := class(formatter):
    Format<override>(x:int):string = "Entity-{x}"
    Format<override>(x:float):string = "Entity-{x}"
```

### Restrictions

**Cannot use `var` with overloaded functions:**

Function-valued variables cannot be overloaded:

```verse
# ERROR 3502: Cannot have var overloaded functions
# var f():void = {}
# var f(x:int):void = {}

# ERROR: Cannot mix var and regular
# var f():void = {}
# f(x:int):void = {}
```

**Cannot overload functions with non-functions:**

A name cannot be both a function and a non-function value:

```verse
# ERROR 3532: Cannot overload with variable
# f:int = 0
# f():void = {}
```

**Cannot overload classes:**

Class names cannot be overloaded:

```verse
# ERROR 3588, 3532: Cannot overload class name
# C := class{}
# C(x:int):C = C{}
```

**Bottom type cannot resolve overloads:**

The bottom type (from `return` without a value) cannot be used for overload resolution:

```verse
# ERROR 3518: Cannot determine which overload
F(X:int):int = X
F(X:float):float = X

# G():void =
#     F(@ignore_unreachable return)  # ERROR - which F?
#     0
```

### Overloading with `<suspends>`

You can mix suspending and non-suspending overloads if the parameter types differ:

```verse
f(x:int)<suspends>:void =
    Sleep(1.0)

f(x:float):void =
    Print("Non-suspending")

# Call non-suspending directly
f(1.0)

# Call suspending with spawn
spawn{f(1)}
```

**Cannot call suspending overload without spawn:**

```verse
# ERROR 3512: suspends version needs spawn context
f(x:int):void = {}
f(x:float)<suspends>:void = {}

# g():void = f(1.0)  # ERROR - float version is suspends
```

**Cannot spawn non-suspending overload:**

```verse
# ERROR 3538: Cannot spawn non-suspends function
f(x:int):void = {}
f(x:float)<suspends>:void = {}

# g():void = spawn{f(1)}  # ERROR - int version not suspends
```

## Types and Overloading

Every function has a type that captures its parameters, effects, and return value. The type syntax uses an underscore as a placeholder for the function name:

<!--verse
X:=
-->
```verse
type{_(:int, :string)<decides>:float}
```

This represents any function that takes an integer and a string, might fail (has the `decides` effect), and returns a float when successful.

Multiple functions may share a name through overloading, as long as their signatures don't create ambiguity. The compiler can distinguish between overloads based on the argument types:

<!--
TODO: these do not compile, the 0.2f gets an error that asks for a f64, but even after that...
-->

<!--NoCompile-->
```verse
Transform(X:int):string = "{X}"
Transform(X:float):string = "{X:0.2f}"
Transform(X:string):string = "String: {X}"

Result1 := Transform(42)        # Calls int version
Result2 := Transform(3.14)      # Calls float version
Result3 := Transform("Hello")   # Calls string version
```

However, overloading has strict limitations based on **type distinctness**. Two types are considered "distinct" for overload purposes only if there is no possible value that could match both types. This restriction prevents ambiguity and ensures that function calls can always be resolved unambiguously at compile time.

### Overload Distinctness Rules

Verse uses precise rules to determine whether two parameter types are distinct enough to allow overloading. Understanding these rules is critical for designing clear APIs.

#### Types That Are NOT Distinct

The following type pairs are **not distinct** and cannot be used to overload functions:

**1. Optional and Logic**

`?t` and `logic` are not distinct because `logic` is equivalent to `?void`:

```verse
# ERROR: Not distinct
f(:?t):void = {}
f(:logic):void = {}  # ERROR 3532
```

**2. Arrays and Maps**

Arrays `[]t` and maps `[k]t` are not distinct:

```verse
domain := enum{A, B, C}
range := class{}

# ERROR: Not distinct
f(:[]range):void = {}
f(:[domain]range):void = {}  # ERROR 3532
```

**3. Functions and Maps**

Function types and maps are not distinct:

```verse
# ERROR: Not distinct
f(:[domain]range):void = {}
f(g(:domain)<transacts><decides>:range):void = {}  # ERROR 3532
```

**4. Functions and Arrays**

Function types and arrays are not distinct because an overloaded function could match both:

```verse
# ERROR: Not distinct
f(:[]range):void = {}
f(g(:domain)<transacts><decides>:range):void = {}  # ERROR 3532
```

**5. Interfaces and Classes**

An interface and any class are never distinct, even if the class doesn't implement the interface, because a subtype of the class might:

```verse
i := interface{}
t := class{}

# ERROR: Not distinct (subtype of t might implement i)
f(:i):void = {}
f(:t):void = {}  # ERROR 3532
```

This also applies when the class implements the interface:

```verse
i := interface{}
t := class(i){}

# ERROR: Not distinct
f(:i):void = {}
f(:t):void = {}  # ERROR 3532
```

**6. Functions with Different Effects**

Functions are not distinct based on effects alone. Changing or removing effects doesn't create a distinct overload:

```verse
a := class{}
b := class{}

# ERROR: Not distinct
f(g(:a)<transacts><decides>:b):void = {}
f(g(:a):b):void = {}  # ERROR 3532
```

**7. Functions with Different Signatures**

Functions with different parameter or return types are not distinct because of function overloading:

```verse
# ERROR: Not distinct
f(g(:b):b):void = {}
f(g(:a):b):void = {}  # ERROR 3532
```

**8. void as Top Type**

`void` is treated as equivalent to the top type (accepts `any`), so it's not distinct from any other type:

```verse
# ERROR: Not distinct
F(:int):void = {}
F(:void):void = {}  # ERROR 3532

# Also applies to classes and structs
a := class{}
F(:a):void = {}
F(:void):void = {}  # ERROR 3532
```

**9. Subtype Relationships**

Classes with subtype relationships are not distinct:

```verse
a := class{}
b := class(a){}

# ERROR: Not distinct
f(:a):void = {}
f(:b):void = {}  # ERROR 3532
```

**10. Tuple Distinctness Rules**

Tuples have complex distinctness rules:

**Empty tuples and arrays are not distinct:**

```verse
a := class{}

# ERROR: Not distinct
f(:tuple(), :a):void = {}
f(:[]a, :a):void = {}  # ERROR 3532
```

**Tuples and arrays are distinct only if tuple element types are completely distinct:**

```verse
a := class{}
b := class(a){}

# ERROR: Not distinct (b is subtype of a)
f(:tuple(a, b), :a):void = {}
f(:[]a, :a):void = {}  # ERROR 3532

# Valid: b is not related to a
a := class{}
b := class{}
f(:tuple(a, b), :a):void = {}  # OK
f(:[]a, :a):void = {}          # OK
```

**Tuples and maps with `int` key are not distinct:**

```verse
a := class{}

# ERROR: Not distinct
f(:tuple(a), :a):void = {}
f(:[int]a, :a):void = {}  # ERROR 3532
```

**Tuples and maps with non-`int` key ARE distinct:**

```verse
a := class{}

# Valid: Distinct types
f(:tuple(a), :a):void = {}
f(:[logic]a, :a):void = {}  # OK
```

**Singleton tuples and optional for `int` are not distinct:**

```verse
a := class{}

# ERROR: Not distinct
f(:tuple(int), :a):void = {}
f(:?int, :a):void = {}  # ERROR 3532
```

**Singleton tuples and optional for non-`int` ARE distinct:**

```verse
a := class{}

# Valid: Distinct types
f(:tuple(a), :a):void = {}
f(:?a, :a):void = {}  # OK
```

## Storing Functions in Variables

Functions are first-class values and can be stored in local variables and passed as parameters:

```verse
# Store function in local variable
Square := (X:int) => X * X
Result := Square(5)

# Pass function as parameter
Apply(F(:int):int, Value:int):int = F(Value)
Apply(Square, 10)  # Returns 100
```

Local function variables are always immutable - you cannot reassign them after binding.

### Function Variables with `var`

You **cannot** use `var` to create mutable function-valued variables:

```verse
# ERROR: Cannot use var for function values
# var GlobalHandler():void = {}  # ERROR

# ERROR: Cannot use var in classes
# my_class := class:
#     var Handler():void = {}  # ERROR
```

The `var` keyword is not supported for function-typed variables. This maintains stability around function references and their contracts.

### Alternative: Mutable Containers

If you need changeable behavior, store function references in mutable containers:

```verse
# Option container with function
callback_manager := class:
    var CurrentHandler:?type{_():void} = false

    SetHandler(NewHandler():void):void =
        set CurrentHandler = option{NewHandler}

    Invoke():void =
        if (Handler := CurrentHandler?):
            Handler()

# Array of functions
multi_callback := class:
    var Handlers:[]type{_():void} = array{}

    AddHandler(H():void):void =
        set Handlers = Handlers + array{H}

    InvokeAll():void =
        for (Handler : Handlers):
            Handler()
```

### Alternative: Mutable Function Members

While `var` doesn't work for functions, classes support **mutable function members** using special syntax. See the [Function Types and Lambdas](#function-types-and-lambdas) section for details on mutable and optional function members declared without bodies.

### Alternative: Interface-Based Polymorphism

For dynamic dispatch, use interfaces with method overriding:

```verse
# Interface-based approach
event_handler := interface:
    Handle():void

default_handler := class(event_handler):
    Handle<override>():void =
        Print("Default handling")

special_handler := class(event_handler):
    Handle<override>():void =
        Print("Special handling")

# Manager can switch between implementations
event_manager := class:
    var Handler:event_handler = default_handler{}

    SetHandler(H:event_handler):void =
        set Handler = H

    Process():void =
        Handler.Handle()

Manager := event_manager{}
Manager.Process()  # Prints "Default handling"
Manager.SetHandler(special_handler{})
Manager.Process()  # Prints "Special handling"
```

This approach provides changeable behavior through polymorphism rather than mutable function references.

## Publishing Functions and Transparency

Publishing a function is a promise of backwards compatibility between the function and its clients. Consider this simple function:

```verse
F1<public>(X:int):int = X + 1
```

The type annotation (`X:int):int`) tells us that this function promises that given any integer it will always return an integer. That contract cannot be broken in future versions of the code. The implementation could change in the future, perhaps to perform additional operations or optimizations, as long as it maintains these type constraints.

Now consider a slightly different version:

<!-- TODO does not compile ?? -->

```verse
F2<public>(X:int) := X + 1
```

The type of this function is inferred from its body. This implies a very different promise: this syntax creates a forever guarantee - the right-hand side will remain exactly the same throughout the lifetime of your code.  Sometimes functions like these are referred to as *transparent*, this transparency allows for powerful compile-time computations.
