# Built-in Data Types

Verse provides a rich set of built-in types that cover the full spectrum of programming needs. The numeric types `int`, `float`, and `rational` handle mathematical operations, counters, and measurements. The `logic` type represents boolean values for conditions and flags. Text is handled through `char`, `char32`, and `string` types for character data, player names, and messages. Container types like arrays, maps, optionals, and tuples manage collections and structured data. Two special types, `any` and `void`, serve unique roles in the type hierarchy as the supertype of all types and the empty type respectively.
Let's explore each built-in type in detail, starting with the numeric types that form the backbone of game logic.

## Intrinsic Functions

Verse includes **intrinsic functions**—built-in operations provided directly by the runtime that cannot be implemented in pure Verse code. These functions receive special compiler treatment and form the foundation for many language features. Understanding intrinsics helps you work effectively with Verse's built-in capabilities and understand their unique constraints.

Intrinsic functions are special because they:

- **Implemented by the runtime**: Written in C++ or other native code, not Verse
- **Cannot be replicated in Verse**: Require access to runtime internals or low-level operations
- **Receive compiler recognition**: The compiler knows about them and may optimize their use

Examples include mathematical operations like `Abs()`, collection methods like `Find()`, and type conversions like `ToString()`.

Most intrinsic functions **cannot be referenced as first-class values**. This means you can call them directly, but you cannot store them in variables or pass them as function arguments:

```verse
Result := Abs(-42)  # Returns 42

# Invalid: Cannot reference without calling
# F := Abs  # ERROR

# Invalid: Cannot pass as parameter
# ApplyFunction(Abs, -42)  # ERROR
```

This restriction exists because intrinsics often require special calling conventions or optimizations that don't fit the standard function model. If you need to pass intrinsic functionality around, wrap it in a lambda or regular function.

Intrinsic functions fall into several categories:

**Mathematical operations:**

- Absolute value, rounding, trigonometry
- Example: `Abs(X)`

**Collection operations:**

- Searching, slicing, concatenating
- Example: `Array.Find(Element)`

**Type conversions:**

- Converting between types
- Example: `ToString(Value)`

**Runtime queries:**

- Asking questions about values at runtime
- Example: `GetSecondsSinceEpoch()`

The following sections document each built-in type and its associated intrinsic operations.

## Integers

The `int` type represents integer, non-fractional values. An `int` can contain a positive number, a negative number, or zero.
<!-- TODO: is the following true? BigInt?
Supported integers range from `-9,223,372,036,854,775,808` to `9,223,372,036,854,775,807`, inclusive.
-->

You can include `int` values within your code as literals.

<!--NoCompile-->
```verse
A :int= -42                                 # civilian size
B := 42424242424242424242424242424242424242424242424242 # scary 

AnswerToTheQuestion :int= 42               # A variable that never changes
CoinsPerQuiver :int= 100                   # A quiver costs this many coins
ArrowsPerQuiver :int= 15                   # A quiver contains this many arrows

# Mutable variables (see Mutability chapter for details on var and set)
var Coins :int= 225                        # The player currently has 225 coins
var Arrows :int= 3                         # The player currently has 3 arrows
var TotalPurchases :int= 0                 # Track total purchases
```

You can use the four basic math operations with integers: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

<!--verse
F(MyInt:int,MyHugeInt:int):void={
-->
```verse
var C :int= (-MyInt + MyHugeInt - 2) * 3   # arithmetic
set C += 1                                 # like saying, set C = C + 1
set C *= 2                                 # like saying, set C = C * 2
```
<!--verse
}
-->

For integers, the operator `/` is failable, and the result is a `rational` type if it succeeds.

## Rationals

The `rational` type represents exact fractions as ratios of integers. Unlike `int` or `float`, you cannot write a `rational` literal directly—rationals are created through integer division using the `/` operator.

<!--verse
F()<decides>:void={
-->
```verse
X := 7 / 3    # X has type rational, representing exactly 7÷3
```
<!--verse
}
-->

Rationals provide *exact arithmetic* without the precision loss of floating-point numbers, making them ideal for game logic requiring precise fractional calculations (resource distribution, turn-based systems, probability calculations).
 Rationals

Integer division with `/` produces a rational value. Division by zero fails:

<!--verse
F()<decides>:void={
-->
```verse
Half := 5 / 2           # rational: exactly 5/2
Third := 10 / 3         # rational: exactly 10/3
Quarter := 1 / 4        # rational: exactly 1/4

if (not (1 / 0)):
    # Division by zero fails
```
<!--verse
}
-->

Rationals are automatically reduced to lowest terms for equality comparisons:

<!--verse
F()<decides>:void={
-->
```verse
# All these are equal - reduced to 5/2
(5 / 2) = (10 / 4)      # true
(5 / 2) = (15 / 6)      # true
(10 / 4) = (15 / 6)     # true
```
<!--verse
}
-->

This normalization ensures that mathematically equivalent rationals compare as equal regardless of how they were constructed.

Negative signs are normalized to the numerator:

<!--verse
F()<decides>:void={
-->
```verse
(1 / -3) = (-1 / 3)     # true: negative moves to numerator
(-1 / -3) = (1 / 3)     # true: double negative becomes positive
```
<!--verse
}
-->

This canonical form simplifies equality checking and ensures consistent behavior.

An important property: *`int` is a subtype of `rational`*. This means any integer can be used where a rational is expected:

<!--verse
F()<decides>:void={
-->
```verse
# Function accepting rational
ProcessRational(X:rational):rational = X

# Can pass integers directly
ProcessRational(5)      # 5 is implicitly 5/1 (rational)
ProcessRational(0)      # 0 is implicitly 0/1 (rational)
```
<!--verse
}
-->

However, you *cannot* return a rational where an int is expected—that would be a narrowing conversion:

<!--NoCompile-->
```verse
# Invalid: Cannot narrow rational to int
# BadFunction(X:rational):int = X  # ERROR 3510
```

Whole number rationals equal their integer equivalents:

<!--verse
F()<decides>:void={
-->
```verse
(2 / 1) = 2             # true
2 = (2 / 1)             # true
(4 / 2) = 2             # true: 4/2 reduces to 2/1, equals 2
(9 / 3) = 3             # true: 9/3 reduces to 3/1, equals 3
```
<!--verse
}
-->

This enables seamless mixing of integer and rational values in calculations.

Two functions convert rationals to integers:

- **`Floor`** — rounds toward negative infinity (down on number line)
- **`Ceil`** — rounds toward positive infinity (up on number line)

<!--verse
F()<decides>:void={
-->
```verse
# Positive rationals
Floor(5 / 2) = 2        # 2.5 → 2 (down)
Ceil(5 / 2) = 3         # 2.5 → 3 (up)

# Negative rationals - note direction!
Floor((-5) / 2) = -3    # -2.5 → -3 (toward negative infinity)
Ceil((-5) / 2) = -2     # -2.5 → -2 (toward positive infinity)

# With negative denominator
Floor(5 / -2) = -3      # Same as (-5)/2
Ceil(5 / -2) = -2       # Same as (-5)/2

# Both negative
Floor((-5) / -2) = 2    # 2.5 → 2
Ceil((-5) / -2) = 3     # 2.5 → 3
```
<!--verse
}
-->

`Floor` rounds toward negative infinity, *not* toward zero. This matches mathematical convention but differs from truncation:

- `Floor(-2.5) = -3` (toward -∞)
- Truncate(-2.5) = -2 (toward zero) — not available in Verse

Rationals can be used as parameter and return types:

<!--verse
F()<decides>:void={
-->
```verse
# Function returning rational
Half(X:int)<transacts><decides>:rational = X / 2

# Use the result
if (Result := Half[7]):
    Floor(Result) = 3   # 7/2 = 3.5, Floor gives 3
    Ceil(Result) = 4    # 7/2 = 3.5, Ceil gives 4
```
<!--verse
}
-->

Because `int` is a subtype of `rational`, you *cannot* overload based solely on these types:

<!--NoCompile-->
```verse
# Invalid: Cannot distinguish int from rational
# ProcessValue(X:int):void = {}
# ProcessValue(X:rational):void = {}  # ERROR 3532
```

The compiler sees `int` as more specific than `rational`, so the signatures would be ambiguous.

Rationals excel at resource distribution and fairness calculations:

<!--verse
F()<decides>:void={
-->
```verse
# Fair resource distribution
DistributeResources(TotalGold:int, NumPlayers:int)<decides>:int =
    if (GoldPerPlayer := TotalGold / NumPlayers):
        Floor(GoldPerPlayer)  # Each player gets whole gold pieces

# Item affordability calculation
Coins:int = 225
CoinsPerQuiver:int = 100
ArrowsPerQuiver:int = 15

if (NumberOfQuivers := Floor(Coins / CoinsPerQuiver)):
    TotalArrows:int = NumberOfQuivers * ArrowsPerQuiver
    # Player can afford 2 quivers = 30 arrows
```
<!--verse
}
-->

## Floats

The `float` type represents all non-integer numerical values. It can hold large values and precise fractions.

The float type is used for storing and handling floating point numbers, such as `1.0`, `-50.5`, and `3.14159`. A float is an IEEE 64-bit float, which means it can contain a positive or negative number that has a decimal point in the range `[-2^1024 + 1, … , 0, … , 2^1024 - 1]`, or has the value `NaN` (Not a Number).

The implementation for float differs from the IEEE standard in the following ways:

- There is only one `NaN` value.
-`NaN` is equal to itself.
- Every number is equal to itself. If two numbers are equal, then no pure code can observe the difference between them.
- `0` cannot be negative.

You can include predefined float values within your code as float literals:

<!--verse
F()<transacts><decides>:void={
-->
```verse
A:float = 1.0
B := 2.14
MaxHealth : float = 100.0

var C:float = A + B
C = 3.14              # succeeds
set C -= 3.14
C = 0.0               # succeeds
C = 0                 # compile error; 0 is not a `float` literal
```
<!--verse
}
-->

You can use the four basic math operations with floats: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

There are also combined operators for doing the basic math operations (addition, subtraction, multiplication, and division), and updating the value of a variable:

<!--verse
F()<transacts>:void={
-->
```verse
var CurrentHealth : float = 100.0
set CurrentHealth /= 2.0    # Halves the value of CurrentHealth
set CurrentHealth += 10.0   # Adds 10 to CurrentHealth
set CurrentHealth *= 1.5    # Multiplies CurrentHealth by 1.5
```
<!--verse
}
-->

To convert an `int` to a `float`, multiply it by `1.0`: `MyFloat := MyInt * 1.0`.

## Mathematical Functions

Verse provides intrinsic mathematical functions for common numerical operations. These functions are optimized by the runtime and work with both `int` and `float` types.

### Abs()

The `Abs()` function returns the absolute value of a number—its distance from zero without regard to sign:

```verse
# Signatures
Abs(X:int):int
Abs(X:float):float
```

```verse
Abs(5)    # Returns 5
Abs(-5)   # Returns 5
Abs(0)    # Returns 0
Abs(3.14) # Returns 3.14
```

Floating-point numbers have two representations of zero: positive zero (+0.0) and negative zero (-0.0). `Abs()` normalizes both to positive zero:

```verse
PositiveZero := Abs(0.0)   # +0.0
NormalizedZero := Abs(-0.0) # Also +0.0 (negative zero converted)
```

This ensures consistent behavior when zero values are used in comparisons or as map keys.

### Min and Max

The `Min()` and `Max()` functions return the minimum or maximum of two values:

```verse
# Signatures
Min(A:int, B:int):int
Min(A:float, B:float):float
Max(A:int, B:int):int
Max(A:float, B:float):float
```

**Special float values:**

```verse
# NaN propagates through comparison
Max(NaN, 5.0)   # Returns NaN
Min(NaN, 5.0)   # Returns NaN
Max(NaN, NaN)   # Returns NaN

# Infinity handling
Max(Inf, 100.0)    # Returns Inf
Min(-Inf, 100.0)   # Returns -Inf
Max(-Inf, -Inf)    # Returns -Inf
Min(Inf, Inf)      # Returns Inf
```

### Rounding

Verse provides multiple rounding functions that convert floats to integers with different rounding strategies:

```verse
# Signatures
Floor(X:float):int   # Round down
Ceil(X:float):int    # Round up
Round(X:float):int   # Round to nearest even (IEEE-754)
Int(X:float):int     # Truncate toward zero
```

Round to nearest even (ties go to even):

```verse
Round(1.5)    # Returns 2 (tie: 1.5 rounds to even 2)
Round(0.5)    # Returns 0 (tie: 0.5 rounds to even 0)
Round(2.5)    # Returns 2 (tie: 2.5 rounds to even 2)
Round(-1.5)   # Returns -2 (tie: -1.5 rounds to even -2)
Round(-0.5)   # Returns 0 (tie: -0.5 rounds to even 0)

Round(1.4)    # Returns 1 (no tie, rounds down)
Round(1.6)    # Returns 2 (no tie, rounds up)
```

The "round to nearest even" strategy (also called banker's rounding) avoids bias when rounding many tie values.

### Sqrt and Pow

```verse
# Signature
Sqrt(X:float):float

# Negative inputs return NaN
Sqrt(-1.0)    # Returns NaN

# Special values
Sqrt(Inf)     # Returns Inf
Sqrt(NaN)     # Returns NaN
```

```verse
# Signature
Pow(Base:float, Exponent:float):float

Pow(2.0, 3.0)     # Returns 8.0 (2³)
Pow(10.0, 2.0)    # Returns 100.0
Pow(4.0, 0.5)     # Returns 2.0 (square root)
Pow(2.0, -1.0)    # Returns 0.5 (reciprocal)

# Special cases
Pow(0.0, 0.0)     # Returns 1.0 (by convention)
Pow(NaN, 0.0)     # Returns 1.0 (0 exponent always 1)
Pow(1.0, NaN)     # Returns 1.0 (1 to any power is 1)
```

### Exp and Ln

```verse
Exp(X:float):float

Exp(0.0)      # Returns 1.0
Exp(1.0)      # Returns 2.718... (e)
Exp(-1.0)     # Returns 0.368... (1/e)

# Special values
Exp(-Inf)     # Returns 0.0
Exp(Inf)      # Returns Inf
Exp(NaN)      # Returns NaN
```

```verse
# Signature
Ln(X:float):float

Ln(1.0)       # Returns 0.0
Ln(2.718...)  # Returns 1.0 (ln(e) = 1)
Ln(10.0)      # Returns 2.302...

# Invalid inputs
Ln(-1.0)      # Returns NaN (negative)
Ln(0.0)       # Returns -Inf (log of zero)

# Special values
Ln(Inf)       # Returns Inf
Ln(NaN)       # Returns NaN
```

```verse
# Signature
Log(Base:float, Value:float):float

Log(10.0, 100.0)   # Returns 2.0 (log₁₀(100) = 2)
Log(2.0, 8.0)      # Returns 3.0 (log₂(8) = 3)
Log(2.0, 2.0)      # Returns 1.0 (logₙ(n) = 1)
```

### Sin, Cos, Tan

Verse provides standard trigonometric functions operating on radians:

```verse
# Signatures
Sin(Angle:float):float
Cos(Angle:float):float
Tan(Angle:float):float

# Common angles (using PiFloat constant)
Sin(0.0)              # Returns 0.0
Sin(PiFloat / 2.0)    # Returns 1.0
Sin(PiFloat)          # Returns 0.0
Sin(-PiFloat / 2.0)   # Returns -1.0

Cos(0.0)              # Returns 1.0
Cos(PiFloat / 2.0)    # Returns 0.0
Cos(PiFloat)          # Returns -1.0

Tan(0.0)              # Returns 0.0
Tan(PiFloat / 4.0)    # Returns 1.0
Tan(-PiFloat / 4.0)   # Returns -1.0

# Special values
Sin(NaN)              # Returns NaN
Sin(Inf)              # Returns NaN

# Signatures
ArcSin(X:float):float   # Returns angle in [-π/2, π/2]
ArcCos(X:float):float   # Returns angle in [0, π]
ArcTan(X:float):float   # Returns angle in [-π/2, π/2]
ArcTan(Y:float, X:float):float  # Two-argument arctangent

# Inverse relationships
ArcSin(0.0)    # Returns 0.0
ArcSin(1.0)    # Returns π/2
ArcSin(-1.0)   # Returns -π/2

ArcCos(1.0)    # Returns 0.0
ArcCos(0.0)    # Returns π/2
ArcCos(-1.0)   # Returns π

ArcTan(0.0)    # Returns 0.0
ArcTan(1.0)    # Returns π/4
ArcTan(-1.0)   # Returns -π/4

# Verify inverse relationship
Angle := PiFloat / 6.0  # 30 degrees
Sin(ArcSin(Sin(Angle))) = Sin(Angle)  # True

# ArcTan(Y, X) returns angle of point (X, Y) from origin
ArcTan(1.0, 1.0)     # Returns π/4 (45 degrees)
ArcTan(1.0, 0.0)     # Returns π/2 (90 degrees)
ArcTan(0.0, 1.0)     # Returns 0.0 (0 degrees)
ArcTan(1.0, -1.0)    # Returns 3π/4 (135 degrees)
ArcTan(-1.0, -1.0)   # Returns -3π/4 (-135 degrees)
```

### Hyperbolic Functions

Hyperbolic functions are analogs of trigonometric functions for hyperbolas:

```verse
# Signatures
Sinh(X:float):float    # Hyperbolic sine
Cosh(X:float):float    # Hyperbolic cosine
Tanh(X:float):float    # Hyperbolic tangent
ArSinh(X:float):float  # Inverse hyperbolic sine
ArCosh(X:float):float  # Inverse hyperbolic cosine
ArTanh(X:float):float  # Inverse hyperbolic tangent

Sinh(0.0)     # Returns 0.0
Sinh(1.0)     # Returns 1.175...
Cosh(0.0)     # Returns 1.0
Cosh(1.0)     # Returns 1.543...
Tanh(0.0)     # Returns 0.0
Tanh(1.0)     # Returns 0.761...

# Special values
Sinh(-Inf)    # Returns -Inf
Sinh(Inf)     # Returns Inf
Cosh(-Inf)    # Returns Inf
Cosh(Inf)     # Returns Inf
Tanh(-Inf)    # Returns -1.0
Tanh(Inf)     # Returns 1.0

ArSinh(0.0)   # Returns 0.0
ArCosh(1.0)   # Returns 0.0
ArTanh(0.0)   # Returns 0.0

# Special values
ArSinh(-Inf)  # Returns -Inf
ArSinh(Inf)   # Returns Inf
ArCosh(Inf)   # Returns Inf
ArCosh(-1.0)  # Returns NaN (domain error)
```

Hyperbolic functions are useful in physics simulations, catenary curves, and certain mathematical models.

### Mod and Quotient

For integer division with remainder, Verse provides `Mod` and `Quotient`:

```verse
# Signatures
Mod(Dividend:int, Divisor:int)<decides>:int
Quotient(Dividend:int, Divisor:int)<decides>:int
```

Both functions are failable—they fail when the divisor is zero.

```verse
# Positive operands
Mod[15, 4]      # Returns 3
Quotient[15, 4] # Returns 3
# Relationship: 15 = 3*4 + 3

# Negative dividend
Mod[-15, 4]      # Returns 1
Quotient[-15, 4] # Returns -4
# Relationship: -15 = -4*4 + 1

# Negative divisor
Mod[-1, -2]      # Returns 1
Quotient[-1, -2] # Returns 1

# Division by zero fails
if (not Mod[10, 0]):
    Print("Cannot mod by zero")
if (not Quotient[10, 0]):
    Print("Cannot divide by zero")
```

The modulo result always satisfies:

```verse
Dividend = Quotient[Dividend, Divisor] * Divisor + Mod[Dividend, Divisor]
```

The sign of the result follows specific rules:

- `Mod` result has the same sign as the divisor (Euclidean division)
- `Quotient` adjusts accordingly to maintain the identity

### Utility Functions

**Sgn - Sign function:**

```verse
# Signatures
Sgn(X:int):int
Sgn(X:float):float
```

Returns -1, 0, or 1 depending on the sign:

```verse
Sgn(10)       # Returns 1
Sgn(0)        # Returns 0
Sgn(-5)       # Returns -1

Sgn(3.14)     # Returns 1.0
Sgn(0.0)      # Returns 0.0
Sgn(-2.71)    # Returns -1.0

# Special float values
Sgn(Inf)      # Returns 1.0
Sgn(-Inf)     # Returns -1.0
Sgn(NaN)      # Returns NaN
```

**Lerp - Linear interpolation:**

```verse
# Signature
Lerp(From:float, To:float, Parameter:float):float
```

Interpolates between two values:

```verse
Lerp(0.0, 10.0, 0.0)    # Returns 0.0 (0% = From)
Lerp(0.0, 10.0, 0.5)    # Returns 5.0 (50%)
Lerp(0.0, 10.0, 1.0)    # Returns 10.0 (100% = To)
Lerp(0.0, 10.0, 2.0)    # Returns 20.0 (extrapolation)
Lerp(10.0, 20.0, 0.3)   # Returns 13.0

# Works with negative ranges
Lerp(-10.0, 10.0, 0.5)  # Returns 0.0
```

The formula is: `From + Parameter * (To - From)`

**IsFinite - Check if float is finite:**

```verse
# Method on float values
X.IsFinite():logic
```

Returns `true` if the value is not NaN, Inf, or -Inf:

```verse
(5.0).IsFinite[]      # Returns true
(0.0).IsFinite[]      # Returns true
(-100.0).IsFinite[]   # Returns true

(Inf).IsFinite[]      # Returns false
(-Inf).IsFinite[]     # Returns false
(NaN).IsFinite[]      # Returns false

# Useful for validation
SafeCalculation(X:float, Y:float)<decides>:float =
    X.IsFinite[] and Y.IsFinite[]
    Result := X / Y
    Result.IsFinite[]
    Result
```

### Mathematical Constants

Verse provides constants for common mathematical values:

**PiFloat - The constant π:**

```verse
PiFloat    # 3.14159265358979323846...
```

Use for angle calculations and circular mathematics:

```verse
# Convert degrees to radians
DegreesToRadians(Degrees:float):float =
    Degrees * PiFloat / 180.0

# Convert radians to degrees
RadiansToDegrees(Radians:float):float =
    Radians * 180.0 / PiFloat

# Circle circumference
Circumference := 2.0 * PiFloat * Radius
```

**Infinity and NaN:**

```verse
Inf     # Positive infinity
-Inf    # Negative infinity (negation of Inf)
NaN     # Not a Number
```

These special float values represent mathematical edge cases:

```verse
# Infinity represents unbounded values
1.0 / 0.0    # Produces Inf
-1.0 / 0.0   # Produces -Inf

# NaN represents undefined results
0.0 / 0.0    # Produces NaN
Sqrt(-1.0)   # Produces NaN

# Check for special values
IsSpecial(X:float):logic =
    not X.IsFinite[]

# NaN comparisons
NaN = NaN    # True (in Verse; differs from IEEE-754)
```

Remember that in Verse, `NaN = NaN` returns `true` (unlike standard IEEE-754), and there is only one `NaN` value.

## Booleans

The `logic` type represents the Boolean values `true` and `false`.

<!--verse
F()<decides>:void={
-->
```verse
A:logic = true
B := false

A = B             # fails
A?                # succeeds
B?                # fails

true?             # succeeds
false?            # fails
```
<!--verse
}
-->

The `logic` type only supports query operations and comparison operations.

Query expressions use the query operator `?` to check if a logic value is true and fail if the logic value is `false`.

For comparison operations, use the failable operator `=` to test if two logic values are the same, and `<>` to test for inequality.

Many programming languages find it idiomatic to use a type like `logic` to signal the success or failure of an operation. In Verse, we use success and failure instead for that purpose, whenever possible. The conditional only executes the `then` branch if the guard succeeds:

<!--verse
ShowTargetLockedIcon():void={}
F(TargetLocked:?int):void={
-->
```verse
 if (TargetLocked?):
    ShowTargetLockedIcon()
```
<!--verse
}
-->

To convert an expression that has the `<decides>` effect to `true` on success or `false` on failure, use
`logic{ exp }`:

<!--verse
using{ /Verse.org/Random }
F(Frequency:int)<decides>:void={
-->
```verse
GotIt := logic{GetRandomInt(0, Frequency) <> 0}   # if success
GotIt?                                            # then this succeeds
GotIt = false                                     # and this fails
not GotIt?                                        # and this fails too
```
<!--verse
}
-->

## Characters and Strings

Text is represented in terms of characters and strings.  

A `char` is a single **UTF-8 code unit** (not a full Unicode code point). A string is therefore an array of characters, written as `[]char`. For convenience, the type alias `string` is provided for `[]char`:  

<!--verse
F():void={
-->
```verse
MyName :string = "Joseph"
MyAlterEgo := "José"
```
<!--verse
}
-->

UTF-8 is used as the character encoding scheme. Each UTF-8 code unit is one byte. A Unicode code point may require between one and four code units. Code points with lower values use fewer bytes, while higher values require more.  

For example:  

- `"a"` requires one byte (`{0o61}`),  
- `"á"` requires two bytes (`{0oC3}{0oA1}`),  
- `"🐈"` (cat emoji) requires four bytes (`{0u1f408}`).  

Thus, strings are sequences of code units, not necessarily sequences of Unicode characters in the abstract sense.  

Because strings are arrays of `char`, you can index into them with `[]`. Indexing has the `<decides>` effect: it succeeds when the index is valid and fails otherwise.  

<!--verse
F(MyName:string):void={
-->
```verse
TheLetterJ := MyName[0]     # succeeds
TheLetterJ = 'J'            # succeeds
MyName[100]                 # fails
```
<!--verse
}
-->

The length of a string is the number of UTF-8 code units it contains, accessed via `.Length`. Note that this is *not the same as the number of Unicode characters*:  

<!--verse
F()<decides>:void={
-->
```verse
"José".Length = 5           # succeeds; 5 UTF-8 code units
"Jose".Length = 4           # succeeds; 4 UTF-8 code units
```
<!--verse
}
-->

Because `string` is just `[]char`, strings declared as `var` can be mutated:  

<!--verse
F()<decides>:void={
-->
```verse
var OuterSpaceFriend :string = "Glorblex"
set OuterSpaceFriend[0] = 'F'
```
<!--verse
}
-->

Strings can be concatenated using the `+` operator:  

<!--verse
F(MyName:string,MyAlterEgo:string):void={
-->
```verse
MyAttemptAtFormatting := "My name is " + MyName + " but my alter ego is " + MyAlterEgo + "."
```
<!--verse
}
-->

Verse also supports string interpolation for more readable formatting:  

<!--verse
F(MyName:string,MyAlterEgo:string):void={
-->
```verse
Formatting := "My name is {MyName} but my alter ego is {MyAlterEgo}."
```
<!--verse
}
-->

Interpolation works for any value that has a `ToString()` function in scope.  

Literal characters are written with single quotes. The type depends on whether the character falls within the ASCII range (`U+0000`–`U+007F`) or not:  

- `'e'` has type `char`,  
- `'é'` has type `char32`.  

<!--verse
F():void={
-->
```verse
A :char = 'e'                       # ok
B :char32 = 'é'                     # ok
# C :char = 'é'                     # error: type of 'é' is char32
# D :char32 = 'e'                   # error: type of 'e' is char
```
<!--verse
}
-->

Character literals can also be written using numeric escape sequences:

<!--verse
F():void={
-->
```verse
E :char = 0o65                      # ok; same as 'e'
F :char32 = 0u00E9                  # ok; same as 'é'
```
<!--verse
}
-->

- `char` represents a single UTF-8 code unit (one byte, `0oXX`).
- `char32` represents a full Unicode code point (`0uXXXXX`).

Hex notation:

- `0oXX` for `char`: two hex digits (0o00 to 0off)
- `0uXXXXX` for `char32`: up to six hex digits (0u00000 to 0u10ffff)

Unlike some languages, Verse does not allow implicit conversion between characters and integers.

**Character escape sequences** work in both character and string literals:

| Escape | Meaning | Codepoint |
|--------|---------|-----------|
| `\t` | Tab | U+0009 |
| `\n` | Newline | U+000A |
| `\r` | Carriage return | U+000D |
| `\"` | Double quote | U+0022 |
| `\'` | Single quote | U+0027 |
| `\\` | Backslash | U+005C |
| `\{` | Left brace | U+007B |
| `\}` | Right brace | U+007D |
| `\<` | Less than | U+003C |
| `\>` | Greater than | U+003E |
| `\&` | Ampersand | U+0026 |
| `\#` | Hash/pound | U+0023 |
| `\~` | Tilde | U+007E |

Examples:

<!--verse
F():void={
-->
```verse
Tab := '\t'
Newline := '\n'
Quote := '\"'
Brace := '\{'
```
<!--verse
}
-->  

Strings can be compared using the failable operators `=` (equality) and `<>` (inequality). Comparison is done by code point, and is case sensitive.  

Equality depends on exact code unit sequences, not visual appearance. Unicode allows multiple encodings for the same abstract character. For example, `"é"` may appear as the single code point `{0u00E9}`, or as the two-code-point sequence `"e"` (`{0u0065}`) plus a combining accent (`{0u0301}`). These two strings look the same, but they are not equal in Verse.  

Checking whether a player has selected the correct item:  

<!--verse
F()<transacts>:logic={
-->
```verse
ExpectedItemInternalName :string = "RedPotion"
SelectedItemInternalName :string = "BluePotion"

if (SelectedItemInternalName = ExpectedItemInternalName):
    return true 
return false
```
<!--verse
}
-->

Padding a timer with leading zeros:  

<!--verse
F()<transacts>:void={
-->
```verse
SecondsLeft :int = 30
SecondsString :string = SecondsLeft    # convert int to string

var Combined :string = "Time Remaining: "
if (SecondsString.Length > 2):
    set Combined += "99"               # clamp to maximum
else if (SecondsString.Length < 2):
    set Combined += "0{SecondsString}" # pad with zero
else:
    set Combined += SecondsString
```
<!--verse
}
-->

Certain characters have special meaning inside strings and must be escaped:

- `{` and `}` are used for interpolation and code points, so literal braces must be written as `"\{\}"`.
- All the escape sequences listed in the table above work in strings.

**String interpolation details:**

String interpolation supports complex expressions, not just simple variables:

<!--verse
F():void={
-->
```verse
# Expression interpolation
Age := 30
Message := "Next year: {Age + 1}"

# Function calls with named arguments
Distance := 5.5
Formatted := "Distance: {Format(Distance, ?Decimals:=2)}"
```
<!--verse
}
-->

**Multi-line strings:**

Strings can span multiple lines using interpolation braces for continuation:

<!--verse
F():void={
-->
```verse
LongMessage := "This is a multi-line{
}string that continues across{
}multiple lines."
```
<!--verse
}
-->

Empty interpolants `{}` are ignored, which is useful for line continuation without adding content.

**String-array equivalence:**

Since `string` is `[]char`, strings and character arrays can be compared:

<!--verse
F()<decides>:void={
-->
```verse
"abc" = array{'a', 'b', 'c'}    # Succeeds
"" = array{}                     # Succeeds - empty string equals empty array
```
<!--verse
}
-->

**Comments in strings:**

Block comments within strings are removed during parsing:

<!--verse
F():void={
-->
```verse
Text := "abc<#this comment is removed#>def"    # Same as "abcdef"
```
<!--verse
}
-->

## Type Conversions

Verse provides intrinsic functions for converting values between types, essential for formatting output, debugging, and interfacing with external systems.

### ToString()

The `ToString()` function converts values to their string representations. It's polymorphic—multiple overloads exist for different types:

```verse
# Signatures
ToString(X:int):string
ToString(X:float):string
ToString(X:char):string
ToString(X:string):string  # Identity function
```

String interpolation implicitly calls `ToString()` on embedded values:

```verse
Age := 25
Score := 98.5

# These are equivalent:
Message1 := "Age: " + ToString(Age) + ", Score: " + ToString(Score)
Message2 := "Age: {Age}, Score: {Score}"
# Both produce: "Age: 25, Score: 98.5"
```

This makes `ToString()` essential for formatting output, even when you don't call it directly.

`ToString()` only works on primitive types. User-defined classes and structs don't have automatic string conversion.

### ToDiagnostic()

The `ToDiagnostic()` function converts values to diagnostic string representations, useful for debugging and logging. While similar to `ToString()`, it may provide more detailed or implementation-specific information:

```verse
# Usage (exact signature depends on type)
DiagnosticText := ToDiagnostic(SomeValue)
```

`ToDiagnostic()` is primarily used for debugging output rather than user-facing strings. The exact format it produces may vary between VM implementations and is not guaranteed to be stable across versions.

### ObjectToJson()

The `ObjectToJson()` function serializes Verse objects to JSON (JavaScript Object Notation) format, enabling data interchange with external systems, debugging complex data structures, and persistent storage:

```verse
# Signature
ObjectToJson(Object:any):string
```

This function accepts any Verse value and produces a JSON string representation. The exact output format varies between VM implementations—Blueprint VM (BPVM) and Verse VM (VVM) may serialize the same object differently.

```verse
# Primitives
ObjectToJson(42)              # "42"
ObjectToJson(3.14)            # "3.14"
ObjectToJson("hello")         # "\"hello\""
ObjectToJson(true)            # "true"

# Arrays
Numbers := array{1, 2, 3}
ObjectToJson(Numbers)         # "[1,2,3]"

# Strings
Name := "Alice"
ObjectToJson(Name)            # "\"Alice\""
```

Classes serialize as JSON objects with field names as keys:

```verse
player := class:
    Name:string = "Alice"
    Health:int = 100
    Score:float = 98.5

Instance := player{}
Json := ObjectToJson(Instance)
# BPVM: {"Name":"Alice","Health":100,"Score":98.5}
# VVM: Similar structure with potential formatting differences
```

The function recursively serializes nested objects:

```verse
transaction := class:
    Item:string
    Amount:int

account := class:
    Owner:string
    Transactions:[]transaction

MyAccount := account{
    Owner := "Bob"
    Transactions := array{
        transaction{Item := "Sword", Amount := 50},
        transaction{Item := "Shield", Amount := 30}
    }
}

Json := ObjectToJson(MyAccount)
# Produces nested JSON with full object hierarchy
# Access nested values via paths like "Transactions/0/Item"
```

Optionals serialize as their contained value or `null`:

```verse
HasValue:?int = option{42}
NoValue:?int = false

ObjectToJson(HasValue)   # "42"
ObjectToJson(NoValue)    # "null"
```

Functions and type values have VM-specific representations:

```verse
MyFunc := (X:int):int => X + 1
MyType := type{player}

# VVM: Functions show full signature
ObjectToJson(MyFunc)  # "(int):int" (or similar)

# Type metadata varies between VMs
ObjectToJson(MyType)  # Different structural representations
```

The function handles objects containing references to themselves:

```verse
node := class:
    Value:int
    var Next:?node = false

Root := node{Value := 1}
Root.Next = option{Root}  # Self-reference

Json := ObjectToJson(Root)
# Serialization handles recursion (exact behavior VM-dependent)
```

While `ObjectToJson()` is highly flexible, it has constraints:

- Output format is not guaranteed to be identical across VM implementations
- Some types (functions, type values) serialize as metadata rather than executable representations
- The function produces JSON strings—parsing them back into Verse objects requires separate deserialization logic (not provided as a built-in intrinsic)
- Circular references are handled, but exact output depends on VM implementation

### Type-Aware JSON Serialization

The `/Verse.org/Persona` module provides `ToJson()` and `FromJson()` functions for **type-aware JSON serialization**. Unlike `ObjectToJson()`, which serializes instances, `ToJson()` generates JSON schemas describing type structures, while `FromJson()` deserializes JSON data into typed Verse values with validation.

These functions enable:

- Schema generation for external APIs
- Type-safe JSON deserialization with validation
- Integration with JSON-based configuration systems
- Dynamic type handling and registration

**Importing the module:**

```verse
using { /Verse.org/Persona }
```

**ToJson - Schema Generation:**

`ToJson[Type]` generates a JSON Schema string describing the structure of a Verse type:

```verse
player_data := struct:
    Name:string
    Score:int
    IsActive:logic

# Generate schema
Schema := ToJson[player_data]
# Result: JSON Schema describing the structure with property types
```

The generated schema follows JSON Schema conventions with properties, types, and required fields.

**FromJson - Deserialization:**

`FromJson[JsonString, Type]` deserializes JSON data into a Verse value, validating against the type structure:

```verse
player_data := struct:
    Name:string
    Score:int

# Valid JSON
JsonData := "{\"Name\":\"Alice\",\"Score\":100}"
if (Player := FromJson[JsonData, player_data]):
    # Player.Name = "Alice", Player.Score = 100

# Invalid JSON (missing required field)
BadJson := "{\"Name\":\"Bob\"}"
if (not FromJson[BadJson, player_data]):
    # Deserialization fails - returns false
```

**Supported Types:**

Different Verse types serialize with different JSON representations:

**Primitives:**

- `logic` → `{"type":"BOOLEAN"}`
- `int` → `{"type":"INTEGER"}` (with min/max ranges)
- `float` → `{"type":"NUMBER"}`
- `char` → `{"type":"INTEGER","minimum":0,"maximum":255}`

**Structures:**

```verse
point := struct:
    X:int
    Y:int

Schema := ToJson[point]
# Properties for X and Y, both required

Json := "{\"X\":10,\"Y\":20}"
Point := FromJson[Json, point]
# Point.X = 10, Point.Y = 20
```

**Classes:**

```verse
entity := class<concrete>:
    Health:int = 100
    IsAlive:logic = true

Schema := ToJson[entity]
# Properties for Health and IsAlive, not required (have defaults)

Json := "{\"Health\":75}"
Entity := FromJson[Json, entity]
# Entity.Health = 75, Entity.IsAlive = true (default)
```

**Enums:**

```verse
status := enum:
    Active
    Inactive
    Paused

Schema := ToJson[struct{State:status}]
# Property with ENUM constraint: ["Active","Inactive","Paused"]

Json := "{\"State\":\"Active\"}"
Data := FromJson[Json, struct{State:status}]
# Data.State = status.Active
```

**Optional Types:**

```verse
config := struct:
    Value:?int = option{42}

# Options serialize as any_of: false (empty) or object with value
Json1 := "{\"Value\":false}"  # Empty option
Result1 := FromJson[Json1, config]
# not Result1.Value?

Json2 := "{\"Value\":{\"\":10}}"  # Has value
Result2 := FromJson[Json2, config]
# Result2.Value? = 10
```

**Arrays and Maps:**

```verse
# Arrays
array_struct := struct:
    Items:[]int

Json := "{\"Items\":[1,2,3]}"
Data := FromJson[Json, array_struct]
# Data.Items = (1, 2, 3)

# Maps serialize as arrays of key-value pairs
map_struct := struct:
    Scores:[string]int

Json := "{\"Scores\":[{\"key\":\"Alice\",\"value\":100}]}"
Data := FromJson[Json, map_struct]
# Data.Scores = map{"Alice" => 100}
```

**Tuples:**

```verse
pair_struct := struct:
    Position:tuple(int, int)

# Tuples use numbered properties
Json := "{\"Position\":[10,20]}"
# Or: "{\"Position\":{\"0\":10,\"1\":20}}"
Data := FromJson[Json, pair_struct]
# Data.Position = (10, 20)
```

**Constrained Types:**

Refinement types include constraints in the schema:

```verse
percentage := type{X:int where 0 <= X, X <= 100}

config := struct:
    Opacity:percentage

Schema := ToJson[config]
# Opacity has {"type":"INTEGER","minimum":0,"maximum":100}

ValidJson := "{\"Opacity\":75}"
InvalidJson := "{\"Opacity\":150}"

FromJson[ValidJson, config]    # Succeeds
FromJson[InvalidJson, config]  # Fails - out of range
```

**Inheritance:**

Subclasses include properties from parent and child:

```verse
base := class<concrete>:
    ID:int = 0

derived := class<concrete>(base):
    Name:string = ""

Schema := ToJson[derived]
# Includes both ID (from base) and Name (from derived)

Json := "{\"ID\":1,\"Name\":\"Test\"}"
Obj := FromJson[Json, derived]
# Obj.ID = 1, Obj.Name = "Test"
```

**Field Qualification:**

Properties use fully-qualified names to avoid ambiguity:

```verse
# In module at /MyGame/Types
player := struct:
    Health:int

# JSON uses qualified names:
# "{\"(/MyGame/Types/player:)Health\":100}"
```

**Types That Cannot Serialize:**

These types return `false` from `ToJson` and `FromJson`:

- **Interfaces** (no concrete structure)
- **Abstract classes** (cannot instantiate)
- **Classes with `<internal>` constructors**
- **Non-concrete classes** (missing `<concrete>`)
- **Parametric types** (generic types with type parameters)
- **Recursive types** (types referencing themselves)
- **String types** (not supported)
- **Types with internal-only fields**

```verse
iface := interface:
    GetValue():int

# Cannot serialize
if (not ToJson[iface]):
    # Interfaces have no schema
```

**Multiple Type Schemas:**

You can generate schemas for multiple types at once:

```verse
type1 := struct{X:int}
type2 := struct{Y:float}

# Generate combined schema
Schema := ToJson[type1, type2]
# Schema has "0" and "1" properties for each type

# Deserialize to tuple of optionals
Json := "{\"0\":{\"X\":42},\"1\":{\"Y\":3.14}}"
Result := FromJson[Json, (type1, type2)]
# Result is tuple(?type1, ?type2)
if (First := Result[0]?):
    # First.X = 42
```

## Optionals

An optional is an immutable container that either holds a value of type `t` or nothing at all. The type is written `?t`. Optionals are useful whenever a value may or may not be present, such as when looking up a key in a map or calling a function that can fail. By making this possibility explicit in the type, Verse allows programmers to handle “no result” situations directly and consistently, instead of relying on ad hoc error codes or special values.

You can create a non-empty optional with `option{...}`, which wraps a value into an optional. For example:

<!--verse
F():void={
-->
```verse
A:?int = option{42}    # an optional containing the integer 42
```
<!--verse
}
-->

If you want to represent “no value,” you use the special constant `false`. This is how Verse spells the empty optional:

<!--verse
F()<decides>:void={
-->
```verse
var B:?int = false     # this optional has no element
B = false              # still empty
```
<!--verse
}
-->

To extract the element of an optional, you write `?` after the optional expression. This produces a `<decides>` expression that succeeds if the optional has an element and fails otherwise. For example:

<!--verse
F(A:?int)<decides>:void={
-->
```verse
S := A? + 2            # succeeds with 44 because A contains 42
```
<!--verse
}
-->

If `A` had been `false`, then the attempt to use `A?` would fail and so would the whole computation. A failing case makes this clearer:

<!--NoCompile-->
```verse
T := B? + 1            # fails, because B is false and has no element
```

This shows how Verse integrates optionals tightly with the effect system: the presence or absence of a value can cause an entire computation to succeed or fail.

The `option{...}` form also works in the opposite direction. When you have a computation with the `<decides>` effect, wrapping it in `option{...}` converts it to an optional. On success you get a non-empty optional; on failure you get `false`:

<!--NoCompile-->
```verse
MaybeAFloat := option{GetAFloatOrFail[]}
```

This symmetry is important. The `?` operator unwraps an optional into a `<decides>` expression, while `option{...}` wraps a `<decides>` expression into an optional. Together they provide a smooth bridge between computations that may fail and values that may be absent.

Although an optional value itself is immutable, you can keep one in a variable and change which optional the variable points to. The keyword `set` is used for this:

<!--verse
F()<decides>:void={
-->
```verse
var C:?int = false
set C = option{2}      # C now refers to an optional containing 2
C? = 2                 # succeeds, since C is not empty
```
<!--verse
}
-->

This ability is useful whenever you want to track success or failure over time, such as gradually computing a result and updating the variable only when you succeed.

A common use case is searching for something that may or may not be there. Imagine a function `Find` that looks through an array of integers and returns the index of the element you want. If the element exists, the function returns `option{index}`; if not, it returns `false`. The caller can then safely decide what to do:

<!--verse
Find(N:[]int, X:int):?int =
    for {I := 0..N.Length} do
        if (N[I] = X) then return option{I}
    return false

F()<decides>:void=
    var Numbers:[]int = array{10, 20, 30}
    Idx:?int = Find[Numbers, 20]    # succeeds with option{1}
    Y := Idx?                       # succeeds with 1
<#
-->
```verse
var Numbers:[]int = array{10, 20, 30}

Find[N:[]int, X:int]:?int =
    for {I := 0..N.Length} do
        if N[I] = X then return option{I}
    return false

Idx:?int = Find[Numbers, 20]    # succeeds with option{1}
Y := Idx?                       # succeeds with 1
```
<!--verse
#>
-->

Here the optional signals the possibility of failure directly in the type. The `?` operator makes it easy to use the result in an expression, while `option{...}` allows you to turn conditional computations back into optionals. The effect is that the idea of “maybe a value, maybe not” becomes a first-class part of the language, rather than an afterthought, and programmers are encouraged to handle the absence of values in a disciplined way.

## Tuple

A tuple is a container that groups two or more values. Unlike arrays, which can only contain elements of one type, tuples allow you to combine values of mixed types and treat them as a unit. The elements of a tuple appear in the order in which you list them, and you access them by their position, called the index. Because the number of elements is always known at compile time, a tuple is both simple to create and safe to use.

The term *tuple* is a back formation from *quadruple*, *quintuple*, *sextuple*, and so on. Conceptually, a tuple is like an unnamed data structure with ordered fields, or like a fixed-size array where each element may have a different type.

A tuple literal is written by enclosing a comma-separated list of expressions in parentheses. For example:

<!--NoCompile-->
```verse
(1, 2, 3)
```

The order of elements matters, so `(3, 2, 1)` is a completely different value. Since tuples allow mixed types, you might write:

<!--NoCompile-->
```verse
(1, 2.0, "three")
```

Tuples can also nest inside each other:

<!--verse
X:tuple(int,tuple(int,float,string),string)=
-->
```verse
(1, (10, 20.0, "thirty"), "three")
```

Tuples are useful when you want to return multiple values from a function or when you want a lightweight grouping of values without the overhead of defining a struct or class. The type of a tuple is written with the `tuple` keyword followed by the types of the elements, but in most cases it can be inferred. For instance, you can write `MyTuple : tuple(int, float, string) = (1, 2.0, "three")`, or simply `MyTuple := (1, 2.0, "three")` and let the compiler deduce the type.

The elements of a tuple are accessed using a zero-based index operator written with parentheses. If `MyTuple := (1, 2.0, "three")`, then `MyTuple(0)` is the integer `1`, `MyTuple(1)` is the float `2.0`, and `MyTuple(2)` is the string `"three"`. Because the compiler knows the number of elements in every tuple, tuple indexing cannot fail: any attempt to use an out-of-bounds index results in a compile-time error.

Another feature of tuples is *expansion*. When a tuple is passed to a function as a single argument, its elements are automatically expanded as if the function had been called with each element separately. For example:

```verse
F(Arg1:int, Arg2:string):void =
    Print("{Arg1}, {Arg2}")

G():void =
    MyTuple := (1, "two")
    F(MyTuple)   # expands to F(1, "two")
```

Tuples also play a role in structured concurrency. The `sync` expression produces a tuple of results, allowing several computations that unfold over time to be evaluated simultaneously. In this way, tuples provide not only a convenient grouping mechanism but also a foundation for composing concurrent computations.

## Arrays

An array is an immutable container that holds zero or more values of the same type `t`. The elements of an array are ordered, and each can be accessed by a zero-based index. Arrays are written with square brackets in their type, for example `[]int` or `[]float`, and are created with the `array{...}` literal form. For instance, `A : []int = array{}` creates an empty array, while `B : []int = array{1, 2, 3}` creates an array of three integers. Accessing elements by index is a failable operation: `B[0]` succeeds with the value `1`, while `B[10]` fails because the index is out of bounds.

Arrays can be concatenated with the `+` operator, and when declared as `var` they can be extended with the shorthand operator `+=`. For example, `var C:[]int= B + array{4}` gives `C` the value `array{1,2,3,4}`, and `set C += array{5}` updates it to `array{1,2,3,4,5}`. The length of an array is available through the `.Length` member, so `C.Length` here would be `5`. Elements are always stored in the order they are inserted, and indexing starts at `0`. Thus `array{10,20,30}[0]` is `10`, and the last valid index of any array is always one less than its length.

Although arrays themselves are immutable, variables declared with `var` can be reassigned to new arrays, or can appear to have their elements changed. For example, `var D:[]int = array{1,2,3}` allows the update `set D[0] = 3`, after which `D` will hold `array{3,2,3}`. What actually happens is that a brand new array is created under the hood, with the specified element updated. In effect, `set D[0] = 3` is compiled into `set D = array{3,D[1],D[2]}`. The old array continues to exist if another variable was referencing it, which means that if `A` and `B` both start as `array{1}` and we update `A[0]`, then `A` and `B` will diverge: `A[0]` is now `2` while `B[0]` is still `1`.

Arrays are useful whenever you want to store multiple values of the same type, such as a list of players in a game: `Players:[]player = array{Player1,Player2}`. Access is by index, for example `Players[0]` is the first player. Since indexing is failable, it is often combined with `if` expressions or iteration. For instance, the following code safely prints out every element of an array:  

<!--verse
using { /Verse.org/VerseCLR }
F():void={
-->
```verse
ExampleArray : []int = array{10, 20, 30, 40, 50}
for (Index := 0..ExampleArray.Length - 1):
    if (Element := ExampleArray[Index]):
        Print("{Element} in ExampleArray at index {Index}")
```
<!--verse
}
-->

which produces  

```
10 in ExampleArray at index 0
20 in ExampleArray at index 1
30 in ExampleArray at index 2
40 in ExampleArray at index 3
50 in ExampleArray at index 4
```

Because arrays are values, “changing” them always means replacing the old array with a new one. With `var` this feels natural, since variables can be reassigned. For example, you can concatenate arrays and then update an element:  

<!--verse
F():void={
-->
```verse
Array1 : []int = array{10, 11, 12}
var Array2 : []int = array{20, 21, 22}
set Array2 = Array1 + Array2 + array{30, 31}
if (set Array2[1] = 77) {}
```
<!--verse
}
-->

After this code runs, iterating through `Array2` prints `10, 77, 12, 20, 21, 22, 30, 31`.

Arrays can also be nested to form multi-dimensional structures, similar to rows and columns of a table. For example, the following creates a two-dimensional 4×3 array of integers:

<!--verse
F():void={
-->
```verse
var Counter : int = 0
Example : [][]int =
    for (Row := 0..3):
        for (Column := 0..2):
            set Counter += 1
```
<!--verse
}
-->

This array can be visualized as  

```
Row 0:  1  2  3
Row 1:  4  5  6
Row 2:  7  8  9
Row 3: 10 11 12
```

and is accessed with two indices: `Example[0][0]` is `1`, `Example[0][1]` is `2`, and `Example[1][0]` is `4`. You can loop through all rows and columns with nested iteration. Arrays in Verse are not restricted to rectangular shapes: each row can have a different length, producing a jagged structure. For example,  

<!--verse
F():void={
-->
```verse
Example : [][]int =
    for (Row := 0..3):
        for (Column := 0..Row):
            Row * Column
```
<!--verse
}
-->

produces a triangular array with rows of increasing length: row 0 has none, row 1 has a single `0`, row 2 has `0, 2, 4`, and row 3 has `0, 3, 6, 9`.

**Nested arrays with custom classes:**

Multi-dimensional arrays work with any type, including custom classes. This enables powerful data structures for game boards, spatial grids, and structured data:

<!--verse
point:=class{X:int,Y:int}
tile_class:=class{Position:tuple(int,int)}
F():void={
-->
```verse
# Define a point class
point := class:
    X:int
    Y:int

# Create a grid of point objects
Grid:[][]point =
    for (Row := 0..2):
        for (Col := 0..4):
            point{X := Row, Y := Col}

# Access individual points
if (TopLeft := Grid[0][0]):
    Print("Top-left point: ({TopLeft.X}, {TopLeft.Y})")
    # Prints: "Top-left point: (0, 0)"

if (BottomRight := Grid[2][4]):
    Print("Bottom-right point: ({BottomRight.X}, {BottomRight.Y})")
    # Prints: "Bottom-right point: (2, 4)"
```
<!--verse
}
-->

**Using nested arrays as class fields:**

Nested arrays with complex initialization work naturally as class field defaults:

<!--verse
point:=class{X:int,Y:int}
tile_class:=class{Position:tuple(int,int)}
-->
```verse
# Game board with tile grid
tile_class := class:
    Position:tuple(int, int)
    var IsOccupied:logic = false

game_board := class:
    # Initialize 10×10 grid of tiles
    Tiles:[][]tile_class =
        for (Y := 0..9):
            for (X := 0..9):
                tile_class{Position := (X, Y)}

    # Get tile at specific position
    GetTile(X:int, Y:int)<decides>:tile_class =
        Row := Tiles[Y]?
        Row[X]?

# Create board instance
Board := game_board{}

# Access specific tile
if (CenterTile := Board.GetTile[5, 5]):
    set CenterTile.IsOccupied = true
```
<!--verse
-->

### Array Type Inference

When you create an empty array with `array{}`, Verse infers the element type from the variable's type annotation:

```verse
IntArray : []int = array{}       # Empty array of integers
FloatArray : []float = array{}   # Empty array of floats
```

Without a type annotation, the compiler cannot determine what type of array you want, so you must either provide the type explicitly or include at least one element that establishes the type.

### Type Compatibility and Subtyping

Arrays determine their element type from the common supertype of all elements. When you create an array with values of different but related types, Verse finds the most specific type that encompasses all elements:

```verse
class1 := class {}
class2 := class(class1) {}
class3 := class(class1) {}

# Array element type is class1 (common supertype)
MixedArray : []class1 = array{class2{}, class3{}}
```

This applies to any type hierarchy, including interfaces. If you mix completely unrelated types, the element type becomes `any`:

```verse
# Array of any - different types with no common supertype
DisjointArray : []any = array{42, 13.37, true}
```

### Tuples and Arrays

Verse provides automatic conversion between tuples and arrays in specific contexts, enabling flexible function calls while maintaining type safety. This conversion is **one-way**: tuples can become arrays, but arrays cannot become tuples.

#### Direct Assignment

Tuples can be directly assigned to array variables when all tuple elements are compatible with the array's element type:

```verse
# Homogeneous tuple to array
X:tuple(int, int) = (1, 2)
Y:[]int = X            # Valid - both elements are int
Y[1] = 2               # Can use as normal array

# Longer tuples work too
Numbers:tuple(int, int, int, int) = (1, 2, 3, 4)
NumberArray:[]int = Numbers
NumberArray.Length = 4
```

This conversion creates an array containing all the tuple's elements in order.

#### Calls with Multiple Arguments

When a function has a single array parameter, you can call it with multiple arguments, which automatically form an array:

```verse
ProcessNumbers(Numbers:[]int):int = Numbers.Length

# All these are equivalent:
ProcessNumbers[1, 2, 3]           # Multiple args → array
ProcessNumbers[(1, 2, 3)]         # Tuple literal → array
Values := (1, 2, 3)
ProcessNumbers[Values]             # Tuple variable → array
```

This "variadic-like" syntax provides convenience while keeping the function signature simple:

```verse
Sum(Numbers:[]int):int =
    var Total:int = 0
    for (N : Numbers):
        set Total += N
    Total

# All these work:
Sum[1, 2, 3, 4]                   # Returns 10
Sum[(5, 6)]                        # Returns 11
Values := (10, 20, 30)
Sum[Values]                        # Returns 60
```

#### Type Safety Rules

The conversion only succeeds when **all tuple elements are compatible** with the array's element type:

**Valid conversions:**

```verse
# Homogeneous tuple - all int
F(X:[]int):int = X.Length
F[1, 2, 3]                        # Valid

# Subtype compatibility
entity := class:
    ID:int

player := class(entity):
    Name:string

ProcessEntities(E:[]entity):int = E.Length

P := player{ID := 1, Name := "Alice"}
E := entity{ID := 2}
ProcessEntities[P, E]             # Valid - player is subtype of entity
```

**Invalid conversions (heterogeneous types):**

```verse
# Mixed int and float
F(X:[]int):int = X.Length
Values := (1, 2.0)
F[Values]                         # ERROR 3509 - 2.0 is float, not int

# Mixed types even if all numeric
G(X:[]float):float = X[0]
G[(1, 2.0)]                       # ERROR 3509 - 1 is int, not float
```

#### Universal Conversion with `[]any`

Functions taking `[]any` accept **any tuple**, regardless of element types:

```verse
GetLength(Items:[]any):int = Items.Length

# All valid - any tuple works
GetLength[1, 2.0]                 # Mixed types OK
GetLength["a", 42, true]          # Different types OK
GetLength[(1, 2.0, "hello")]      # Explicit tuple OK
```

This enables generic functions that work with heterogeneous data.

#### Common Supertype Conversion

When tuple elements share a common supertype (via inheritance or interface), they convert to an array of that supertype:

```verse
interface1 := interface:
    GetID():int

class1 := class(interface1):
    GetID<override>():int = 1

class2 := class(interface1):
    GetID<override>():int = 2

ProcessInterfaces(Items:[]interface1):int = Items.Length

X:class1 = class1{}
Y:class2 = class2{}

# Valid - both classes implement interface1
ProcessInterfaces[X, Y]           # Returns 2
```

The compiler finds the most specific common supertype and uses it for the array element type.

#### Nested Arrays and Optional Arrays

Tuple-to-array conversion works with nested structures:

**Nested arrays:**

```verse
ProcessMatrix(Matrix:[][]int):int = Matrix.Length

# Nested tuples → nested arrays
Matrix := ((1, 2), (3, 4))
ProcessMatrix[Matrix]             # Valid

# Or with explicit nesting
ProcessMatrix[((1, 2), (3, 4))]   # Valid
```

**Optional arrays:**

```verse
ProcessOptional(Items:?[]int)<decides>:int = Items?[0]

# Optional tuple → optional array
Values := option{(1, 2)}
ProcessOptional[Values]           # Valid
```

**Tuples containing arrays:**

```verse
ProcessComplex(Data:tuple([]int, int)):int = Data(0).Length

# First element of tuple becomes array
ProcessComplex[((1, 2), 3)]       # Valid - (1,2) becomes []int
```

#### Restrictions: Array to Tuple Conversion

The conversion is **one-way only**. Arrays cannot convert to tuples:

**Invalid: Array cannot assign to tuple:**

```verse
Array:[]int = array{1, 2}
Tuple:tuple(int, int) = Array     # ERROR 3509
```

**Invalid: Array cannot spread into parameters:**

```verse
F(X:int, Y:int):int = X + Y

Values:[]int = array{1, 2}
F(Values)                         # ERROR 3509 - cannot spread array
```

**Why this restriction exists:** Arrays have dynamic length, while tuples have fixed length known at compile time. Converting arrays to tuples would require runtime length checks and could break type safety.

**Workaround using indexing:**

```verse
F(X:int, Y:int):int = X + Y

Values:[]int = array{1, 2}
if (X := Values[0], Y := Values[1]):
    F(X, Y)                       # Valid - explicit indexing
```

#### Type Compatibility Requirements

For tuple-to-array conversion to succeed, the array element type must be a **supertype of all tuple element types**:

```verse
# Valid: int is supertype of int
F(X:[]int):void = {}
F[1, 2, 3]                        # All elements are int

# Invalid: int is not supertype of float
F(X:[]int):void = {}
F[1.0, 2.0]                       # ERROR 3509 - float ≠ int

# Valid: any is supertype of everything
G(X:[]any):void = {}
G[1, 2.0, "hello"]                # All types subtype of any

# Valid: Common interface
interface1 := interface:
class1 := class(interface1):
class2 := class(interface1):

H(X:[]interface1):void = {}
H[class1{}, class2{}]             # Both implement interface1
```

### Array Slicing

Arrays support slicing operations through the `.Slice` method, which extracts a contiguous portion of an array. Slicing is a failable operation—it succeeds only when the indices are valid.

The two-parameter form `Array.Slice[Start, End]` returns elements from index `Start` up to but not including index `End`:

```verse
Numbers : []int = array{10, 20, 30, 40, 50}
if (Slice := Numbers.Slice[1, 4]):
    # Slice is array{20, 30, 40}
```

The one-parameter form `Array.Slice[Start]` returns all elements from `Start` to the end:

```verse
if (Slice := Numbers.Slice[2]):
    # Slice is array{30, 40, 50}
```

Slicing fails if indices are negative, out of bounds, or if `Start` is greater than `End`. Creating an empty slice is valid when `Start` equals `End`:

```verse
Numbers.Slice[2, 2]  # Succeeds with array{}
Numbers.Slice[2, 1]  # Fails - Start > End
Numbers.Slice[-1, 2] # Fails - negative index
Numbers.Slice[0, 10] # Fails - End beyond array length
```

Slicing also works on strings and character tuples, returning a string:

```verse
"hello".Slice[1, 4] = "ell"
```

### Array Methods

Arrays provide intrinsic methods for searching, removing, and replacing elements. These operations create new arrays rather than modifying existing ones, maintaining Verse's immutability guarantees.

#### Find

The `Find()` method searches for the first occurrence of an element and returns its index, or `false` if not found:

```verse
# Signature
Array.Find[Element:t]<decides>:int  # Returns ?int
```

```verse
Numbers := array{1, 2, 3, 1, 2, 3}

if (Index := Numbers.Find[2]):
    # Index is 1 (first occurrence)
    Print("Found at index {Index}")

if (not Numbers.Find[0]):
    # Element not in array
    Print("Not found")
```

`Find()` returns an optional (`?int`), enabling safe handling of missing elements without exceptions or special sentinel values.

#### Removing Elements

**RemoveFirstElement()** - Remove first occurrence:

```verse
# Signature
Array.RemoveFirstElement[Element:t]<decides>:[]t  # Returns ?[]t
```

```verse
Numbers := array{1, 2, 3, 1, 2, 3}

if (Updated := Numbers.RemoveFirstElement[2]):
    # Updated is array{1, 3, 1, 2, 3}
    Print("Removed first 2")

if (not Numbers.RemoveFirstElement[0]):
    # Element not found - returns false
    Print("Element not in array")
```

**RemoveAllElements()** - Remove all occurrences:

```verse
# Signature
Array.RemoveAllElements[Element:t]:[]t
```

```verse
Numbers := array{1, 2, 3, 1, 2, 3}
Updated := Numbers.RemoveAllElements[2]
# Updated is array{1, 3, 1, 3}

# Returns unchanged array if element not found
Same := Numbers.RemoveAllElements[0]
# Same is array{1, 2, 3, 1, 2, 3}
```

**Remove() by index** - Remove element at specific position:

```verse
# Signature
Array.Remove[Index:int]<decides>:[]t  # Returns ?[]t
```

```verse
Numbers := array{10, 20, 30, 40}

if (Updated := Numbers.Remove[1]):
    # Updated is array{10, 30, 40}

if (not Numbers.Remove[-1]):
    # Negative index fails

if (not Numbers.Remove[10]):
    # Out of bounds fails
```

#### Replacing Elements

**ReplaceFirstElement()** - Replace first occurrence:

```verse
# Signature
Array.ReplaceFirstElement[OldValue:t, NewValue:t]<decides>:[]t  # Returns ?[]t
```

```verse
Numbers := array{1, 2, 3, 1, 2, 3}

if (Updated := Numbers.ReplaceFirstElement[2, 99]):
    # Updated is array{1, 99, 3, 1, 2, 3}

if (not Numbers.ReplaceFirstElement[0, 99]):
    # Element not found - returns false
```

**ReplaceAllElements()** - Replace all occurrences:

```verse
# Signature
Array.ReplaceAllElements[OldValue:t, NewValue:t]:[]t
```

```verse
Numbers := array{1, 2, 3, 1, 2, 3}
Updated := Numbers.ReplaceAllElements[2, 99]
# Updated is array{1, 99, 3, 1, 99, 3}

# Returns unchanged array if element not found
Same := Numbers.ReplaceAllElements[0, 99]
# Same is array{1, 2, 3, 1, 2, 3}
```

**ReplaceElement()** - Replace at specific index:

```verse
# Signature
Array.ReplaceElement[Index:int, NewValue:t]<decides>:[]t  # Returns ?[]t
```

```verse
Numbers := array{10, 20, 30, 40}

if (Updated := Numbers.ReplaceElement[1, 99]):
    # Updated is array{10, 99, 30, 40}

if (not Numbers.ReplaceElement[-1, 99]):
    # Negative index fails

if (not Numbers.ReplaceElement[10, 99]):
    # Out of bounds fails
```

**ReplaceAll()** - Pattern-based replacement:

```verse
# Signature
Array.ReplaceAll[Pattern:[]t, Replacement:[]t]:[]t
```

```verse
Numbers := array{1, 2, 3, 4, 2, 3, 5}
Pattern := array{2, 3}
Replacement := array{99}
Updated := Numbers.ReplaceAll[Pattern, Replacement]
# Updated is array{1, 99, 4, 99, 5}

# Works with different length patterns
Numbers2 := array{1, 2, 2, 1, 2, 2, 1}
Updated2 := Numbers2.ReplaceAll[array{2, 2}, array{9, 9, 9}]
# Updated2 is array{1, 9, 9, 9, 1, 9, 9, 9, 1}
```

`ReplaceAll()` finds contiguous subsequences matching `Pattern` and replaces each with `Replacement`. The replacement can be any length, including empty.

#### Inserting Elements

**Insert()** - Insert element at specific position:

```verse
# Signature
Array.Insert[Index:int, Element:t]<decides>:[]t  # Returns ?[]t
```

```verse
Numbers := array{10, 20, 40}

if (Updated := Numbers.Insert[2, 30]):
    # Updated is array{10, 20, 30, 40}
    # Inserted at index 2, existing elements shift right

# Can insert at start
if (Updated2 := Numbers.Insert[0, 5]):
    # Updated2 is array{5, 10, 20, 40}

# Can insert at end (index = Length is valid)
if (Updated3 := Numbers.Insert[Numbers.Length, 50]):
    # Updated3 is array{10, 20, 40, 50}

# Out of bounds fails
if (not Numbers.Insert[-1, 5]):
    # Negative index fails

if (not Numbers.Insert[Numbers.Length + 1, 5]):
    # Beyond Length fails
```

#### Arrays Concatenate

The `Concatenate()` function is a variadic intrinsic that combines any number of arrays into one:

```verse
# Signature
Concatenate(Arrays:[]t...):[]t
```

Unlike the `+` operator which joins two arrays, `Concatenate()` accepts zero or more arrays:

```verse
# Empty call returns empty array
Empty := Concatenate()  # array{}

# Single array returns that array
Single := Concatenate(array{1, 2, 3})  # array{1, 2, 3}

# Two arrays
TwoArrays := Concatenate(array{1, 2}, array{3, 4})  # array{1, 2, 3, 4}

# Multiple arrays
Many := Concatenate(array{1}, array{2, 3}, array{4}, array{5, 6})
# Many is array{1, 2, 3, 4, 5, 6}
```

**Empty arrays are handled seamlessly:**

```verse
# Empty arrays contribute nothing
Result1 := Concatenate(array{1, 2}, array{}, array{3})  # array{1, 2, 3}
Result2 := Concatenate(array{}, array{}, array{})       # array{}

# Can concatenate many empty arrays
EmptyResult := Concatenate(for (I := 0..100): array{})  # array{}
```

**Variadic array input:**

`Concatenate()` shines when combining arrays generated dynamically:

```verse
# Build array of arrays, then flatten
Chunks := for (I := 0..5): array{I * 10, I * 10 + 1, I * 10 + 2}
# Chunks is array{array{0,1,2}, array{10,11,12}, array{20,21,22}, ...}

Flattened := Concatenate(Chunks)
# Flattened is array{0, 1, 2, 10, 11, 12, 20, 21, 22, ...}
```

**Comparison with `+` operator:**

```verse
# Using + operator (binary)
A1 := array{1, 2}
A2 := array{3, 4}
A3 := array{5, 6}
Result1 := A1 + A2 + A3  # Works but requires multiple operations

# Using Concatenate (variadic)
Result2 := Concatenate(A1, A2, A3)  # Single operation

# Result1 = Result2 = array{1, 2, 3, 4, 5, 6}
```

**Type homogeneity:**

All arrays must have the same element type:

```verse
# Valid: All arrays are []int
Numbers := Concatenate(array{1, 2}, array{3}, array{4, 5})

# Invalid: Cannot mix types
# Mixed := Concatenate(array{1, 2}, array{"three"})  # ERROR
```

**String concatenation:**

`Concatenate()` also works on strings, joining multiple strings into one:

```verse
# String concatenation
Text := Concatenate("Hello", " ", "World", "!")  # "Hello World!"

# Empty strings
WithEmpties := Concatenate("Start", "", "End")  # "StartEnd"

# Single string
OnlyOne := Concatenate("Alone")  # "Alone"
```

The variadic nature makes `Concatenate()` particularly useful when the number of arrays/strings isn't known at compile time or when flattening nested structures.

Arrays in Verse are thus immutable values with predictable behavior, but through `var` they offer the convenience of mutable variables. They can be concatenated, iterated, sliced, searched, and manipulated, making them one of the most flexible and fundamental data structures in the language.

## Maps

Maps are one of the core container types, alongside arrays and optionals. If arrays are ordered sequences indexed by integers, and optionals are the smallest container of all, holding either zero or one value, then Maps generalize both ideas: like arrays, they provide efficient lookup, but instead of being limited to integer indices, they allow any *comparable* type as a key. You can think of a map as an array indexed by arbitrary keys, or as a larger optional that can hold many key–value associations at once.

A map is an immutable associative container that stores zero or more key–value pairs of type `[k]v`, written as `(Key:k, Value:v)`. Maps are the standard way to associate values with other values: you supply a key, and the map returns the value associated with it.

Maps are useful whenever you want to store data that is naturally indexed by something other than an integer position. For example, you might want to store the weights of different objects keyed by their names:  

<!--verse
F():void={
-->
```verse
Empty := map{}

var Weights:[string]float = map{
    "ant" => 0.0001,
    "elephant" => 500.0,
    "galaxy" => 500000000000.0
}
```
<!--verse
}
-->

Looking up a value in a map uses square brackets. The expression succeeds if the key is present and fails if it is not. Lookups are designed to be fast, with amortized *O(1)* time complexity:  

<!--verse
F(Weights:[string]float)<decides>:void={
-->
```verse
0.00001 < Weights["ant"]    # succeeds, since "ant" is a key
Weights["car"]              # fails, since "car" is not a key
```
<!--verse
}
-->

If you want to update a map stored in a variable, you use `set`. This works both for adding a new key–value pair and for changing the value of an existing key. If you try to modify a key that is not present, the operation fails:  

<!--verse
F()<decides><transacts>:void={
-->
```verse
var Friendliness:[string]int = map{"peach" => 1000}

set Friendliness["pelican"] = 17     # add a new key
set Friendliness["peach"] += 2000    # update an existing key
set Friendliness["tomato"] += 1000   # fails; "tomato" is not in the map
```
<!--verse
}
-->

Every map also carries its size, accessible as the `Length` field:  

<!--verse
F(Friendliness:[string]int)<decides>:void={
-->
```verse
Friendliness.Length = 2              # the map has 2 entries
```
<!--verse
}
-->

When constructing a map with duplicate keys, only the last value is kept. This is because a map enforces uniqueness of keys, so earlier entries are silently overwritten:  

<!--verse
F():void={
-->
```verse
WordCount:[string]int = map{
    "apple" => 0,
    "apple" => 1,
    "apple" => 2
}
# WordCount contains only {"apple" => 2}
```
<!--verse
}
-->

Maps can also be iterated over, letting you traverse all key–value pairs exactly in the order they were inserted:  

<!--verse
using { /Verse.org/VerseCLR }
F():void={
-->
```verse
ExampleMap:[string]string = map{
    "a" => "apple",
    "b" => "bear",
    "c" => "candy"
}

for (Key -> Value : ExampleMap):
    Print("{Value} in ExampleMap at key {Key}")
```
<!--verse
}
-->

This produces:  

- “apple in ExampleMap at key a”  
- “bear in ExampleMap at key b”  
- “candy in ExampleMap at key c”  

Sometimes you want to remove an entry from a map. Since maps are immutable, “removing” means creating a new map that excludes the given key. For example, here is a function that removes an element from a `[string]int` map:  

```verse
RemoveKeyFromMap(TheMap:[string]int, ToRemove:string):[string]int =
    var NewMap:[string]int = map{}
    for (Key -> Value : TheMap, Key <> ToRemove):
        set NewMap = ConcatenateMaps(NewMap, map{Key => Value})
    return NewMap
```

The key type of a map must belong to the class `comparable`, which guarantees that two keys can be checked for equality. All basic scalar types such as `int`, `float`, `rational`, `logic`, `char`, and `char32` are comparable, and so are compound types like arrays, maps, tuples, and `struct`s whose components are comparable. Classes and interfaces cannot be used as keys, since their instances do not provide a built-in notion of equality.

### Comparable Key Types

Not all types can be used as map keys. A type must be comparable—meaning values of that type can be checked for equality. Here's a comprehensive guide to what can and cannot be used as map keys:

**Types that can be used as map keys:**

- `logic` - boolean values
- `int`, `nat`, `float`, `rational` - numeric types
- `char`, `char32` - character types
- `string` - text
- Enumerations - custom enum types
- Classes marked with `<unique>` - unique classes only
- `?t` where `t` is comparable - optionals of comparable types
- `[]t` where `t` is comparable - arrays of comparable elements
- `[k]v` where `k` and `v` are comparable - maps as keys
- `tuple(t0, t1, ...)` where all elements are comparable - tuples of comparable types
- `struct` types where all fields are comparable

**Types that cannot be used as map keys:**

- `false` - the empty type
- `type` - type values themselves
- Function types like `t -> u`
- `subtype(t)` - subtype expressions
- `^t` - weak references
- Regular classes (without `<unique>`)
- Interfaces

Attempting to use a non-comparable type as a key results in a compile-time error.

### Type Inference and Supertypes

Like arrays, maps infer their key and value types from the common supertype of all keys and values. When you create a map with mixed but related types, Verse finds the most specific types that encompass all keys and all values:

```verse
class1 := class<unique> {}
class2 := class<unique>(class1) {}
class3 := class<unique>(class1) {}

Instance2 := class2{}
Instance3 := class3{}

# Key type is class1 (common supertype of class2 and class3)
# Value type remains int
MixedKeyMap : [class1]int = map{Instance2 => 1, Instance3 => 2}
```

For value types, if you mix unrelated types, the value type becomes `any`:

```verse
# Value type is any - unrelated value types
MixedValueMap : [string]any = map{"int" => 42, "float" => 3.14}
```

### Map Ordering and Equality

Maps preserve insertion order, which is significant for both iteration and equality checks. When you insert entries into a map, they maintain the order of insertion. Two maps are equal only if they contain the same key–value pairs **in the same order**:

```verse
var Scores:[string]int = map{}
set Scores["Alice"] = 100
set Scores["Bob"] = 90
set Scores["Carol"] = 95

# This map equals Scores
Map1 := map{"Alice" => 100, "Bob" => 90, "Carol" => 95}

# This map does NOT equal Scores - different order
Map2 := map{"Bob" => 90, "Alice" => 100, "Carol" => 95}
```

When a map literal contains duplicate keys, the last value overwrites earlier values, but the key's position remains from its **first** occurrence:

```verse
Map := map{0 => "zero", 1 => "one", 0 => "ZERO", 2 => "two"}
# Equivalent to map{0 => "ZERO", 1 => "one", 2 => "two"}
# The key 0 stays in its original position
```

Iteration over the map will visit entries in their preserved insertion order.

### Empty Maps and Type Inference

Empty maps can infer their key and value types from context, similar to arrays:

```verse
StringToInt : [string]int = map{}  # Empty map with inferred types

var Scores : [string]int = map{}
set Scores = ConcatenateMaps(Scores, map{"Alice" => 100})
```

Without type context, you may need to provide explicit type annotations.

### Map Variance

Maps exhibit different variance behavior for keys and values. A map type `[K1]V1` is a subtype of `[K2]V2` when:
- **Keys are contravariant**: `K2` is a subtype of `K1` (more general keys → more specific keys)
- **Values are covariant**: `V1` is a subtype of `V2` (more specific values → more general values)

This means a map that accepts more general keys and returns more specific values can be used where a map with more specific keys and more general values is expected:

```verse
class1 := class<unique> {}
class2 := class<unique>(class1) {}  # class2 is a subtype of class1

# Map with general keys, specific values: [class1]class2
GeneralKeyMap : [class1]class2 = map{class1{} => class2{}}

# Can be assigned to map with specific keys, general values: [class2]class1
# This works because:
# - Keys: class2 <: class1 (contravariant - we can look up with more specific keys)
# - Values: class2 <: class1 (covariant - we get back more specific values)
SpecificKeyMap : [class2]class1 = GeneralKeyMap
```

The contravariance in keys reflects how maps are used: if a map can handle lookups with general keys (like `class1`), it can certainly handle lookups with more specific keys (like `class2`). The covariance in values means getting back more specific values is always safe when expecting general ones.

When modifying a mutable map through `set`, you can only insert keys and values that match the map's declared types:

```verse
class1 := class<unique> {}
class2 := class<unique>(class1) {}

var Map : [class2]int = map{}
Key2 : class2 = class2{}
Key1 : class1 = Key2

set Map[Key2] = 1      # Succeeds - exact type match
# set Map[Key1] = 2    # ERROR - cannot use supertype as key
```

### Floating-Point Keys

When using `float` values as keys, be aware of special cases in floating-point equality:

- Positive and negative zero (`0.0` and `-0.0`) are treated as equal, so they map to the same key
- `NaN` (not-a-number) compares equal to itself in map contexts, unlike standard float comparison

```verse
Map1 := map{0.0 => "zero", -0.0 => "negative zero"}
# Second entry overwrites first - both zeros are the same key

Map2 := map{NaN => "first", NaN => "second"}
# Second entry overwrites first - NaN equals NaN in maps
```

### Nested Maps

Maps can contain other maps as values, enabling multi-level associations:

```verse
# Map from strings to maps of ints to strings
NestedMap : [string][int]string = map{
    "numbers" => map{1 => "one", 2 => "two"},
    "letters" => map{0 => "a", 1 => "b"}
}

if (InnerMap := NestedMap["numbers"]):
    if (Value := InnerMap[1]):
        # Value is "one"
```

Maps as keys are currently not fully supported, though the type system allows declaring them.

### Concatenating Maps: ConcatenateMaps()

The `ConcatenateMaps()` function merges multiple maps into a single map, similar to how `Concatenate()` combines arrays:

```verse
# Signature
ConcatenateMaps(Maps:[]map(k,v)...):map(k,v)
```

`ConcatenateMaps()` is variadic—it accepts any number of maps and combines them into one. When maps contain duplicate keys, values from **later** maps override values from earlier ones:

**Basic usage:**

```verse
Map1 := map{1 => "one", 2 => "two"}
Map2 := map{3 => "three", 4 => "four"}
Map3 := map{5 => "five"}

Combined := ConcatenateMaps(Map1, Map2, Map3)
# Combined is map{1 => "one", 2 => "two", 3 => "three", 4 => "four", 5 => "five"}
```

**Handling duplicate keys:**

```verse
Base := map{1 => "original", 2 => "base"}
Override := map{2 => "updated", 3 => "new"}

Result := ConcatenateMaps(Base, Override)
# Result is map{1 => "original", 2 => "updated", 3 => "new"}
# Key 2 was overridden by the later map
```

The right-to-left precedence ensures that later maps take priority, enabling a natural override pattern.

**Empty maps:**

```verse
# Empty maps contribute nothing
M1 := map{1 => "a"}
M2 := map{}
M3 := map{2 => "b"}

Result := ConcatenateMaps(M1, M2, M3)  # map{1 => "a", 2 => "b"}

# Concatenating only empty maps produces an empty map
Empty := ConcatenateMaps(map{}, map{}, map{})  # map{}

# Single map returns that map
Single := ConcatenateMaps(map{1 => "one"})  # map{1 => "one"}
```

**Practical applications:**

```verse
# Configuration merging with cascading overrides
default_config := class:
    DefaultSettings:map(string, int) = map{
        "volume" => 50,
        "brightness" => 75,
        "difficulty" => 1
    }

    UserSettings:map(string, int) = map{}

    SessionSettings:map(string, int) = map{}

    # Merge with priority: Default < User < Session
    GetEffectiveSettings():map(string, int) =
        ConcatenateMaps(DefaultSettings, UserSettings, SessionSettings)

Config := default_config{}
set Config.UserSettings = map{"volume" => 80}  # User preference
set Config.SessionSettings = map{"brightness" => 100}  # Session override

FinalSettings := Config.GetEffectiveSettings()
# FinalSettings is map{"volume" => 80, "brightness" => 100, "difficulty" => 1}
```

```verse
# Merging data from multiple sources
player_stats := class:
    BaseStats:map(string, int)
    BonusStats:map(string, int)
    TemporaryStats:map(string, int)

    TotalStats():map(string, int) =
        ConcatenateMaps(BaseStats, BonusStats, TemporaryStats)

Player := player_stats{
    BaseStats := map{"strength" => 10, "agility" => 8},
    BonusStats := map{"strength" => 5, "intelligence" => 3},
    TemporaryStats := map{"agility" => 2}
}

Stats := Player.TotalStats()
# Stats is map{"strength" => 15, "agility" => 10, "intelligence" => 3}
# strength was overridden (10 -> 15), agility was overridden (8 -> 10)
```

```verse
# Building maps incrementally
BuildResourceMap(Levels:[]int):map(string, int) =
    LevelMaps := for (Level : Levels):
        map{
            "level_{ToString(Level)}_wood" => Level * 10,
            "level_{ToString(Level)}_stone" => Level * 5
        }
    ConcatenateMaps(LevelMaps)

Resources := BuildResourceMap(array{1, 2, 3})
# Resources is map{
#     "level_1_wood" => 10,
#     "level_1_stone" => 5,
#     "level_2_wood" => 20,
#     "level_2_stone" => 10,
#     "level_3_wood" => 30,
#     "level_3_stone" => 15
# }
```

**Type constraints:**

All maps must have the same key and value types:

```verse
# Valid: All maps have same types
M1 := map{1 => "a"}
M2 := map{2 => "b"}
Combined := ConcatenateMaps(M1, M2)  # OK

# Invalid: Mismatched key types
# BadMix := ConcatenateMaps(
#     map{1 => "a"},        # [int]string
#     map{"x" => "b"}       # [string]string
# )  # ERROR: Type mismatch
```

**Comparison with manual merging:**

```verse
# Manual approach
Map1 := map{1 => "a", 2 => "b"}
Map2 := map{2 => "updated", 3 => "c"}
var Manual := Map1
for (Key->Value : Map2):
    set Manual[Key] = Value
# Manual is map{1 => "a", 2 => "updated", 3 => "c"}

# Using ConcatenateMaps
Auto := ConcatenateMaps(Map1, Map2)
# Auto is map{1 => "a", 2 => "updated", 3 => "c"}

# Results are identical, but ConcatenateMaps is more concise
```

The variadic nature of `ConcatenateMaps()` makes it ideal for configuration systems, data aggregation, and any scenario where multiple maps need to be merged with clear precedence rules.

### Weak Maps

The `weak_map` type is a specialized supertype of `map` designed for persistent data storage with weak key references. It behaves similarly to ordinary maps for individual entry access, but deliberately restricts bulk operations. You cannot ask for its length, you cannot iterate over its entries, and you cannot use `ConcatenateMaps`. These restrictions enable efficient weak reference semantics and integration with Verse's persistence system.

#### Basic Usage

A `weak_map` is declared with `weak_map(k,v)` and can be initialized from an ordinary `map{}`. Updating and accessing individual entries works the same way as regular maps:

<!--verse
F()<decides>:void={
-->
```verse
var MyWeakMap:weak_map(int,int) = map{}

set MyWeakMap[0] = 1
Value := MyWeakMap[0]         # succeeds with 1

set MyWeakMap = map{0 => 2}   # reassignment still works (for local variables)
```
<!--verse
}
-->

Because `weak_map` is a supertype of `map`, you can assign regular maps to weak_map variables when needed, but you lose the ability to count or iterate once you are working with a weak map.

#### Restrictions

**No Length Property (Error 3506):**

```verse
var MyWeakMap:weak_map(int,int) = map{1 => 2}
# ERROR: weak_map has no Length property
# Size := MyWeakMap.Length
```

**No Iteration (Error 3524):**

```verse
var MyWeakMap:weak_map(int,int) = map{1 => 2, 3 => 4}
# ERROR: Cannot iterate over weak_map
# for (Entry : MyWeakMap) {}
```

**Cannot Coerce to Comparable (Error 3509):**

```verse
var MyWeakMap:weak_map(int,int) = map{}
# ERROR: weak_map cannot be converted to comparable
# C:comparable = MyWeakMap
```

**Cannot Join with Regular Maps (Error 3510):**

```verse
var MyWeakMap:weak_map(int,int) = map{1 => 2}
# ERROR: Cannot join weak_map with regular map to produce regular map
# Result:[int]int = if (true?) then MyWeakMap else map{3 => 4}
```

#### Module-Scoped weak_map Variables

When using `weak_map` as a module-scoped variable (for persistent data), there are additional critical restrictions:

**Cannot Read Complete Map (Error 3502):**

```verse
# Module-scoped persistent weak_map
var PlayerData:weak_map(player, int) = map{}

GetAllData():weak_map(player, int) =
    # ERROR: Cannot read complete module-scoped weak_map
    # PlayerData
    map{}  # Must construct new map instead
```

**Cannot Write Complete Map (Error 3502):**

```verse
var PlayerData:weak_map(player, int) = map{}

ResetAllData():void =
    # ERROR: Cannot replace module-scoped weak_map
    # set PlayerData = map{}
    {}
```

**Individual Entry Access Works:**

```verse
var PlayerData:weak_map(player, int) = map{}

# OK: Can read individual entries
GetPlayerScore(Player:player):int =
    if (Score := PlayerData[Player]):
        Score
    else:
        0

# OK: Can write individual entries
SetPlayerScore(Player:player, Score:int):void =
    set PlayerData[Player] = Score
```

This restriction exists because module-scoped weak_maps integrate with the persistence system, which only tracks individual entry updates, not complete map replacements.

#### Type Requirements for Module-Scoped Variables

For module-scoped `var weak_map` variables, both key and value types have strict requirements:

**Key Type Must Have `<module_scoped_var_weak_map_key>` Specifier (Error 3502):**

```verse
# Valid key type
persistent_class := class<unique><allocates><computes><persistent><module_scoped_var_weak_map_key> {}

var ValidData:weak_map(persistent_class, int) = map{}

# Invalid key type - missing specifier
regular_class := class<unique><allocates><computes> {}

# ERROR: Key type lacks <module_scoped_var_weak_map_key>
# var InvalidData:weak_map(regular_class, int) = map{}
```

**Value Type Must Be Persistable (Error 3502):**

```verse
persistent_class := class<unique><allocates><computes><persistent><module_scoped_var_weak_map_key> {}

# Valid: persistable value type
persistable_struct := struct<persistable>:
    Value:int

var ValidData:weak_map(persistent_class, persistable_struct) = map{}

# Invalid: non-persistable value type
regular_struct := struct:
    Value:int

# ERROR: Value type must be persistable
# var InvalidData:weak_map(persistent_class, regular_struct) = map{}
```

Common key types that satisfy the requirements:

- **`player`** - The standard key type for player-specific data
- **`persistent_key`** - Custom persistent keys with validity tracking
- **`session_key`** - Transient keys that don't persist across sessions

#### Covariance

The `weak_map` type is **covariant** in its key type, meaning you can use a weak_map with a subclass key type where a parent class key type is expected:

```verse
base_class := class<unique> {}
derived_class := class(base_class) {}

value_struct := struct {}

CreateDerivedMap():weak_map(derived_class, value_struct) =
    map{}

# OK: weak_map is covariant in key type
BaseMap:weak_map(base_class, value_struct) = CreateDerivedMap()

# ERROR 3509: Cannot go the other way (contravariance)
# DerivedMap:weak_map(derived_class, value_struct) = BaseMap
```

This covariance also allows regular maps to be assigned to weak_maps with compatible key types:

```verse
DerivedKey := derived_class{}
RegularMap:[derived_class]value_struct = map{DerivedKey => value_struct{}}

# OK: Regular map converts to weak_map with covariant key
WeakMap:weak_map(base_class, value_struct) = RegularMap
```

#### Partial Field Updates

When the value type is a struct or class, you can update individual fields of stored values:

```verse
player_data := struct<persistable>:
    Level:int
    Score:int

var PlayerData:weak_map(player, player_data) = map{}

UpdatePlayerLevel(Player:player, NewLevel:int):void =
    # Set entire struct first
    set PlayerData[Player] = player_data{Level := NewLevel, Score := 0}

    # Then update just one field
    set PlayerData[Player].Level = NewLevel + 1
```

#### Transaction and Rollback Semantics

Like all mutable state in Verse, `weak_map` updates participate in transaction semantics. If a `<decides>` expression fails, all changes are rolled back:

```verse
var GameData:weak_map(int, int) = map{}

AttemptUpdate():void =
    if:
        set GameData[1] = 100
        set GameData[2] = 200
        false?  # Transaction fails

    # Both updates rolled back
    # GameData[1] is still false
    # GameData[2] is still false
```

This applies to complete map replacements (for local variables), individual entries, and partial field updates.

#### Island Limits

There is a **limit on the number of persistent `weak_map` variables** per island. In the standard environment, this limit is 4 persistent weak_maps. Exceeding this limit produces error 3502:

```verse
key_class := class<unique><allocates><computes><persistent><module_scoped_var_weak_map_key> {}

var Map1:weak_map(key_class, int) = map{}  # OK
var Map2:weak_map(key_class, int) = map{}  # OK
var Map3:weak_map(key_class, int) = map{}  # OK
var Map4:weak_map(key_class, int) = map{}  # OK

# ERROR 3502: Exceeds island limit
# var Map5:weak_map(key_class, int) = map{}
```

**Exception:** If the value type is a class (not a primitive or struct), the weak_map doesn't count toward this limit:

```verse
value_class := class<final><persistable> {}

var Map1:weak_map(key_class, int) = map{}       # Counts (1/4)
var Map2:weak_map(key_class, int) = map{}       # Counts (2/4)
var Map3:weak_map(key_class, int) = map{}       # Counts (3/4)
var Map4:weak_map(key_class, value_class) = map{}  # Doesn't count (class value)
```

#### Integration with Persistence System

The `weak_map(player, t)` type is the primary mechanism for storing persistent player data in Verse. When used at module scope, these maps automatically integrate with the game's save system:

```verse
player_stats := struct<persistable>:
    Level:int
    Experience:int

# Automatically persisted across game sessions
var PlayerStats:weak_map(player, player_stats) = map{}

UpdatePlayerStats(Player:player, XP:int):void =
    if (Stats := PlayerStats[Player]):
        set PlayerStats[Player].Experience = Stats.Experience + XP
    else:
        set PlayerStats[Player] = player_stats{Level := 1, Experience := XP}
```

For more details on persistent data, see the Persistable Types chapter.

## type

The `type` type is a *metatype* - a type whose values are themselves types. Every Verse type can be used as a value of type `type`. This enables powerful generic programming through parametric functions, where types are parameters that can be passed around and constrained.

You can create variables and parameters that hold type values:

```verse
# Variable holding a type value
IntType:type = int
StringType:type = string

# Function that takes a type as parameter
CreateDefault(T:type):?T = false

# Usage
X:?int = CreateDefault(int)      # T = int, returns false
Y:?string = CreateDefault(string)  # T = string, returns false
```

All Verse types can be type values:

```verse
# Primitives
PrimitiveType:type = int

# User-defined types
MyClass := class {}
ClassType:type = MyClass

MyStruct := struct {Value:int}
StructType:type = MyStruct

# Collection types
ArrayType:type = []int
MapType:type = [string]int
TupleType:type = tuple(int, string)
OptionType:type = ?int

# Function types
FuncType:type = int->string

# Parametric types
generic_class(t:type) := class {Data:t}
ParametricType:type = generic_class(int)

# Metatypes
SubtypeValue:type = subtype(MyClass)

# Type literals
TypeLiteralValue:type = type{_(:int):string}
```

This universality makes `type` the foundation for Verse's generic programming - any type can be abstracted over.

### Type Parameters

The most common use of `type` is in **where clauses** to create parametric (generic) functions:

```verse
# Identity function - works with any type
Identity(X:t where t:type):t = X

# Usage - type parameter inferred
Identity(42)        # t = int
Identity("hello")   # t = string
Identity(true)      # t = logic
```

The `where t:type` constraint means "`t` can be any Verse type." The type system infers `t` from the argument and ensures type safety throughout the function.

While `where t:type` accepts any type, you can use more specific constraints like `subtype` to limit which types are valid:

```verse
# Only accepts types that are subtypes of comparable
Sort(Items:[]t where t:subtype(comparable)):[]t =
    # Can use comparison operations because t is comparable
    ...
```

For comprehensive documentation on parametric functions, see the Functions chapter.

### Type as First-Class Values

Unlike many languages where types only exist at compile time, Verse treats types as *first-class values* that can be computed, stored, and manipulated:

```verse
# Function that returns a type value
GetTypeForSize(Size:int):type =
    if (Size <= 8):
        int
    else:
        string

# Store type in data structure
TypeRegistry:[string]type = map{
    "Integer" => int,
    "Text" => string,
    "Flag" => logic
}
```

**Passing types between functions:**

```verse
# Helper function that takes a type parameter
CreateArray(ElementType:type, Size:int):[]ElementType =
    # This pattern works in some contexts
    ...

# Function that uses the helper
MakeIntArray():[]int =
    CreateArray(int, 10)
```

### Returning Options of Type Parameters

A common pattern is to have functions return `?t` where `t` is a type parameter, allowing the function to work with any type while potentially failing:

```verse
# Function that might produce a value of any type
MaybeValue(T:type, Condition:logic):?T =
    if (Condition):
        # Cannot construct T generically, return failure
        false
    else:
        false

# Specific usage
X:?int = MaybeValue(int, false)  # Returns false as ?int
```

This pattern is particularly useful for generic containers and factory functions that may or may not be able to produce a value.

### Type Constraints

The `type` constraint in where clauses is the most permissive - it accepts any Verse type. For more specific requirements, Verse provides additional constraints:

```verse
# Most permissive: any type
Generic(X:t where t:type):t = X

# More specific: must be subtype of comparable
RequiresComparison(X:t where t:subtype(comparable)):logic =
    X = X  # Can use = because t is comparable

# Even more specific: must be exact subtype
RequiresExactType(X:t, Y:u where t:type, u:subtype(t)):t =
    X  # Y is guaranteed to be compatible with t
```

The type system enforces these constraints at compile time, preventing invalid type usage.

### Limitations

While `type` enables powerful abstractions, there are some limitations:

**Cannot construct arbitrary types generically:**

```verse
# Cannot do this - no way to construct a value of arbitrary type t
# MakeValue(T:type):T = ???  # What would this return for T=int? T=string?
```

**Cannot inspect type structure at runtime:**

```verse
# Cannot do this - no runtime type introspection
# GetFieldNames(T:type):[]string = ???
```

**Type parameters must be inferred or explicit:**

```verse
# Type parameter must be determinable from usage
Identity(X:t where t:type):t = X

# OK: t inferred from argument
Identity(42)

# ERROR: t cannot be inferred from no arguments
# MakeDefault(where t:type):t = ???
```

## Any

The `any` type is the *supertype of all types*. Every type in the language is a subtype of `any`. Because of this, `any` itself supports very few operations: whatever functionality `any` provides must also be implemented by every other type. In practice, there is very little you can do directly with values of type `any`. Still, it is important to understand the type, because it sometimes arises when working with code that mixes different kinds of values, or when the type checker has no more precise type to assign.  

One way `any` appears is when combining values that do not share a more specific supertype. For example:  

```verse
Letters := enum:
    A
    B
    C

letter := class:
    Value : char
    Main(Arg : int) : void =
        X := if (Arg > 0) then:
            Letters.A
        else:
            letter{Value := 'D'}
```

In this example, `X` is assigned either a value of type `Letters` or of type `letter`. Since these two types are unrelated, the compiler assigns `X` the type `any`, which is their lowest common supertype.  

A more useful role for `any` is as the type of a parameter that is required syntactically but not actually used. This pattern can arise when implementing interfaces that require a certain method signature.  

```verse
FirstInt(X:int, :any) : int = X
```

Here, the second parameter is ignored. Because it can be any value of any type, it is given the type `any`.  

In more general code, the same idea can be expressed using *parametric types*, making the function flexible while still precise:  

```verse
First(X:t, :any where t:type) : t = X
```

This version works for any type `t`, returning a value of type `t` while discarding the unused argument of type `any`.  

## Void

The `void` type is the *empty type*. Unlike `any`, which contains all possible values, `void` contains none. It represents the absence of a value and is used in places where no result is returned.  

Because `void` has no values, you can never construct or assign a value of type `void`. This makes it useful as a marker type in function signatures and control flow.  

A function whose purpose is to perform an effect, rather than compute a value, has return type `void`.  

<!--verse
Print(:string):void={}
-->
```verse
LogMessage(Msg:string) : void =
    Print(Msg)
```

Here, `LogMessage` performs an action (printing) but does not return a result. The `void` return type makes that explicit.
