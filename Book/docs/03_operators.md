# Operators - Complete Reference

## Overview

Operators in Verse are special functions that perform actions on their operands. They provide concise syntax for common operations like arithmetic, comparison, logical operations, and assignment. Understanding operators is essential for writing effective Verse code, as they form the foundation of most expressions and control flow decisions.

In Verse, operators follow strict precedence rules and have specific behaviors regarding failure and effects. Many operators are failable, meaning they can fail at runtime and must be used in appropriate failure contexts.

## Operator Formats

Verse operators come in three formats based on their position relative to their operands:

### Prefix Operators
Prefix operators appear before their single operand:
- `not Expression` - Logical negation
- `-Value` - Numeric negation
- `+Value` - Numeric positive (for alignment)

### Infix Operators
Infix operators appear between their two operands:
- `A + B` - Addition
- `A * B` - Multiplication
- `A = B` - Equality comparison
- `A and B` - Logical AND

### Postfix Operators
Postfix operators appear after their single operand:
- `Value?` - Query operator for logic values

## Operator Precedence

When multiple operators appear in the same expression, they are evaluated according to their precedence level. Higher precedence operators are evaluated first. Operators with the same precedence are evaluated left to right (except for assignment and unary operators which are right-associative).

### Complete Precedence Table (Based on Parser Implementation)

From the Verse parser implementation, the precedence levels from highest to lowest are:

| Precedence | Operators | Category | Format | Associativity |
|------------|-----------|----------|--------|---------------|
| 11 | `.`, `[]`, `()`, `{}`, `?` (postfix) | Member access, Indexing, Call, Construction, Query | Postfix | Left |
| 10 | `-` (unary), `not` | Unary operations | Prefix | Right |
| 9 | `*`, `/`, `%` | Multiplication, Division, Modulo | Infix | Left |
| 8 | `+`, `-` (binary) | Addition, Subtraction | Infix | Left |
| 7 | `<`, `<=`, `>`, `>=` | Relational comparison | Infix | Left |
| 6 | `==`, `!=` | Equality comparison | Infix | Left |
| 5 | `and` | Logical AND | Infix | Left |
| 4 | `or` | Logical OR | Infix | Left |
| 3 | `..` | Range | Infix | Left |
| 2 | Lambda expressions | Function literals | Special | N/A |
| 1 | `:=`, `=` | Assignment | Infix | Right |

Note: The parser implementation shows that `==` and `!=` are used for equality comparison (not `=` and `<>` as in some documentation). The `=` operator is used for assignment alongside `:=`.

### Grouping with Parentheses

You can override the default precedence by using parentheses to group expressions:

```verse
# Without parentheses - multiplication first
Result := 2 + 3 * 4  # Result is 14

# With parentheses - addition first
Result := (2 + 3) * 4  # Result is 20

# Complex expression with grouping
Damage := BaseDamage * (1.0 + BonusPercent / 100.0) - ArmorReduction
```

## Arithmetic Operators

Arithmetic operators perform mathematical operations on numeric values. They work with both `int` and `float` types, with some special behaviors for type conversion and integer division.

### Basic Arithmetic

| Operator | Operation | Types | Notes |
|----------|-----------|-------|-------|
| `+` | Addition | `int`, `float` | Also concatenates strings and arrays |
| `-` | Subtraction | `int`, `float` | Can be used as unary negation |
| `*` | Multiplication | `int`, `float` | Converts `int` to `float` when mixed |
| `/` | Division | `int` (failable), `float` | Integer division returns `rational` |
| `%` | Modulo | `int`, `float` | Remainder after division |

```verse
# Basic arithmetic
Sum := 10 + 20           # 30
Difference := 50 - 15     # 35
Product := 6 * 7          # 42
Quotient := 20.0 / 4.0    # 5.0

# Unary operators
Negative := -42           # -42
Positive := +42           # 42 (for alignment)

# Integer division (failable, returns rational)
if (Result := 10 / 3):
    IntResult := Floor(Result)  # 3

# Type conversion through multiplication
IntValue:int = 42
FloatValue:float = IntValue * 1.0  # Converts to 42.0
```

### Compound Assignment Operators

Compound assignment operators combine an arithmetic operation with assignment:

| Operator | Equivalent To | Types |
|----------|---------------|-------|
| `set +=` | `set X = X + Y` | `int`, `float`, `string`, `array` |
| `set -=` | `set X = X - Y` | `int`, `float` |
| `set *=` | `set X = X * Y` | `int`, `float` |
| `set /=` | `set X = X / Y` | `float` only |

```verse
var Score:int = 100
set Score += 50    # Score is now 150
set Score -= 25    # Score is now 125
set Score *= 2     # Score is now 250

var Health:float = 100.0
set Health /= 2.0  # Health is now 50.0

# Note: set /= doesn't work with integers due to failable division
# var IntValue:int = 10
# set IntValue /= 2  # Compile error!
```

### Special Behaviors

#### String and Array Concatenation

The `+` operator concatenates strings and arrays:

```verse
# String concatenation
Greeting := "Hello, " + "World!"  # "Hello, World!"
var Message:string = "Score: "
set Message += "100"  # "Score: 100"

# Array concatenation
Array1 := array{1, 2, 3}
Array2 := array{4, 5, 6}
Combined := Array1 + Array2  # array{1, 2, 3, 4, 5, 6}
```

#### Integer Division and Rationals

Integer division with `/` is unique in Verse:
- It's failable (can fail if dividing by zero)
- It returns a `rational` type, not an `int`
- You must use `Floor()` or `Ceil()` to convert to `int`

```verse
# Integer division workflow
if (Ratio := 7 / 2):
    Lower := Floor(Ratio)  # 3
    Upper := Ceil(Ratio)   # 4

# Division by zero fails gracefully
if (Result := 10 / 0):
    # This block never executes
    Print("Impossible!")
else:
    Print("Division by zero detected")
```

## Comparison Operators

Comparison operators test relationships between values and are failable expressions that succeed or fail based on the comparison result.

### Relational Operators

| Operator | Meaning | Supported Types | Example |
|----------|---------|-----------------|---------|
| `<` | Less than | `int`, `float` | `Score < 100` |
| `<=` | Less than or equal | `int`, `float` | `Health <= 0.0` |
| `>` | Greater than | `int`, `float` | `Level > 5` |
| `>=` | Greater than or equal | `int`, `float` | `Time >= MaxTime` |

### Equality Operators

| Operator | Meaning | Supported Types | Example |
|----------|---------|-----------------|---------|
| `==` | Equal to | All comparable types | `Name == "Player1"` |
| `!=` | Not equal to | All comparable types | `State != idle` |
| `=` | Equal to (alternative) | All comparable types | `Name = "Player1"` |
| `<>` | Not equal to (alternative) | All comparable types | `State <> idle` |

Note: The parser implementation supports both `==`/`!=` and `=`/`<>` for equality comparisons, though `==`/`!=` appear to be the primary operators in the parser code.

```verse
# Numeric comparisons
if (Score > HighScore):
    Print("New high score!")

if (Health <= 0.0):
    HandlePlayerDeath()

# Equality with different types
if (PlayerName = "Admin"):
    EnableAdminMode()

if (CurrentState <> GameState.Playing):
    ShowMenu()

# Comparison in complex expressions
if (Level >= 10 and Score > 1000):
    UnlockAchievement()
```

### Comparable Types

The following types support comparison operations:
- Numeric types: `int`, `float`, `rational`
- Boolean: `logic`
- Text: `string`, `char`, `char32`
- Enumerations: `enum` types
- Collections: `array`, `map`, `tuple` (if elements are comparable)
- Structs: If all fields are comparable (V1 feature)
- Classes: Only with `=` and `<>` if they contain at least one `var` member

Note: Comparisons between different types generally fail:
```verse
0 = 0.0  # Fails: int vs float
"5" = 5  # Fails: string vs int
```

## Logical Operators

Logical operators work with failable expressions and control the flow of success and failure.

### Query Operator (`?`)

The query operator checks if a `logic` value is `true`:

```verse
var IsReady:logic = true

if (IsReady?):
    StartGame()

# Equivalent to:
if (IsReady = true):
    StartGame()
```

### Not Operator

The `not` operator negates the success or failure of an expression:

```verse
if (not IsGameOver?):
    ContinuePlaying()

# Effects are not committed with not
var X:int = 0
if (not (set X = 5)):
    # X is still 0 here, even though the assignment "tried" to happen
    Print("X is {X}")  # Prints "X is 0"
```

### And Operator

The `and` operator succeeds only if both operands succeed:

```verse
if (HasKey? and DoorUnlocked?):
    EnterRoom()

# Both expressions must succeed
if (Player.Level > 5 and Player.HasItem("Sword")):
    AllowQuestAccess()
```

### Or Operator

The `or` operator succeeds if at least one operand succeeds:

```verse
if (HasKeyCard? or HasMasterKey?):
    OpenDoor()

# Short-circuit evaluation - second operand not evaluated if first succeeds
if (QuickCheck() or ExpensiveCheck()):
    ProcessResult()
```

### Truth Table for Logical Operators

| Expression P | Expression Q | P and Q | P or Q | not P |
|--------------|--------------|---------|---------|-------|
| Succeeds | Succeeds | Succeeds (Q's value) | Succeeds (P's value) | Fails |
| Succeeds | Fails | Fails | Succeeds (P's value) | Fails |
| Fails | Succeeds | Fails | Succeeds (Q's value) | Succeeds |
| Fails | Fails | Fails | Fails | Succeeds |

## Assignment and Initialization Operators

### Variable Initialization (`:=`)

The `:=` operator initializes constants and variables:

```verse
# Constant initialization (immutable)
MaxHealth:int = 100
PlayerName:string = "Hero"

# Variable initialization (mutable)
var CurrentHealth:int = 100
var Score:int = 0

# Type inference
AutoTyped := 42  # Inferred as int
```

### Variable Assignment (`set =`)

The `set =` operator updates variable values:

```verse
var Points:int = 0
set Points = 100

var Position:vector3 = vector3{X := 0.0, Y := 0.0, Z := 0.0}
set Position = vector3{X := 10.0, Y := 20.0, Z := 0.0}
```

### Assignment in Failure Context

Assignment can be used in failure contexts, making it failable:

```verse
var MyArray:[]int = array{1, 2, 3}
if (set MyArray[10] = 5):
    # This won't execute because index 10 is out of bounds
    Print("Set succeeded")
else:
    Print("Assignment failed")
```

## Special Operators

### Indexing Operator (`[]`)

Used for multiple purposes in Verse:
1. **Array/Map indexing** - Access elements in collections
2. **Function calls** (Verse-style) - Call functions with bracket syntax
3. **Computed member access** - Access object members dynamically

```verse
# Array indexing (failable)
MyArray := array{10, 20, 30}
if (Element := MyArray[1]):
    Print("Element at index 1: {Element}")  # Prints 20

# Map lookup (failable)
Scores:[string]int = map{"Alice" => 100, "Bob" => 85}
if (AliceScore := Scores["Alice"]):
    Print("Alice's score: {AliceScore}")

# String indexing (failable)
Name:string = "Verse"
if (FirstChar := Name[0]):
    Print("First character: {FirstChar}")  # Prints 'V'

# Function call with brackets (Verse-style)
Result := MyFunction[Arg1, Arg2]  # Alternative to MyFunction(Arg1, Arg2)
EmptyCall := MyFunction[]  # Call with no arguments
```

### Member Access Operator (`.`)

Accesses fields and methods of objects:

```verse
Player.Health
Player.GetName()
MyVector.X
Config.Settings.MaxPlayers

# Line continuation supported after dot
LongExpression := MyObject.
    FirstMethod().
    SecondMethod()
```

### Range Operator (`..`)

Creates ranges for iteration:

```verse
# Inclusive range
for (I := 0..4):
    Print("{I}")  # Prints 0, 1, 2, 3, 4

# In array slicing context
AllElements := 0..MyArray.Length-1
```

### Object Construction Operator (`{}`)

Used to construct objects when placed after an identifier:

```verse
# Object construction with type name
Point := Point{x := 10, y := 20}

# Fields can be separated by commas or newlines
Player := PlayerData{
    Name := "Hero"
    Level := 5
    Health := 100.0
}

# Trailing commas are allowed
Config := GameConfig{
    MaxPlayers := 100,
    EnablePvP := true,
}
```

### Tuple Access Operator (`()`)

When used with a single argument after an expression, accesses tuple elements:

```verse
MyTuple := (10, 20, 30)
FirstElement := MyTuple(0)  # Access first element
SecondElement := MyTuple(1)  # Access second element
```

## Type Conversion and Operators

### Implicit Conversions

Verse has limited implicit type conversion. Most conversions must be explicit:

```verse
# No implicit int to float conversion
MyInt:int = 42
# MyFloat:float = MyInt  # Error!
MyFloat:float = MyInt * 1.0  # OK: explicit conversion

# No implicit numeric to string conversion
Score:int = 100
# Message:string = "Score: " + Score  # Error!
Message:string = "Score: {Score}"  # OK: string interpolation
```

### Mixed Type Operations

When operators work with mixed types, specific rules apply:

```verse
# int * float -> float
Result := 5 * 2.0  # Result is 10.0 (float)

# Comparisons must be same type
if (5 = 5):     # OK
if (5.0 = 5.0): # OK
# if (5 = 5.0):   # Error: different types
```

## Operator Overloading

Verse does not support custom operator overloading. Operators have fixed meanings defined by the language. However, some operators work with multiple types through built-in polymorphism:

- `+` works with numbers, strings, and arrays
- `=` and `<>` work with all comparable types
- `[]` works with arrays, maps, and strings

## Best Practices

### Use Parentheses for Clarity

Even when not required by precedence, parentheses can improve readability:

```verse
# Less clear
if (A = B and C = D or E = F):
    DoSomething()

# More clear
if ((A = B and C = D) or E = F):
    DoSomething()
```

### Handle Failable Operations

Always handle failable operations appropriately:

```verse
# Good: Handle potential failure
if (Result := MyArray[Index]):
    ProcessElement(Result)
else:
    HandleMissingElement()

# Bad: Assuming success without checking
# Element := MyArray[Index]  # Could crash if index out of bounds
```

### Prefer Compound Assignment

Use compound assignment operators for cleaner code:

```verse
# Prefer this:
set Score += Points

# Over this:
set Score = Score + Points
```

### Be Explicit with Type Conversions

Make type conversions explicit to avoid confusion:

```verse
# Clear intention
FloatResult := IntValue * 1.0

# Ambiguous intention
# Result := IntValue * SomeFloat
```

## Common Pitfalls

### Integer Division Returns Rational

Remember that integer division doesn't return an integer:

```verse
# Wrong assumption
# HalfValue:int = 10 / 2  # Error: returns rational, not int

# Correct approach
if (Ratio := 10 / 2):
    HalfValue:int = Floor(Ratio)
```

### Assignment vs Comparison

Be careful not to confuse `=` (comparison) with `:=` or `set =` (assignment):

```verse
# Comparison (failable)
if (Score = 100):
    Print("Perfect score!")

# Assignment (not a comparison)
Score := 100  # Initialization
set Score = 100  # Update
```

### Short-Circuit Evaluation

Remember that `or` short-circuits but `and` doesn't:

```verse
# Second function not called if first succeeds
if (FastCheck() or SlowCheck()):
    Process()

# Both functions always evaluated
if (Check1() and Check2()):
    Process()
```

### Effects with `not`

The `not` operator rolls back effects:

```verse
var Counter:int = 0

# Counter is NOT incremented
if (not (set Counter += 1)):
    Print("Counter: {Counter}")  # Still 0

# Counter IS incremented
if (set Counter += 1):
    Print("Counter: {Counter}")  # Now 1
```
