# Operators

Operators are functions that perform actions on their operands. They provide concise syntax for common operations like arithmetic, comparison, logical operations, and assignment.

## Operator Formats

Verse operators come in three formats based on their position relative to their operands:

**Prefix Operators**

Prefix operators appear before their single operand:

- `not Expression` - Logical negation
- `-Value` - Numeric negation
- `+Value` - Numeric positive (for alignment)

**Infix Operators**

Infix operators appear between their two operands:

- `A + B` - Addition
- `A * B` - Multiplication
- `A = B` - Equality comparison
- `A and B` - Logical AND

**Postfix Operators**

Postfix operators appear after their single operand:

- `Value?` - Query operator for logic values

## Precedence

When multiple operators appear in the same expression, they are evaluated according to their precedence level. Higher precedence operators are evaluated first. Operators with the same precedence are evaluated left to right (except for assignment and unary operators which are right-associative).

The precedence levels from highest to lowest are:

| Precedence | Operators | Category | Format | Associativity |
|------------|-----------|----------|--------|---------------|
| 11 | `.`, `[]`, `()`, `{}`, `?` (postfix) | Member access, Indexing, Call, Construction, Query | Postfix | Left |
| 10 | `-` (unary), `not` | Unary operations | Prefix | Right |
| 9 | `*`, `/`, `%` | Multiplication, Division, Modulo | Infix | Left |
| 8 | `+`, `-` (binary) | Addition, Subtraction | Infix | Left |
| 7 | `<`, `<=`, `>`, `>=` | Relational comparison | Infix | Left |
| 5 | `and` | Logical AND | Infix | Left |
| 4 | `or` | Logical OR | Infix | Left |
| 3 | `..` | Range | Infix | Left |
| 2 | Lambda expressions | Function literals | Special | N/A |
| 1 | `:=`, `=` | Assignment | Infix | Right |

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

<!--verse
R():void={
-->
```verse
# Basic arithmetic
Sum := 10 + 20      # 30
Diff := 50 - 15     # 35
Prod := 6 * 7       # 42
Quot := 20.0 / 4.0  # 5.0

# Unary operators
Negative := -42     # -42
Positive := +42     # 42 (for alignment)

# Integer division (failable, returns rational)
if (Result := 10 / 3):
    IntResult := Floor(Result)  # 3

# Type conversion through multiplication
IntValue:int = 42
FloatValue:float = IntValue * 1.0  # Converts to 42.0
```
<!--verse
}
-->

### Compound Assignments

Compound assignment operators combine an arithmetic operation with assignment:

| Operator | Equivalent To | Types |
|----------|---------------|-------|
| `set +=` | `set X = X + Y` | `int`, `float`, `string`, `array` |
| `set -=` | `set X = X - Y` | `int`, `float` |
| `set *=` | `set X = X * Y` | `int`, `float` |
| `set /=` | `set X = X / Y` | `float` only |

<!--verse
F():void={
-->
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
<!--verse
}
-->

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
| `=` | Equal to | All comparable types | `Name = "Player1"` |
| `<>` | Not equal | All comparable types | `State <> idle` |

<!--NoCompile-->
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

The following types support equality comparison operations (`=` and `<>`):

- Numeric types: `int`, `nat`, `float`, `rational`
- Boolean: `logic`
- Text: `string`, `char`, `char32`
- Enumerations: All `enum` types
- Collections: `array`, `map`, `tuple`, `option` (if elements are comparable)
- Structs: If all fields are comparable
- Unique classes: Classes marked with `<unique>` (identity equality only)

Comparisons between different types generally fail:

<!--verse
F()<decides>:void={
-->
```verse
0 = 0.0  # Fails: int vs float
"5" = 5  # Fails: string vs int
```
<!--verse
}
-->

## Logical Operators

Logical operators work with failable expressions and control the flow of success and failure.

### Query Operator (`?`)

The query operator checks if a `logic` value is `true`:

<!--verse
StartGame():void={}
F():void={
-->
```verse
var IsReady:logic = true

if (IsReady?):
    StartGame()

# Equivalent to:
if (IsReady = true):
    StartGame()
```
<!--verse
}
-->

### Not Operator

The `not` operator negates the success or failure of an expression:

<!--verse
using { /Verse.org/VerseCLR }
F(IsGameOver:?int):void={
-->
```verse
if (not IsGameOver?):
    ContinuePlaying()

# Effects are not committed with not
var X:int = 0
if (not (set X = 5, IsGameOver?)):
    # X is still 0 here, even though the assignment "tried" to happen
    Print("X is {X}")  # Prints "X is 0"
```
<!--verse
}
-->

### And Operator

The `and` operator succeeds only if both operands succeed:

<!--NoCompile-->
```verse
if (HasKey? and DoorUnlocked?):
    EnterRoom()

# Both expressions must succeed
if (Player.Level > 5 and Player.HasItem("Sword")):
    AllowQuestAccess()
```

### Or Operator

The `or` operator succeeds if at least one operand succeeds:

<!--NoCompile-->
```verse
if (HasKeyCard? or HasMasterKey?):
    OpenDoor()

# Short-circuit evaluation - second operand not evaluated if first succeeds
if (QuickCheck() or ExpensiveCheck()):
    ProcessResult()
```

### Truth Table

Consider two expressions `P` and `Q` which may either succeed or fail, the following table shows the result of logical operators applied to them:

| Expression P | Expression Q | P and Q | P or Q | not P |
|--------------|--------------|---------|---------|-------|
| Succeeds | Succeeds | Succeeds (Q's value) | Succeeds (P's value) | Fails |
| Succeeds | Fails | Fails | Succeeds (P's value) | Fails |
| Fails | Succeeds | Fails | Succeeds (Q's value) | Succeeds |
| Fails | Fails | Fails | Fails | Succeeds |

## Assignment and Initialization

The `:=` operator initializes constants and variables:

<!--NoCompile-->
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

The `set =` operator updates variable values:

<!--verse
vector3:=struct{X:float, Y:float, Z:float}
F():void={
-->
```verse
var Points:int = 0
set Points = 100

var Position:vector3 = vector3{X := 0.0, Y := 0.0, Z := 0.0}
set Position = vector3{X := 10.0, Y := 20.0, Z := 0.0}
```
<!--verse
}
-->

## Special Operators

### Indexing

The square bracket operator is used for multiple purposes in Verse:

1. **Array/Map indexing** - Access elements in collections
2. **Function calls** - Call functions which may fail
3. **Computed member access** - Access object members dynamically

<!--verse
MyFunction1(X:int, Y:int)<decides>:void={}
MyFunction2(?X:int=0, ?Y:int=0)<decides>:void={}
F(Arg1:int,Arg2:int)<decides>:void={
-->
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

# Function call that can fail
Result := MyFunction1[Arg1, Arg2]          # Can fail
Result := MyFunction2[?X:=Arg1, ?Y:=Arg2]  # Named arguments
EmptyCall := MyFunction2[]                 # and optional values
```
<!--verse
}
-->

### Member Access

The dot operator accesses fields and methods of objects:

<!--NoCompile-->
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

### Range

The range operator creates ranges for iteration:

<!--verse
using { /Verse.org/VerseCLR }
F():void={
-->
```verse
# Inclusive range
for (I := 0..4):
    Print("{I}")  # Prints 0, 1, 2, 3, 4
```
<!--verse
}
-->

### Object Construction

Curly braces are used to construct objects when placed after a type:

<!--verse
point:=struct{X:int, Y:int}
player_data:=struct{Name:string,Level:int,Health:float}
game_config:=struct{MaxPlayers:int,EnablePvP:logic}
F():void={
-->
```verse
# Object construction with type name
Point := point{X:= 10, Y:= 20}

# Fields can be separated by commas or newlines
Player := player_data {
    Name := "Hero"
    Level := 5
    Health := 100.0
}

# Trailing commas are not allowed
Config := game_config{
    MaxPlayers := 100,
    EnablePvP := true # ,  -- not allowed
}
```
<!--verse
}
-->

### Tuple Access

Round braces when used with a single argument after a tuple expression, accesses tuple elements:

<!--verse
F():void={
-->
```verse
MyTuple := (10, 20, 30)
FirstElement := MyTuple(0)  # Access first element
SecondElement := MyTuple(1)  # Access second element
```
<!--verse
}
-->

## Type Conversions

Verse has limited implicit type conversion. Most conversions must be explicit:

<!--verse
F():void={
-->
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
<!--verse
}
-->

When operators work with mixed types, specific rules apply:

<!--verse
F():void={
-->
```verse
# int * float -> float
Result := 5 * 2.0  # Result is 10.0 (float)

# Comparisons must be same type
if (5 = 5):     # OK
if (5.0 = 5.0): # OK
# if (5 = 5.0):   # Error: different types
```
<!--verse
}
-->

<!-- TODO CHECK THIS
## Operator Overloading

Verse features operator overloading for arithmetic operators.

### Overloadable Operators

You can overload the following operators by defining specially-named functions:

**Arithmetic binary operators:**

- `operator'+'(L:type, R:type):result` - Addition
- `operator'-'(L:type, R:type):result` - Subtraction
- `operator'*'(L:type, R:type):result` - Multiplication
- `operator'/'(L:type, R:type):result` - Division

**Unary operators:**

- `prefix'-'(V:type):result` - Negation

### Basic Example

```verse
# Define a 2D vector type
vec2i := struct{X:int, Y:int}

# Overload unary negation
prefix'-'(V:vec2i):vec2i =
    vec2i{X := -V.X, Y := -V.Y}

# Overload addition
operator'+'(L:vec2i, R:vec2i):vec2i =
    vec2i{X := L.X + R.X, Y := L.Y + R.Y}

# Overload subtraction
operator'-'(L:vec2i, R:vec2i):vec2i =
    vec2i{X := L.X - R.X, Y := L.Y - R.Y}

# Use the overloaded operators
V1 := vec2i{X := 3, Y := 4}
V2 := vec2i{X := 5, Y := 6}

Negated := -V1                # vec2i{X := -3, Y := -4}
Sum := V1 + V2                # vec2i{X := 8, Y := 10}
Difference := V1 - V2         # vec2i{X := -2, Y := -2}
```

### Multiple Overloads

You can provide multiple overloads for the same operator with different parameter types:

```verse
# Scalar multiplication - vector * int
operator'*'(L:vec2i, R:int):vec2i =
    vec2i{X := L.X * R, Y := L.Y * R}

# Scalar multiplication - int * vector
operator'*'(L:int, R:vec2i):vec2i =
    vec2i{X := L * R.X, Y := L * R.Y}

# Use both forms
V := vec2i{X := 11, Y := 12}
Result1 := V * 2    # vec2i{X := 22, Y := 24}
Result2 := 3 * V    # vec2i{X := 33, Y := 36}
```

### Operators with Effects

Operator overloads can have effects like `<decides>` or `<transacts>`:

```verse
# Division that can fail
operator'/'(L:vec2i, R:int)<transacts><decides>:vec2i =
    if (R <> 0):
        vec2i{X := Floor(L.X / R), Y := Floor(L.Y / R)}
    else:
        false

# Use with failure handling
V := vec2i{X := 15, Y := 16}

if (Result := V / 2):
    # Result is vec2i{X := 7, Y := 8}

if (Bad := V / 0):
    # Never executes - division by zero fails
```

### Operators and Type Classes

When working with type classes like `comparable`, you need to be careful. Custom operators don't automatically make types comparable:

```verse
vec2i := struct{X:int, Y:int}
operator'+'(L:vec2i, R:vec2i):vec2i = vec2i{X := L.X + R.X, Y := L.Y + R.Y}

# vec2i is NOT automatically comparable
# You would need to make it <unique> or define comparison operators (not yet supported)
```

### Operators in Pure Contexts

Intrinsic (built-in) operators can be used in `<computes>` contexts without restriction:

```verse
# Valid: built-in + in computes context
C := class{X:int = 1 + 2}  # OK
```

However, custom operator overloads require explicit effect annotations to use in pure contexts:

```verse
# Custom operator without <computes>
MyType := class<computes>{}
operator'+'(L:MyType, R:MyType):MyType = MyType{}

# ERROR 3582: Custom operator not allowed in computes context
# D := class<computes>{Value:MyType = MyType{} + MyType{}}

# To fix, add <computes> to operator
operator'+'(L:MyType, R:MyType)<computes>:MyType = MyType{}
D := class<computes>{Value:MyType = MyType{} + MyType{}}  # OK
```

### Restrictions

**Cannot overload comparison operators (yet):**

The following operators cannot be overloaded in current Verse:

- Comparison: `<`, `<=`, `>`, `>=`, `=`, `<>`
- Mutation: `+=`, `-=`, `*=`, `/=` (not supported because non-unique pointers aren't available yet)
- Indexing: `operator'()'` - error 3514

```verse
# ERROR 3514: Cannot overload indexing
# operator'()'(V:vec2i, I:int):int = if (I = 0) {V.X} else {V.Y}
```

**Cannot overload built-in array/map indexing:**

```verse
# ERROR 3514, 3518, 3532: Cannot override built-in indexing
# operator'()'(A:[]int, I:int):int = 42
```

**Internal operators not visible externally:**

Operators marked `<internal>` are only visible within their defining module:

```verse
# In module A
c := class{}
operator'+'<internal>(Lhs:c, Rhs:c):c = c{}

# In module B using A
# Result := c{} + c{}  # ERROR 3509 - internal operator not visible
```
-->