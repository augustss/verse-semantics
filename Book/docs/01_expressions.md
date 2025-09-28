# Expressions

Everything is an expression. This fundamental design principle sets the language apart from many traditional programming languages where statements and expressions are distinct concepts. Every piece of code you write produces a value, even constructs you might expect to be purely side-effecting. This creates a powerful and consistent programming model where code can be composed and combined in ways that feel natural and predictable.

## Primary Expressions

At the foundation of the expression system lie the primary expressions—the atomic units from which more complex expressions are built. These include literals, identifiers, parenthesized expressions, and the powerful tuple construct that provides lightweight data aggregation.

### Literals and Basic Values

The language supports the expected set of literal values, but treats them uniformly as expressions. Integer literals can be written in decimal (42, -17), hexadecimal (0x1F), or binary (0b1010) notation. Floating-point literals support standard notation (3.14, -0.5) as well as scientific notation (1.0e10) and abbreviated forms (.5, 3.). String literals can use either double or single quotes and support escape sequences for special characters.

What makes the treatment of literals interesting is that they're not special cases in the grammar—they're simply expressions that evaluate to themselves. This means you can use a literal anywhere an expression is expected, creating a highly uniform syntax:

```verse
Result := if Condition then 42 else 3.14  # Literals in conditional expression
array{1, 2, 3}                            # Literals in array construction
Point{X:=0.0, Y:=1.0}                     # Literals in object construction
```

### Identifiers and References

Identifiers in Verse serve as references to values, whether they're constants, variables, functions, or types. The language doesn't syntactically distinguish between these different kinds of identifiers—the context determines their meaning. This creates a clean, minimal syntax where the same identifier can represent different entities in different contexts:

```verse
int               # Reference to the int type
GetValue          # Reference to a function
Counter           # Reference to a variable
my_class          # Reference to a class
```

### Parentheses and Grouping

Parentheses serve dual purposes in Verse: they group expressions to control evaluation order, and they create tuple expressions. A parenthesized expression simply evaluates to the value of its contents, allowing you to override the default operator precedence or improve readability:

```verse
(A + B) * C       # Group addition before multiplication
if (X > 0 and Y > 0) then Positive else Negative
```

### Tuples: Lightweight Aggregation

Tuples provide a fundamental way to group multiple values without defining a formal structure. The syntax distinguishes between parentheses used for grouping and those used for tuple construction through the presence of commas:

```verse
()                # Empty tuple
(42,)             # Single-element tuple (note the trailing comma)
(X, Y)            # Two-element tuple
(1, "hello", true) # Mixed-type tuple
```

The trailing comma for single-element tuples might seem unusual, but it's necessary to distinguish between `(42)` as a parenthesized expression evaluating to 42, and `(42,)` as a tuple containing the single element 42. This syntactic clarity ensures that the language remains unambiguous while providing maximum expressiveness.

Tuples can be accessed using function-call syntax with a single integer argument:

```verse
point := (10, 20)
x := point(0)     # Access first element
y := point(1)     # Access second element
```

## Postfix Operations: Building Complexity

Verse builds complex expressions through postfix operations—operations that follow their operand and can be chained together. This creates a left-to-right reading order that feels natural and allows for intuitive composition.

### Member Access

The dot operator provides access to members of objects, modules, and other structured values. Member access expressions evaluate to the value of the specified member:

```verse
player.health           # Access field
config.maxPlayers       # Access nested value
math.sqrt(16)          # Access and call module function
point.x                 # Access struct field
```

Member access can be chained, creating paths through nested structures:

```verse
game.players[0].inventory.items[5].name
```

### Computed Access and Indexing

Square brackets provide computed access to elements, whether for arrays, maps, or other indexable structures. The expression within brackets is evaluated to determine which element to access:

```verse
array[0]                # Array indexing
map["key"]              # Map lookup
matrix[row][col]        # Nested indexing
Data[ComputeIndex()]    # Dynamic index computation
```

Verse also supports an alternative function call syntax using square brackets, allowing `Func[]` as equivalent to `Func()`. This provides stylistic flexibility and can make certain patterns more readable.

### Function Calls

Function calls in Verse use parentheses with comma-separated arguments. The language treats function calls as expressions that evaluate to the function's return value:

```verse
Sqrt(16)                        # Single argument
Max(A, B)                       # Multiple arguments
Initialize()                    # No arguments
Process(GetData(), Transform)   # Nested calls
```

The uniformity of function calls as expressions means they can appear anywhere a value is needed, enabling functional composition patterns:

```verse
Result := Transform(Filter(GetData()))
```

## Object Construction: Creating Instances

Object construction in Verse uses a distinctive brace syntax that clearly indicates the creation of a new instance. The syntax requires explicit field initialization using the `:=` operator:

```verse
Point{X:=10, Y:=20}
Player{Name:="Hero", Level:=1, Health:=100}
Config{
    MaxPlayers := 16,
    EnablePvP := true,
    Difficulty := "normal"
}
```

The use of `:=` for field initialization reinforces that these are binding operations—you're binding values to fields at construction time. Object constructors can be nested, creating complex initialization expressions:

```verse
Game := GameState{
    Player := Player{
        Position := Point{X:=0, Y:=0},
        Inventory := Inventory{Capacity:=20}
    },
    Settings := Config{Difficulty:="hard"}
}
```

## Control Flow as Expressions

One of Verse's most distinctive features is that control flow constructs are expressions, not statements. This means that if-expressions, loops, and case expressions all produce values that can be used in larger expressions.

### Conditional Expressions

The if-then-else construct in Verse is an expression that evaluates to one of two values based on a condition:

```verse
Result := if X > 0 then "positive" else "negative"
Value := if Condition then ComputeA() else ComputeB()
```

The else clause can be omitted, though this affects the type of the expression. Verse supports multiple syntactic forms for if-expressions, including parenthesized conditions and indented bodies:

```verse
# Standard form
if Condition then Value1 else Value2

# Parenthesized condition
if (ComplexCondition()) then Value1 else Value2

# Indented form
if:
    Condition
then:
    Value1
else:
    Value2
```

### For Expressions: Iteration as Computation

For expressions in Verse iterate over collections and produce values. The basic form iterates over elements:

```verse
for (Item : Collection) { Process(Item) }
```

An extended form provides access to both index and item:

```verse
for (Index -> Item : Collection) {
    Print("Item at {Index} is {Item}")
}
```

Since for expressions are expressions, they can produce values and be composed with other expressions. The body of a for expression is evaluated for each iteration, and the expression as a whole has a value determined by these evaluations.

### Loop Expressions: Unbounded Iteration

Loop expressions provide indefinite iteration, continuing until explicitly terminated through failure or other control flow:

```verse
loop {
    Value := GetNext()
    if Done(Value) then break
    Process(Value)
}
```

The loop construct can use indented syntax for clarity:

```verse
loop:
    UpdateState()
    CheckConditions()
    PerformAction()
```

### Case Expressions: Pattern-Based Selection

Case expressions provide multi-way branching based on value matching:

```verse
description := case(color) {
    Red => "Danger",
    Yellow => "Warning",
    Green => "Safe",
    _ => "Unknown"
}
```

The `_` pattern serves as a catch-all, ensuring the case expression is exhaustive. Case expressions evaluate to the value of the matched branch, making them useful for value computation as well as control flow.

## Lambda Expressions: Functions as Values

Lambda expressions create anonymous functions, treating functions as first-class values that can be passed around and composed:

```verse
increment := x => x + 1
add := (x, y) => x + y
constant := () => 42
```

The arrow syntax (`=>`) clearly separates parameters from the body, and the body is an expression whose value becomes the lambda's return value. Lambdas capture their environment, creating closures:

```verse
multiplier := factor => (x => x * factor)
double := multiplier(2)
result := double(21)  # Returns 42
```

## Binary Operations: Combining Values

Binary expressions in Verse follow a carefully designed precedence hierarchy that balances mathematical conventions with programming practicality. Understanding this hierarchy is crucial for writing correct expressions without excessive parentheses.

### Assignment and Binding

At the lowest precedence level, assignment operators bind values to identifiers. The `:=` operator creates immutable bindings, while `=` performs mutable assignment:

```verse
x := 42           # Immutable binding
y := x * 2        # Binding to computed value
z := w := 10      # Right-associative chaining
```

Assignment operators are right-associative, meaning that `a := b := c` groups as `a := (b := c)`. This allows for natural chaining of assignments while maintaining clarity about evaluation order.

Compound assignments provide shorthand for common update patterns:

```verse
Counter += 1      # Equivalent to: set Counter = Counter + 1
Total *= Factor   # Equivalent to: set Total = Total * Factor
```

### Range Expressions

The range operator (`..`) creates ranges for iteration and bounds checking:

```verse
1..10             # Range from 1 to 10
Start..End        # Variable-defined range
for (I : 0..Count) { Process(I) }
```

Ranges are expressions that produce values, allowing them to be stored and passed around:

```verse
ValidRange := 0..100
if Value in ValidRange then Accept() else Reject()
```

### Logical Operations

Logical operators combine boolean values with short-circuit evaluation. Verse uses keyword operators (`and`, `or`, `not`) rather than symbols, improving readability:

```verse
if X > 0 and Y > 0 then ProcessQuadrant1()
Result := Validated or UseDefault()
if not IsReady() then Wait()
```

The precedence ensures that `and` binds tighter than `or`, matching mathematical logic conventions:

```verse
# Evaluates as: (A and B) or (C and D)
Condition := A and B or C and D
```

### Comparison Operations

Comparison operators produce boolean values and can be chained for range checking:

```verse
if 0 <= Value <= 100 then InRange()
IsValid := X > Minimum and X < Maximum
Same := A == B
Different := X != Y
```

All comparison operators have the same precedence and are evaluated left-to-right, allowing natural mathematical notation for range checks.

### Arithmetic Operations

Arithmetic operations follow standard mathematical precedence, with multiplication and division binding tighter than addition and subtraction:

```verse
result := a + b * c      # Multiplication first
average := (a + b) / 2   # Parentheses override precedence
remainder := total % pageSize
```

Unary operators have the highest precedence among arithmetic operations:

```verse
negative := -value
inverted := not flag
result := -x * y    # Unary minus applies to x only
```

## Set Expressions: Mutation in a Functional World

While Verse emphasizes immutability, practical programming often requires mutation. Set expressions provide controlled mutation of variables and mutable fields:

```verse
set Counter = 0
set Player.Health = MaxHealth
set Grid[X][Y] = NewValue
set Cache["key"] = ComputeValue()
```

Set expressions are themselves expressions, though they're typically used for their side effects rather than their value. The left-hand side must be a valid lvalue—something that can be assigned to:

```verse
set X = 10                    # Variable assignment
set Obj.Field = Value         # Field assignment
set Arr[Index] = Element      # Array element assignment
set Map[Key] = MappedValue    # Map entry assignment
```

Complex lvalues are supported, allowing updates deep within data structures:

```verse
set Game.Players[CurrentPlayer].Inventory.Items[Slot] = NewItem
```

## Compound and Block Expressions

Compound expressions, delimited by braces, group multiple expressions into a single expression. The value of a compound expression is the value of its last sub-expression:

```verse
Result := {
    Temp := ComputeIntermediate()
    Adjustment := CalculateAdjustment(Temp)
    Temp + Adjustment
}
```

Compound expressions create new scopes for variables, allowing local bindings that don't affect the enclosing scope:

```verse
{
    X := 10    # Local to this block
    Y := 20
    X + Y
}              # X and Y no longer accessible
```

Expressions within a compound can be separated by semicolons, commas, or newlines, though mixing separators is discouraged in newer versions of Verse:

```verse
{ A; B; C }           # Semicolon separation
{ A, B, C }           # Comma separation
{                     # Newline separation
    a
    b
    c
}
```

## Array Expressions: Collections as Values

Array expressions create array values using the `array` keyword followed by elements in braces:

```verse
numbers := array{1, 2, 3, 4, 5}
empty := array{}
mixed := array{1, "two", 3.0}  # Mixed types if allowed
```

Arrays can also be constructed using indented syntax for clarity with longer lists:

```verse
Colors := array:
    "red"
    "green"
    "blue"
    "yellow"
```

Array expressions are first-class values that can be passed to functions, returned from functions, and stored in variables:

```verse
ProcessArray(array{1, 2, 3})
GetDefaultValues() := array{0, 0, 0}
```

## Type Expressions: Computing with Types

Verse's `type{}` construct represents one of its most sophisticated features—the ability to compute with types themselves. This construct takes an expression and produces its type as a value:

```verse
MyType := type{GetValue()}          # Type of function call
ElementType := type{array[0]}       # Type of array element
ResultType := type{a + b}           # Type of expression result
```

Type expressions enable generic programming patterns without traditional template syntax. You can write functions that accept types computed from expressions:

```verse
Process(Value : type{Compute()}) : type{Transform()} =
    Transform(Value)
```

This becomes particularly powerful with function types, where you can capture complex signatures including effects:

```verse
ValidatorType := type{_(:int)<decides> : void}
Validator : ValidatorType = CheckValue

ProcessorType := type{_(:string)<transacts> : int}
Processor : ProcessorType = ProcessData
```

The underscore in function type expressions represents a placeholder for the function name, focusing on the signature rather than the identity.

## Expression Composition: The Power of Uniformity

The true power of Verse's expression system emerges when different expression types are composed. Since everything is an expression, components can be combined in ways that would be impossible or awkward in statement-oriented languages:

```verse
# Control flow in initialization
Player := player{
    Health := if IsHardMode then 50 else 100,
    Position := point{
        X := for (I : 0..10) { if ValidPosition(I) then break I },
        Y := 0
    }
}

# Nested expressions in function calls
Result := Process(
    if NeedsFiltering then Filter(Data) else Data,
    Transform(X => X * 2)
)

# Lambda with complex body
Operation := X => {
    Validated := Verify(X)
    Transformed := Transform(Validated)
    Finalize(Transformed)
}
```

This composability extends to the type system, where type expressions can be embedded within other constructs:

```verse
# Array of computed type
Handlers : []type{_(:event)<decides>:void} = [H1, H2, H3]

# Map with computed value type
Cache : [string]type{ComputeValue()} = map{}
```
