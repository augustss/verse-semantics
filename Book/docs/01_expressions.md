# Expressions

Everything is an expression. This fundamental design principle sets the language apart from many traditional programming languages where statements and expressions are distinct concepts. Every piece of code you write produces a value, even constructs you might expect to be purely side-effecting. This creates a powerful and consistent programming model where code can be composed and combined in ways that feel natural and predictable.

## Primary Expressions

At the foundation of the expression system lie the primary expressions—the atomic units from which more complex expressions are built. These include literals, identifiers, parenthesized expressions, and the powerful tuple construct that provides lightweight data aggregation.

### Literals and Basic Values

The language supports the expected set of literal values, but treats them uniformly as expressions. Integer literals can be written in decimal (42, -17), hexadecimal (0x1F), or binary (0b1010) notation. Floating-point literals support standard notation (3.14, -0.5) as well as scientific notation (1.0e10) and abbreviated forms (.5, 3.). String literals can use either double or single quotes and support escape sequences for special characters.

What makes the treatment of literals interesting is that they're not special cases in the grammar—they're simply expressions that evaluate to themselves. This means you can use a literal anywhere an expression is expected, creating a highly uniform syntax:

<!--NoCompile-->
```verse
Result := if (Condition) then 42 else 3.14  # Literals in conditional expression
array{1, 2, 3}                              # Literals in array construction
Point{X:=0.0, Y:=1.0}                       # Literals in object construction
```

### Identifiers and References

Identifiers serve as references to values, whether they're constants, variables, functions, or types. The language doesn't syntactically distinguish between these different kinds of identifiers—the context determines their meaning. This creates a clean, minimal syntax where the same identifier can represent different entities in different contexts:

<!--NoCompile-->
```verse
int               # Reference to the int type
GetValue()        # Reference to a function
Counter           # Reference to a variable
my_class          # Reference to a class
```

### Parentheses and Grouping

Parentheses serve dual purposes: they group expressions to control evaluation order, and they create tuple expressions. A parenthesized expression simply evaluates to the value of its contents, allowing you to override the default operator precedence or improve readability:

<!--NoCompile-->
```verse
(A + B) * C       # Group addition before multiplication
if (X > 0 and Y > 0) then Positive else Negative
```

### Tuples: Lightweight Aggregation

Tuples provide a fundamental way to group two or more values without defining a formal structure. The syntax distinguishes between parentheses used for grouping and those used for tuple construction through the presence of commas:

<!--NoCompile-->
```verse
(X, Y)            # Two-element tuple
(1, "hello", true) # Mixed-type tuple
```

Tuples can be accessed using function-call syntax with a single integer argument:

<!--NoCompile-->
```verse
point := (10, 20)
x := point(0)     # Access first element
y := point(1)     # Access second element
```

Tuple types are written:

<!--NoCompile-->
```verse
tuple(int,int)
tuple(int,string,logic)
```

While the type of an unary element can be accepted by the compiler, `tuple(int)`, there is currently no syntax to write a tuple of one element.  

<!-- 
TODO: Check the above, I saw a mention of (1,)  <= not the trailing comma

TODO: What about tuples of zero element.   The following is accepted:

    pt : tuple() := ()

-->

## Postfix Operations: Building Complexity

Verse builds complex expressions through postfix operations—operations that follow their operand and can be chained together. This creates a left-to-right reading order that feels natural and allows for intuitive composition.

### Member Access

The dot operator provides access to members of objects, modules, and other structured values. Member access expressions evaluate to the value of the specified member:

<!--NoCompile-->
```verse
Player.Health           # Access field
Config.MaxPlayers       # Access nested value
math.Sqrt(16)           # Access and call module function
Point.X                 # Access struct field
```

Member access can be chained, creating paths through nested structures:

<!--NoCompile-->
```verse
Game.Players[0].Inventory.Items[5].Name
```

### Computed Access and Indexing

Square brackets provide computed access to elements, whether for arrays, maps, or other indexable structures. The expression within brackets is evaluated to determine which element to access:

<!--NoCompile-->
```verse
Array[0]                # Array indexing
Map["key"]              # Map lookup
Matrix[Row][Col]        # Nested indexing
Data[ComputeIndex()]    # Dynamic index computation
```

The function call syntax with square brackets, `Func[]` is equivalent to `Func()` for functions that may fail.

### Function Calls

Function calls use parentheses with comma-separated arguments. The language treats function calls as expressions that evaluate to the function's return value:

<!--NoCompile-->
```verse
Sqrt(16)                        # Single argument
Max(A, B)                       # Multiple arguments
Initialize()                    # No arguments
Process[GetData(), Transform]   # Nested calls, outer call may fail
```

## Object Construction: Creating Instances

Object construction uses a distinctive brace syntax that clearly indicates the creation of a new instance. The syntax requires explicit field initialization using the `:=` operator:

<!--
point := struct{ X:int, Y:int }
player := struct{Name:string, Level:int, Health:int}
config := struct { MaxPlayers:int, Difficulty:string, EnablePvP:logic }
F():void={
-->
```verse
point{X:=10, Y:=20}
player{Name:="Hero", Level:=1, Health:=100}
config{
    MaxPlayers := 16,
    EnablePvP := true,
    Difficulty := "normal"
}
```
<!--
}
-->

The use of `:=` for field initialization reinforces that these are binding operations—you're binding values to fields at construction time. Object constructors can be nested, creating complex initialization expressions:

<!--
game_state:=struct{Player:player, Settings:config}
config:=struct{Difficulty:string}
player:=struct{ Position:point, Inventory:inventory}
point:=struct{ X:int, Y:int}
inventory:=struct{Capacity:int}
F():void={
-->
```verse
Game := game_state{
    Player := player{
        Position := point{X:=0, Y:=0},
        Inventory := inventory{Capacity:=20}
    },
    Settings := config{Difficulty:="hard"}
}
```
<!--
}
-->

## Control Flow as Expressions

One of Verse's distinctive features is that control flow constructs are expressions, not statements. This means that if-expressions, loops, and case expressions all produce values that can be used in larger expressions.

### Conditional Expressions

The if-then-else construct is an expression that evaluates to one of two values based on a condition:

<!--
ComputeA():int=1
ComputeB():int=1
F(X:int,Condition:logic):void={
-->
```verse
Result := if (X > 0) then "positive" else "negative"
Value := if (Condition=true) then ComputeA() else ComputeB()
```
<!--
}
-->

The else clause can be omitted, though this affects the type of the expression. Verse supports multiple syntactic forms for if-expressions, including parenthesized conditions and indented bodies:

<!--NoCompile-->
```verse
# Standard form
if (Condition) then Value1 else Value2

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

For expressions iterate over collections and produce values. The basic form iterates over elements:

<!--verse
Process(i:int):void={}
F(Collection:[]int):void={
-->
```verse
for (Item : Collection) { Process(Item) }
```
<!--verse
}
-->

An extended form provides access to both index and item:

<!--verse
Print(S:string):void={}
Process(i:int):void={}
F(Collection:[]int):void={
-->
```verse
for (Index -> Item : Collection) {
    Print("Item at {Index} is {Item}")
}
```
<!--verse
}
-->

Since for expressions are expressions, they can produce values and be composed with other expressions. The body of a for expression is evaluated for each iteration, and the expression as a whole has a value determined by these evaluations.

### Loop Expressions: Unbounded Iteration

Loop expressions provide indefinite iteration, continuing until explicitly terminated through failure or other control flow:

<!--verse
GetNext():int=1
Done(i:int)<computes><decides>:void={}
Process(i:int):void={}
F():void={
-->
```verse
loop {
    Value := GetNext()
    if (Done[Value]) then break
    Process(Value)
}
```
<!--verse
}
-->

The loop construct can use indented syntax for clarity.

### Case Expressions: Pattern-Based Selection

Case expressions provide multi-way branching based on value matching:

<!--verse
color := enum:
    Red
    Yellow
    Green
    Other
F(Color:color): void={
-->
```verse
Description := case(Color) {
    color.Red => "Danger",
    color.Yellow => "Warning",
    color.Green => "Safe",
    _ => "Unknown"
}
```
<!--verse
}
-->

The `_` pattern serves as a catch-all, ensuring the case expression is exhaustive. Case expressions evaluate to the value of the matched branch, making them useful for value computation as well as control flow.

## Lambda Expressions: Functions as Values

<!-- TODO: Not yet true -->

Lambda expressions create anonymous functions, treating functions as first-class values that can be passed around and composed:

<!--NoCompile-->
```verse
Increment := X => X + 1
Add := (X, Y) => X + Y
Constant := () => 42
```

The arrow syntax (`=>`) clearly separates parameters from the body, and the body is an expression whose value becomes the lambda's return value. Lambdas capture their environment, creating closures:

<!--NoCompile-->
```verse
Multiplier := Factor => (X => X * Factor)
Double := Multiplier(2)
Result := Double(21)  # Returns 42
```

## Binary Operations: Combining Values

Binary expressions follow a carefully designed precedence hierarchy that balances mathematical conventions with programming practicality. Understanding this hierarchy is crucial for writing correct expressions without excessive parentheses.

### Assignment and Binding

At the lowest precedence level, assignment operators bind values to identifiers. The `:=` operator creates immutable bindings, while `set =` performs mutable assignment:

<!--verse
F():void={
-->
```verse
X := 42           # Immutable binding
Y := X * 2        # Binding to computed value
Z := W := 10      # Right-associative chaining
```
<!--verse
}
-->

Assignment operators are right-associative, meaning that `a := b := c` groups as `a := (b := c)`. This allows for natural chaining of assignments while maintaining clarity about evaluation order.

Compound assignments provide shorthand for common update patterns:

<!--verse
F()<transacts>:void={
var Counter :int = 0
var Total :int = 0
Factor:=2
-->
```verse
set Counter += 1      # Equivalent to: set Counter = Counter + 1
set Total *= Factor   # Equivalent to: set Total = Total * Factor
```
<!--verse
}
-->

### Range Expressions

The range operator (`..`) creates ranges for iteration and bounds checking:

<!--verse
End()<computes>:int=10
F():void= 
    for (I := 1..10):
        for (J := I..(I+10)):
            for (K:= J..End()) {}

<#
-->
```verse
1..10             # Range from 1 to 10
Start..End        # Variable-defined range
for (I : 0..Count) { Process(I) }
```
<!--verse
#>
-->

<!-- No true
Ranges are expressions that produce values, allowing them to be stored and passed around:

```verse
ValidRange := 0..100
if (Value) in ValidRange then Accept() else Reject()
```
-->

### Logical Operations

Logical operators combine boolean values with short-circuit evaluation. Verse uses keyword operators (`and`, `or`, `not`) rather than symbols, improving readability:

<!--NoCompile-->
```verse
if (X > 0 and Y > 0) then ProcessQuadrant1()
Result := logic{Validated or UseDefault[]}
if (not IsReady[]) then Wait()
```

The precedence ensures that `and` binds tighter than `or`, matching mathematical logic conventions:

<!--NoCompile-->
```verse
# Evaluates as: (A and B) or (C and D)
Condition := logic{ExpA and ExpB or ExpC and ExpD}
```

### Comparison Operations

Comparison operators produce boolean values and can be chained for range checking:

<!--verse
InRange():void={}
F(Value:int, X:int, Minimum:int, Maximum:int, A:int, B:int):void={
-->
```verse
if (0 <= Value <= 100) then InRange()
IsValid := logic{X > Minimum and X < Maximum}
Same := logic{A == B}
Different := logic{X != Y}
```
<!--verse
}
-->

All comparison operators have the same precedence and are evaluated left-to-right, allowing natural mathematical notation for range checks.

### Arithmetic Operations

Arithmetic operations follow standard mathematical precedence, with multiplication and division binding tighter than addition and subtraction:

<!--verse
F():void={
A:=1
B:=2
C:=3
-->
```verse
Result := A + B * C      # Multiplication first
Average := (A + B) / 2   # Parentheses override precedence
```
<!--verse
}
-->

Unary operators have the highest precedence among arithmetic operations:

<!--verse
F():void={
Flag:=true
Value:=1
X:=1
Y:=2
-->
```verse
Negative := -Value
Inverted := logic{not Flag=true}
Result := -X * Y    # Unary minus applies to x only
```
<!--verse
}
-->

## Set Expressions: Mutation in a Functional World

While Verse emphasizes immutability, practical programming often requires mutation. Set expressions provide controlled mutation of variables and mutable fields:

<!--verse
c := class { var Field:int = 0 }
F( Element:int, Value:int, Index:int, Key:string, MappedValue:string)<transacts><decides>:int={
var Obj:c = c{}
var Arr:[]int = array{1}
var Map:[string]string = map{ "hi" => "hp" }
 var X :int=0
-->
```verse
set X = 10                    # Variable assignment
set Obj.Field = Value         # Field assignment
set Arr[Index] = Element      # Array element assignment
set Map[Key] = MappedValue    # Map entry assignment
```
<!--verse
}
-->

Set expressions are themselves expressions, though they're typically used for their side effects rather than their value. The left-hand side must be a valid lvalue—something that can be assigned to.

Complex lvalues are supported, allowing updates deep within data structures:

<!--NoCompile-->
```verse
set Game.Players[CurrentPlayer].Inventory.Items[Slot] = NewItem
```

## Compound and Block Expressions

Compound expressions, delimited by braces, group multiple expressions into a single expression. The value of a compound expression is the value of its last sub-expression:

<!--verse
ComputeIntermediate():int=3
CalculateAdjustement(o:int):int=3
F():void={
-->
```verse
Result := {
    Temp := ComputeIntermediate()
    Adjustment := CalculateAdjustment(Temp)
    Temp + Adjustment
}
```
<!--verse
}
-->

Compound expressions create new scopes for variables, allowing local bindings that don't affect the enclosing scope:

<!--NoCompile-->
```verse
{
    X := 10    # Local to this block
    Y := 20
    X + Y
}              # X and Y no longer accessible
```

Expressions within a compound can be separated by semicolons, commas, or newlines, though mixing separators is discouraged in newer versions of Verse:

<!--NoCompile-->
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

<!--verse
F():void={
-->
```verse
Numbers := array{1, 2, 3, 4, 5}
Empty := array{}
Mixed := array{1, "two", 3.0}  # Mixed types if allowed
```
<!--verse
}
-->

Arrays can also be constructed using indented syntax for clarity with longer lists:

<!--verse
F():void={
-->
```verse
Colors := array:
    "red"
    "green"
    "blue"
    "yellow"
```
<!--verse
}
-->

<!-- Not supported
## Type Expressions: Computing with Types

Verse's `type{}` construct represents one of its most sophisticated features—the ability to compute with types themselves. This construct takes an expression and produces its type as a value:

```verse
MyType := type{GetValue()}          # Type of function call
ElementType := type{array[0]}       # Type of array element
ResultType := type{a + b}           # Type of expression result
```

Type expressions enable generic programming patterns without traditional template syntax. This is particularly powerful with function types, where you can capture complex signatures including effects:

```verse
ValidatorType := type{_(:int)<decides> : void}
Validator : ValidatorType = CheckValue

ProcessorType := type{_(:string)<transacts> : int}
Processor : ProcessorType = ProcessData
```

The underscore in function type expressions represents a placeholder for the function name, focusing on the signature rather than the identity.
-->

## Expression Composition: The Power of Uniformity

The true power of Verse's expression system emerges when different expression types are composed. Since everything is an expression, components can be combined in ways that would be impossible or awkward in statement-oriented languages:

<!-- TODO:  
   Check that this is correct.  (Besides the use of lambdas that we will support some day)
-->

<!--NoCompile-->
```verse
# Control flow in initialization
Player := player{
    Health := if (IsHardMode) then 50 else 100,
    Position := point{
        X := for (I := 0..10) { if (ValidPosition(I)) then return I },
        Y := 0
    }
}

# Nested expressions in function calls
Result := Process(
    if (NeedsFiltering) then Filter(Data) else Data,
    Transform(X => X * 2)
)

# Lambda with complex body
Operation := X => {
    Validated := Verify(X)
    Transformed := Transform(Validated)
    Finalize(Transformed)
}
```

<!-- TODO : this don't seem to work

This composability extends to the type system, where type expressions can be embedded within other constructs:

```verse
# Array of computed type
Handlers : []type{_(:event)<decides>:void} = [H1, H2, H3]

# Map with computed value type
Cache : [string]type{ComputeValue()} = map{}
```

-->
