# Verse Language Grammar

This document describes the  grammar for the Verse programming language.

**!!THIS IS AN APPROXIMATION. DO WE FEEL IT IS HELPFUL?**

## Lexical Elements

### Keywords

The parser recognizes several categories of keywords:

**Specifiers (in angle brackets `<>`):**

```
abstract, computes, private, public, protected, final, decides, inline,
native, override, suspends, transacts, internal, reads, writes, allocates,
scoped, converges, castable, concrete, unique, final_super, open, closed,
native_callable, module_scoped_var_weak_map_key, epic_internal
```

**Block-forming Keywords (followed by `:`):**

```
if, then, else, for, block, loop, array, case
```

**Data Structure Keywords:**

```
module, interface, class, struct, enum
```

**Declaration Keywords:**

```
var, set, using
```

**Type Keywords:**

```
int, float, string, logic, char, any, void, option, comparable, rational, type
```

**Reserved Words:**

```
do, while, break, return, yield, spawn, sync, race
```

### Operators

**Arithmetic:**

```
+    -    *    /    %
```

**Assignment:**

```
:=          # Definition/constant assignment (right-associative)
=           # Mutable assignment
+=  -=  *=  /=    # Compound assignment
```

**Comparison:**

```
==   !=   <   <=   >   >=
```

**Logical:**

```
and   or   not      # Keyword operators (not &&, ||, !)
```

**Special:**

```
..    # Range operator (1..10)
->    # Arrow (used in for loops)
=>    # Lambda arrow (x => x + 1)
.     # Member access
[]    # Indexing/computed access or function call
()    # Function call or grouping
{}    # Object constructor or compound expression
```

### Literals

**Numeric:**

```
INTEGER: 42, -17, 0, 0x1F, 0b1010
FLOAT:   3.14, -0.5, 1.0e10, .5, 3.
```

**String:**

```
STRING: "hello", 'world'
Escape sequences: \n, \t, \r, \\, \", \', \b, \f, \uXXXX
```

**Boolean:**

```
true, false    # Parsed as identifiers, not dedicated boolean tokens
```

### Comments

```
# Single line comment
<# Multi-line
   comment #>
```

## Declarations

### Program Structure

```
program = initial_trivia? using_statement* declaration*

using_statement = "using" "{" package_list "}"
package_list = package_name ("," package_name)*
package_name = ("/" identifier)+
```

### Variable Declarations

**Constant Declarations:**

```
const_decl = identifier specifiers? ":" type? ("=" expression)?
           | identifier specifiers? ":=" (expression | type)

Examples:
X := 42                    # Type inference with initializer
Name := "Alice"            # String constant
X : int = 42              # Explicit type with initializer
Pi : float = 3.14159      # Float constant
Count : int               # No initializer (default value)
X<public> := 42           # With specifier

# Type aliases (new functionality)
Numbers := []float         # Array type alias
Matrix := [][]int          # Multi-dimensional array type
Coords := tuple(float, float)  # Tuple type alias
IntPredicate := type{_(:int)<transacts><decides> : void}  # Function type alias

# Object constructor assignments
Point := point{X:=10, Y:=20}          # Object initialization
Player := player{Name:="hero", Level:=1}  # Complex object
Nested := config{Db:=database{Host:="localhost"}}  # Nested objects
```

**Variable Declarations:**

```
var_decl = "var" identifier specifiers? ":" type ("=" expression)?

Examples:
var X : int = 42          # Mutable variable with initializer
var Y : string            # Mutable variable without initializer
var Counter<public> : int = 0    # With specifier
```

### Function Declarations

```
func_decl = identifier visibility_specifier? "(" param_list? ")" post_specifiers? (":" type)? ("=" | ":=") expression
          | pre_specifiers? identifier "(" param_list? ")" post_specifiers? (":" type)? ("=" | ":=") expression
          | identifier visibility_specifier? "(" param_list? ")" post_specifiers? (":" type)?    # Interface signature

param_list = param ("," param)*
param = identifier ":" type

# Specifier Categories
visibility_specifier = "<" ("public" | "private" | "protected" | "internal" | "scoped") ">"
post_specifiers = specifier_list     # Non-visibility specifiers after parameters
pre_specifiers = specifier_list      # Specifiers before function name

specifier_list = "<" specifier ("," specifier)* ">"
specifier = "public" | "private" | "protected" | "internal" | "scoped"     # Visibility specifiers
          | "decides" | "suspends" | "transacts" | "override" | "abstract"  # Behavior specifiers
          | "final" | "native" | "inline" | "reads" | "writes" | ...        # Other specifiers

Examples:
F() := 42                             # Simple function
Calculate(X: int, Y: string) := X + 1 # With parameters
GetValue() : int = 42                 # With return type
MyFunc<public>() := Body              # With visibility specifier
Process<private>(X: int)<decides> := Body  # Visibility + behavior specifiers
<decides>ValidateInput(X: int) := X > 0    # Pre-specifier (behavior)
F()<decides><suspends> := Body        # Multiple post-specifiers
ProcessData(Input: string) : string   # Interface signature (no body)
```

**Important Notes:**

- **Visibility specifiers** (public, private, protected, internal, scoped) affect access control
- **Behavior specifiers** (decides, suspends, transacts, override, etc.) affect function behavior
- Visibility specifiers after function name are separated in the logical AST for clarity
- Multiple behavior specifiers can be combined, but only one visibility level applies

### Data Structure Declarations

#### Assignment Syntax

```
data_struct_decl = identifier specifiers? ":=" kind specifiers? ("(" argument ")")? ("{" body "}" | ":" indented_body)

kind = "module" | "interface" | "class" | "struct" | "enum"

Examples:
my_class := class<concrete> { Field := 42 }
point := struct { X : float; Y : float }
color := enum { Red, Green, Blue }
icontract := interface { Method() : int }
utils := module { Helper() := 42 }
derived_class := class<final>(base_class) { Body }
```

#### Indented Bodies

Both syntaxes support indented bodies:

```
my_class := class:
    Field := 42
    Method() := Field

class my_class:
    Field := 42
    Method() := Field
```

## Expressions

### Primary Expressions

```
primary = literal
        | identifier
        | "(" expression ")"
        | tuple_expression
        | object_constructor
        | array_expression
        | compound_expression

literal = INTEGER | FLOAT | STRING

tuple_expression = "(" ")"                        # Empty tuple
                 | "(" expression "," ")"         # Single-element tuple (trailing comma)
                 | "(" expression ("," expression)+ ")"  # Multi-element tuple

Examples:
42              # Integer literal
3.14            # Float literal
"hello"         # String literal
Identifier      # Variable reference
(Expr)          # Parenthesized expression
()              # Empty tuple
(1, 2)          # Two-element tuple
(X, Y, Z)       # Three-element tuple
("hello", 42, true)  # Mixed-type tuple
```

### Member Access and Calls

```
postfix = primary postfix_op*
postfix_op = "." identifier           # Member access
           | "[" expression "]"       # Computed access
           | "(" argument_list? ")"   # Function call or tuple access

argument_list = expression ("," expression)*

# Note: Single-argument function calls like tuple(0) are treated as tuple access
# Multi-argument or no-argument calls are regular function calls

Examples:
Obj.Property       # Dot notation
Arr[Index]         # Computed access
Func(Args)         # Function call
MyTuple(0)         # Tuple element access (single argument)
MyTuple(1)         # Access second element
Obj.Method()       # Method call
Matrix[I][J]       # Chained access
Func[]             # Bracket call syntax
```

### Object Construction

```
object_constructor = identifier "{" field_list? "}"
field_list = field ("," field)* ","?
field = identifier ":=" expression

Examples:
point{X:=1, Y:=2}           # Object constructor
point{X:=1, Y:=2,}          # Trailing comma allowed
empty{}                     # Empty constructor
Player := point{X:=10, Y:=20}      # In assignments
Config := config{
  MaxPlayers := 100,
  EnablePvP := true
}                           # Multi-line constructor
Nested := player{Pos:=point{X:=0, Y:=0}}  # Nested constructors
```

### Array Expressions

```
array_expression = "array" "{" expression_list? "}"
                 | "array" ":" indented_expression_list

expression_list = expression ("," expression)* ","?

Examples:
array{1, 2, 3}              # Braced array
array:                      # Indented array
    1
    2
    3
```

### Compound Expressions

```
compound_expression = "{" statement_list? "}"
                    | "{" ":" indented_statement_list "}"

statement_list = statement (";" statement)* ";"?
statement = expression

Examples:
{ A; B; C }                 # Semicolon-separated
{                           # Newline-separated
    X := 1
    Y := 2
}
```

### Control Flow Expressions

#### If Expressions

```
if_expression = "if" "(" expression ")" "then" expression ("else" expression)?
              | "if" expression "then" expression ("else" expression)?
              | "if" ":" expression "then" ":" expression ("else" ":" expression)?

Examples:
if Condition then Value else Other
if: Condition then: Value else: Other
if Condition then Value     # No else clause
if(X > 0) then Positive else Negative
```

#### For Expressions

```
for_expression = "for" "(" loop_spec ")" expression
               | "for" ":" loop_spec "do" ":" expression

loop_spec = identifier ":" expression                    # for item in collection
          | identifier "->" identifier ":" expression   # for index -> item in collection

Examples:
for(I : 1..10) { Process(I) }
for(Item : Items) { Handle(Item) }
for(I -> Item : Items) { Use(I, Item) }  # With index
for: Range do: Body                       # Indented syntax
```

#### Loop Expressions

```
loop_expression = "loop" expression
                | "loop" ":" indented_expression

Examples:
loop Body
loop:
    Statement1
    Statement2
```

#### Case Expressions

```
case_expression = "case" "(" expression ")" "{" case_list "}"
                | "case" "(" expression ")" ":" indented_case_list

case_list = case_item ("," case_item)* ","?
case_item = expression "=>" expression
          | "_" "=>" expression              # Default case

Examples:
case(Value) { 0 => "zero", 1 => "one", _ => "other" }
case(Value):
    0 => "zero"
    1 => "one"
    _ => "other"
```

### Lambda Expressions

```
lambda = identifier "=>" expression                    # Single parameter
       | "(" param_list? ")" "=>" expression          # Multiple parameters

Examples:
X => X + 1              # Single parameter
(X, Y) => X + Y         # Multiple parameters
() => 42                # No parameters
```

### Binary Expressions

**Operator Precedence (lowest to highest):**

1. **Assignment** (right-associative): `:=`, `=`, `+=`, `-=`, `*=`, `/=`
2. **Range**: `..`
3. **Lambda**: `=>`
4. **Logical OR**: `or`
5. **Logical AND**: `and`
6. **Comparison**: `<`, `<=`, `>`, `>=`, `==`, `!=`
7. **Addition**: `+`, `-`
8. **Multiplication**: `*`, `/`, `%`
9. **Unary**: `-`, `not`
10. **Postfix**: `.`, `[]`, `()`

```
Examples:
X := Y := Z             # Right associative assignment
A + B * C               # Multiplication has higher precedence
X and Y or Z            # Left associative logical operators
1..10                   # Range expression
Start..End              # Variable range
```

### Set Expressions

```
set_expression = "set" lvalue "=" expression

lvalue = identifier
       | postfix_expression

Examples:
set X = NewValue             # Mutable reassignment
set Obj.Prop = Value         # Member assignment
set Arr[I] = Item            # Array element assignment
set X[21] = 123              # Array with spaces in brackets
set Matrix[I + 1] = Value    # Expression as index
set Grid[X][Y] = NewValue    # Nested array access
set Scores["player1"] = 100  # Map/dictionary access
set Obj.Data[Index] = Val    # Member + array combination
```

## Types

### Basic Types

```
type = basic_type modifier*
     | tuple_type
     | map_type
     | type_expression

basic_type = "int" | "float" | "string" | "logic" | "char" | "any" | "void" | "option"
           | "comparable" | "rational" | "tuple"
           | identifier                    # User-defined types

tuple_type = "tuple" "(" type_list ")"
           | "tuple" "(" ")"               # Empty tuple type

map_type = "[" type "]" type             # Map type: [keytype]valuetype

type_expression = "type" "{" expression "}"    # Type of arbitrary expression

type_list = type ("," type)*

modifier = "[]"                          # Array type
         | "?"                           # Optional type

Examples:
int                     # Integer type
[]int                   # Array of integers
[][]string              # 2D array of strings
?int                    # Optional integer
[]?string               # Array of optional strings
option<int>             # Option type (generic)
option<string>          # Option of string
option<[]int>           # Option of array
weak_map(session, int)  # Parameterized type with parentheses
comparable              # Built-in comparable type
rational                # Built-in rational type
tuple(int, string)      # Two-element tuple type
tuple(float, float, float)  # Three-element tuple type
tuple()                 # Empty tuple type
tuple(string, ?int, logic)  # Mixed tuple with optional element
?tuple(int, int)        # Optional tuple type
[]tuple(string, int)    # Array of tuples
[string]int             # Map from string to int
[UserID]User            # Map from UserID to User
[string][]int           # Map from string to array of ints
[][string]User          # Array of maps from string to User
?[string]int            # Optional map type
[tuple(int, string)]User # Map with tuple key type
type{Foo()}             # Type of function call expression
type{GetValue()}        # Type of another function call
type{Obj.Method()}      # Type of member access expression
type{Arr[0]}            # Type of array access expression
type{X + Y * 2}         # Type of arithmetic expression
```

### Type Expression Construct

The `type{expression}` construct allows you to get the type of any arbitrary expression, similar to `decltype` in C++. This is particularly useful for:

1. **Function Type Inference**: Getting the type signature of functions
2. **Generic Programming**: Working with types that depend on expressions
3. **Template-like Functionality**: Creating type-safe abstractions

#### Basic Usage Examples

```
# Basic type inference
Foo() : int = 0
Bar(X : type{Foo()}) : type{Foo()} = X

# Function parameter types
ProcessValue(Callback : type{GetValue()}) : int = Callback()

# Return type inference
GetProcessor() : type{GetValue()} = GetValue

# Variable type inference
Processor := GetValue
Result : type{Processor()} = Processor()

# Complex expression types
Calculate(X : int, Y : int) : int = X + Y
Operation : type{Calculate(1, 2)} = Calculate(3, 4)

# Member access types
Obj.Method() : string = "result"
MemberType : type{Obj.Method()} = Obj.Method()

# Array element types
Arr[0] : int = GetValue()
ArrayType : type{Arr[0]} = Arr[1]
```

#### Advanced Function Type Support

The parser now supports complex function type signatures with effects and specifiers:

```
# Function types with effects
Validator : type{_(:int)<decides> : void} = CheckValue
Processor : type{_(:string)<transacts> : int} = ProcessData

# Multiple parameter function types
Combiner : type{_(X:int, Y:string)<suspends> : float} = ComplexOperation

# Function types in declarations
Handlers : []type{_(:string)<decides> : void} = [Handler1, Handler2]

# Generic function type parameters
ExecuteCallback<t>(Fn : type{_(:t) : t}, Value : t) : t = Fn(Value)

# Complex nested function types
Pipeline : type{_(:type{GetValue()}) : type{ProcessValue()}} = Transform

# Object constructor with function types
config := class:
    Validator : type{_(:int)<decides> : void}
    Processor : type{_(:string) : int}
```

#### Type Expression Grammar

```
type_expression = "type" "{" expression "}"

# Function type signature patterns
function_type_signature = ("_" | "__") "(" parameter_list? ")" specifiers? (":" type)?

parameter_list = parameter ("," parameter)*
parameter = identifier ":" type

specifiers = "<" specifier ("," specifier)* ">"
specifier = "decides" | "suspends" | "transacts" | "reads" | "writes" | ...

Examples:
type{GetValue()}                    # Simple function call type
type{_(:int) : string}             # Function signature type
type{_(:int)<decides> : void}      # Function with effect specifier
type{_(X:int, Y:string) : float}   # Multi-parameter function type
type{Obj.Method()}                 # Member access type
type{Arr[Index]}                   # Array access type
type{x + y * 2}                    # Arithmetic expression type
```

## Syntax Variations

### Braced vs Indented Bodies

Most constructs support both braced and indented syntax:

```
# Braced syntax
if condition then { statements }
class MyClass { field := 42 }

# Indented syntax
if: condition then:
    statements

class MyClass:
    field := 42
```

### Function Call Syntaxes

Functions can be called with either parentheses or brackets:

```
func(arg1, arg2)        # Traditional parentheses
func[arg1, arg2]        # Bracket syntax
func[]                  # Empty brackets
```

## Specifier System

Specifiers provide metadata and modify behavior:

### Specifier Syntax

```
specifier = "<" specifier_name ("{" content "}")? ">"

specifier_name = "public" | "private" | "abstract" | "final" | ...

Examples:
<public>                # Simple specifier
<scoped{MyModule}>      # Specifier with content
```

### Specifier Placement

```
# Before identifiers
<public> MyFunction() := Body
<private> MyVariable := Value

# After types
MyFunction() <decides> := Body
MyVariable : int <final> = Value

# Multiple specifiers
<public> MyFunction() <decides> <suspends> := Body
```

## Advanced Features

### Concurrent Programming Constructs

Verse provides first-class support for concurrent and parallel execution:

```
concurrent_expr = spawn_expr | race_expr | sync_expr | branch_expr

spawn_expr = "spawn" "{" expression "}"                    # Braced form
           | "spawn" ":" indented_expression               # Indented form

race_expr = "race" ":" indented_expression_list            # First-wins semantics

sync_expr = "sync" ":" indented_expression_list            # Synchronized execution

branch_expr = "branch" "{" expression_list "}"             # Braced form
            | "branch" ":" indented_expression_list        # Indented form

Examples:
# Spawn - async execution
spawn{DoWork()}
spawn{Player.Teleport(Location)}
spawn:
    TaskA()
    TaskB()

# Race - first to complete wins
race:
    FastPath()
    SlowPath()
    TimeoutHandler()

# Sync - synchronized operations
sync:
    Database.Write(Data1)
    Database.Write(Data2)
    Database.Commit()

# Branch - parallel execution paths
branch:
    ProcessRoute1()
    ProcessRoute2()
    ProcessRoute3()

# Nested constructs
spawn:
    race:
        FastCompute()
        SlowCompute()
```

**Concurrent Features:**

- ✅ **Spawn expressions**: Braced form fully supported
- ⚠️  **Indented forms**: Some complex patterns need enhancement
- ✅ **Nested constructs**: `spawn{race: ...}` patterns work
- ✅ **Real-world usage**: Fortnite/UEFN patterns supported

### Tuple System

Advanced tuple support with both expressions and types:

```
tuple_expr = "(" ")"                                      # Empty tuple
           | "(" expression "," ")"                       # Single-element
           | "(" expression ("," expression)+ ")"         # Multi-element

tuple_type = "tuple" "(" type_list ")"
           | "tuple" "(" ")"

Examples:
# Tuple expressions
()                      # Empty tuple
(x, y)                  # Two-element tuple
(1, 2, 3)              # Three-element tuple
("hello", 42, true)    # Mixed-type tuple

# Tuple types in declarations
var Pos : tuple(int, int) = (0, 0)
var Empty : tuple() = ()
var Coords : tuple(float, float, float) = (0.0, 1.0, 2.0)

# Function parameters
Distance(P1: tuple(int, int), P2: tuple(int, int)) : float = ...
```

### Type System Extensions

```
type_expr = "type" "{" expression "}"                     # Type of expression

map_type = "[" type "]" type                              # Map type

Examples:
# Type expressions (partial support)
type{Foo()}             # Type of function call
type{GetValue()}        # Type of method call
type{X + Y * 2}         # Type of arithmetic expression

# Map types (full support)
var Lookup : [string]int                    # String to int map
var Users : [user_id]user                    # UserID to User map
var Nested : [string][]int                  # String to int array map
var Complex : [][string]user                # Array of string-to-User maps
```

### Decorator Support

```
decorator = "@" identifier ("(" argument_list? ")")?

Examples:
@deprecated
MyFunction() := Body

@custom(Param1, Param2)
my_class := class { }

@editable
BranchingLogic<public>():void = branch: MainPath(); AlternatePath()
```

### Comments and Trivia

```
# Single line comment
<# Multi-line comment
   spanning multiple lines #>

# Comments are preserved in AST for source reconstruction
```

### Control Flow Statements

```
# Control flow statements
break                   # Break from loop
continue               # Continue to next iteration
return                 # Return from function
return expression      # Return with value

# Concurrent constructs
spawn expression       # Spawn concurrent task
yield expression       # Yield in generators (reserved)
```

## Grammar Summary

```
program = initial_trivia? using_statement* declaration*

declaration = const_decl | var_decl | func_decl | data_struct_decl

expression = assignment_expr

assignment_expr = range_expr ((":" | ":=" | "=" | "+=" | "-=" | "*=" | "/=") assignment_expr)?

range_expr = lambda_expr (".." lambda_expr)?

lambda_expr = logical_or_expr ("=>" lambda_expr)?

logical_or_expr = logical_and_expr ("or" logical_and_expr)*

logical_and_expr = comparison_expr ("and" comparison_expr)*

comparison_expr = additive_expr (("==" | "!=" | "<" | "<=" | ">" | ">=") additive_expr)*

additive_expr = multiplicative_expr (("+" | "-") multiplicative_expr)*

multiplicative_expr = unary_expr (("*" | "/" | "%") unary_expr)*

unary_expr = ("-" | "not" | "*") unary_expr | postfix_expr

# The * operator is tuple expansion (splatting) for function calls
# Examples:
# F(*Tuple)        # Expand tuple elements as function arguments
# Combine(*A, *B)  # Multiple expansions in one call

postfix_expr = primary_expr postfix_op*

postfix_op = "." identifier | "[" expression "]" | "(" argument_list? ")"

primary_expr = literal | identifier | "(" expression ")" | object_constructor
             | array_expr | compound_expr | if_expr | for_expr | loop_expr | case_expr
```
