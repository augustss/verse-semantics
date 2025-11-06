# Mutability

Immutability is the default in Verse. When you create a value, it stays that value forever — unchanging, predictable, and safe to share. This foundational principle makes programs easier to reason about, eliminates entire categories of bugs, and enables powerful optimizations. But games are dynamic worlds where state constantly evolves: health decreases, scores increase, inventories change. Verse embraces both paradigms, providing immutability by default while offering controlled, explicit mutation when you need it.

The distinction between immutable and mutable data in Verse goes deeper than just whether values can change. It fundamentally affects how data flows through your program, how values are shared between functions, and how the compiler reasons about your code. Understanding this distinction is crucial for writing efficient, correct Verse programs.

## The Pure Foundation

In Verse's pure fragment, computation happens without side effects. Values are created but never modified. Functions transform inputs into outputs without changing anything along the way. This isn't a limitation — it's a powerful foundation that makes code predictable and composable.

<!--NoCompile-->
```verse
# Immutable values and structures
point := struct<computes>:
    X:float = 0.0
    Y:float = 0.0

Origin := point{}
UnitX := point{X := 1.0}
UnitY := point{Y := 1.0}

# These values are eternal - Origin will always be (0, 0)
Distance(P1:point, P2:point)<reads>:float =
    DX := P2.X - P1.X
    DY := P2.Y - P1.Y
    Sqrt(DX * DX + DY * DY)
```

In this pure world, equality means structural equality — two values are equal if they have the same shape and content. For primitive types and structs, this happens automatically. For classes, which have identity beyond their content, equality requires more careful consideration.

<!--verse
using{/Verse.org/VerseCLR}
linked_list := class:
    Value:int = 0
    Next:?linked_list = false

    Equals(Other:linked_list)<computes><decides>:void =
        Self.Value = Other.Value
        if (Self.Next?):
            Tmp := Self.Next?
            OtherNext := Other.Next?
            Tmp.Equals[OtherNext]
        else:
            not Other.Next?

F():void={
List1 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}
List2 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}

if (List1.Equals[List2]):
    Print("Structurally equal")  
}<#
-->
```verse
# Recursive data structures using classes
linked_list := class:
    Value:int = 0
    Next:?linked_list = false

    # Custom equality check for structural comparison
    Equals(Other:linked_list)<computes><decides>:void =
        Self.Value = Other.Value
        # Both have no next, or both have next and those are equal
        if (Self.Next?):
            Tmp := Self.Next?
            OtherNext := Other.Next?
            Tmp.Equals[OtherNext]
        else:
            not Other.Next?

List1 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}
List2 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}

if (List1.Equals[List2]):
    Print("Structurally equal")  # This succeeds
```
<!--verse
#>
-->

Pure computation forms the backbone of functional programming in Verse. It's predictable, testable, and parallelizable. When a function is marked `<computes>`, you know it will always produce the same output for the same input, with no hidden dependencies or surprising behaviors.

## Introducing Mutation

Mutation enters through two keywords: `var` and `set`. The `var` annotation declares that a variable can be reassigned. The `set` keyword performs that reassignment. Together, they provide controlled mutation with clear visibility.

<!--NoCompile-->
```verse
# Immutable variable - cannot be reassigned
Score:int = 100

# Mutable variable - can be reassigned 
var Health:float = 100.0       # type annotation is required
set Health = 75.0  # Allowed
```

Every use of `var` and `set` has implications for effects. Reading from a `var` variable requires the `<reads>` effect. Using `set` requires both `<reads>` and `<writes>` effects. This isn't bureaucracy — it's transparency. The effects make mutation visible in function signatures, so callers know when functions might observe or modify state.

### Requirements for var Declarations

Mutable variable declarations have strict requirements that prevent common errors:

**Must provide explicit type:**

```verse
# Valid - explicit type
var X:int = 0

# Invalid - cannot use := syntax with var
# var X := 0  # ERROR 3515
```

The type inference syntax `:=` cannot be used with `var`. You must explicitly declare the type.

**Must provide initial value:**

```verse
# Valid - initialized
var Health:float = 100.0

# Invalid - no initial value
# var Score:int  # ERROR 3601
```

Every `var` declaration requires an initial value. Uninitialized mutable variables are not allowed.

**Cannot be completely untyped:**

```verse
# Invalid - neither type nor value
# var X  # ERROR 3502
```

### var Declarations as Expressions

Variable declarations with `var` can be used as expressions, evaluating to their initial value:

```verse
X := (var Y:int = 42)  # X = 42, Y declared and mutable
X = 42
```

However, `var` declarations **cannot be the target of `set`**:

```verse
# Invalid
# set (var Z:int = 0) = 1  # ERROR 3509
```

The `var` keyword declares a new mutable variable; you cannot assign to the declaration itself.

### set with Block Expressions

The `set` statement can use block expressions, which allows complex computations and side effects:

```verse
var X:int = 0
var Y:int = 1

set X = block:
    set Y = X      # Side effect: Y becomes 0
    2              # Block result: X becomes 2

X = 2
Y = 0
```

This pattern is useful when the new value requires intermediate computations or when you need multiple side effects during assignment.

### Scope and Redeclaration Restrictions

**Cannot redeclare in same scope:**

Variables cannot be redeclared with `:=` once they exist in scope:

```verse
var X:int = 0

# Invalid - X already exists
# X := 1  # ERROR 3653
```

This applies even in conditional branches:

```verse
var A:int = 1

if (SomeCondition?):
    # Invalid - A already declared in outer scope
    # A := 2  # ERROR 3653
```

**Cannot redeclare with assignment syntax:**

```verse
var A:int = 1
var B:int = 2

# Invalid - looks like assignment but A already exists
# A := B  # ERROR 3653
```

Use `set A = B` instead to assign to existing mutable variables.

**Cannot nest var declarations:**

```verse
# Invalid
# var (var X):int = 0  # ERROR 3549
```

The `var` keyword cannot be nested within itself.

## Deep vs Shallow Mutability

Verse's approach to mutability differs significantly between structs and classes, reflecting their different roles in the language.

### Struct Mutability: Deep and Structural

When you declare a struct variable with `var`, you're declaring the entire structure as mutable — the variable itself and all its nested fields, recursively. This deep mutability means you can modify any part of the structure tree.

<!--verse
point:=struct<computes>{X:float=100.0}
player_stats := struct<computes>:
    Level:int = 1
    Position:point = point{}
    Inventory:[]string = array{}

f():void={
Stats1:player_stats = player_stats{}
var Stats2:player_stats = player_stats{}
set Stats2.Level = 2  # OK
set Stats2.Position.X = 100.0  # OK - nested fields are mutable
set Stats2.Inventory = Stats2.Inventory + array{"Sword"}  # OK
}<#
-->
```verse
player_stats := struct<computes>:
    Level:int = 1
    Position:point = point{}
    Inventory:[]string = array{}

# Immutable struct variable - nothing can change
Stats1:player_stats = player_stats{}
# set Stats1.Level = 2  # ERROR: Cannot modify immutable struct

# Mutable struct variable - everything can change
var Stats2:player_stats = player_stats{}
set Stats2.Level = 2  # OK
set Stats2.Position.X = 100.0  # OK - nested fields are mutable
set Stats2.Inventory = Stats2.Inventory + array{"Sword"}  # OK
```
<!--verse
}
-->

When you assign one struct variable to another, Verse performs a deep copy. The two variables become independent, each with their own copy of the data. Changes to one don't affect the other.

<!--verse
point:=struct{}
player_stats := struct<computes>:
    Level:int = 1
    Position:point = point{}
    Inventory:[]string = array{}
F():void={
-->
```verse
var Original:player_stats = player_stats{Level := 5}
var Copy:player_stats = Original

set Copy.Level = 10
# Original.Level is still 5 - they're independent copies
```
<!--verse
}
-->

This deep-copy semantics extends to all value types: structs, arrays, maps, and tuples. When you pass a struct to a function, the function receives its own copy. When you store a struct in a container, the container holds a copy. This prevents aliasing and makes reasoning about struct mutations local and predictable.

### Class Mutability: Reference Semantics

Classes behave differently. They have reference semantics — when you assign a class instance, you're sharing a reference to the same object, not creating a copy. The `var` annotation on a class variable only affects whether that variable can be reassigned to reference a different object. It doesn't affect the mutability of the object's fields.

<!--verse
game_character := class:
    Name:string = "Hero"
    var Health:float = 100.0  # This field is always mutable
    MaxHealth:float = 100.0   # This field is always immutable
F():void={
Player1:game_character = game_character{}
set Player1.Health = 50.0  # OK: Health field is mutable

var Player2:game_character = Player1  # Same object
set Player2 = game_character{Name := "Villain"}  # OK: Can reassign
set Player2.Health = 75.0  # OK: Modifies the new object
}<#
-->
```verse
game_character := class:
    Name:string = "Hero"
    var Health:float = 100.0  # This field is always mutable
    MaxHealth:float = 100.0   # This field is always immutable

# Immutable variable, but mutable fields can still change
Player1:game_character = game_character{}
# set Player1 = game_character{}  # ERROR: Cannot reassign non-var variable
set Player1.Health = 50.0  # OK: Health field is mutable

# Mutable variable allows reassignment
var Player2:game_character = Player1  # Same object
set Player2 = game_character{Name := "Villain"}  # OK: Can reassign
set Player2.Health = 75.0  # OK: Modifies the new object

# Player1 and the original Player2 reference were the same object
# After reassignment, Player2 references a different object
```
<!--verse
#>
-->

The key insight: for classes, field mutability is determined at class definition time, not at variable declaration time. A `var` field is always mutable, regardless of how you access it. A non-`var` field is always immutable, even if accessed through a `var` variable.

<!--verse
point:=struct{X:float=1.0}
container := class:
    ImmutableData:point= point{}  # Always immutable
    var MutableData:int = 0  # Always mutable
f():void={
Box:container = container{}
set Box.MutableData = 42  # Allowed
}<#
-->
```verse
container := class:
    ImmutableData:point= point{}  # Always immutable
    var MutableData:int = 0  # Always mutable

# Even through an immutable variable, mutable fields can change
Box:container = container{}
set Box.MutableData = 42  # Allowed
# set Box.ImmutableData = Point{X := 1.0}  # ERROR: Field is immutable
```
<!--verse
#>
-->

### Collection Mutability: Arrays and Maps

Arrays and maps follow struct semantics—they are values, not references. When you copy a collection, you get an independent copy. Mutations to one copy don't affect the other.

#### Basic Array Mutation

Mutable arrays allow element replacement:

<!--verse
F():void={
-->
```verse
var Numbers:[]int = array{0, 1}
Numbers[0] = 0
Numbers[1] = 1

set Numbers[0] = 42
Numbers[0] = 42
Numbers[1] = 1  # Unchanged

set Numbers[1] = 666
Numbers[0] = 42
Numbers[1] = 666
```
<!--verse
}
-->

**Important**: You cannot add elements beyond the array's current length:

<!--verse
F():void={
-->
```verse
var A:[]int = array{0}
not (set A[1] = 1)  # Fails - index out of bounds
# Must use concatenation: set A = A + array{1}
```
<!--verse
}
-->

#### Basic Map Mutation

Mutable maps allow both updating existing keys and adding new keys:

<!--verse
F():void={
-->
```verse
var Scores:[int]int = map{0 => 1, 1 => 2}
set Scores[1] = 42
Scores[1] = 42

# Adding new keys
set Scores[2] = 100
Scores[2] = 100

# Map with string keys
var Config:[string]int = map{"volume" => 50}
set Config["brightness"] = 75
```
<!--verse
}
-->

**Important**: Looking up a non-existent key doesn't add it:

<!--verse
F():void={
-->
```verse
M:[int]int := map{}
not (M[0] = 0)  # Key doesn't exist, comparison fails
# M is still empty - lookup didn't add the key
```
<!--verse
}
-->

#### Nested Collection Mutation

Collections can be nested, and `set` works through multiple levels:

**Map of arrays:**

<!--verse
F():void={
-->
```verse
var Data:[int][]int = map{}
set Data[666] = array{42}
Data[666] = array{42}

# Mutate nested array element
set Data[666][0] = 1234
Data = map{666 => array{1234}}
Data[666] = array{1234}
```
<!--verse
}
-->

**Array of maps:**

<!--verse
F():void={
-->
```verse
var Grid:[][int]int = array{map{}}

# Replace entire map at index
set Grid[0] = map{42 => 666}
Grid[0] = map{42 => 666}
Grid[0][42] = 666

# Add new key to nested map
set Grid[0][1234] = 4321
Grid[0] = map{42 => 666, 1234 => 4321}
Grid[0][42] = 666
Grid[0][1234] = 4321

# Update existing key in nested map
set Grid[0][42] = 1122
Grid[0][42] = 1122
```
<!--verse
}
-->

**Array of arrays:**

<!--verse
F():void={
-->
```verse
var Matrix:[][]int = array{array{1234}}
set Matrix[0][0] = 42
Matrix = array{array{42}}
Matrix[0] = array{42}
Matrix[0][0] = 42

# Replace inner array
set Matrix[0] = array{666}
Matrix[0] = array{666}
Matrix[0][0] = 666
```
<!--verse
}
-->

#### Value Semantics for Collections

Extracting a value from a mutable collection creates an independent copy:

<!--verse
F():void={
-->
```verse
var X:[][int]int = array{map{42 => 1122, 1234 => 4321}}

# Y gets a copy of the map, not a reference
Y := X[0]
Y = map{42 => 1122, 1234 => 4321}

# Mutating X doesn't affect Y
set X[0][0] = 111
X[0] = map{42 => 1122, 1234 => 4321, 0 => 111}
Y = map{42 => 1122, 1234 => 4321}  # Unchanged

# Replacing entire element doesn't affect Y
set X[0] = map{42 => 4242}
X[0] = map{42 => 4242}
Y = map{42 => 1122, 1234 => 4321}  # Still unchanged
```
<!--verse
}
-->

This is different from class reference semantics—collections copy, classes share.

#### Collections with Mutable Values

When collections contain classes or structs with mutable fields, you can mutate through the collection:

<!--verse
the_class := class:
    var X:[]int = array{0}
F():void={
-->
```verse
C := the_class{}
set C.X[0] = 4266642
C.X[0] = 4266642
```
<!--verse
}
-->

**Map values with mutable members:**

<!--verse
class0 := class:
    var AM:int = 20
F():void={
-->
```verse
var M:[int]class0 = map{0 => class0{}}
M[0].AM = 20

# Mutate class field through map
set M[0].AM = 30
M[0].AM = 30
```
<!--verse
}
-->

**But note**: The map constructed from a `var` doesn't track changes to the source variable:

<!--verse
F():void={
-->
```verse
var I0:int = 42
M:[int]int = map{0 => I0}
M[0] = 42

set I0 = 0
M[0] = 42  # Still 42! Map has a copy of the value
```
<!--verse
}
-->

### Arrays of Structs: Independent Copies

When you store structs in an array, each element is an independent copy:

<!--verse
struct0 := struct<computes>:
    A:int = 10
F():void={
-->
```verse
S0 := struct0{A := 88}
var A0:[]struct0 = array{S0, S0}

# All three have the value 88, but are independent
S0.A = 88
A0[0].A = 88
A0[1].A = 88

# Mutating one doesn't affect the others
set A0[0].A = 99
S0.A = 88     # Unchanged
A0[0].A = 99  # Changed
A0[1].A = 88  # Unchanged
```
<!--verse
}
-->

### Arrays of Classes: Shared References

Arrays of classes behave very differently—all references to the same object share mutations:

<!--verse
class0 := class:
    var AM:int = 20
F():void={
-->
```verse
C0 := class0{}
var A1:[]class0 = array{C0, C0, C0}

# All three array elements reference the same object
A1[0].AM = 20
A1[1].AM = 20
A1[2].AM = 20

# Mutating through one affects all references
set A1[0].AM = 30
A1[0].AM = 30
A1[1].AM = 30  # Changed!
A1[2].AM = 30  # Changed!

set A1[1].AM = 40
A1[0].AM = 40  # All three see the change
A1[1].AM = 40
A1[2].AM = 40

# Replacing an element breaks the sharing for that element
set A1[1] = class0{}
A1[0].AM = 40  # Still references original
A1[1].AM = 20  # New object with default value
A1[2].AM = 40  # Still references original
```
<!--verse
}
-->

This is a critical distinction: **structs in collections are copies, classes in collections are shared references**.

### Compound Assignment Operators

Verse supports compound assignment operators that combine arithmetic with mutation:

<!--verse
struct0 := struct<computes>:
    A:int = 10
F():void={
-->
```verse
var S0:struct0 = struct0{}

set S0.A += 10
S0.A = 20

set S0.A -= 3
S0.A = 17

set S0.A *= 4
S0.A = 68
```
<!--verse
}
-->

Available compound operators:
- `set += ` - Addition assignment (int, float, string, array)
- `set -= ` - Subtraction assignment (int, float)
- `set *= ` - Multiplication assignment (int, float)
- `set /= ` - Division assignment (float only)

**Important**: `set /=` doesn't work with integers because integer division is failable.

Compound assignments work anywhere regular assignment does:

<!--verse
F():void={
-->
```verse
var Score:int = 100
set Score += 50
set Score *= 2

var Data:[]int = array{1, 2, 3}
set Data += array{4, 5}  # Array concatenation
Data = array{1, 2, 3, 4, 5}

var Numbers:[][]int = array{array{1}}
set Numbers[0][0] *= 42
Numbers[0][0] = 42
```
<!--verse
}
-->

### Tuple Mutability: Replacement Only

Tuples can be replaced entirely but individual elements cannot be mutated:

<!--verse
F():void={
-->
```verse
var T0:tuple(int, int) = (10, 20)
T0(0) = 10
T0(1) = 20

# Can replace entire tuple
set T0 = (30, 40)
T0(0) = 30
T0(1) = 40
```
<!--verse
}
-->

**Cannot mutate elements** (compile error 3509):

```verse
# ERROR 3509:
var T0:tuple(int, int) = (50, 60)
set T0(0) = 70  # ERROR: Cannot mutate tuple elements
```

This restriction applies even when the tuple is mutable. You must replace the entire tuple to change its contents.

### Map Ordering and Mutation

Maps preserve **insertion order**, and this order is maintained through mutations:

#### New Keys Append to End

<!--verse
F():void={
-->
```verse
var M:[int]int = map{2 => 2}

set M[1] = 1  # Appends to end
set M[0] = 0  # Appends to end

# Iteration order is insertion order: 2, 1, 0
Keys := array{2, 1, 0}
var Index:int = 0
for (Key->Value : M):
    Keys[Index] = Key
    set Index += 1

M = map{2 => 2, 1 => 1, 0 => 0}
```
<!--verse
}
-->

#### Updating Existing Keys Preserves Position

<!--verse
F():void={
-->
```verse
var M:[string]int = map{"a" => 3, "b" => 1, "c" => 2}

# Mutating value keeps key position
set M["a"] = 0
M = map{"a" => 0, "b" => 1, "c" => 2}  # Same order

# Another update
set M["c"] = 0
set M["a"] = 2
M = map{"a" => 2, "b" => 1, "c" => 0}  # Still same order
```
<!--verse
}
-->

#### Order Matters for Equality

Map equality considers both keys/values **and order**:

<!--verse
F()<decides>:void={
-->
```verse
var M:[string]int = map{"a" => 3, "b" => 1, "c" => 2}
set M["a"] = 0

# Same keys and values, same order = equal
M = map{"a" => 0, "b" => 1, "c" => 2}

# Same keys and values, different order = not equal
M <> map{"b" => 1, "c" => 2, "a" => 0}
```
<!--verse
}
-->

## Critical Mutability Restrictions

Verse imposes several important restrictions on where and how mutation can occur. These aren't arbitrary—they prevent unsound behaviors and maintain type safety.

### Cannot Mutate Immutable Class Fields

Classes might contain unique pointers or other resources that cannot be safely cloned. Therefore, you cannot mutate immutable fields of a class instance:

```verse
# ERROR 3509:
classX := class:
    AI:int = 20  # Immutable field

CX:classX = classX{}
CX.AI = 20
set CX.AI = 30  # ERROR 3509: Cannot mutate immutable class field
```

This restriction applies even when the class instance itself is immutable. Only `var` fields of classes can be mutated.

### Only <computes> Structs Allow Field Mutation

Only structs marked `<computes>` (pure structs) allow field mutation through a variable:

<!--verse
F():void={
-->
```verse
# OK: <computes> struct allows field mutation
s1 := struct<computes>{M:int = 0}
var S1:s1 = s1{}
set S1.M = 1
S1.M = 1
```
<!--verse
}
-->

```verse
# ERROR 3509: Non-pure struct disallows field mutation
s2 := struct{M:int = 0}
var S2:s2 = s2{}
set S2.M = 1  # ERROR 3509
```

This restriction ensures that only predictable, effect-free structs can be mutated.

### Cannot Have Immutable Class in Mutation Path

When mutating nested structures, you cannot have an immutable class in the "middle" of the path:

```verse
# ERROR 3509:
struct0 := struct<computes>{A:int = 10}
struct1 := struct<computes>{S0:struct0 = struct0{}}
class0 := class{CI:struct1 = struct1{}}  # Immutable class
struct2 := struct<computes>{C0:class0 = class0{}}
struct3 := struct<computes>{S2:struct2 = struct2{}}

var S3:[]struct3 = array{struct3{}, struct3{}}
set S3[1].S2.C0.CI.S0.A = 7  # ERROR 3509: class0 is immutable
```

**But** you CAN mutate through `var` members of that class:

<!--verse
struct0 := struct<computes>{A:int = 10}
struct1 := struct<computes>{S0:struct0 = struct0{}}
class0 := class:
    CI:struct1 = struct1{}
    var CM:struct1 = struct1{}
struct2 := struct<computes>{C0:class0 = class0{}}
struct3 := struct<computes>{S2:struct2 = struct2{}}
F():void={
-->
```verse
var S3:[]struct3 = array{struct3{}, struct3{}}

# OK: Replacing entire array element
set S3 = array{struct3{}, struct3{}, S3[1]}

# OK: Mutating through var member
set S3[0].S2.C0.CM = struct1{}
set S3[0].S2.C0.CM.S0.A = 77
S3[0].S2.C0.CM.S0.A = 77
```
<!--verse
}
-->

### Cannot Mutate Immutable Arrays

Even with a mutable index, you cannot mutate an immutable array:

```verse
# ERROR 3509:
I:int = 2
A:[]int = array{5, 6, 7}
set A[I] = 2  # ERROR: A is not var
```

The array itself must be declared `var` to allow element mutation:

<!--verse
F():void={
-->
```verse
I:int = 2
var A:[]int = array{5, 6, 7}
set A[I] = 2  # OK: A is var
```
<!--verse
}
-->

### Set Keyword Restrictions

The `set` keyword can only appear as part of an assignment expression:

```verse
# ERROR 3682: set without assignment
var X:int = 3
set X  # ERROR 3682: set must be part of assignment
```

```verse
# ERROR 3682: set in conditional without assignment completion
var A:[int]int = map{}
if (set A[4]) {}  # ERROR 3682
```

The `set` keyword must be followed by `=` or a compound assignment operator.

## Identity and Uniqueness

The `<unique>` specifier gives classes identity-based equality. Without it, classes can't be compared for equality at all (you'd need to write custom comparison methods). With it, equality means identity — two references are equal only if they refer to the exact same object.

<!--verse
using { /Verse.org/VerseCLR }
unique_item := class<unique>:
    var Count:int = 0

F():void={
Item1:unique_item = unique_item{}
Item2:unique_item = Item1  # Same object
Item3:unique_item = unique_item{}  # Different object

if (Item1 = Item2):
    Print("Same object")  # This prints

if (Item1 = Item3):
    Print("Same object")  
}<#
-->
```verse
unique_item := class<unique>:
    var Count:int = 0

Item1:unique_item = unique_item{}
Item2:unique_item = Item1  # Same object
Item3:unique_item = unique_item{}  # Different object

if (Item1 = Item2):
    Print("Same object")  # This prints

if (Item1 = Item3):
    Print("Same object")  # This doesn't print - different objects
```
<!--verse
#>
-->

This identity-based equality is crucial for game objects that need distinct identities even when their data is identical. Two monsters might have the same stats, but they're still different monsters.

<!-- ## Practical Implications

The distinction between struct and class mutability has profound implications for how you design data structures. Structs are ideal for value-like data that you want to manipulate locally without affecting other parts of your program. Classes are better for entities with identity that might be referenced from multiple places.

<!--NoCompile-->
```verse
# Struct: Each player has their own copy of stats
player := struct<computes><persistable>:
    Name:string = "Player"
    Level:int = 1
    Gold:int = 0

# Modifying one player's save doesn't affect others
var Save1:player= player{Name := "Alice"}
var Save2:player= Save1  # Deep copy
set Save2.Name= "Bob"  # Only affects Save2

# Class: Shared game state
game_world := class:
    var CurrentTime:float = 0.0
    var ActivePlayers:[]player = array{}

# All references see the same world state
World:game_world = game_world{}
# Any code with a reference to World sees the same CurrentTime
```

Understanding mutability in Verse means understanding these key concepts:

### Core Principles

1. **Immutability by default**: Values don't change unless explicitly marked as mutable
2. **Deep copying for structs**: Struct assignment creates independent copies with deep mutability
3. **Reference sharing for classes**: Class assignment shares references with field-level mutability control

### Collections

4. **Collections have value semantics**: Arrays and maps copy like structs, not references
5. **Nested mutations work**: `set X[0][1] = value` works through multiple collection levels
6. **Extraction creates copies**: Getting a value from a collection gives you an independent copy

### Structs vs Classes in Collections

7. **Structs in collections are independent**: Array of structs contains copies, mutations don't propagate
8. **Classes in collections are shared**: Array of the same class instance shares mutations across all elements

### Maps and Ordering

9. **Maps preserve insertion order**: New keys append to end, updates preserve position
10. **Map equality includes order**: Same keys/values in different order are not equal

### Restrictions

11. **Cannot mutate immutable class fields**: Only `var` fields of classes can be mutated
12. **Only `<computes>` structs allow field mutation**: Non-pure structs cannot have fields mutated
13. **No immutable classes in mutation paths**: Cannot mutate through immutable class instance
14. **Arrays require `var` for mutation**: Immutable arrays cannot have elements changed
15. **Tuples: replace only**: Can replace entire tuple but not individual elements

### Compound Operators

16. **Compound assignments available**: `+=`, `-=`, `*=`, `/=` (float only) combine operation with mutation

This design eliminates many common bugs while providing the flexibility needed for game development. You get the benefits of functional programming where appropriate, and controlled mutation where necessary. The key is that mutation is always explicit, always visible, and always intentional.

When designing your data structures, consider:

- Use **structs** for value-like data (points, stats, configurations) that should copy independently
- Use **classes** for entities with identity (players, game objects) that should share state
- Use **`<computes>`** on structs that need field mutation
- Mark class fields as **`var`** only when they need to change
- Remember **collections copy**, so extracting from mutable collections gives you independent values
- Be aware that **arrays of classes share references** while **arrays of structs copy**
- Use **map ordering** intentionally, knowing it affects equality
- Understand **restrictions** to avoid compilation errors in complex nested structures
-->
