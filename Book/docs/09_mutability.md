# Mutability

Immutability is the default in Verse. When you create a value, it stays that value forever — unchanging, predictable, and safe to share. This foundational principle makes programs easier to reason about, eliminates entire categories of bugs, and enables powerful optimizations. But games are dynamic worlds where state constantly evolves: health decreases, scores increase, inventories change. Verse embraces both paradigms, providing immutability by default while offering controlled, explicit mutation when you need it.

The distinction between immutable and mutable data in Verse goes deeper than just whether values can change. It fundamentally affects how data flows through your program, how values are shared between functions, and how the compiler reasons about your code. Understanding this distinction is crucial for writing efficient, correct Verse programs.

## The Pure Foundation

In Verse's pure fragment, computation happens without side effects. Values are created but never modified. Functions transform inputs into outputs without changing anything along the way. This isn't a limitation — it's a powerful foundation that makes code predictable and composable.

```verse
# Immutable values and structures
Point := struct<computes>:
    X:float = 0.0
    Y:float = 0.0

Origin := Point{}
UnitX := Point{X := 1.0}
UnitY := Point{Y := 1.0}

# These values are eternal - Origin will always be (0, 0)
Distance(P1:Point, P2:Point)<computes>:float =
    DX := P2.X - P1.X
    DY := P2.Y - P1.Y
    Sqrt(DX * DX + DY * DY)
```

In this pure world, equality means structural equality — two values are equal if they have the same shape and content. For primitive types and structs, this happens automatically. For classes, which have identity beyond their content, equality requires more careful consideration.

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
            Next := Self.Next?
            OtherNext := Other.Next?
            Next.Equals(OtherNext)
        else:
            not Other.Next?

List1 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}
List2 := linked_list{Value := 1, Next := option{linked_list{Value := 2}}}

if (List1.Equals(List2)):
    Print("Structurally equal")  # This succeeds
```

Pure computation forms the backbone of functional programming in Verse. It's predictable, testable, and parallelizable. When a function is marked `<computes>`, you know it will always produce the same output for the same input, with no hidden dependencies or surprising behaviors.

## Introducing Mutation

Mutation enters through two keywords: `var` and `set`. The `var` annotation declares that a variable can be reassigned. The `set` keyword performs that reassignment. Together, they provide controlled mutation with clear visibility.

```verse
# Immutable variable - cannot be reassigned
Score:int = 100
# Score = 200  # ERROR: Cannot assign to non-var variable

# Mutable variable - can be reassigned
var Health:float = 100.0
set Health = 75.0  # Allowed

# Type annotation is required for var
var Shield:float = 50.0  # Must specify type
```

Every use of `var` and `set` has implications for effects. Reading from a `var` variable requires the `<reads>` effect. Using `set` requires both `<reads>` and `<writes>` effects. This isn't bureaucracy — it's transparency. The effects make mutation visible in function signatures, so callers know when functions might observe or modify state.

## Deep vs Shallow Mutability

Verse's approach to mutability differs significantly between structs and classes, reflecting their different roles in the language.

### Struct Mutability: Deep and Structural

When you declare a struct variable with `var`, you're declaring the entire structure as mutable — the variable itself and all its nested fields, recursively. This deep mutability means you can modify any part of the structure tree.

```verse
player_stats := struct<computes>:
    Level:int = 1
    Position:Point = Point{}
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

When you assign one struct variable to another, Verse performs a deep copy. The two variables become independent, each with their own copy of the data. Changes to one don't affect the other.

```verse
var Original:player_stats = player_stats{Level := 5}
var Copy:player_stats = Original

set Copy.Level = 10
# Original.Level is still 5 - they're independent copies
```

This deep-copy semantics extends to all value types: structs, arrays, maps, and tuples. When you pass a struct to a function, the function receives its own copy. When you store a struct in a container, the container holds a copy. This prevents aliasing and makes reasoning about struct mutations local and predictable.

### Class Mutability: Reference Semantics

Classes behave differently. They have reference semantics — when you assign a class instance, you're sharing a reference to the same object, not creating a copy. The `var` annotation on a class variable only affects whether that variable can be reassigned to reference a different object. It doesn't affect the mutability of the object's fields.

```verse
game_character := class:
    Name:string = "Hero"
    var Health:float = 100.0  # This field is always mutable
    MaxHealth:float = 100.0   # This field is always immutable

# Immutable variable, but mutable fields can still change
Player1:game_character = game_character{}
# Player1 = game_character{}  # ERROR: Cannot reassign non-var variable
set Player1.Health = 50.0  # OK: Health field is mutable

# Mutable variable allows reassignment
var Player2:game_character = Player1  # Same object
set Player2 = game_character{Name := "Villain"}  # OK: Can reassign
set Player2.Health = 75.0  # OK: Modifies the new object

# Player1 and the original Player2 reference were the same object
# After reassignment, Player2 references a different object
```

The key insight: for classes, field mutability is determined at class definition time, not at variable declaration time. A `var` field is always mutable, regardless of how you access it. A non-`var` field is always immutable, even if accessed through a `var` variable.

```verse
container := class:
    ImmutableData:Point = Point{}  # Always immutable
    var MutableData:int = 0  # Always mutable

# Even through an immutable variable, mutable fields can change
Box:container = container{}
set Box.MutableData = 42  # Allowed
# set Box.ImmutableData = Point{X := 1.0}  # ERROR: Field is immutable
```

## Identity and Uniqueness

The `<unique>` specifier gives classes identity-based equality. Without it, classes can't be compared for equality at all (you'd need to write custom comparison methods). With it, equality means identity — two references are equal only if they refer to the exact same object.

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

This identity-based equality is crucial for game objects that need distinct identities even when their data is identical. Two monsters might have the same stats, but they're still different monsters.

## Practical Implications

The distinction between struct and class mutability has profound implications for how you design data structures. Structs are ideal for value-like data that you want to manipulate locally without affecting other parts of your program. Classes are better for entities with identity that might be referenced from multiple places.

```verse
# Struct: Each player has their own copy of stats
player_save := struct<computes><persistable>:
    Name:string = "Player"
    Level:int = 1
    Gold:int = 0

# Modifying one player's save doesn't affect others
var Save1:player_save = player_save{Name := "Alice"}
var Save2:player_save = Save1  # Deep copy
set Save2.Name = "Bob"  # Only affects Save2

# Class: Shared game state
game_world := class:
    var CurrentTime:float = 0.0
    var ActivePlayers:[]player = array{}

# All references see the same world state
World:game_world = game_world{}
# Any code with a reference to World sees the same CurrentTime
```

Understanding mutability in Verse means understanding these three key concepts:

1. **Immutability by default**: Values don't change unless explicitly marked as mutable
2. **Deep copying for structs**: Struct assignment creates independent copies with deep mutability
3. **Reference sharing for classes**: Class assignment shares references with field-level mutability control

This design eliminates many common bugs while providing the flexibility needed for game development. You get the benefits of functional programming where appropriate, and controlled mutation where necessary. The key is that mutation is always explicit, always visible, and always intentional.