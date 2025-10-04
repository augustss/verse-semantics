# The Verse Programming Language

## Overview

Verse is a functional logic programming language developed by Epic Games for creating gameplay in Unreal Editor for Fortnite and building experiences in the metaverse. It represents a departure from traditional programming languages, designed not just for today's needs but with a vision spanning decades or even centuries into the future.

Verse is built on three fundamental principles:

- **It's Just Code**:
Complex concepts that might require special syntax or constructs in other languages are expressed as regular Verse code. There's no magic—everything is built from the same primitive constructs, creating a uniform and predictable programming model.

- **Just One Language**:
The same language constructs work at both compile-time and run-time. There's no separate template language, macro system, or preprocessor. What you write is what executes, whether during compilation or at runtime.

- **Metaverse First**:
Verse is designed for a future where code runs in a single global simulation—the metaverse. This influences every aspect of the language, from its strong compatibility guarantees to its effect system that tracks side effects and ensures safe concurrent execution.

Verse aims to be:

- **Simple enough** for first-time programmers to learn, with consistent rules and minimal special cases.

- **Powerful enough** for complex game logic and distributed systems, with advanced features that scale to large codebases.

- **Safe enough** for untrusted code to run in a shared environment, with strong sandboxing and effect tracking.

- **Fast enough** for real-time games and simulations, with an implementation that can optimize pure computations aggressively.

- **Stable enough** to last for decades, with strong backward compatibility guarantees and careful evolution.

**Why Verse?**

Traditional programming languages carry decades of historical baggage and design compromises. Verse starts fresh, learning from the past but not  bound by it. It's designed for a future where:

- Code lives forever in a persistent metaverse
- Millions of developers contribute to a shared codebase
- Programs must be safe, concurrent, and composable by default
- Backward compatibility is not optional but essential
- The boundary between compile-time and runtime is fluid

Ready to dive in? Start with [Built-in Types](01_builtins.md) to understand Verse's fundamental data types, or jump to [Expressions](05_expressions.md) to see how everything in Verse computes values.

For experienced programmers coming from other languages, the [Failure System](08_failure.md) and [Effects](09_effects.md) sections highlight Verse's most distinctive features.

## Key Features

**Everything is an Expression**

In Verse, there are no statements—everything is an expression that produces a value. This creates a highly composable system where any piece of code can be used anywhere a value is expected.

<!--verse
Condition()<decides> := {}
Main() :void = { Array := array{1}
-->
```verse
# Even control flow produces values
Result := if (Condition[]) then "yes" else "no"

# Loops are expressions
Multiply := for (X : Array) { X * 42 }
```
<!--verse
} 
-->

**Failure as Control Flow**

Instead of boolean conditions and exceptions, Verse uses failure as a primary control flow mechanism. Expressions can succeed (producing a value) or fail (producing no value), and this failure propagates naturally through the program.

<!--verse
ValidateInput(x:string)<decides>:void= {}
ProcessData(x:string):void= {}
  
x := class {
  Data:="hi"
  M()<decides>:={
-->
```verse
ValidateInput[Data] # Proceeds only if validation succeeds
ProcessData(Data)
```
<!--verse
} }
-->

**Strong Static Typing with Inference**

Verse features a powerful type system that catches errors at compile time while minimizing the need for type annotations through inference.

<!--verse
M():={
-->
```verse
X := 42                    # Type inferred as int
Name := "Verse"            # Type inferred as string
```
<!--verse
}
-->

**Effect Tracking**

The language tracks side effects through its effect system, making it clear what a function can do beyond computing its return value.

<!--verse
x := class {
  GetCurrentValue()<reads>:int=1
  var Score:int=0
-->
```verse
PureCompute()<computes>:int = 2 + 2           # No side effects
ReadState()<reads>:int = GetCurrentValue()     # Can read memory
UpdateGame()<transacts>:void = set Score += 10 # Full transactional effects
```
<!--verse
}
-->

**Built-in Concurrency**

Concurrency is a first-class feature with structured concurrency primitives that make concurrent programming safe and predictable.

<!--verse
TaskA()<suspends>:void={}
TaskB()<suspends>:void={}
TaskC():void={}
FastPath()<suspends>:void={}
SlowButReliablePath()<suspends>:void={}
Main()<suspends>:void= {
-->
```verse
# Run tasks concurrently and wait for all
sync:
    TaskA()
    TaskB()
    TaskC()

# Race tasks and take first result
race:
    FastPath()
    SlowButReliablePath()
```
<!--verse
}
-->

**Speculative Execution**

Verse can speculatively execute code and roll back changes if the execution fails, enabling powerful patterns for validation and error handling.

<!--verse
TryComplexOperation()<decides>:void={}
M():={
-->
```verse
if (TryComplexOperation[]):
    # Changes are committed
else:
    # Changes are rolled back automatically
```
<!--verse
}
-->

Welcome to Verse—a language built not just for today's games, but for tomorrow's metaverse.

## Example: a Game Inventory System

Let's explore Verse through a comprehensive example that demonstrates its key features. We'll build an inventory management system for a game, showing how Verse's unique features come together to create robust, maintainable code.

```verse
# Module declaration - start by importing utility functions
using { /Verse.org/VerseCLR }

# Define item rarity as an enumeration - showing Verse's type system
item_rarity := enum<persistable>:
    common
    uncommon
    rare
    epic
    legendary

# Struct for immutable item data - functional programming style
item_stats := struct<persistable>:
    Damage:float = 0.0
    Defense:float = 0.0
    Weight:float = 1.0
    Value:int = 0

# Class for game items - object-oriented features with functional constraints
game_item := class<final><persistable>:
    Name:string
    Rarity:item_rarity = item_rarity.common
    Stats:item_stats = item_stats{}
    StackSize:int = 1
    
    # Method with decides effect - can fail
    GetRarityMultiplier()<decides>:float =
        case(Rarity):
            item_rarity.common => 1.0
            item_rarity.uncommon => 1.5
            item_rarity.rare => 2.0
            item_rarity.epic => 3.0
            _ => false  # Fails if the item is legenday
    
    # Computed property using closed-world function
    GetEffectiveValue()<transacts><decides> :int=
        Floor[Stats.Value * GetRarityMultiplier[]]

# Inventory system with state management and effects
inventory_system := class:
    var Items:[]game_item = array{}
    var MaxWeight:float = 100.0
    var Gold:int = 1000

    # Method demonstrating failure handling and transactional semantics
    AddItem(NewItem:game_item)<transacts><decides>:void =
        # Calculate new weight - speculative execution
        CurrentWeight := GetTotalWeight()
        NewWeight := CurrentWeight + NewItem.Stats.Weight

        # This check might fail, rolling back any changes
        NewWeight <= MaxWeight
        
        # Only executes if weight check passes
        set Items += array{NewItem}
        Print("Added {NewItem.Name} to inventory")

    # Method with query operator and failure propagation
    RemoveItem(ItemName:string)<transacts><decides>:game_item =
        var RemovedItem:?game_item = false
        var NewItems:[]game_item = array{}
        
        for (Item : Items):
            if (Item.Name = ItemName, not RemovedItem?):
                set RemovedItem = option{Item}
            else:
                set NewItems += array{Item}
        set Items = NewItems
        RemovedItem?  # Fails if item not found

    # Purchase with complex failure logic and rollback
    PurchaseItem(ShopItem:game_item)<transacts><decides>:void =
        # Multiple failure points - any failure rolls back all changes
        Price := ShopItem.GetEffectiveValue[]
        Price <= Gold  # Fails if not enough gold
        
        # Tentatively deduct gold
        set Gold = Gold - Price
        
        # Try to add item - might fail due to weight
        AddItem[ShopItem]
        
        # All succeeded - changes are committed
        Print("Purchased {ShopItem.Name} for {Price} gold")

    # Higher-order function with type parameters and where clauses
    FilterItems(Predicate:type{_(:game_item)<decides>:void} ) :[]game_item =
        for (Item : Items, Predicate[Item]):
            Item

    GetTotalWeight()<transacts>:float =
        var Total:float = 0.0
        for (Item : Items):
            set Total += Item.Stats.Weight
        Total

# Player class using composition
player_character<public> := class:
    Name<public>:string
    var Level:int = 1
    var Experience:int = 0
    var Inventory:inventory_system = inventory_system{}
    
    LevelUpThreshold := 100

    GainExperience(Amount:int)<transacts>:void =
        set Experience += Amount
        
        # Automatic level up check with failure handling
        loop:
            RequiredXP := LevelUpThreshold * Level
            if (Experience >= RequiredXP):
                set Experience -= RequiredXP
                set Level += 1
                Print("{Name} leveled up to {Level}!")
            else:
                break
    
    # Method showing qualified access
    EquipStarterGear()<transacts><decides>:void =
        StarterSword := game_item{
            Name := "Rusty Sword"
            Rarity := item_rarity.common
            Stats := item_stats{Damage := 10.0, Weight := 5.0, Value := 50}
        }
        # These might fail if inventory is full
        Inventory.AddItem[StarterSword]

# Example usage demonstrating control flow and failure handling
RunExample<public>()<suspends>:void =
    # Create a player (can't fail)
    Hero := player_character{Name := "Verse Hero"}
    
    # Try to equip starter gear (might fail)
    if (Hero.EquipStarterGear[]):
        Print("Hero equipped with starter gear")
    
    # Demonstrate transactional behavior
    ExpensiveItem := game_item{
        Name := "Golden Crown"
        Rarity := item_rarity.epic
        Stats := item_stats{Value := 2000, Weight := 90.0}  # Very heavy!
    }
    
    # This might fail due to weight or insufficient gold
    if (Hero.Inventory.PurchaseItem[ExpensiveItem]):
        Print("Purchase successful!")
    else:
        Print("Purchase failed - gold remains at {Hero.Inventory.Gold}")
    
    # Use higher-order functions
    RareItems := Hero.Inventory.FilterItems((I:game_item)<decides>:void =>
        I.Rarity = item_rarity.rare or I.Rarity = item_rarity.legendary)
    
    Print("Found {RareItems.Length} rare items")
```

This example showcases nearly every major feature of Verse in a practical context. Let's explore what makes this code uniquely Verse:

**Type System and Data Modeling**

The example begins with Verse's rich type system. The `item_rarity` enum provides type-safe constants without the boilerplate of traditional enumerations. The `item_stats` struct marked as `<persistable>` can be saved and loaded from persistent storage, essential for game saves. The `game_item` class uses `<unique>` to ensure reference equality semantics and `<persistable>` for save game support.

Notice how types flow naturally through the code. There's no need for explicit type annotations in most places because Verse's type inference is sophisticated enough to deduce them. When we do specify types, like `Items:[]game_item`, it's to document intent rather than satisfy the compiler.

**Failure as Control Flow**

Throughout the code, failure drives control flow rather than exceptions or error codes. The `<decides>` effect marks functions that can fail, and failure propagates naturally through expressions. When `GetRarityMultiplier()` encounters an unknown rarity, it doesn't throw an exception or return a sentinel value - it simply fails, and the calling code handles this gracefully.

The `AddItem` method demonstrates how failure creates elegant validation. The expression `NewWeight <= MaxWeight` either succeeds (allowing execution to continue) or fails (preventing the item from being added). There's no if statement, no explicit control flow - just a declarative assertion of what must be true.

**Transactional Semantics and Speculative Execution**

Methods marked with `<transacts>` provide automatic rollback on failure. In `PurchaseItem`, we deduct gold from the player, then try to add the item. If adding fails (perhaps due to weight limits), the gold deduction is automatically rolled back. This eliminates entire categories of bugs related to partial state updates.

This transactional behavior extends to complex operations. When multiple changes need to succeed or fail together, Verse ensures consistency without manual transaction management.

**Functions as First-Class Values**

The `FilterItems` method accepts a predicate function, demonstrating higher-order programming. The lambda expression in `RunExample` shows how functions can be created inline and passed around like any other value. This functional programming style combines naturally with the imperative and object-oriented features.

**Optional Types and Query Operators**

The inventory removal logic uses optional types (`?game_item`) to represent values that might not exist. The query operator `?` extracts values from options, failing if the option is empty. This eliminates null pointer exceptions while providing convenient syntax for handling absent values.

**Pattern Matching and Control Flow**

The `case` expression in `GetRarityMultiplier` demonstrates pattern matching. Unlike a switch statement, `case` is an expression that produces a value. The underscore `_` provides a catch-all pattern, though in this example it leads to failure.

The `if` expression similarly produces values and can bind variables in its condition. The compound conditions show how multiple operations can be chained with automatic failure propagation.

**Module System and Access Control**

The code begins with `using` statements that import functionality from other modules. The path-based module system ensures that dependencies are unambiguous and permanently addressable. Access specifiers like `<public>` control visibility at a fine-grained level.

**Immutable by Default**

Data structures are immutable unless explicitly marked with `var`. This eliminates large classes of bugs and makes concurrent programming safer. When we do need mutation, it's explicit and tracked by the effect system.

## Naming Conventions

Verse has a set of naming conventions that make code readable and predictable. While the language doesn't enforce these conventions, following them ensures your code integrates well with the broader Verse ecosystem and is immediately familiar to other Verse developers.

Verse uses PascalCase (CamelCase starting with uppercase) for most identifiers:

```verse
# Variables and constants use PascalCase
PlayerHealth:int = 100
MaxInventorySize:int = 50
IsGameActive:logic = true

# Functions use PascalCase
CalculateDamage(Base:float, Multiplier:float):float =
    Base * Multiplier

GetPlayerName(Id:int):string =
    PlayerDatabase[Id].Name

# Classes and structs use snake_case
player_character := class:
    Name:string
    Level:int

inventory_item := struct:
    ItemId:int
    Quantity:int

# Enums and their values use snake_case
game_state := enum:
    main_menu
    in_game
    paused
    game_over
```

Generic type parameters typically use single lowercase letters or short descriptive names:

```verse
# Single letter for simple generics
Find(Array:[]t, Target:t where t:subtype(comparable)):?int

# Descriptive names for complex relationships
Transform(Input:input_type, Processor:type{_(input_type):output_type}
          where input_type:type, output_type:type):output_type
```

Module names follow the snake_case pattern, while paths use a hierarchical structure with forward slashes and PascalCase for path segments:

```verse
# Module definition
inventory_system := module:
    # Module contents

# Path structure uses PascalCase for segments
using { /Fortnite.com/Characters/PlayerController }
using { /MyGame.com/Systems/CombatSystem }
using { /Verse.org/Random }
```

Class and struct fields use PascalCase, and methods follow the same PascalCase convention as functions:

```verse
player := class:
    Name:string          # PascalCase for fields
    Health:float
    MaxHealth:float
    CurrentLevel:int

    # Methods use PascalCase like functions
    TakeDamage(Amount:float):void =
        set Health = Max(0.0, Health - Amount)

    IsAlive():logic =
        Health > 0.0
```

## Code Formatting

Verse code follows consistent formatting patterns that emphasize readability:

Verse uses 4-space indentation for code blocks. The colon introduces a block, with subsequent lines indented:

```verse
if (Condition):
    DoSomething()
    DoSomethingElse()

for (Item : Inventory):
    ProcessItem(Item)
    UpdateDisplay()

class_definition := class:
    Field1:int
    Field2:string

    Method():void =
        ImplementationHere()
```

Complex expressions benefit from clear formatting that shows structure:

```verse
# Multi-line conditionals
Result := if (Player.Health > 50):
    "healthy"
else if (Player.Health > 20):
    "injured"
else:
    "critical"

# Chained operations with clear precedence
FinalDamage :=
    BaseDamage *
    LevelMultiplier *
    (1.0 + BonusPercentage / 100.0)

# Pattern matching with aligned arrows
DamageMultiplier := case(Rarity):
    common => 1.0
    uncommon => 1.5
    rare => 2.0
    epic => 3.0
    legendary => 5.0
```

Functions follow a consistent pattern with effects and return types clearly specified:

```verse
# Simple pure function
Add(X:int, Y:int):int = X + Y

# Function with effects
ProcessTransaction(Amount:int)<transacts><decides>:void =
    ValidateAmount(Amount)
    DeductBalance(Amount)
    RecordTransaction()

# Multi-line function with clear structure
CalculateReward(
    PlayerLevel:int,
    Difficulty:difficulty_level,
    CompletionTime:float
)<decides>:int =
    BaseReward := GetBaseReward(Difficulty)?
    LevelBonus := PlayerLevel * 10
    TimeBonus := CalculateTimeBonus(CompletionTime)
    BaseReward + LevelBonus + TimeBonus
```

## Comments

Comments are the programmer's way of leaving notes in the code, explaining not just what the code does, but why it does it. In Verse, comments are ignored during execution but are invaluable for understanding and maintaining code.

Verse offers several styles of comments to suit different documentation needs. The simplest is the single-line comment, which begins with `#` and continues to the end of the line:

```verse
CalculateDamage := 100 * 1.5  # Apply critical hit multiplier
```

When you need to document something within a line of code without breaking it up, inline block comments provide the perfect solution. These are enclosed between `<#` and `#>`:

```verse
Result := BaseValue <# original amount #> * Multiplier <# scaling factor #> + Bonus
```

For more extensive documentation, multi-line block comments span across multiple lines, making them ideal for explaining complex algorithms or providing detailed context:

```verse
<# This function implements the quadratic damage falloff formula
   used throughout the game. The falloff ensures that damage
   decreases smoothly with distance, creating strategic positioning
   choices for players. #>
CalculateFalloffDamage(Distance:float, MaxDamage:float):float =
    # Implementation here
```

One of Verse's more elegant features is nested block comments, which allow you to temporarily disable code that already contains comments without having to remove or modify existing documentation:

```verse
<# Temporarily disabled for testing
   OriginalFunction()  <# This had a bug #>
   NewFunction()       # Testing this approach
#>
```

Verse also supports indented comments, a unique feature that begins with `<#>` on its own line. Everything indented by four spaces on subsequent lines becomes part of the comment:

```verse
<#>
    This entire block is a comment because it's indented.
    It provides a clean way to write longer documentation
    without cluttering each line with comment markers.
DoSomething()  # This is not part of the indented comment
```

## Syntactic Styles

Verse offers flexible syntax to accommodate different programming styles and situations. The same logic can be expressed using braces, indentation, or inline forms, allowing you to choose the clearest representation for each context.

**Braced Style:** The braced style uses curly braces to delimit blocks, familiar to programmers from C-family languages:

```verse
Result := if (Score > 90) {
    "excellent"
} else if (Score > 70) {
    "good"
} else {
    "needs improvement"
}
```

**Indented Style:**
The indented style uses colons and indentation to define structure, similar to Python:

```verse
Result := if (Score > 90):
    "excellent"
else if (Score > 70):
    "good"
else:
    "needs improvement"
```

**Inline Style:**
For simple expressions, the inline style keeps everything on one line:

```verse
Result := if (Score > 90) then "excellent" else if (Score > 70) then "good" else "needs improvement"
```

**Dotted Style:**
The dotted style uses a period to introduce the expression:

```verse
Result := if (Score > 90). "excellent" else. "needs improvement"
```

**Mixed Styles:**
You can even mix styles when it makes sense. The colon-based indented form is particularly useful for the condition while keeping the branches inline:

```verse
Result := if:
    ComplexCondition() and
    AnotherCheck() and
    YetAnotherValidation()
then { "condition met" } ese { "condition not met" }
```

All these forms are semantically equivalent—they produce the same result. The choice between them is about readability and context. Use braces when working with existing brace-heavy code, indentation for cleaner vertical layouts, and inline forms for simple expressions. This flexibility lets you write code that reads naturally.
