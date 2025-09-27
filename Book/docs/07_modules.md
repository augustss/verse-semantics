# Modules and Paths - Complete Reference

## Overview

Modules and paths are fundamental concepts in Verse that provide code organization, namespace management, and the ability to share and reuse code across projects. Think of modules as containers that group related functionality together, similar to packages in other programming languages, but with stronger guarantees about versioning and compatibility.

In the context of game development, modules allow you to separate different aspects of your game logic into manageable, reusable pieces. For example, you might have one module for player inventory management, another for combat mechanics, and yet another for UI interactions. Each module encapsulates its own functionality while exposing only the necessary interfaces to other parts of your code.

The module system in Verse is designed to support the vision of a persistent, shared Metaverse where code can be published once and used by anyone, anywhere, with confidence that it will continue to work even as the original author updates and improves it. This is achieved through strict backward compatibility rules and a global namespace system that ensures every piece of published code has a unique, permanent address.

## What is a Module?

A Verse module is fundamentally an atomic unit of code organization that serves multiple purposes in the language ecosystem. At its core, a module is:

- **An atomic unit of code** that can be redistributed and depended upon. This means that when you create a module, you're creating a self-contained piece of functionality that others can use without needing to understand its internal implementation. The module acts as a black box, exposing only what you choose to make public.

- **A way to organize related code definitions together**. Rather than having all your code in a single file or scattered across many files with no clear structure, modules let you group related functions, classes, constants, and other definitions in a logical manner. This organization makes your code easier to understand, maintain, and debug.

- **A namespace that prevents naming conflicts**. In any large codebase, especially one that combines code from multiple authors, naming conflicts are inevitable. Two developers might both want to call their player class "Player" or their damage function "CalculateDamage". Modules solve this by providing separate namespaces - you can have `combat_module.CalculateDamage` and `magic_module.CalculateDamage` without any conflict.

- **A versioned unit that can evolve while maintaining backward compatibility**. Once you publish a module, Verse ensures that any changes you make won't break existing code that depends on it. You can add new features, fix bugs, and improve performance, but you cannot remove public members or change their types in incompatible ways.

Each module is intrinsically linked to the file system structure of your project. When you create a folder in your Verse project, that folder automatically becomes a module. The module's name is simply the folder's name, making the relationship between your file organization and your code organization completely transparent.

All `.verse` files within the same folder are considered part of that module and share the same namespace. This means that if you have three files - `player.verse`, `inventory.verse`, and `equipment.verse` - all in a folder called `player_systems`, they all contribute to the `player_systems` module and can reference each other's definitions without any import statements. This automatic grouping makes it easy to split large modules across multiple files for better organization while maintaining the logical unity of the module.

## Module Paths

Module paths are the addressing system that makes Verse's vision of a shared, persistent Metaverse possible. Just as every website on the internet has a unique URL, every module in Verse has a unique path that identifies it globally. This path system is more than just a naming convention - it's a fundamental part of how Verse manages code distribution, versioning, and dependencies.

### Understanding the Path System

The path system borrows conceptually from web domains but adapts them for the needs of a programming language. A module path starts with a forward slash `/` and typically includes a domain-like identifier followed by one or more path segments. This creates a hierarchical namespace that is both human-readable and globally unique.

The format `/domain/path/to/module` serves several important purposes:

- **Persistent and unique identification**: Once a module is published at a particular path, that path belongs to it forever. No other module can ever claim the same path, ensuring that dependencies always resolve to the correct code.

- **Ownership and authority**: The domain portion of the path (like `Fortnite.com` or `Verse.org`) indicates who owns and maintains the module. This helps developers understand the source and trustworthiness of the code they're using.

- **Discoverability**: Because paths follow a predictable pattern, developers can often guess or easily find the modules they need. Documentation and tooling can also leverage this structure to provide better discovery experiences.

- **Hierarchical organization**: The path structure naturally supports organizing related modules together. For example, all UI-related modules might live under `/YourGame.com/UI/`, making them easy to find and understand as a group.

### Standard Module Paths

Epic Games provides several standard modules that are commonly used in Verse development:

- `/Verse.org/Verse` - Core language features and standard library functions
- `/Verse.org/Random` - Random number generation utilities
- `/Verse.org/Simulation` - Simulation and timing utilities
- `/Fortnite.com/Devices` - Integration with Fortnite Creative devices
- `/UnrealEngine.com/Temporary/Diagnostics` - Debugging and diagnostic tools
- `/UnrealEngine.com/Temporary/SpatialMath` - 3D math and spatial operations

The use of "Temporary" in some paths indicates that these modules are provisional and may be reorganized in future versions of Verse. This naming convention helps set expectations about the stability of the API.

### Custom Module Paths

When you create your own modules, they can exist at various levels of the path hierarchy:

- `/YourGame/` - Top-level module for your game
- `/YourGame/Player/` - Player-related functionality
- `/YourGame/Player/Inventory/` - Specific inventory management
- `/pizlonator@fn.com/NightDeath/` - Personal or experimental modules

The ability to include email-like identifiers (such as `pizlonator@fn.com`) allows individual developers to claim their own namespace without needing to own a domain. This democratizes the module system while still maintaining uniqueness guarantees.

## Creating Modules

### File-Based Modules

When you create a subfolder in a Verse project, a module is automatically created for that folder. The file structure directly maps to the module hierarchy.

### Inline Module Definition

You can create modules within a `.verse` file using the following syntax:

```verse
# Colon syntax
module1 := module:
    # Module contents here
    MyConstant<public>:int = 42

    MyClass<public> := class:
        Value:int = 0

# Bracket syntax (also supported)
module2 := module
{
    # Module contents here
    AnotherConstant<public>:string = "Hello"
}
```

### Nested Modules

Modules can contain other modules, creating a hierarchy:

```verse
base_module<public> := module:
    submodule<public> := module:
        submodule_class<public> := class:
            Value:int = 100

    module_class<public> := class:
        Name:string = ""
```

The file structure `module_folder/base_module` is equivalent to:

```verse
module_folder := module:
    base_module := module:
        submodule := module:
            submodule_class := class:
                # Class definition
```

## Module Members

A module can contain:
- Constants and variables
- Functions
- Classes, interfaces, and structs
- Enums
- Other module definitions
- Type definitions

Example:

```verse
game_systems := module:
    # Constants
    MaxPlayers<public>:int = 100
    DefaultHealth<public>:float = 100.0

    # Variables
    var<private> CurrentPlayers:int = 0

    # Functions
    CalculateDamage<public>(BaseDamage:float, Multiplier:float):float =
        BaseDamage * Multiplier

    # Classes
    player_stats<public> := class:
        Health:float = DefaultHealth
        Score:int = 0

    # Nested modules
    inventory_system<public> := module:
        item<public> := class:
            Name:string = ""
            Weight:float = 0.0
```

## Importing Modules

The import system in Verse is designed to be explicit and predictable. Unlike some languages that automatically import commonly used modules or search multiple locations for dependencies, Verse requires you to explicitly declare every external module you want to use. This explicitness helps prevent naming conflicts and makes dependencies clear.

### The Using Statement

The `using` statement is the primary mechanism for importing modules into your Verse code. It appears at the top of your file, before any other code definitions, and makes the contents of the specified module available in your current scope.

The basic syntax is straightforward - the keyword `using` followed by the module path in curly braces:

```verse
using { /Verse.org/Random }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
```

When you import a module, all its public members become available in your code. However, you still need to qualify them with the module name unless the names are unambiguous. This qualification requirement helps maintain code clarity and prevents accidental use of the wrong definition when multiple modules define similar names.

### Understanding Import Resolution

When Verse encounters a `using` statement, it follows a specific resolution process:

1. **Absolute paths** (starting with `/`) are resolved from the global module registry
2. **Relative paths** (without leading `/`) are resolved relative to the current module's location
3. **Nested modules** can be accessed through their parent modules

This resolution process happens at compile time, meaning that all imports must be resolvable when your code is compiled. There's no runtime module loading or dynamic imports in Verse.

### Local and Relative Imports

For modules within your own project, you have flexibility in how you reference them:

```verse
# Absolute import from your project root
using { /MyGameProject/Systems/Combat }

# Import from a sibling folder
using { ../UI/MainMenu }

# Import from the same directory
using { player_controller }

# Import from a subdirectory
using { Subsystems/WeaponSystem }
```

The choice between absolute and relative imports often depends on your project structure and whether you plan to reorganize your modules. Absolute imports are more stable when refactoring, while relative imports can make module groups more portable.

### Importing Nested Modules

Nested modules present special considerations for importing. The order in which you import modules matters, and there are multiple valid approaches:

```verse
# Method 1: Import parent first, then child
using { game_systems }
using { inventory }  # Assumes inventory is nested in game_systems

# Method 2: Direct path to nested module
using { game_systems.inventory }

# Method 3: Import parent and access child through qualification
using { game_systems }
# Later in code: game_systems.inventory.Item

# IMPORTANT: This order causes an error
# using { inventory }      # Error: inventory not found
# using { game_systems }   # Too late, inventory import already failed
```

The restriction on import order exists because Verse resolves imports sequentially. When you import a nested module directly, Verse needs to know about its parent module first. This is why importing the parent before the child always works, while the reverse order fails.

### Import Scope and Visibility

Imports in Verse have file scope - they only affect the file in which they appear. If you have multiple `.verse` files in the same module, each file needs its own import statements for external modules. However, files within the same module can see each other's definitions without imports:

```verse
# File: player_module/health.verse
health_component := class:
    CurrentHealth:float = 100.0

# File: player_module/armor.verse
# No import needed for health_component since it's in the same module
armor_component := class:
    HealthComp:health_component = health_component{}
```

### Managing Import Conflicts

When two imported modules define members with the same name, you need to disambiguate:

```verse
using { /GameA/Combat }
using { /GameB/Combat }

# Both modules might define CalculateDamage
# You must use qualified names:
DamageA := Combat.CalculateDamage(10.0)  # Error: ambiguous
DamageA := /GameA/Combat.CalculateDamage(10.0)  # OK: fully qualified
DamageB := /GameB/Combat.CalculateDamage(10.0)  # OK: fully qualified
```

### Qualified Names and Qualified Access

After importing, you can refer to module contents using qualified names. Verse provides two forms of qualification: standard dot notation for most cases, and special qualified access syntax for disambiguation.

#### Standard Qualified Names

The most common form uses dot notation to access module members:

```verse
using { game_systems }

MyFunction():void =
    # Direct access to public members
    Damage := game_systems.CalculateDamage(50.0, 2.0)

    # Access nested module members
    NewItem := game_systems.inventory_system.item{Name := "Sword"}

    # Alternatively, import the nested module directly
    using { game_systems.inventory_system }
    AnotherItem := item{Name := "Shield"}
```

#### Qualified Access Expression

When you need to disambiguate between identifiers with the same name from different modules, or when you want to explicitly specify the scope of an identifier, Verse provides a qualified access expression using parentheses and a colon:

```verse
# Qualified access syntax: (qualifier:)identifier

using { combat_module }
using { magic_module }

ProcessDamage():void =
    # Both modules define CalculateDamage
    PhysicalDamage := (combat_module:)CalculateDamage(100.0)
    MagicalDamage := (magic_module:)CalculateDamage(100.0)

    # Explicitly qualify local vs module identifiers
    LocalItem := item{Name := "Sword"}  # Local definition
    ModuleItem := (inventory_module:)item{Name := "Shield"}  # From module
```

The qualified access expression `(module:)identifier` is particularly useful in several scenarios:

1. **Resolving naming conflicts**: When multiple imported modules export the same identifier
2. **Explicit scoping**: When you want to make it clear which module an identifier comes from for readability
3. **Accessing shadowed names**: When a local definition shadows a module member
4. **Generic programming**: When working with parametric types where the qualifier might be computed

```verse
# Example with computed qualifiers in generic code
ResolveConflict(ModuleA:module_type, ModuleB:module_type)<decides>:void =
    # Use qualified access to ensure we get the right function
    ResultA := (ModuleA:)Process()
    ResultB := (ModuleB:)Process()

    # The qualifier can be any expression that evaluates to a module
    DynamicModule := if (UseAlternative[]) then ModuleA else ModuleB
    FinalResult := (DynamicModule:)Process()
```

This qualified access syntax extends beyond modules to classes and other scopes:

```verse
my_class := class:
    Value:int = 10

    GetValue():int =
        # Explicitly access class member even if shadowed
        (my_class:)Value

outer_scope := module:
    Constant:int = 42

    inner_function():void =
        Constant:int = 7  # Shadows outer Constant

        # Access both versions using qualified access
        LocalVal := Constant  # Gets 7
        OuterVal := (outer_scope:)Constant  # Gets 42
```

## Access Specifiers and Visibility

Module members have access specifiers that control their visibility:

### Access Levels

| Specifier | Visibility | Usage |
|-----------|------------|-------|
| `<public>` | Universally accessible | Members intended for external use |
| `<internal>` | Only within the module (default) | Module-private implementation |
| `<private>` | Only in immediate enclosing scope | Local to class/struct |
| `<protected>` | Current class and subtypes | Inheritance hierarchies |
| `<scoped>` | Current scope and enclosing scopes | Special use cases |

Example:

```verse
my_module := module:
    # Internal by default - not accessible outside module
    InternalConstant:int = 10

    # Public - accessible everywhere
    PublicFunction<public>():void =
        Print("Hello from module")

    # Mixed access on variables
    var<protected> ProtectedVar<public>:int = 5  # Read public, write protected

    public_class<public> := class:
        PublicField<public>:int = 0
        PrivateField<private>:string = ""

        PublicMethod<public>():void =
            Print("Public method")

        ProtectedMethod<protected>():void =
            Print("Protected method")
```

## Module-Scoped Variables

Variables defined at module scope are global to any game instance where the variable is in scope.

### Session-Scoped Variables

Use `weak_map(session, t)` for variables that persist for the duration of a game session:

```verse
using { /Verse.org/Simulation }

var GlobalCounter:weak_map(session, int) = map{}

IncrementCounter():void =
    CurrentValue := if (Value := GlobalCounter[GetSession()]) then Value + 1 else 0
    if (set GlobalCounter[GetSession()] = CurrentValue) {}
```

### Persistent Player Data

Use `weak_map(player, t)` for data that persists across game sessions:

```verse
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }

var PlayerSaveData:weak_map(player, player_data) = map{}

player_data := class<final><persistable>:
    Level:int = 1
    Experience:int = 0
    UnlockedItems:[]string = array{}

SavePlayerProgress(Player:player, NewData:player_data):void =
    set PlayerSaveData[Player] = NewData
```

## Metaverse and Publishing

### Publishing Modules

When you publish a module to the Metaverse:
1. The module path becomes globally accessible
2. Public members become part of the module's API
3. The module must maintain backward compatibility

Example of publishing evolution:

```verse
# Initial publication
Thing<public>:int = 666

# Valid updates:
# - Change the value (not the type)
Thing<public>:int = 10

# - Make the type more specific (subtype)
Thing<public>:nat = 20  # nat is a subtype of int

# Invalid updates (would be rejected):
# - Remove the member
# - Change to incompatible type
# Thing<public>:string = "hello"  # Would fail
```

### Backward Compatibility Guarantees

The Metaverse provides guarantees for published modules:
- Public members will never stop existing
- Types will never change in incompatible ways
- Values can be updated by the publisher
- New members can be added

## Local Qualifiers

In V1, the `(local:)` qualifier can disambiguate identifiers within functions:

```verse
MyModule := module:
    X:int = 1

    # Without local qualifier - would cause shadowing error
    # Foo(X:int):int = X + X  # Ambiguous!

    # With local qualifier - clear disambiguation
    Foo((local:)X:int):int =
        (MyModule:)X + (local:)X  # Module X + parameter X
```

## Best Practices

### Module Organization

1. **Single Responsibility**: Each module should have a clear, focused purpose
2. **Hierarchical Structure**: Use nested modules for related functionality
3. **Clear Naming**: Use descriptive names that indicate the module's purpose
4. **Documentation**: Document public APIs clearly

### Access Control

1. **Minimal Public Surface**: Only make public what needs to be
2. **Use Internal by Default**: Keep implementation details private
3. **Protected for Inheritance**: Use protected for extensible classes
4. **Consistent Access Patterns**: Be consistent within a module

### Importing

1. **Import What You Need**: Don't import entire module hierarchies unnecessarily
2. **Order Matters**: Import base modules before submodules
3. **Avoid Circular Dependencies**: Structure modules to prevent circular imports
4. **Use Qualified Names When Unclear**: Disambiguate with full paths when needed

### Versioning and Evolution

1. **Plan for Change**: Design APIs with future evolution in mind
2. **Maintain Compatibility**: Never break existing public interfaces
3. **Deprecate Gracefully**: Mark old APIs as deprecated before removal
4. **Version Documentation**: Keep clear records of API changes

## Common Patterns

### Factory Module Pattern

```verse
entity_factory := module:
    entity_base<public> := class<abstract>:
        Name:string = ""
        Health:float = 100.0

    player_entity<public> := class(entity_base):
        PlayerID:int = 0

    npc_entity<public> := class(entity_base):
        AILevel:int = 1

    CreatePlayer<public>(ID:int, PlayerName:string):player_entity =
        player_entity{PlayerID := ID, Name := PlayerName}

    CreateNPC<public>(NPCName:string, Level:int):npc_entity =
        npc_entity{Name := NPCName, AILevel := Level}
```

### Service Module Pattern

```verse
game_service := module:
    # Private implementation
    var<private> ServiceState:service_state = service_state{}

    service_state := class:
        IsRunning:logic = false
        ConnectionCount:int = 0

    # Public interface
    Start<public>():void =
        set ServiceState = service_state{IsRunning := true}

    Stop<public>():void =
        set ServiceState = service_state{IsRunning := false}

    GetStatus<public>():string =
        if (ServiceState.IsRunning?) then "Running" else "Stopped"
```

### Configuration Module Pattern

```verse
game_config := module:
    # Configuration constants
    MaxPlayers<public>:int = 100
    DefaultSpawnDelay<public>:float = 3.0
    EnablePvP<public>:logic = true

    # Difficulty settings
    difficulty<public> := module:
        Easy<public>:int = 0
        Normal<public>:int = 1
        Hard<public>:int = 2

        GetMultiplier<public>(Level:int):float =
            case(Level):
                Easy => 0.5
                Normal => 1.0
                Hard => 2.0
                _ => 1.0
```

## Troubleshooting Common Module Issues

When working with modules, you may encounter various issues. Understanding these common problems and their solutions will help you debug module-related errors more efficiently.

### Module Not Found Errors

**Problem**: The compiler reports that a module cannot be found when you try to import it.

**Common Causes and Solutions**:

1. **Incorrect path**: Double-check the module path in your `using` statement. Remember that paths are case-sensitive.
   ```verse
   # Wrong: different case
   using { /verse.org/random }  # Error: module not found

   # Correct: proper case
   using { /Verse.org/Random }  # Works
   ```

2. **Missing parent module import**: When importing nested modules, ensure the parent is imported first.
   ```verse
   # Wrong: child before parent
   using { inventory }  # Error if inventory is nested

   # Correct: parent first
   using { game_systems }
   using { inventory }
   ```

3. **File location mismatch**: Ensure your file structure matches your module structure. If you have a folder named `player_systems`, all files in that folder are part of the `player_systems` module.

### Access Denied Errors

**Problem**: You can't access a member of an imported module.

**Common Causes and Solutions**:

1. **Missing access specifier**: Members without the `<public>` specifier are internal by default.
   ```verse
   # In module_a
   SecretValue:int = 42  # Internal by default
   PublicValue<public>:int = 100  # Explicitly public

   # In another module
   using { module_a }
   X := module_a.SecretValue  # Error: not accessible
   Y := module_a.PublicValue  # Works
   ```

2. **Protected or private members**: These are not accessible outside their defining scope.
   ```verse
   # In a class
   class_a := class:
       PrivateField<private>:int = 10
       ProtectedField<protected>:int = 20
       PublicField<public>:int = 30

   # Outside the class
   Obj := class_a{}
   X := Obj.PrivateField  # Error: private
   Y := Obj.PublicField   # Works
   ```

### Circular Dependency Errors

**Problem**: Two modules try to import each other, creating a circular dependency.

**Solution**: Restructure your code to avoid circular dependencies:

1. **Extract common code**: Move shared definitions to a third module that both can import.
2. **Use interfaces**: Define interfaces in a separate module to break the dependency cycle.
3. **Reconsider architecture**: Circular dependencies often indicate a design issue that needs rethinking.

### Name Collision Errors

**Problem**: Two imported modules define members with the same name.

**Solution**: Use fully qualified names to disambiguate:
```verse
using { /GameA/Combat }
using { /GameB/Combat }

# Ambiguous
Damage := CalculateDamage(10.0)  # Error: which CalculateDamage?

# Explicit
DamageA := /GameA/Combat.CalculateDamage(10.0)  # Clear
DamageB := /GameB/Combat.CalculateDamage(10.0)  # Clear
```

### Persistence Issues

**Problem**: Module-scoped variables aren't persisting as expected.

**Common Causes and Solutions**:

1. **Wrong type used**: Ensure you're using `weak_map(player, t)` for player persistence.
2. **Type not persistable**: Check that your custom types have the `<persistable>` specifier.
3. **Initialization timing**: Make sure you're initializing persistent data at the right time in the game lifecycle.

### Local Qualifier Conflicts (V1)

**Problem**: Shadowing errors when local identifiers conflict with module members.

**Solution**: Use the `(local:)` qualifier to disambiguate:
```verse
module_x := module:
    Value:int = 10

    ProcessValue((local:)Value:int):int =
        (module_x:)Value + (local:)Value  # Clear distinction
```

## Detailed Example: Building a Game Module System

Let's walk through building a complete module system for a game, explaining each step and decision along the way.

### Step 1: Planning the Module Structure

First, we need to plan our module hierarchy. For a typical game, we might want:
- Core game systems (player, combat, inventory)
- UI components
- Game configuration
- Utility functions

This translates to a folder structure:
```
MyGame/
├── Core/
│   ├── Player/
│   ├── Combat/
│   └── Inventory/
├── UI/
├── Config/
└── Utils/
```

### Step 2: Creating the Configuration Module

Let's start with a configuration module that other modules will depend on:

```verse
# File: MyGame/Config/game_settings.verse

# This module holds all game-wide configuration
# Other modules will import this to access shared settings

# Basic game parameters
MaxPlayers<public>:int = 100
DefaultPlayerHealth<public>:float = 100.0
RespawnDelay<public>:float = 5.0

# Nested module for damage configuration
damage_config<public> := module:
    BaseDamage<public>:float = 10.0
    CriticalMultiplier<public>:float = 2.0

    # Function to calculate final damage
    CalculateFinalDamage<public>(Base:float, IsCritical:logic):float =
        if (IsCritical?) then Base * CriticalMultiplier else Base

# Enum for game modes
game_mode<public> := enum:
    Deathmatch
    TeamDeathmatch
    CaptureTheFlag
    Survival
```

This configuration module demonstrates several important concepts:
- Public members that other modules can access
- Nested modules for organizing related settings
- Functions within modules for configuration-related calculations
- Enums for defining game constants

### Step 3: Creating the Player Module

Now let's create a player module that uses the configuration:

```verse
# File: MyGame/Core/Player/player_manager.verse

using { /MyGame/Config }  # Import our configuration
using { /Verse.org/Simulation }
using { /Fortnite.com/Devices }

# Player data structure
player_data := class<final><persistable>:
    Name:string = "Player"
    Level:int = 1
    Experience:int = 0
    TotalKills:int = 0
    TotalDeaths:int = 0

# Runtime player state (not persisted)
player_state := class:
    CurrentHealth:float = game_settings.DefaultPlayerHealth
    IsAlive:logic = true
    Position:vector3 = vector3{X := 0.0, Y := 0.0, Z := 0.0}

# Global player tracking
var ActivePlayers:weak_map(player, player_state) = map{}
var PlayerData:weak_map(player, player_data) = map{}

# Initialize a new player
InitializePlayer<public>(Player:player):void =
    # Set up persistent data if not exists
    if (not PlayerData[Player]):
        set PlayerData[Player] = player_data{Name := Player.GetName()}

    # Set up runtime state
    set ActivePlayers[Player] = player_state{}

# Handle player damage
ApplyDamage<public>(Player:player, Damage:float, IsCritical:logic):void =
    if (State := ActivePlayers[Player]):
        FinalDamage := damage_config.CalculateFinalDamage(Damage, IsCritical)
        NewHealth := State.CurrentHealth - FinalDamage

        if (NewHealth <= 0.0):
            HandlePlayerDeath(Player)
        else:
            set ActivePlayers[Player] = player_state:
                CurrentHealth := NewHealth
                IsAlive := State.IsAlive
                Position := State.Position

# Internal function (not public)
HandlePlayerDeath(Player:player):void =
    if (State := ActivePlayers[Player]):
        set ActivePlayers[Player] = player_state:
            CurrentHealth := 0.0
            IsAlive := false
            Position := State.Position

        # Update persistent stats
        if (Data := PlayerData[Player]):
            set PlayerData[Player] = player_data:
                Name := Data.Name
                Level := Data.Level
                Experience := Data.Experience
                TotalKills := Data.TotalKills
                TotalDeaths := Data.TotalDeaths + 1
```

This player module shows:
- How to import and use configuration from another module
- Separation of persistent and runtime data
- Public functions for external module interaction
- Private helper functions for internal logic
- Use of weak_maps for player data storage

### Step 4: Creating an Interacting Combat Module

Finally, let's create a combat module that uses both the configuration and player modules:

```verse
# File: MyGame/Core/Combat/combat_system.verse

using { /MyGame/Config }
using { /MyGame/Core/Player }
using { /Verse.org/Random }

# Weapon definition
weapon := class:
    Name:string = "Default Weapon"
    BaseDamage:float = 10.0
    CriticalChance:float = 0.1  # 10% chance
    FireRate:float = 1.0  # Shots per second

# Combat event
combat_event := struct:
    Attacker:player
    Target:player
    Weapon:weapon
    Damage:float
    WasCritical:logic

# Process an attack between players
ProcessAttack<public>(Attacker:player, Target:player, WeaponUsed:weapon):void =
    # Calculate if this is a critical hit
    CritRoll := GetRandomFloat(0.0, 1.0)
    IsCritical := CritRoll < WeaponUsed.CriticalChance

    # Calculate damage
    BaseDamage := WeaponUsed.BaseDamage

    # Apply damage through player module
    player_manager.ApplyDamage(Target, BaseDamage, IsCritical)

    # Log the combat event (for analytics, achievements, etc.)
    LogCombatEvent(combat_event:
        Attacker := Attacker
        Target := Target
        Weapon := WeaponUsed
        Damage := BaseDamage
        WasCritical := IsCritical)

# Internal logging function
LogCombatEvent(Event:combat_event):void =
    # Implementation would log to analytics system
    Print("Combat: {Event.Attacker} hit {Event.Target} for {Event.Damage}")
```

This combat module demonstrates:
- Importing multiple modules
- Using public functions from other modules
- Creating module-specific data structures
- Interaction between different game systems

## Naming Conventions

Verse follows specific naming conventions that, while not enforced by the compiler, are strongly encouraged for consistency and readability:

- **Module names**: Use `snake_case` (e.g., `game_systems`, `player_inventory`)
- **Type names**: Use `snake_case` (e.g., `player_stats`, `item_data`)
- **Value names**: Use `CamelCase` (e.g., `MaxHealth`, `PlayerScore`)
- **Function names**: Use `CamelCase` (e.g., `CalculateDamage`, `GetPlayerStats`)

Following these conventions makes your code consistent with Epic's standard library and easier for other Verse developers to understand.

## Summary

Modules and paths provide the foundation for code organization and sharing in Verse. They enable:
- Clean separation of concerns through encapsulation
- Reusable code libraries that can be shared across projects
- Clear API boundaries between different systems
- Namespace management to avoid naming conflicts
- Version evolution with strong compatibility guarantees

Understanding modules is essential for building maintainable, scalable Verse applications that can integrate with the broader Metaverse ecosystem. The module system's design reflects Verse's vision of a persistent, shared universe where code can be published once and trusted to work forever, while still allowing for updates and improvements.