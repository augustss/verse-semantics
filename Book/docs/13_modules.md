# Modules

Modules and paths are fundamental concepts for code organization, namespace management, and the ability to share and reuse code across projects. Think of modules as containers that group related functionality together, similar to packages in other programming languages, but with stronger guarantees about versioning and compatibility.

In the context of game development, modules allow you to separate different aspects of your game logic into manageable, reusable pieces. For example, you might have one module for player inventory management, another for combat mechanics, and yet another for UI interactions. Each module encapsulates its own functionality while exposing only the necessary interfaces to other parts of your code.

The module system is designed to support the vision of a persistent, shared Metaverse where code can be published once and used by anyone, anywhere, with confidence that it will continue to work even as the original author updates and improves it. This is achieved through strict backward compatibility rules and a global namespace system that ensures every piece of published code has a unique, permanent address.

Each module is intrinsically linked to the file system structure of your project. When you create a folder in your Verse project, that folder automatically becomes a module. The module's name is simply the folder's name, making the relationship between your file organization and your code organization completely transparent.

All `.verse` files within the same folder are considered part of that module and share the same namespace. This means that if you have three files - `player.verse`, `inventory.verse`, and `equipment.verse` - all in a folder called `player_systems`, they all contribute to the `player_systems` module and can reference each other's definitions without any import statements. This automatic grouping makes it easy to split large modules across multiple files for better organization while maintaining the logical unity of the module.

## Paths

Paths are the addressing system that makes Verse's vision of a shared, persistent Metaverse possible. Just as every website on the internet has a unique URL, every module has a unique path that identifies it globally. This path system is more than just a naming convention - it's a fundamental part of how Verse manages code distribution, versioning, and dependencies.

### Understanding  Paths

Paths borrow conceptually from web domains with adaptations for the needs of a programming language. A path starts with a forward slash `/` and typically includes a domain-like identifier followed by one or more path segments. This creates a hierarchical namespace that is both human-readable and globally unique.

The format `/domain/path/to/module` serves several important purposes:

- **Persistent and unique identification**: Once a module is published at a particular path, that path belongs to it forever. No other module can ever claim the same path, ensuring that dependencies always resolve to the correct code.

- **Ownership and authority**: The domain portion of the path (like `Fortnite.com` or `Verse.org`) indicates who owns and maintains the module. This helps developers understand the source and trustworthiness of the code they're using.

- **Discoverability**: Because paths follow a predictable pattern, developers can often guess or easily find the modules they need. Documentation and tooling can also leverage this structure to provide better discovery experiences.

- **Hierarchical organization**: The path structure naturally supports organizing related modules together. For example, all UI-related modules might live under `/YourGame.com/UI/`, making them easy to find and understand as a group.

### Standard  Paths

Epic Games provides several standard modules that are commonly used:

- `/Verse.org/Verse` - Core language features and standard library functions
- `/Verse.org/Random` - Random number generation utilities
- `/Verse.org/Simulation` - Simulation and timing utilities
- `/Fortnite.com/Devices` - Integration with Fortnite Creative devices
- `/UnrealEngine.com/Temporary/Diagnostics` - Debugging and diagnostic tools
- `/UnrealEngine.com/Temporary/SpatialMath` - 3D math and spatial operations

The use of "Temporary" in some paths indicates that these modules are provisional and may be reorganized in future versions of Verse. This naming convention helps set expectations about the stability of the API.

### Custom Paths

When you create your own modules, they can exist at various levels of the path hierarchy:

- `/YourGame/` - Top-level module for your game
- `/YourGame/Player/` - Player-related functionality
- `/YourGame/Player/Inventory/` - Specific inventory management
- `/pizlonator@fn.com/NightDeath/` - Personal or experimental modules

The ability to include email-like identifiers (such as `pizlonator@fn.com`) allows individual developers to claim their own namespace without needing to own a domain. This democratizes the module system while still maintaining uniqueness guarantees.

## Creating Modules

A module can contain:

- Constants and variables
- Functions
- Classes, interfaces, and structs
- Enums
- Other module definitions
- Type definitions

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

### Module Body Restrictions

Module bodies have strict requirements about what they can contain. Understanding these restrictions helps avoid common errors when defining modules.

**Modules Can Only Contain Definitions:**

A module body can only contain definition statements—declarations that bind names to values. You cannot include arbitrary expressions or executable statements:

<!--NoCompile-->
```verse
# Valid: All definitions
config := module:
    MaxValue:int = 100
    DefaultName:string = "Player"

    CalculateScore(Base:int):int = Base * 10

    player_class := class:
        Name:string

# Invalid: Contains non-definition expressions
bad_module := module:
    MaxValue:int = 100
    1 + 2  # ERROR 3560: Not a definition

# Invalid: Contains function call
bad_module2 := module:
    InitFunction():void = {}
    InitFunction()  # ERROR 3585: Cannot call function in module body
```

The restriction ensures that module initialization is deterministic and doesn't execute arbitrary code when the module is loaded.

**Type Annotations Required:**

All data definitions at module scope must explicitly specify their type. Type inference with `:=` alone is not allowed:

<!--NoCompile-->
```verse
# Invalid: Missing type annotation
bad_module := module:
    Value := 42  # ERROR 3547: Must specify type domain

# Valid: Explicit type annotation
good_module := module:
    Value:int = 42  # OK: Type explicitly specified
```

This requirement makes module interfaces explicit and helps with separate compilation and module evolution.

**Valid Module Contents:**

Modules can contain these categories of definitions:

```verse
utilities := module:
    # Constants with explicit types
    Version<public>:int = 1
    AppName<public>:string = "MyApp"

    # Functions
    Calculate<public>(X:int):int = X * 2

    # Classes, interfaces, structs
    data_class<public> := class:
        Value:int

    data_interface<public> := interface:
        GetValue():int

    data_struct<public> := struct:
        X:float
        Y:float

    # Enums
    status<public> := enum:
        Active
        Inactive

    # Nested modules
    nested<public> := module:
        NestedFunction<public>():void = {}

    # Type aliases
    coordinate<public> := tuple(float, float)
```

### Modules Are Not First-Class Values

Unlike functions, classes, or data values, modules are not first-class citizens in Verse. You cannot treat modules as values that can be stored, passed, or manipulated at runtime.

**Cannot Assign Modules to Variables:**

<!--NoCompile-->
```verse
my_module := module:
    Value<public>:int = 42

# Invalid: Cannot treat module as value
# M:my_module = my_module  # ERROR 3502, 3547
```

Modules exist purely as namespaces and organizational constructs at compile time. The module identifier `my_module` can only be used in specific contexts.

**Cannot Pass Modules as Arguments:**

<!--NoCompile-->
```verse
my_module := module:
    X<public>:int = 1

# Invalid: Cannot pass module as parameter
# ProcessModule(M:module):void = {}  # ERROR
# ProcessModule(my_module)  # ERROR
```

There is no `module` type that can be used in function signatures.

**Cannot Create Collections of Modules:**

<!--NoCompile-->
```verse
module_a := module:
    Value:int = 1

module_b := module:
    Value:int = 2

# Invalid: Cannot create tuple or array of modules
# Modules := (module_a, module_b)  # ERROR 3502
```

**Valid Module Usage:**

Modules can only be used in these specific ways:

1. **In qualified access expressions** using dot notation:
```verse
config := module:
    MaxPlayers<public>:int = 100

Players := config.MaxPlayers  # OK: Access member
```

2. **As qualifiers** in explicit qualification syntax:
```verse
my_module := module:
    Value<public>:int = 42

    GetValue<public>():int = (my_module:)Value  # OK: As qualifier
```

3. **In `using` statements** for imports:
```verse
using { my_module }  # OK: Import module
```

These restrictions ensure that modules remain purely compile-time organizational tools and don't incur runtime overhead.

## Importing Modules

The import system is designed to be explicit and predictable. Unlike some languages that automatically import commonly used modules or search multiple locations for dependencies, Verse requires you to explicitly declare every external module you want to use. This explicitness helps prevent naming conflicts and makes dependencies clear.

### Using

The `using` statement is the primary mechanism for importing modules into your Verse code. It appears at the top of your file, before any other code definitions, and makes the contents of the specified module available in your current scope.

The basic syntax is straightforward - the keyword `using` followed by the module path in curly braces:

```verse
using { /Verse.org/Random }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
```

When you import a module, all its public members become available in your code. However, you still need to qualify them with the module name unless the names are unambiguous. This qualification requirement helps maintain code clarity and prevents accidental use of the wrong definition when multiple modules define similar names.

**Using is a Statement, Not an Expression:**

The `using` directive is a statement-level declaration that must appear at the top level of your code. You cannot use it as an expression or embed it in other expressions:

<!--NoCompile-->
```verse
# Invalid: using in expression context
# f():void = using{MyModule}  # ERROR 3669

# Invalid: using in conditional
# if (using{MyModule}, Condition?):
#     DoSomething()  # ERROR 3669

# Invalid: using in class/struct/interface body
# my_class := class:
#     using{MyModule}  # ERROR 3537
#     Field:int

# Invalid: using module path in function body
# ProcessData():void =
#     using{/MyProject/UtilityModule}  # ERROR 3669
#     Calculate()
```

Module `using` statements must appear at the file or module level, not nested within other constructs. This ensures that imports are visible and consistent throughout the scope where they're declared.

**Note:** While module imports with paths are not allowed in function bodies, Verse does support **local scope `using`** with local variables and parameters. See [Local Scope Using](#local-scope-using) below for details.

**Valid using placement:**

```verse
# At file level (most common)
using { /Verse.org/Random }
using { /Verse.org/Simulation }

ProcessData():void =
    # Use imported functions
    Value := GetRandomFloat(0.0, 1.0)

# Within module definition
utilities := module:
    using { /Verse.org/Random }

    GenerateId<public>():int =
        GetRandomInt(1, 1000000)
```

### Import Resolution

When Verse encounters a `using` statement, it follows a specific resolution process:

1. **Absolute paths** (starting with `/`) are resolved from the global module registry
2. **Relative paths** (without leading `/`) are resolved relative to the current module's location
3. **Nested modules** can be accessed through their parent modules

This resolution process happens at compile time, meaning that all imports must be resolvable when your code is compiled. There's no runtime module loading or dynamic imports in Verse.

### Local and Relative Imports

For modules within your own project, you have flexibility in how you reference them:

<!--NoCompile-->
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

Imports have file scope - they only affect the file in which they appear. If you have multiple `.verse` files in the same module, each file needs its own import statements for external modules. However, files within the same module can see each other's definitions without imports:

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

### Qualified Names and  Access

After importing, you can refer to module contents using qualified names. Verse provides two forms of qualification: standard dot notation for most cases, and special qualified access syntax for disambiguation.

When you need to disambiguate between identifiers with the same name from different modules, or when you want to explicitly specify the scope of an identifier, use a qualified access expression using parentheses and a colon:

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
        LocalVal := (local:)Constant  # Gets 7
        OuterVal := (outer_scope:)Constant  # Gets 42
```

## Module-Scoped Variables

Variables defined at module scope are global to any game instance where the variable is in scope.

Use `weak_map(session, t)` for variables that persist for the duration of a game session:

```verse
using { /Verse.org/Simulation }

var GlobalCounter:weak_map(session, int) = map{}

IncrementCounter():void =
    CurrentValue := if (Value := GlobalCounter[GetSession()]) then Value + 1 else 0
    if (set GlobalCounter[GetSession()] = CurrentValue) {}
```

Use `weak_map(player, t)` for data that persists across game sessions:

```verse
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }

var PlayerSaveData:weak_map(player, player_data) = map{}

player_data := class<final><persistable>:
    Level:int = 1
    Experience:int = 0
    UnlockedItems:[]string = array{}

SavePlayerProgress(Player:player, NewData:player_data)<decides>:void =
    set PlayerSaveData[Player] = NewData
```

## Metaverse and Publishing

When you publish a module to the Metaverse, the module path becomes globally accessible, its public members become part of the module's API, and from that point the module must maintain backward compatibility.

The following example of shows how evolution works:

<!--NoCompile-->
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

## Local Qualifiers

In V1, the `(local:)` qualifier can disambiguate identifiers within functions. This is critical for evolution compatibility—when external modules add new public definitions after your code is published, `(local:)` ensures your local definitions take precedence.

### Basic Usage

```verse
# External module adds ShadowX after your code published
ExternalModule<public> := module:
    ShadowX<public>:int = 10  # Added later!

MyModule := module:
    using{ExternalModule}

    # Without (local:) - error 3588/3532: shadowing conflict
    # Foo():float =
    #     ShadowX:float = 0.0  # Error: conflicts with ExternalModule.ShadowX
    #     ShadowX

    # With (local:) - clear disambiguation
    Foo():float =
        (local:)ShadowX:float = 0.0  # Local variable
        (local:)ShadowX              # Returns 0.0, not 10
```

### Valid `(local:)` Locations

The `(local:)` qualifier can be used in these contexts:

**Function parameters:**
```verse
ProcessValue((local:)Value:int):int =
    (local:)Value + 1
```

**Function body data definitions:**
```verse
Compute():int =
    (local:)Result:int = 42
    (local:)Result
```

**For loop variables:**
```verse
SumValues():int =
    var Total:int = 0
    for ((local:)I := 0..10):
        set Total += (local:)I
    Total
```

**If conditions:**
```verse
CheckValue():float =
    if ((local:)X := GetValue[], (local:)X > 5.0):
        (local:)X
    else:
        0.0
```

**Block scopes:**
```verse
ComputeInBlock():int =
    block:
        (local:)Temp:int = 10
        (local:)Temp * 2
```

**Class blocks:**
```verse
my_class := class:
    var Value<public>:int = 0
    block:
        (local:)Value:int = 42
        set (/PackagePath/my_class:)Value = (local:)Value
```

### Invalid `(local:)` Locations

The `(local:)` qualifier **cannot** be used in these contexts (all produce error 3612):

**Module members:**
```verse
# Error 3612: local not allowed here
MyModule := module:
    (local:)X:int = 5  # Compile error
```

**Class/struct fields:**
```verse
# Error 3612: local not allowed here
my_class := class:
    (local:)Field:int = 5  # Compile error
```

**Interface methods:**
```verse
# Error 3612: local not allowed here
my_interface := interface:
    (local:)Method():void  # Compile error
```

**Enum values:**
```verse
# Error 3612: local only allowed inside functions
my_enum := enum:
    (local:)Value  # Compile error
```

**Using statements:**
```verse
# Error 3612: local not allowed here
using{(local:)SomeModule}  # Compile error
```

### Nested Scope Limitation

Currently, you **cannot** redefine a `(local:)` qualified identifier in nested blocks (error 3532):

```verse
# Error 3532: cannot redefine local identifier
F((local:)X:int):int =
    block:
        (local:)X:float = 5.5  # Error: X already defined in function
    (local:)X
```

This limitation may be lifted in future versions to support more complex scoping patterns.

## Automatic Qualification

When you write Verse code, you use simple, unqualified identifiers for clarity and readability. However, the Verse compiler internally transforms all identifiers into fully-qualified forms that explicitly specify their scope and origin. This process, called **automatic qualification**, ensures that every identifier is unambiguous and can be resolved to exactly one definition.

Understanding automatic qualification helps you understand how Verse resolves names, why certain errors occur, and how the module system maintains correctness even in complex codebases with many modules and overlapping names.

### What Gets Qualified

The compiler qualifies several categories of identifiers:

1. **Top-level definitions** - Functions, variables, classes, modules at package scope
2. **Type references** - All types, including built-in types like `int` and `string`
3. **Function parameters** - Local parameters get the `(local:)` qualifier
4. **Class and interface members** - Methods, fields, nested within composite types
5. **Module members** - Public and internal definitions within modules
6. **Nested scopes** - References within nested modules, classes, and functions

### Qualification Patterns

Verse uses several patterns to qualify identifiers based on their scope:

**Package-level qualification**: Definitions at the root of a package are qualified with the package path:

```verse
# What you write:
Function(X:int):int = X

# How the compiler sees it:
(/YourPackage:)Function((local:)X:(/Verse.org/Verse:)int):(/Verse.org/Verse:)int = (local:)X
```

The package path `/YourPackage` becomes the qualifier for `Function`, while the parameter `X` gets the special `(local:)` qualifier, and the built-in type `int` is qualified with its standard library path `/Verse.org/Verse`.

**Local scope qualification**: Function parameters and local variables are marked with `(local:)`:

```verse
# What you write:
ProcessValue(Input:int, Multiplier:int):int =
    Input * Multiplier

# How the compiler sees it:
(/YourPackage:)ProcessValue((local:)Input:(/Verse.org/Verse:)int, (local:)Multiplier:(/Verse.org/Verse:)int):(/Verse.org/Verse:)int =
    (local:)Input * (local:)Multiplier
```

**Nested scope qualification**: Members within classes, interfaces, or modules get qualified with their container's path:

```verse
# What you write:
player_class := class:
    Health:float = 100.0

    TakeDamage(Amount:float):void =
        set Health = Health - Amount

# How the compiler sees it:
(/YourPackage:)player_class := class:
    (/YourPackage/player_class:)Health:(/Verse.org/Verse:)float = 100.0

    (/YourPackage/player_class:)TakeDamage((local:)Amount:(/Verse.org/Verse:)float):(/Verse.org/Verse:)void =
        set (/YourPackage/player_class:)Health = (/YourPackage/player_class:)Health - (local:)Amount
```

Notice how `Health` and `TakeDamage` are qualified with `/YourPackage/player_class` to indicate they're members of the class.

**Module member qualification**: Definitions within modules are qualified with the module path:

```verse
# What you write:
config := module:
    MaxPlayers<public>:int = 100

    GetPlayerLimit<public>():int = MaxPlayers

# How the compiler sees it:
(/YourPackage:)config := module:
    (/YourPackage/config:)MaxPlayers<public>:(/Verse.org/Verse:)int = 100

    (/YourPackage/config:)GetPlayerLimit<public>():(/Verse.org/Verse:)int =
        (/YourPackage/config:)MaxPlayers
```

### Built-in Type Qualification

All built-in types are qualified with their standard library paths. This makes it explicit where these types come from and maintains consistency with user-defined types:

```verse
# Common built-in types and their full qualifications:
int       → (/Verse.org/Verse:)int
float     → (/Verse.org/Verse:)float
string    → (/Verse.org/Verse:)string
logic     → (/Verse.org/Verse:)logic
message   → (/Verse.org/Verse:)message
```

When you write `X:int`, the compiler expands it to `X:(/Verse.org/Verse:)int`, making the type's origin explicit.

### Complex Example: Module with References

Here's a more realistic example showing how qualification works across multiple scopes:

```verse
# What you write:
game_system := module:
    BaseValue:int = 42

    calculator := module:
        Multiplier:int = 2

        Calculate(Input:int):int =
            Input * Multiplier + BaseValue

# How the compiler sees it:
(/YourGame:)game_system := module:
    (/YourGame/game_system:)BaseValue:(/Verse.org/Verse:)int = 42

    (/YourGame/game_system:)calculator := module:
        (/YourGame/game_system/calculator:)Multiplier:(/Verse.org/Verse:)int = 2

        (/YourGame/game_system/calculator:)Calculate((local:)Input:(/Verse.org/Verse:)int):(/Verse.org/Verse:)int =
            (local:)Input * (/YourGame/game_system/calculator:)Multiplier + (/YourGame/game_system:)BaseValue
```

Notice how:
- The parameter `Input` is `(local:)`
- `Multiplier` is qualified with its containing module path
- `BaseValue` is qualified with the outer module path
- All type references are qualified with the Verse standard library path

### Qualification with Using Statements

When you import modules with `using`, the compiler still qualifies all identifiers, but it can resolve unqualified names to the imported modules:

```verse
# What you write:
using { /Verse.org/Random }

GenerateRandomValue():float =
    GetRandomFloat(0.0, 1.0)

# How the compiler sees it:
using { /Verse.org/Random }

(/YourGame:)GenerateRandomValue():(/Verse.org/Verse:)float =
    (/Verse.org/Random:)GetRandomFloat(0.0, 1.0)
```

The compiler resolves `GetRandomFloat` to `/Verse.org/Random:GetRandomFloat` based on the `using` statement.

### When Automatic Qualification Matters

You rarely need to think about automatic qualification during normal development, as the compiler handles it transparently. However, understanding it helps in several situations:

**Debugging name resolution errors**: When the compiler reports ambiguous or unresolved identifiers, understanding qualification helps you see why:

```verse
using { /ModuleA }
using { /ModuleB }

# Both modules define Calculate
Result := Calculate(10)  # ERROR: Ambiguous - could be either module
```

The error occurs because the compiler cannot automatically qualify `Calculate` - it could be either `(/ModuleA:)Calculate` or `(/ModuleB:)Calculate`.

**Shadowing conflicts**: When a local variable has the same name as a module member:

```verse
MyModule := module:
    Value:int = 100

    Process(Value:int):int =
        # Without explicit qualification, this is ambiguous
        Value + Value  # Which Value? Module or parameter?
```

The compiler needs qualification to distinguish `(/MyModule:)Value` from `(local:)Value`.

**Understanding error messages**: Compiler error messages sometimes show qualified names to precisely identify which definition is involved:

```
Error: Cannot assign (/Verse.org/Verse:)string to (/Verse.org/Verse:)int at line 42
```

This makes it clear that the error involves the built-in `string` and `int` types, not user-defined types with the same names.

**Working with generated or reflected code**: Tools that generate Verse code or analyze code structure work with the qualified form, so understanding it helps when working with such tools.

### Explicit Qualification

While the compiler automatically qualifies identifiers, you can also explicitly qualify them using the qualified access syntax `(qualifier:)identifier`. This is useful when you want to override automatic resolution or make your intent explicit:

```verse
game_system := module:
    Value:int = 100

    # Explicitly qualify to avoid any ambiguity
    GetValue():int = (game_system:)Value

    # Use local qualifier for parameters
    SetValue((local:)Value:int):void =
        set (game_system:)Value = (local:)Value
```

Explicit qualification is particularly valuable when:
- Resolving naming conflicts between imported modules
- Making code more self-documenting
- Overriding shadowing behavior
- Working with dynamic or computed qualifiers

### Summary

Automatic qualification is Verse's mechanism for ensuring every identifier has a unique, unambiguous meaning. The compiler transforms your readable, unqualified code into fully-qualified internal representations using patterns like:

- `(/PackagePath:)` for package-level definitions
- `(local:)` for parameters and local variables
- `(/PackagePath/Container:)` for nested members
- `(/Verse.org/Verse:)` for built-in types

Understanding this transformation helps you reason about name resolution, debug scoping issues, and write more precise code when needed using explicit qualification.

## Local Scope Using

While module-level `using` imports modules by their paths, Verse also supports **local scope `using`** within function bodies to enable member access inference from local variables and parameters. This feature makes code cleaner when working with objects that have many member accesses.

### Basic Syntax

Local scope `using` takes a local variable or parameter identifier (not a module path) and makes its members accessible without explicit qualification:

```verse
entity := class:
    Name:string = "Entity"
    Health:int = 100

    UpdateHealth(Amount:int):void =
        set Health = Health + Amount

ProcessEntity(E:entity):void =
    # Explicit member access
    Print(E.Name)
    E.UpdateHealth(-10)
    Print(E.Health)

    # With local using - inferred member access
    using{E}
    Print(Name)         # Inferred as: E.Name
    UpdateHealth(-10)   # Inferred as: E.UpdateHealth(-10)
    Print(Health)       # Inferred as: E.Health
```

The `using{E}` expression makes all members of `E` accessible without the `E.` prefix within the current scope.

### With Local Variables

Local `using` works with variables defined in the same function:

```verse
CreateAndProcess():void =
    Player := player{Name := "Alice", Score := 100}

    # Without using
    Print(Player.Name)
    set Player.Score = Player.Score + 50

    # With using
    using{Player}
    Print(Name)         # Inferred as: Player.Name
    set Score = Score + 50  # Inferred as: Player.Score
```

### Block Scoping

The `using` scope is limited to the block where it appears and any nested blocks:

**Using in same block:**
```verse
ProcessData():void =
    block:
        Data := data_record{}
        using{Data}
        UpdateField(Value)  # Inferred as: Data.UpdateField(Data.Value)
    # Data members no longer accessible here
```

**Using from outer block:**
```verse
ProcessData():void =
    Data := data_record{}
    block:
        using{Data}  # Can use variable from outer scope
        UpdateField(Value)  # Works - Data in scope
```

**Nested block inheritance:**
```verse
ProcessData():void =
    Data := data_record{}
    using{Data}  # Applies to this block and nested blocks

    block:
        # Inner block inherits outer using
        UpdateField(Value)  # Still infers Data.UpdateField(Data.Value)
```

### Order Dependency

Member inference only works **after** the `using` expression is encountered:

```verse
# ERROR 3506: Cannot infer before using
ProcessData(Data:data_record):void =
    # UpdateField()  # ERROR - before using
    using{Data}
    UpdateField()  # OK - after using

# ERROR 3506: Using scope doesn't extend backward
ProcessData(Data:data_record):void =
    block:
        using{Data}
        UpdateField()  # OK - within using scope
    # UpdateField()  # ERROR - after using scope ended
```

The `using` statement acts as a declaration point - inference is not retroactive.

### Multiple Using and Conflict Resolution

You can have multiple `using` expressions in the same scope, but conflicting member names must be explicitly qualified:

```verse
player_stats := class:
    Health:int = 100
    Mana:int = 50
    GetInfo():string = "Player"

enemy_stats := class:
    Health:int = 80
    Armor:int = 20
    GetInfo():string = "Enemy"

ProcessCombat(Player:player_stats, Enemy:enemy_stats):void =
    using{Player}
    Print(GetInfo())  # Player.GetInfo()
    Print(Mana)       # Player.Mana (no conflict)

    using{Enemy}
    # Now both are in scope
    Print(Armor)      # Enemy.Armor (no conflict with Player)

    # ERROR 3588/3518: Conflicts must be qualified
    # Print(Health)   # Ambiguous - both have Health
    # Print(GetInfo())  # Ambiguous - both have GetInfo

    # Must qualify conflicting members
    Print(Player.Health)
    Print(Enemy.Health)
    Print(Player.GetInfo())
    Print(Enemy.GetInfo())
```

When members exist in multiple `using` contexts, you must explicitly qualify to disambiguate.

### Mutable Member Access

Local `using` works with mutable fields through the `set` keyword:

```verse
config := class:
    var Volume:float = 1.0
    var Quality:int = 2

UpdateSettings(Settings:config):void =
    using{Settings}

    # Mutable field access
    set Volume = 0.8     # Inferred as: set Settings.Volume = 0.8
    set Quality = 3      # Inferred as: set Settings.Quality = 3
```

### Error Cases and Restrictions

**Error 3666 - Cannot use same identifier twice:**

```verse
# ERROR 3666
ProcessData(Data:data_record):void =
    using{Data}
    using{Data}  # ERROR - already in using
```

**Error 3667 - Cannot use Self type:**

You cannot use `using` with an object of the same type you're currently inside:

```verse
# ERROR 3667
entity := class:
    Process():void =
        Other := entity{}
        using{Other}  # ERROR - same type as Self
```

This prevents confusion between `Self` members and `using` members.

**Error 3668 - Cannot use supertype of existing using:**

```verse
# ERROR 3668
base_class := class:
    BaseMethod():void = {}

derived_class := class(base_class):
    DerivedMethod():void = {}

Process():void =
    Derived := derived_class{}
    using{Derived}

    Base := base_class{}
    using{Base}  # ERROR - base_class is supertype of derived_class
```

Prevents ambiguity when a subclass is already in the `using` scope.

**Error 3588 - Data member name conflict:**

```verse
# ERROR 3588
class_a := class:
    Value:int = 1
class_b := class:
    Value:int = 2

Process():void =
    ObjA := class_a{}
    ObjB := class_b{}
    using{ObjA}
    using{ObjB}
    # Value  # ERROR - ambiguous, must qualify
    ObjA.Value  # OK
```

**Error 3518 - Method name conflict:**

```verse
# ERROR 3518
class_a := class:
    Method():void = {}
class_b := class:
    Method():void = {}

Process():void =
    ObjA := class_a{}
    ObjB := class_b{}
    using{ObjA}
    using{ObjB}
    # Method()  # ERROR - ambiguous, must qualify
    ObjA.Method()  # OK
```

**Error 3669 - Cannot use module paths:**

Local scope `using` only accepts identifiers, not module paths:

```verse
# ERROR 3669
ProcessData():void =
    using{/Verse.org/Simulation}  # ERROR - module paths not allowed
    using{MyModule}                # ERROR - module identifiers not allowed
```

Module imports must be at file/module level using full paths.

**Error 3669 - Cannot use member paths:**

Currently, you cannot use member access expressions in `using`:

```verse
# ERROR 3669
outer := class:
    Inner:inner_class = inner_class{}

inner_class := class:
    Value:int = 0

Process():void =
    Outer := outer{}
    using{Outer.Inner}  # ERROR - no member paths allowed
```

This may be supported in a future version (SOL-4877).

**Error 3669 - Cannot use inferred members:**

```verse
# ERROR 3669
Process():void =
    Outer := outer{}
    using{Outer}
    using{Inner}  # ERROR - even though Inner could be inferred from Outer
```

You cannot chain `using` with inferred members. This may be supported in future (SOL-4877).

### Comparison: Module Using vs Local Using

| Aspect | Module Using | Local Using |
|--------|--------------|-------------|
| **Syntax** | `using { /Module/Path }` | `using{Variable}` |
| **Location** | File/module level only | Function body/blocks |
| **Target** | Module paths | Local variables/parameters |
| **Scope** | Entire file/module | Current block and nested blocks |
| **Purpose** | Import module members | Infer member access from object |
| **Qualification** | Module name prefix | Variable member access |
| **Order** | Must be before definitions | Must be before inferred usage |
| **Conflicts** | Resolved by qualification | Require explicit qualification |

### Best Practices

**Use local `using` when:**
- You have many member accesses on the same object
- The member names are unambiguous in context
- The code becomes more readable without repeated qualifications

**Avoid local `using` when:**
- Only a few member accesses occur
- Multiple objects with similar members create conflicts
- Explicit qualification aids understanding

**Example - Good use case:**

```verse
# Many member accesses - local using improves readability
ProcessPlayer(P:player):void =
    using{P}
    UpdateHealth(Damage)
    IncrementScore(Points)
    CheckAchievements(Level)
    UpdateInventory(Item)
    SaveProgress(Checkpoint)
    NotifyFriends(Status)
```

**Example - Poor use case:**

```verse
# Few accesses - using adds complexity without benefit
ProcessPlayer(P:player):void =
    using{P}
    UpdateHealth(Damage)  # Just use P.UpdateHealth(P.Damage) directly
```

### Future Enhancements

The following features are planned for local scope `using` (tracked in Jira SOL-4877):

- **Member paths**: `using{Outer.Inner}` to use nested object members
- **Chained inference**: `using{Outer}` followed by `using{Inner}` where `Inner` is inferred
- **Module identifiers**: Potentially allowing module identifiers in local scope

These enhancements would make local `using` more flexible while maintaining type safety.

## Troubleshooting

When working with modules, you may encounter various issues. Understanding these common problems and their solutions will help you debug module-related errors more efficiently.

### Module Not Found Errors

**Problem**: The compiler reports that a module cannot be found when you try to import it.

**Common Causes and Solutions**:

1. **Incorrect path**: Double-check the module path in your `using` statement. Remember that paths are case-sensitive.

<!--NoCompile-->
```verse
   # Wrong: different case
   using { /verse.org/random }  # Error: module not found

   # Correct: proper case
   using { /Verse.org/Random }  # Works
```

2. **Missing parent module import**: When importing nested modules, ensure the parent is imported first.

<!--NoCompile-->
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

<!--NoCompile-->
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

<!--NoCompile-->
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

## Qualified Identifier Error Codes

Understanding error codes for qualified identifiers helps diagnose naming and scoping issues:

### Qualifier Errors

**Error 3506:** Undefined identifier in qualifier
```verse
# Error: identifier 'Unknown' not found
(Unknown:)Value:int = 5
```

**Error 3612:** Qualifier not allowed in this context
```verse
# Error: qualifiers not allowed here
module_x := module:
    (module_x:)Field:int = 5  # Invalid: use at module level
```

**Error 3514:** Reserved keyword cannot be used as identifier
```verse
# Error: 'local' is reserved
module_x := module:
    local<public>:int = 42  # Invalid even without qualifier
```

**Error 3525:** Multiple qualifiers not supported
```verse
# Error: only one qualifier allowed
(ModuleA, ModuleB:)Value:int = 5
```

**Error 3587:** Invalid path literal
```verse
# Error: path does not exist
C := class((/Invalid/Path:)BaseClass){}
```

### Shadowing Errors

**Error 3588/3532:** Identifier shadows another definition
```verse
# Without (local:) qualifier
ExternalModule<public> := module:
    Value<public>:int = 10

MyModule := module:
    using{ExternalModule}
    Process():int =
        Value:int = 5  # Error 3588/3532: shadows ExternalModule.Value
        Value

# Fix with (local:)
MyModule := module:
    using{ExternalModule}
    Process():int =
        (local:)Value:int = 5  # OK: explicitly local
        (local:)Value
```

### Unsupported Features

**Error 3552:** Unsupported qualified identifier form
```verse
# Some qualified forms not yet fully supported
C := class:
    m:int
    f():void = C{(C:)m := 1}  # Error 3552: not yet supported
```

**Path literals with using/import:**

Path literals cannot currently be used to refer to classes in `using` or `import` statements (error 3587):
```verse
# Error 3587: classes not supported in using
A<public> := module:
    C<public> := class{}

using {/Verse.org/VerseTests/A/C}  # Error: can only use paths to modules

# Error 3587: classes not supported in import
Test := import(/Verse.org/VerseTests/A/C)  # Error
```

This limitation may be lifted in future versions.

## Detailed Example

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
