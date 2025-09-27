# Access Specifiers in Verse: A Comprehensive Guide

## Introduction: The Philosophy of Controlled Access

Access specifiers in Verse represent a sophisticated system for controlling visibility and accessibility of code elements. Unlike many programming languages that treat access control as a binary public/private distinction or a simple three-tier hierarchy, Verse provides a nuanced spectrum of access levels that reflect the complex reality of modern software development, particularly in the context of a persistent, global metaverse where code from many authors must coexist safely.

The design of Verse's access specifier system embodies several key principles. First, it recognizes that different contexts require different levels of access control—from the intimacy of private implementation details to the broad accessibility of public APIs. Second, it acknowledges that access control isn't just about reading values but also about modification rights, leading to Verse's unique dual-specifier system for variables. Finally, it integrates seamlessly with Verse's effect system and other language features, creating a cohesive approach to program safety and modularity.

## The Spectrum of Visibility

Verse defines five primary visibility levels that form a carefully designed hierarchy, each serving specific architectural needs. Understanding when and why to use each level is crucial for creating well-structured, maintainable code.

### Public: Universal Accessibility

The `<public>` specifier represents the broadest level of access, making an identifier universally accessible from any code that can reference the containing module or type. When you mark something as public, you're making a strong commitment about its availability and stability:

```verse
player_manager<public> := module:
    MaxPlayers<public>:int = 100

    GetPlayerCount<public>():int =
        CurrentPlayerCount

    player<public> := class:
        Name<public>:string
        Level<public>:int = 1
```

Public members form the contract between your code and the outside world. In the metaverse context, public declarations are particularly significant because they represent guarantees that extend potentially forever—once published, removing or incompatibly changing a public member breaks the promise you've made to other developers who depend on your code.

The public specifier can be applied to modules, classes, interfaces, structs, enums, methods, and data members. When applied to a type definition itself, it makes the type available for use outside its defining module. When applied to members within a type, it makes those members accessible to any code that has access to an instance of that type.

### Protected: Inheritance-Based Access

The `<protected>` specifier creates a middle ground between public and private, allowing access within the defining class and any classes that inherit from it. This level exists specifically to support inheritance hierarchies while maintaining encapsulation:

```verse
game_entity := class:
    var Position<protected>:vector3 = vector3{x:=0.0, y:=0.0, z:=0.0}
    var Health<protected>:int = 100

    UpdatePosition<protected>(NewPos:vector3):void =
        set Position = NewPos
        OnPositionChanged()

    OnPositionChanged<protected>():void = {}  # Overridable by subclasses

player := class(game_entity):
    MoveToSpawn():void =
        UpdatePosition(GetSpawnLocation())  # Can access protected member
        set Health = MaxHealth              # Can modify protected variable
```

Protected access enables the template method pattern and other inheritance-based designs while preventing external code from accessing implementation details that should remain within the class hierarchy. This is particularly valuable for game entities and other hierarchical structures where parent classes need to share behavior with children without exposing that behavior to the world.

### Private: Encapsulated Implementation

The `<private>` specifier provides the strictest access control, limiting visibility to the immediately enclosing scope. Private members are truly internal implementation details that can be changed freely without affecting any external code:

```verse
inventory := class:
    var Items<private>:[]item = []
    var Capacity<private>:int = 20
    var CurrentWeight<private>:float = 0.0

    AddItem<public>(NewItem:item)<transacts><decides>:void =
        ValidateCapacity(NewItem)?
        set Items = Items + [NewItem]
        set CurrentWeight = CurrentWeight + NewItem.Weight

    ValidateCapacity<private>(NewItem:item)<decides>:void =
        Items.Length < Capacity
        CurrentWeight + NewItem.Weight <= MaxWeight
```

Private members are the building blocks of encapsulation. They allow you to maintain invariants, hide complexity, and create clean abstractions. Changes to private members never break external code, giving you the freedom to refactor and optimize implementation details as needed.

### Internal: Module-Level Sharing

The `<internal>` specifier, which is the default access level when no specifier is provided, makes members accessible within the defining module but not outside it. This creates a natural boundary for collaborative code that needs to share implementation details without exposing them publicly:

```verse
physics := module:
    # Internal types and constants
    gravity_constant:float = 9.81

    collision_detector := class:
        DetectCollision<internal>(A:game_entity, B:game_entity):?collision_info =
            # Implementation details

    physics_world := class:
        var Entities<internal>:[]game_entity = []

        SimulateStep<internal>(DeltaTime:float):void =
            for (Entity : Entities):
                ApplyGravity(Entity, DeltaTime)
                CheckCollisions(Entity)
```

Internal access is ideal for module-wide utilities, shared implementation details, and helper functions that multiple classes within a module need but shouldn't be exposed to external code. It provides a clean separation between the module's public interface and its implementation machinery.

### Scoped: Hierarchical Visibility

The `<scoped>` specifier provides access within the current scope and any enclosing scopes. This unique access level is particularly important for assets exposed to Verse, which automatically receive the scoped specifier:

```verse
ui_system := module:
    dialog<scoped> := class:
        var Content<scoped>:string = ""

        button<scoped> := class:
            Label<scoped>:string
            OnClick<scoped>:type{():void}

            Press():void =
                OnClick()
```

Scoped access creates a hierarchical visibility model where inner scopes can access outer scope members, facilitating nested class designs and complex module structures while maintaining clear boundaries.

## Dual Specifiers: Separating Read and Write Access

One of Verse's most innovative features is the ability to apply different access specifiers to reading and writing operations on the same variable. This fine-grained control allows you to create variables that are widely readable but narrowly writable, implementing common patterns like read-only properties elegantly:

```verse
game_state := class:
    # Public read, protected write
    var<protected> Score<public>:int = 0

    # Public read, private write
    var<private> PlayerCount<public>:int = 0

    # Internal read, private write
    var<private> SessionID<internal>:string = GenerateID()
```

This dual-specifier system solves a common problem in object-oriented programming where you want to expose state for reading without allowing external modification. Rather than requiring getter methods or property syntax, Verse makes this pattern a first-class language feature.

The syntax places the write-access specifier on the `var` keyword and the read-access specifier on the identifier itself. This visual separation makes the access levels immediately clear when reading code. The write specifier must be at least as restrictive as the read specifier—you cannot have a variable that's privately readable but publicly writable, as this would violate basic encapsulation principles.

## Specifiers and the Type System

Access specifiers interact with Verse's type system in sophisticated ways. When you define a class with members of varying access levels, you're actually defining different views of that type depending on the access context:

```verse
secure_container := class:
    PublicData<public>:string = "visible"
    var ProtectedData<protected>:int = 42
    PrivateData<private>:float = 3.14

    # From outside the class, only PublicData is visible
    # From subclasses, PublicData and ProtectedData are visible
    # From within the class, all three are visible
```

This creates a form of structural subtyping where the same type presents different interfaces in different contexts. External code sees a narrower interface than internal code, automatically enforcing encapsulation at the type level.

## Beyond Visibility: Behavioral Specifiers

While visibility specifiers control access, Verse also provides behavioral specifiers that control how code executes. These specifiers work alongside visibility specifiers to create a complete picture of a member's characteristics:

```verse
validated_operation := class:
    # Combines visibility with behavioral specifiers
    Process<public>(Input:data)<decides><transacts>:result =
        Validate(Input)?
        Transform(Input)

    Validate<private>(Input:data)<decides>:void =
        Input.IsValid

    Transform<protected>(Input:data)<transacts><converges>:result =
        # Transformation logic
```

The behavioral specifiers form several categories:

**Effect Specifiers** control what effects a function can have:
- `<computes>`: Pure computation with no side effects
- `<reads>`: Can read mutable state
- `<writes>`: Can modify mutable state
- `<transacts>`: Full transactional effects
- `<allocates>`: Can allocate memory
- `<converges>`: Guaranteed to complete

**Control Flow Specifiers** affect how functions interact with Verse's control flow:
- `<decides>`: Can fail (return early through failure)
- `<suspends>`: Can suspend execution (for async operations)

**Implementation Specifiers** describe how functions are implemented:
- `<native>`: Implemented in native code
- `<inline>`: Should be inlined by the compiler
- `<override>`: Overrides a parent class method
- `<abstract>`: Must be implemented by subclasses
- `<final>`: Cannot be overridden by subclasses

## Structural Specifiers for Types

Classes and other type definitions can carry structural specifiers that fundamentally affect their behavior:

```verse
# Unique instances with reference semantics
unique_entity<unique> := class<allocates>:
    ID:string = GenerateUniqueID()

# Concrete class that can be instantiated
game_item<concrete> := class:
    Name:string
    Value:int

# Abstract base class
vehicle<abstract> := class:
    Speed<abstract>():float

# Final inheritance point
player<final_super> := class(game_entity):
    # This class will always directly inherit from game_entity
```

The `<unique>` specifier creates classes with reference semantics where each instance is unique. The `<concrete>` and `<abstract>` specifiers control instantiation. The `<final_super>` specifier locks inheritance relationships, providing strong guarantees about class hierarchies that tools and compilers can rely upon.

## Enums: Open vs Closed

Enumerations in Verse can be marked as either `<open>` or `<closed>`, affecting their evolution and usage:

```verse
# Closed enum - exhaustive pattern matching possible
game_state<closed> := enum:
    Menu
    Playing
    Paused
    GameOver

# Open enum - new values can be added after publication
player_action<open> := enum:
    Move
    Jump
    Attack
    # More actions can be added later
```

Closed enums enable exhaustive pattern matching since the compiler knows all possible values. Open enums provide extensibility at the cost of exhaustiveness. Once published, a closed enum cannot be opened, as this would break code that relies on exhaustive matching.

## Access Patterns and Best Practices

Understanding when to use each access level requires thinking about your code's architecture and evolution. The principle of least privilege suggests starting with the most restrictive access that works and only broadening it when necessary.

For public APIs, every public member is a commitment. Before making something public, consider whether it truly needs to be part of your module's contract or if it's an implementation detail that happens to be needed elsewhere temporarily. Public members should be stable, well-documented, and designed for longevity.

Protected access should be used thoughtfully in inheritance hierarchies. Not everything in a base class needs to be protected—only those members that form the inheritance contract between parent and child classes. Overuse of protected access can create tight coupling between classes in a hierarchy.

Private access is your default for implementation details. Most helper functions, intermediate calculations, and state management should be private. This gives you maximum flexibility to refactor and optimize without breaking dependent code.

The dual-specifier pattern for variables is particularly powerful for maintaining invariants. By making variables publicly readable but privately or protectively writable, you can expose state for observation while maintaining complete control over modifications:

```verse
resource_manager := class:
    var<private> TotalResources<public>:int = 1000
    var<private> AllocatedResources<public>:int = 0
    var<private> AvailableResources<public>:int = 1000

    AllocateResources<public>(Amount:int)<decides><transacts>:void =
        Amount <= AvailableResources
        set AllocatedResources = AllocatedResources + Amount
        set AvailableResources = AvailableResources - Amount
```

## Evolution and Compatibility

Access specifiers play a crucial role in code evolution. Changing access levels after publication can break compatibility:

- Narrowing access (public to private) breaks external code that depends on the member
- Widening access (private to public) is generally safe but creates new commitments
- Changing protected members affects the inheritance contract

The `<castable>` specifier on classes has special compatibility requirements—once published, it cannot be added or removed, as this would affect the safety of dynamic casts throughout the codebase.

When designing for long-term evolution, consider using internal access for members that might eventually become public. This allows you to test and refine APIs within your module before committing to public exposure.

## Integration with the Module System

Access specifiers work hand-in-hand with Verse's module system to create clear boundaries and dependencies. Modules naturally group related functionality, and access specifiers control what crosses module boundaries:

```verse
network_system<public> := module:
    # Public API
    Connection<public> := class:
        Connect<public>(Address:string)<suspends><decides>:void
        Send<public>(Data:message)<suspends><transacts>:void

    # Internal implementation
    protocol_handler<internal> := class:
        ProcessPacket<internal>(Packet:raw_data):void

    # Private utilities
    ChecksumValidator<private>():validator =
        # Implementation
```

This layered approach creates clean, maintainable architectures where each module exposes a carefully designed public interface while keeping implementation details hidden.

## Conclusion: Access Control as Design Language

Access specifiers in Verse are more than just visibility markers—they're a design language for expressing architectural intent. Through the careful application of public, protected, private, internal, and scoped access, combined with behavioral and structural specifiers, you communicate not just what code can access what, but why those boundaries exist and how they should be respected.

The dual-specifier system for variables, the integration with the effect system, and the careful consideration of evolution and compatibility make Verse's access control system one of the most sophisticated in modern programming languages. It provides the tools needed to build robust, maintainable software for the unique challenges of a persistent, global metaverse where code from many authors must safely coexist and evolve over extended timescales.

As you design your Verse programs, think of access specifiers as promises—promises about what will remain stable, what might change, and what boundaries must be respected. These promises, encoded in the language itself, create the foundation for building software that can evolve gracefully while maintaining the trust and compatibility that the metaverse demands.