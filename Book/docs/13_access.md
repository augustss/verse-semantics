# Access Specifiers

Access specifiers control visibility and accessibility of code elements. They provide a nuanced spectrum of access levels that reflect the complex reality of modern software development, particularly in the context of a persistent, global metaverse where code from many authors must coexist safely.

Five primary visibility levels are defined that form a carefully designed hierarchy, each serving specific architectural needs. Understanding when and why to use each level is crucial for creating well-structured, maintainable code.

| Specifier | Visibility | Usage |
|-----------|------------|-------|
| `<public>` | Universally accessible | Members intended for external use |
| `<internal>` | Only within the module (default) | Module-private implementation |
| `<private>` | Only in immediate enclosing scope | Local to class/struct |
| `<protected>` | Current class and subtypes | Inheritance hierarchies |
| `<scoped>` | Current scope and enclosing scopes | Special use cases |

## Public

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

## Protected

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

## Private

The `<private>` specifier provides the strictest access control, limiting visibility to the immediately enclosing scope. Private members are truly internal implementation details that can be changed freely without affecting any external code:

<!--verse
item:=struct{Weight:float=0.0}
-->
```verse
inventory := class:
    var Items<private>:[]item = array{}
    var Capacity<private>:int = 20
    var CurrentWeight<private>:float = 0.0
    MaxWeight:float=20.0

    AddItem<public>(NewItem:item)<transacts><decides>:void =
        ValidateCapacity[NewItem]
        set Items = Items + [NewItem]
        set CurrentWeight = CurrentWeight + NewItem.Weight

    ValidateCapacity<private>(NewItem:item)<reads><decides>:void =
        Items.Length < Capacity
        CurrentWeight + NewItem.Weight <= MaxWeight
```

Private members are the building blocks of encapsulation. They allow you to maintain invariants, hide complexity, and create clean abstractions. Changes to private members never break external code, giving you the freedom to refactor and optimize implementation details as needed.

## Internal

The `<internal>` specifier, which is the default access level when no specifier is provided, makes members accessible within the defining module but not outside it. This creates a natural boundary for collaborative code that needs to share implementation details without exposing them publicly:

```verse
physics := module:
    # Internal types and constants
    gravity_constant:float = 9.81

    collision_detector := class:
        DetectCollision<internal>(A:game_entity, B:game_entity):?collision_info =
            # Implementation details

    physics_world := class:
        var Entities<internal>:[]game_entity = array{}

        SimulateStep<internal>(DeltaTime:float):void =
            for (Entity : Entities):
                ApplyGravity(Entity, DeltaTime)
                CheckCollisions(Entity)
```

Internal access is ideal for module-wide utilities, shared implementation details, and helper functions that multiple classes within a module need but shouldn't be exposed to external code. It provides a clean separation between the module's public interface and its implementation machinery.

## Scoped

**TODO**

## Separating Read and Write Access

An innovative features is the ability to apply different access specifiers to reading and writing operations on the same variable. This fine-grained control allows you to create variables that are widely readable but narrowly writable, implementing common patterns like read-only properties elegantly:

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

## Best Practices

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

## Evolution

Access specifiers play a crucial role in code evolution. Changing access levels after publication can break compatibility:

- Narrowing access (public to private) breaks external code that depends on the member
- Widening access (private to public) is generally safe but creates new commitments
- Changing protected members affects the inheritance contract

The `<castable>` specifier on classes has special compatibility requirements—once published, it cannot be added or removed, as this would affect the safety of dynamic casts throughout the codebase.

When designing for long-term evolution, consider using internal access for members that might eventually become public. This allows you to test and refine APIs within your module before committing to public exposure.
