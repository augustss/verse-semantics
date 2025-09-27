# Composite Types

Composite types allow you to create custom data structures that model the entities and concepts in your game world. Rather than working solely with primitive types like integers and strings, you can define rich, structured types that represent players, weapons, game states, and any other domain-specific concepts your game requires.

Verse provides four fundamental composite type constructors, each serving a distinct purpose in your type architecture. Classes provide object-oriented programming with inheritance and polymorphism, enabling you to model complex hierarchies of game entities. Interfaces define contracts that classes must fulfill, promoting loose coupling and enabling multiple inheritance of behavior specifications. Structs offer lightweight, value-oriented data containers perfect for simple data aggregation without the overhead of object-oriented features. Enums represent fixed sets of named values, ideal for modeling game states, item types, or any domain with a known set of alternatives.

## Classes

Classes form the backbone of object-oriented programming in Verse. A class serves as a blueprint for creating objects that share common properties and behaviors. When you define a class, you're creating a new type that bundles data (fields) with operations on that data (methods), encapsulating related functionality into a cohesive unit.

The simplest form of a class groups related data together. Consider modeling a character in your game:

```verse
character := class:
    Name : string
    var Health : int = 100
    var Level : int = 1
    MaxHealth : int = 100
```

This class definition establishes several important concepts. Fields without the `var` modifier are immutable after construction—once you create a character with a specific name, that name cannot change. Fields marked with `var` are mutable and can be modified after the object is created. Default values provide sensible starting points, making object construction more convenient while ensuring objects start in valid states.

### Object Construction

Creating instances of a class involves specifying values for its fields through an archetype expression:

```verse
Hero := character{Name := "Aldric", Health := 100, Level := 5}
Villager := character{Name := "Martha"}  # Uses default values for unspecified fields
```

The archetype syntax uses named parameters, making the construction explicit and self-documenting. Any field with a default value can be omitted from the archetype, and the default will be used. Fields without defaults must be specified, ensuring objects are always fully initialized. Fields can be passed to an archetype in any order.

### Methods and Behavior

Classes become truly powerful when you add methods that operate on the class's data:

```verse
character := class:
    Name : string
    var Health : int = 100
    var Level : int = 1
    MaxHealth : int = 100

    TakeDamage(Amount : int) : void =
        set Health = Max(0, Health - Amount)

    Heal(Amount : int) : void =
        set Health = Min(MaxHealth, Health + Amount)

    IsAlive() : logic = Health > 0

    LevelUp() : void =
        set Level += 1
        set MaxHealth = 100 + (Level * 10)
        set Health = MaxHealth  # Full heal on level up
```

Methods have access to all fields of the class and can modify mutable fields. They encapsulate the logic for how objects of the class should behave, ensuring that state changes happen in controlled, predictable ways.

### The Self Identifier

Within class methods, `Self` refers to the specific instance the method was called on. While you can access fields directly by name, `Self` becomes necessary when you need to pass the entire object to another function:

```verse
character := class:
    Name : string

    Announce() : void =
        # Direct field access
        Print("Character name: {Name}")

        # Using Self to pass the whole object
        LogCharacterAction(Self, "announced")

LogCharacterAction(Character : character, Action : string) : void =
    Print("{Character.Name} {Action}")
```

### Inheritance and Subclassing

Classes support single inheritance, allowing you to create specialized versions of existing classes. This creates an "is-a" relationship where the subclass is a more specific type of the superclass:

```verse
entity := class:
    var Position : vector3 = vector3{}
    var IsActive : logic = true

    Activate() : void = set IsActive = true
    Deactivate() : void = set IsActive = false

character := class(entity):  # character inherits from entity
    Name : string
    var Health : int = 100

    TakeDamage(Amount : int) : void =
        set Health = Max(0, Health - Amount)
        if (Health = 0):
            Deactivate()  # Can call inherited methods

player := class(character):  # player inherits from character
    var Score : int = 0
    var Lives : int = 3

    AddScore(Points : int) : void =
        set Score += Points
```

Inheritance creates a type hierarchy where a `player` is also a `character`, and a `character` is also an `entity`. This means you can use a `player` object anywhere a `character` or `entity` is expected, enabling polymorphic behavior.

### Method Overriding

Subclasses can override methods defined in their superclasses to provide specialized behavior:

```verse
entity := class:
    OnUpdate<public>() : void = {}  # Default no-op implementation

enemy := class(entity):
    var Target : ?character = false

    OnUpdate<public><override>() : void =
        if (Target?.IsAlive[]):
            MoveToward(Target)
        else:
            Patrol()

turret := class(entity):
    var Rotation : float = 0.0

    OnUpdate<public><override>() : void =
        set Rotation = Mod(Rotation + 1.0, 360.0)
        ScanForTargets()
```

The override mechanism ensures that the correct method implementation is called based on the actual type of the object, not the type of the variable holding it. This is the foundation of polymorphic behavior in object-oriented programming.

### Constructor Functions

Classes don't have traditional constructor methods like you might find in other object-oriented languages. Instead, Verse uses a more functional approach to object construction through direct field  initialization and the Make pattern for complex initialization logic.

For classes requiring validation or complex initialization, Verse uses factory functions rather than constructor methods. These are typically named `Make`, are annotated `<constructor>` and return an instance of the class. The factory function can perform  validation, compute derived values, or fail if requirements aren't met:

```verse
  MakePlayer<constructor>(Name:string, Level:int)<decides>:player =
      Level > 0
      Level <= MaxLevel
      player{Name := Name, Health := Level *100, Mana := Level* 50}
```

 For classes with mutable fields (marked with `var`), initialization sets the starting values that can change during   the object's lifetime. Immutable fields must be initialized during construction and cannot be modified afterward.  This distinction makes the construction phase critical for  establishing invariants that will hold throughout the object's existence.

### Access Specifiers

Classes support fine-grained control over member visibility through access specifiers:

```verse
game_state := class:
    Score<public> : int = 0                    # Anyone can read
    var Lives<private> : int = 3               # Only this class can access
    var Shield<protected> : float = 100.0      # This class and subclasses
    DebugInfo<internal> : string = ""          # Same module only

    # Public method - anyone can call
    GetLives<public>() : int = Lives

    # Protected method - subclasses can override
    OnLifeLost<protected>() : void = {}

    # Private helper - only this class
    ValidateState<private>() : void = {}
```

Access specifiers apply to both fields and methods, controlling who can read fields and call methods. The default visibility is `internal`, restricting access to the same module. This encapsulation is crucial for maintaining class invariants and hiding implementation details.

### The Concrete Specifier

The `<concrete>` specifier enforces that all fields have default values, allowing construction with an empty archetype:

```verse
config := class<concrete>:
    MaxPlayers : int = 8
    TimeLimit : float = 300.0
    FriendlyFire : logic = false

# Can construct with empty archetype
DefaultConfig := config{}
```

This is particularly useful for configuration classes where reasonable defaults exist for all values.

A concrete class `C` can be constructed by writing `C{}`, that is to say with the empty archetype.

A concrete class may have non-concrete subclasses.

### The Unique Specifier

The `<unique>` specifier creates classes with reference semantics where each instance has a distinct identity. When a class is marked as `<unique>`, instances become comparable using the equality operators (= and <>), with equality based on object identity rather than field values.

**Identity-Based Equality**

Classes marked with `<unique>` compare by identity, not by value:

```verse
entity := class<unique>:
   Name : string
   Position : vector3

E1 := entity{Name := "Guard", Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}}
E2 := entity{Name := "Guard", Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}}
E3 := E1

E1 = E2  # Fails - different instances despite identical field values
E1 = E3  # Succeeds - same instance
```

This specifier is ideal for:

- Game Entities: Where each entity in the world must be distinguishable regardless of current state
- Session Objects: Where identity matters more than current property values
- Resource Handles: Where you need to track specific instances rather than equivalent values

Without `<unique>`, class instances cannot be compared for equality at all—the language prevents meaningless
comparisons. With `<unique>`, you gain the ability to use instances as map keys, store them in sets, and perform
identity checks, essential for tracking specific objects throughout their lifetime.

### The Abstract Specifier

The `<abstract>` specifier marks classes that cannot be instantiated directly — they exist solely as base classes  for inheritance. When you declare a class with `<abstract>`, you're creating a template that defines structure and behavior for subclasses to inherit and implement.

Abstract classes serve as architectural foundations in a type hierarchy. They define contracts through abstract methods that subclasses must implement, while potentially providing concrete methods and fields that subclasses inherit. This creates a powerful pattern for code reuse and polymorphic behavior.

```verse
  vehicle<abstract> := class:
      Speed():float             # Abstract method
      MaxPassengers:int = 1

      # Concrete method all vehicles share
      CanTransport(Count:int)<decides>:void =
          Count <= MaxPassengers

  car := class(vehicle):
      Speed<override>():float = 60.0
      MaxPassengers<override>:int = 4

  bicycle := class(vehicle):
      Speed<override>():float = 15.0
```

Abstract methods within abstract classes have no implementation — they're pure declarations that establish what subclasses must provide. An abstract method creates a contract: any non-abstract subclass must override all abstract methods or the code won't compile.

### The Castable Specifier

The `<castable>` specifier enables runtime type checking and safe downcasting for classes. When a class is marked with `<castable>`, you can use dynamic type tests and casts to determine if an object is an instance of that class or its subclasses at runtime.

Without `<castable>`, Verse's type system operates purely at compile time. The `<castable>` specifier adds runtime type information, allowing code to inspect and react to actual object types during execution. This bridges the gap between static type safety and dynamic polymorphism.

```verse
  component<public> := class<abstract><unique><castable>:
      Parent:entity

  entity<public> := class<concrete><unique><transacts><castable>:
      FindDescendantEntities(entity_type:castable_subtype(entity)):generator(entity_type)
```

The `castable_subtype` type constructor works with `<castable>` classes to enable type-safe filtered queries. When you  call `FindDescendantEntities(player)`, the function returns only entities that are actually player instances or  subclasses thereof, verified at runtime through the castable mechanism.

Once a class is published with `<castable>`, this decision becomes permanent. You cannot add or remove the `<castable>` specifier after publication because doing so would break existing code that relies on runtime type checking. Code that performs casts would suddenly fail or behave incorrectly if the castable property changed.

### The Final Specifier

The `<final>` specifier prevents inheritance, creating a terminal point in a class hierarchy. When you mark a class  with `<final>`, no other class can inherit from it. For methods, `<final>` prevents overriding in subclasses, locking  the implementation at that level of the hierarchy.

Classes marked with `<final>` serve as concrete implementations that cannot be extended. This is particularly important for persistable classes, which require `<final>` to ensure their structure remains stable for serialization:

```verse
  player_profile := class<final><persistable>:
      Username:string = "Player"
      Level:int = 1
      Gold:int = 0

  player_data := class<final><persistable>:
      Version:int = 1
      LastLogin:string = ""
      Statistics:player_stats = player_stats{}
```

The `<final>` requirement for persistable classes prevents schema evolution problems. If subclasses could extend persistable classes, the serialization system would face ambiguity about which fields to persist and how to handle  polymorphic deserialization.

For methods, `<final>` locks behavior at a specific point in the inheritance chain:

```verse
  base_entity := class:
      GetName<virtual>():string = "Entity"

  game_object := class(base_entity):
      GetName<override><final>():string = "GameObject"
      # Any subclass of game_object cannot override GetName
```

The related `<final_super>` specifier marks classes as terminal base classes — they can be inherited from but their subclasses cannot be further extended.  `<final_super_base>` marks a class as the ultimate root of a restricted inheritance tree. Classes with this   specifier can be inherited from, but their subclasses automatically become final — they cannot be further  extended. This creates a two-level inheritance limit starting from the base:

```verse
  component<native><public> := class<abstract><unique><castable><final_super_base>:
      Parent:entity

  # Can inherit from component (first level)

  physics_component := class(component):  # implicitly final_super
      Mass:float = 1.0

 # Cannot inherit from physics_component - it's implicitly final

# gravity_component := class(physics_component): # COMPILE ERROR
```

So, `<final_super>` marks a class that inherits from a `<final_super_base>` class, explicitly declaring it as the final inheritance point. While classes inheriting from `<final_super_base>` are implicitly final, using `<final_super>`  makes this finality explicit and self-documenting:

```verse
  # Explicitly marking as final_super (though implicitly final anyway)
  name_component := class<final_super>(component):
      Name:string = ""

  copter_camera_component := class<final_super>(copter_camera_component_director_version):
      # Terminal implementation
```

This pattern is particularly valuable in component architectures where you want a base component interface that  various concrete components implement, but don't want those implementations to spawn their own inheritance  subtrees. The base class defines the contract, immediate subclasses provide  implementations, and inheritance stops  there — clean, controlled, and predictable.

This design enforces architectural discipline, preventing the "inheritance explosion" that can occur when every class becomes a potential base for further specialization. By limiting inheritance depth, these specifiers promote composition over deep inheritance, leading to more maintainable and understandable code structures.

### The Persistable Specifier

The `<persistable>` specifier marks types that can be saved and restored across game sessions, enabling permanent storage of player progress, achievements, and game state. This specifier transforms ephemeral gameplay into  lasting progression, creating the foundation for meaningful player investment.

Persistence  works through module-scoped `weak_map(player, t)` variables, where `t` is any persistable type.  These special maps automatically synchronize with backend storage — when players join, their data loads; when they leave or data changes, it saves. The system handles all serialization, network transfer, and storage management transparently.

```verse
  player_inventory := class<final><persistable>:
      Gold:int = 0
      Items:[]string = array{}
      UnlockedAreas:[]string = array{}

  # This variable automatically persists across sessions

  SavedInventories : weak_map(player, player_inventory) = map{}
```

The `<persistable>` specifier enforces strict structural requirements to guarantee data integrity across versions. Classes must be `<final>` because inheritance would complicate serialization schemas. They cannot contain `var`  fields, preserving immutability guarantees even in persistent storage. They cannot be `<unique>` since identity-based equality doesn't survive serialization. These constraints ensure that what you save today can be   reliably loaded tomorrow, next month, or next year.

## Interfaces

Interfaces define contracts that classes can implement, specifying what methods a class must provide without dictating how those methods work. Unlike classes, interfaces contain no data and no implementation—they purely describe behavior that implementing classes must provide.

An interface declares method signatures that implementing classes must fulfill:

```verse
damageable := interface:
    TakeDamage(Amount:int):void
    GetHealth():int
    IsAlive():logic

healable := interface:
    Heal(Amount:int):void
    GetMaxHealth():int
```

These interfaces establish contracts without any implementation details. Any class that implements `damageable` must provide all three methods with matching signatures.

### Implementing Interfaces

Classes implement interfaces by inheriting from them and providing concrete implementations of all required methods:

```verse
character := class(damageable, healable):
    var Health : int = 100
    MaxHealth : int = 100

    TakeDamage(Amount:int):void =
        set Health = Max(0, Health - Amount)

    GetHealth():int = Health

    IsAlive():logic = Health > 0

    Heal(Amount:int):void =
        set Health = Min(MaxHealth, Health + Amount)

    GetMaxHealth():int = MaxHealth
```

A class can implement multiple interfaces, effectively achieving multiple inheritance of behavior contracts. This provides more flexibility than single class inheritance while maintaining type safety.

### Interface-Based Programming

Interfaces enable programming to abstractions rather than concrete implementations:

```verse
# Function works with any damageable object
ApplyDamageOverTime(Target:damageable, DamagePerSecond:int, Duration:float)<suspends>:void =
    for (I := 0..Floor(Duration)):
        if (Target.IsAlive()):
            Target.TakeDamage(DamagePerSecond)
            Sleep(1.0)

# Function works with any healable object
HealToFull(Target:healable):void =
    MaxHealth := Target.GetMaxHealth()
    CurrentHealth := Target.GetHealth()  # Assumes healable extends damageable
    Target.Heal(MaxHealth - CurrentHealth)
```

This approach creates loose coupling between different parts of your code. The `ApplyDamageOverTime` function doesn't need to know about specific character classes—it works with anything that implements the `damageable` interface.

### Interface Hierarchies

Interfaces can extend other interfaces, creating hierarchies of contracts:

```verse
combatant := interface(damageable, healable):
    Attack(Target:damageable):void
    GetAttackPower():int

boss := interface(combatant):
    UseSpecialAbility():void
    GetPhase():int
```

A class implementing `boss` must provide methods from `boss`, `combatant`, `damageable`, and `healable`—the entire interface hierarchy.

## Structs

Structs provide lightweight data containers without the object-oriented features of classes. They're value types optimized for simple data aggregation, making them perfect for mathematical types, data transfer objects, and any scenario where you need a simple bundle of related values without behavior.

Structs group related data with minimal overhead:

```verse
vector2 := struct:
    X : float = 0.0
    Y : float = 0.0

color := struct:
    R : int = 0
    G : int = 0
    B : int = 0
    A : int = 255  # Alpha channel

damage_info := struct:
    Amount : int = 0
    Type : damage_type = damage_type.Physical
    Source : ?character = false
    IsCritical : logic = false
```

All struct fields are public and immutable by default. Structs cannot have methods, constructors, or participate in inheritance hierarchies. This simplicity makes them efficient and predictable.

### Struct Construction and Usage

Creating struct instances uses the same archetype syntax as classes:

```verse
Origin := vector2{}  # Uses defaults: (0.0, 0.0)
PlayerPos := vector2{X := 100.0, Y := 250.0}
RedColor := color{R := 255}  # Other channels default to 0/255

# Structs are values - assignment creates a copy
NewPos := PlayerPos
# NewPos is a separate instance with the same values
```

Since structs are value types, assigning a struct to a variable creates a copy of all its data. This differs from classes, which use reference semantics.

### Struct Comparison

Structs with all comparable fields support equality comparison:

```verse
vector3i := struct:
    X : int = 0
    Y : int = 0
    Z : int = 0

Origin := vector3i{}
UnitX := vector3i{X := 1}

if (Origin = vector3i{}):  # Succeeds - all fields match
    Print("At origin")

if (Origin = UnitX):  # Fails - X fields differ
    Print("Same position")
```

Comparison happens field by field, succeeding only if all corresponding fields are equal.

### Persistable Structs

Structs can be marked as persistable for use with Verse's persistence system:

```verse
player_stats := struct<persistable>:
    HighScore : int = 0
    GamesPlayed : int = 0
    WinRate : float = 0.0

# Can be used in persistent storage
PlayerData : weak_map(player, player_stats) = map{}
```

Once published, persistable structs cannot be modified, ensuring data compatibility across game updates.

### When to Use Structs

Structs excel in specific scenarios where their limitations become strengths. Use structs for simple data containers that group related values without behavior, like coordinates, colors, or configuration data. They're ideal for data transfer objects that move information between systems without needing methods. Mathematical types benefit from struct's value semantics and comparison capabilities. Persistable data that needs to remain stable across game updates fits naturally into structs.

Avoid structs when you need methods, inheritance, mutable fields, or complex initialization logic—these scenarios call for classes instead.

## Enums

Enums define types with a fixed set of named values, perfect for representing states, types, or any concept with a known, finite set of alternatives. They make code more readable by replacing magic numbers with meaningful names and provide compile-time safety by restricting values to the defined set.

An enum lists all possible values for a type:

```verse
game_state := enum:
    MainMenu
    Playing
    Paused
    GameOver

damage_type := enum:
    Physical
    Fire
    Ice
    Lightning
    Poison

direction := enum:
    North
    East
    South
    West
```

Each value in the enum becomes a named constant of that enum type. The compiler ensures that variables of an enum type can only hold one of these defined values.

### Using Enums

Enums provide type-safe alternatives to error-prone string or integer constants:

```verse
var CurrentState : game_state = game_state.MainMenu

ProcessInput(Input : string) : void =
    case (CurrentState):
        game_state.MainMenu =>
            if (Input = "Start"):
                set CurrentState = game_state.Playing
        game_state.Playing =>
            if (Input = "Pause"):
                set CurrentState = game_state.Paused
        game_state.Paused =>
            if (Input = "Resume"):
                set CurrentState = game_state.Playing
            else if (Input = "Quit"):
                set CurrentState = game_state.MainMenu
        game_state.GameOver =>
            if (Input = "Restart"):
                set CurrentState = game_state.MainMenu
```

The `case` expression with enums provides exhaustive pattern matching, ensuring you handle all possible states. The compiler can warn if you miss cases, preventing bugs from unhandled states.

### Open vs Closed Enums

Enums can be marked as open or closed, affecting how they can evolve after publication:

```verse
# Closed enum - cannot add values after publication
day_of_week := enum<closed>:  # <closed> is the default
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday

# Open enum - can add new values after publication
weapon_type := enum<open>:
    Sword
    Bow
    Staff
    # Can add Wand, Dagger, etc. in updates
```

Closed enums are the default and work best for fixed sets like days of the week or cardinal directions. Open enums accommodate future expansion, useful for item types, enemy types, or other game content that grows over time.

### Enum Comparison

Enum values support equality comparison:

```verse
CurrentWeapon := weapon_type.Sword
if (CurrentWeapon = weapon_type.Sword):
    PlaySwordAnimation()

PreviousState := game_state.Playing
if (CurrentState <> PreviousState):
    OnStateChanged(PreviousState, CurrentState)
```

### Persistable Enums

Enums can be made persistable for save systems:

```verse
player_class := enum<persistable>:
    Warrior
    Mage
    Rogue
    Cleric

save_data := struct<persistable>:
    SelectedClass : player_class = player_class.Warrior
    Level : int = 1
```

Persistable enums maintain value compatibility across game updates, though open enums can have new values added.

## Combining Composite Types

The real power of Verse's type system emerges when you combine different composite types to model complex game systems. Each type serves its purpose while working together to create a cohesive architecture.

### Classes with Interfaces

Classes implementing interfaces create flexible, extensible systems:

```verse
# Define behavior contracts
interactive := interface:
    Interact(Player:character):void
    CanInteract(Player:character):logic
    GetInteractionPrompt():string

# Implement in various classes
door := class(entity, interactive):
    var IsLocked : logic = false
    RequiredKey : string = ""

    Interact(Player:character):void =
        if (not IsLocked):
            Open()
        else if (Player.HasItem(RequiredKey)):
            Unlock()
            Open()

    CanInteract(Player:character):logic =
        not IsOpen

    GetInteractionPrompt():string =
        if (IsLocked) then "Locked" else "Open Door"

chest := class(entity, interactive):
    var Contents : []item = array{}
    var IsOpened : logic = false

    Interact(Player:character):void =
        if (not IsOpened):
            set IsOpened = true
            for (Item : Contents):
                Player.AddItem(Item)

    CanInteract(Player:character):logic =
        not IsOpened

    GetInteractionPrompt():string =
        "Open Chest"
```

### Structs in Classes

Structs serve as value types within classes:

```verse
transform := struct:
    Position : vector3 = vector3{}
    Rotation : rotation = rotation{}
    Scale : vector3 = vector3{X := 1.0, Y := 1.0, Z := 1.0}

game_object := class:
    var Transform : transform = transform{}
    Model : string = ""

    MoveTo(NewPosition : vector3) : void =
        set Transform = transform:
            Position := NewPosition
            Rotation := Transform.Rotation
            Scale := Transform.Scale

    Rotate(Degrees : float) : void =
        # Create new transform with updated rotation
        NewRotation := rotation{Yaw := Degrees}
        set Transform = transform:
            Position := Transform.Position
            Rotation := NewRotation
            Scale := Transform.Scale
```

### Enums for Type Safety

Enums make state management and type categorization explicit:

```verse
ability_type := enum:
    Instant
    Channeled
    Passive
    Toggle

ability := class<abstract>:
    Name : string
    Type : ability_type
    Cooldown : float = 0.0

    Cast<abstract>(Caster:character, Target:?character):void

fireball := class(ability):
    Name<override> : string = "Fireball"
    Type<override> : ability_type = ability_type.Instant
    Cooldown<override> : float = 5.0
    Damage : int = 50

    Cast<override>(Caster:character, Target:?character):void =
        if (TargetChar := Target?):
            TargetChar.TakeDamage(Damage)
            PlayFireballEffect(Caster, TargetChar)
```

### Complex Game Systems

Combining all composite types creates rich, maintainable game architectures:

```verse
# Core types and interfaces
faction := enum:
    Neutral
    Alliance
    Horde

targetable := interface:
    GetPosition():vector3
    GetFaction():faction
    IsTargetable():logic

# Base entity with transform
entity := class:
    var Transform : transform = transform{}
    var IsActive : logic = true

    GetPosition():vector3 = Transform.Position

# Combat unit combining everything
unit := class(entity, targetable, damageable):
    Name : string
    Faction : faction = faction.Neutral
    var Health : int = 100
    MaxHealth : int = 100
    var Target : ?targetable = false

    GetFaction():faction = Faction
    IsTargetable():logic = IsActive and IsAlive()

    TakeDamage(Amount:int):void =
        set Health = Max(0, Health - Amount)
        if (Health = 0):
            OnDeath()

    GetHealth():int = Health
    IsAlive():logic = Health > 0

    OnDeath():void =
        set IsActive = false
        set Target = false

# Specialized unit types
soldier := class(unit):
    Weapon : weapon_type = weapon_type.Sword
    Armor : int = 10

    TakeDamage<override>(Amount:int):void =
        ReducedDamage := Max(0, Amount - Armor)
        super.TakeDamage(ReducedDamage)

# Game manager using all types
combat_manager := class:
    var Units : []unit = array{}
    var CombatState : game_state = game_state.Playing

    FindTargets(Attacker:unit, Range:float):[]targetable =
        var Targets : []targetable = array{}
        for (PotentialTarget : Units):
            if (PotentialTarget.GetFaction() <> Attacker.GetFaction()):
                if (Distance(Attacker.GetPosition(), PotentialTarget.GetPosition()) <= Range):
                    if (PotentialTarget.IsTargetable()):
                        set Targets = Targets + array{PotentialTarget}
        Targets
```

## Best Practices

### Choosing the Right Type

The choice between class, interface, struct, and enum shapes your program's architecture fundamentally. Classes excel when you need objects with identity, behavior, and state that changes over time. They support inheritance and polymorphism, making them ideal for game entities, systems, and anything requiring object-oriented design patterns.

Interfaces shine when defining contracts that multiple unrelated classes might implement. They enable loose coupling and dependency inversion, crucial for testable, maintainable code. Use interfaces when you care about what an object can do, not what it is.

Structs serve best as simple data containers without behavior. Their value semantics and immutability make them perfect for coordinates, colors, configuration data, and data transfer objects. The lack of methods and inheritance keeps them lightweight and predictable.

Enums provide type-safe sets of named constants, eliminating magic numbers and making code self-documenting. They're invaluable for states, types, categories, and any fixed set of alternatives your game needs to distinguish.

### Design Principles

Follow the single responsibility principle by ensuring each type has one clear purpose. A class should represent one concept, an interface should define one cohesive contract, a struct should group one set of related data, and an enum should represent one category of values.

Favor composition over inheritance when designing class hierarchies. Rather than deep inheritance trees, compose objects from smaller, focused components. This creates more flexible, maintainable systems that are easier to understand and modify.

Use interfaces to define capabilities rather than identities. An interface named `shootable` is better than `enemy` because it describes what the object can do rather than what it is, enabling more flexible use.

Keep structs simple and focused on data. If you find yourself wanting to add methods or complex validation to a struct, consider using a class instead. Structs should remain pure data containers.

Make enums intention-revealing. Each enum value should be self-explanatory, eliminating the need for comments or external documentation to understand what it represents.
