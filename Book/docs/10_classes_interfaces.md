# Classes and Interfaces

Classes and interfaces are Verse's object-oriented building blocks that enable rich type hierarchies with inheritance, polymorphism, and interface-based contracts. Classes provide object-oriented programming with fields, methods, and single inheritance, enabling you to model complex hierarchies of game entities with shared behavior and specialized implementations. Interfaces define contracts that classes must fulfill, promoting loose coupling and enabling multiple inheritance of behavior specifications.

Together, classes and interfaces form a powerful system for modeling game entities, components, and systems with both is-a relationships (through class inheritance) and can-do contracts (through interface implementation).

Let's explore classes first, then delve into interfaces and how they complement each other.

## Classes

Classes form the backbone of object-oriented programming in Verse. A class serves as a blueprint for creating objects that share common properties and behaviors. When you define a class, you're creating a new type that bundles data (fields) with operations on that data (methods), encapsulating related functionality into a cohesive unit.

Class definitions occur at module scope. You cannot define a class inside another class, struct, interface, or function. Classes are top-level type definitions that establish the type system's structure:

```verse
# Valid: class at module scope
my_module := module:
    entity := class:
        ID:int

# Invalid: class inside another class
# outer := class:
#     inner := class:  # ERROR: classes must be at module scope
#         Value:int
```

The simplest form of a class groups related data together. Consider modeling a character in your game:

```verse
character := class:
    Name : string
    var Health : int = 100
    var Level : int = 1
    MaxHealth : int = 100
```

This class definition establishes several important concepts. Fields without the `var` modifier are immutable after construction—once you create a character with a specific name, that name cannot change. Fields marked with `var` are mutable and can be modified after the object is created (see [Mutability](05_mutability.md) for details on `var` and `set`). Default values provide sensible starting points, making object construction more convenient while ensuring objects start in valid states.

### Object Construction

Creating instances of a class involves specifying values for its fields through an archetype expression:

<!--verse
character := class:
    Name : string
    var Health : int = 100
    var Level : int = 1
    MaxHealth : int = 100
-->
```verse
Hero := character{Name := "Aldric", Health := 100, Level := 5}
Villager := character{Name := "Martha"}  # Uses default values for unspecified fields
```

The archetype syntax uses named parameters, making the construction explicit and self-documenting. Any field with a default value can be omitted from the archetype, and the default will be used. Fields without defaults must be specified, ensuring objects are always fully initialized. Fields can be passed to an archetype in any order.

### Methods

Classes become truly powerful when you add methods that operate on the class's data:

```verse
character := class:
    Name : string
    var Health : int = 100
    var Level : int = 1
    var MaxHealth : int = 100

    TakeDamage(Amount : int) : void =
        set Health = Max(0, Health - Amount)

    Heal(Amount : int) : void =
        set Health = Min(MaxHealth, Health + Amount)

    IsAlive()<decides>:void= Health > 0

    LevelUp() : void =
        set Level += 1
        set MaxHealth = 100 + (Level * 10)
        set Health = MaxHealth  # Full heal on level up
```

Methods have access to all fields of the class and can modify mutable fields. They encapsulate the logic for how objects of the class should behave, ensuring that state changes happen in controlled, predictable ways.

All methods in non-abstract classes must have implementations. Unlike interfaces (which can declare abstract methods), a concrete class method declaration without an implementation is an error:

```verse
# Valid: method with implementation
valid_class := class:
    Compute():int = 42

# Invalid: method without implementation in concrete class
# invalid_class := class:
#     Compute():int  # ERROR: needs implementation
```

### Blocks for Initialization

Classes can include `block` clauses in their body, which execute when an instance is created. These blocks run initialization code that goes beyond simple field assignment, allowing you to perform setup logic, validation, or side effects during construction:

```verse
logged_entity := class:
    ID:int
    var CreationTime:float = 0.0

    block:
        # This executes when an instance is created
        Print("Creating entity with ID: {ID}")
        set CreationTime = GetCurrentTime()

Entity := logged_entity{ID := 42}
# Prints: "Creating entity with ID: 42"
```

Block clauses have access to all fields of the class, including `Self`, and can modify mutable fields. They execute in the order they appear in the class definition:

```verse
multi_step_init := class:
    var Step1:int = 0
    var Step2:int = 0

    block:
        set Step1 = 10

    var Step3:int = 0

    block:
        set Step2 = Step1 + 5  # Can access earlier fields
        set Step3 = Step2 * 2

Instance := multi_step_init{}
# Instance.Step1 = 10, Step2 = 15, Step3 = 30
```

**Execution order with inheritance:** When a class inherits from another class, the Verse VM executes blocks in subclass-before-superclass order, while the BP VM uses superclass-before-subclass order. For portable code, avoid depending on the execution order of blocks across inheritance hierarchies.

**Constraints on block clauses:**

- Blocks cannot contain failure (`<decides>`) operations
- Blocks cannot call suspending (`<suspends>`) functions
- Blocks can use `defer` statements, which execute when the block exits
- Block clauses are only allowed in classes, not in interfaces, structs, or modules

Block clauses are particularly useful for:

- Logging object creation
- Computing derived values during initialization
- Registering objects with global systems
- Performing validation that goes beyond simple field checks

### Self

Within class methods, `Self` is a special keyword that refers to the current instance of the class. Each method invocation has its own `Self` that refers to the specific object the method was called on.

You can use `Self` in multiple ways within method bodies:

**Passing the instance to other functions:**

<!--verse
using { /Verse.org/VerseCLR }
-->
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

**Returning the instance:**

```verse
builder := class<unique>:
    var Config:[string]string = map{}

    SetOption(Key:string, Value:string):builder =
        set Config[Key] = Value
        Self  # Return this instance for method chaining

B := builder{}
B.SetOption("width", "800").SetOption("height", "600")
```

**Accessing fields through Self:**

```verse
counter := class<unique>:
    var Count:int = 0

    Increment():int =
        set Count = Count + 1
        Self.Count  # Explicit Self access (same as just Count)
```

**Calling methods through Self:**

```verse
validator := class<unique>:
    Valid:logic = true

    Check():logic = Valid

    DoubleCheck():logic =
        Self.Check()  # Call another method on this instance
```

You can capture `Self` when creating nested objects:

```verse
container := class:
    ID:int

    CreateChild():child_with_parent =
        child_with_parent{Parent := Self}  # Capture this instance

child_with_parent := class:
    Parent:container

C := container{ID := 42}
Child := C.CreateChild()
Child.Parent.ID = 42  # Child stores reference to C
```

### Inheritance

Classes support single inheritance, allowing you to create specialized versions of existing classes. This creates an "is-a" relationship where the subclass is a more specific type of the superclass:

<!--verse
vector3:=struct{}
-->
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

**Important constraints on inheritance:**

1. **Single class inheritance only:** A class can inherit from at most one other class, though it can implement multiple interfaces. Multiple class inheritance is not supported:

```verse
base1 := class:
    Value1:int

base2 := class:
    Value2:int

# Valid: inherit from one class and multiple interfaces
interface1 := interface:
    Method1():void

interface2 := interface:
    Method2():void

derived := class(base1, interface1, interface2):
    # Valid: one class, multiple interfaces

# Invalid: cannot inherit from multiple classes
# invalid := class(base1, base2):  # ERROR
```

2. **No shadowing of data members:** Subclasses cannot declare fields with the same name as fields in their superclass. This prevents ambiguity and ensures clear data ownership:

```verse
base := class:
    Value:int

# Invalid: cannot shadow parent's field
# derived := class(base):
#     Value:int  # ERROR: shadowing base.Value
```

3. **No method signature changes:** When overriding a method, you must use the exact same signature. Changing parameter types or return types creates a shadowing error:

```verse
base := class:
    Compute():int = 42

# Invalid: different return type
# derived := class(base):
#     Compute():float = 3.14  # ERROR: signature doesn't match
```

To override a method, use the `<override>` specifier with the matching signature.

### Super

Within a subclass, you can use the `super` keyword to refer to the superclass type. This is primarily used to access the superclass's implementation or to construct a superclass instance:

```verse
entity := class:
    ID:int
    Name:string

    Display():void =
        Print("Entity {ID}: {Name}")

character := class(entity):
    Health:int

    Display<override>():void =
        # Create a superclass instance to call its method
        super{ID := ID, Name := Name}.Display()
        Print("Health: {Health}")
```

The `super` keyword represents the superclass type itself. When you write `super{...}`, you're creating an instance of the superclass with the specified field values. This allows you to delegate to superclass behavior while adding subclass-specific functionality.

Within an overriding method, you can call the parent class's implementation using the `(super:)` syntax. This is the primary way to invoke parent method implementations while adding or modifying behavior:

```verse
base := class:
    Method():void =
        Print("Base implementation")

derived := class(base):
    Method<override>():void =
        # Call parent implementation first
        (super:)Method()
        Print("Derived implementation")

# Creates instance and calls Method()
# Output:
# Base implementation
# Derived implementation
Instance := derived{}
Instance.Method()
```

The `(super:)` syntax explicitly calls the parent class's version of the current method. This is cleaner and more efficient than constructing a parent instance with `super{...}` when you only need to call parent methods.

**Basic Usage:**

```verse
entity := class:
    Position:vector3

    Move(Delta:vector3):void =
        Print("Entity moving by {Delta}")
        # Update position logic here

character := class(entity):
    var Stamina:float = 100.0

    Move<override>(Delta:vector3):void =
        # Call parent movement logic
        (super:)Move(Delta)
        # Add character-specific behavior
        set Stamina -= 1.0
```

**With Effect Specifiers:**

The `(super:)` syntax works seamlessly with all effect specifiers:

```verse
async_base := class:
    Process()<suspends>:void =
        Sleep(1.0)
        Print("Base processing")

async_derived := class(async_base):
    Process<override>()<suspends>:void =
        # Parent method suspends, so this suspends too
        (super:)Process()
        Print("Derived processing")

transactional_base := class:
    var Value:int = 0

    Update()<transacts>:void =
        set Value += 1

transactional_derived := class(transactional_base):
    var Counter:int = 0

    Update<override>()<transacts>:void =
        (super:)Update()
        set Counter += 1
```

**Virtual Dispatch Through Parent Methods:**

When parent methods call other methods, virtual dispatch still applies based on the actual object type. This means `Self` binds to the derived instance even when calling through `(super:)`:

```verse
base := class:
    # Virtual method that can be overridden
    GetValue():int = 10

    # Parent method that uses GetValue
    ComputeDouble():int =
        2 * GetValue()  # Calls derived GetValue if overridden

derived := class(base):
    # Override GetValue to return different value
    GetValue<override>():int = 20

    # Override ComputeDouble to call parent, but GetValue dispatch is virtual
    ComputeDouble<override>():int =
        # Calls base.ComputeDouble, which calls derived.GetValue!
        (super:)ComputeDouble()

Instance := derived{}
Result := Instance.ComputeDouble()  # Returns 40, not 20
```

In this example, even though `ComputeDouble` calls the parent implementation, the `GetValue()` call inside the parent uses virtual dispatch and calls the derived version.

**With Overloaded Methods:**

The `(super:)` syntax works with overloaded methods, calling the parent's version of the same overload:

```verse
base := class:
    Process(X:int):void =
        Print("Base int: {X}")

    Process(S:string):void =
        Print("Base string: {S}")

derived := class(base):
    Process<override>(X:int):void =
        (super:)Process(X)  # Calls parent's int overload
        Print("Derived int: {X}")

    Process<override>(S:string):void =
        (super:)Process(S)  # Calls parent's string overload
        Print("Derived string: {S}")
```

**Return Type Covariance:**

When overriding methods with `(super:)`, the return type can be a subtype of the parent's return type (covariant return types):

```verse
base_type := class:
    Name:string

derived_type := class(base_type):
    Value:int

base := class:
    Create():base_type =
        base_type{Name := "base"}

derived := class(base):
    # Override with more specific return type
    Create<override>():derived_type =
        # Can still call parent even with different return type
        Parent := (super:)Create()
        derived_type{Name := Parent.Name, Value := 42}
```

### Method Overriding

Subclasses can override methods defined in their superclasses to provide specialized behavior:

<!--verse
character:=class:
    IsAlive()<decides><transacts>:void={}
MoveToward(:?character)<transacts>:void={}
Patrol()<transacts>:void={}
ScanForTargets()<transacts>:void={}
-->
```verse
entity := class:
    OnUpdate<public>() : void = {}  # Default no-op implementation

enemy := class(entity):
    var Target : ?character = false

    OnUpdate<override>()<transacts> : void =
        if (Target?.IsAlive[]):
            MoveToward(Target)
        else:
            Patrol()

turret := class(entity):
    var Rotation:int= 0.

    OnUpdate<override>()<transacts>: void =
        if (V:= Mod[Rotation, 360]):
            set Rotation = V
        ScanForTargets()
```

The override mechanism ensures that the correct method implementation is called based on the actual type of the object, not the type of the variable holding it. This is the foundation of polymorphic behavior in object-oriented programming.

### Constructor Functions

Classes don't have traditional constructor methods like you might find in other object-oriented languages. Instead, Verse provides two approaches to object construction: direct field initialization through archetype expressions, and constructor functions for complex initialization scenarios.

For simple cases where you just need to set field values, use archetype expressions directly:

```verse
player := class:
    Name:string
    var Health:int = 100
    Level:int = 1

# Direct construction with archetype
Hero := player{Name := "Aldric", Health := 150, Level := 5}
```

When you need validation, computation, or complex initialization logic, use constructor functions annotated with `<constructor>`:

<!--verse
player := class:
    Name:string
    var Health:int = 100
    Level:int = 1
MaxLevel:int=99
-->
```verse
MakePlayer<constructor>(Name:string, Level:int)<computes> := player:
    Name := Name
    Level := Level
    Health := Level * 100

# Call constructor function without <constructor> in the call
Hero := MakePlayer("Aldric", 5)
```

Constructor functions are regular functions that return class instances, but the `<constructor>` annotation enables special capabilities like delegating to other constructors. When calling a constructor function from normal code, use just the function name—the `<constructor>` annotation only appears in the definition.

Constructor functions can have effects that control their behavior. Common effects include `<computes>`, `<allocates>`, and `<transacts>`. A particularly useful effect is `<decides>`, which allows constructors to fail if preconditions aren't met:

```verse
MakeValidatedPlayer<constructor>(Name:string, Level:int)<computes><decides> := player:
    Level > 0
    Level <= MaxLevel
    Name := Name
    Level := Level
    Health := Level * 100

# Constructor can fail - use with failure syntax
if (ValidPlayer := MakeValidatedPlayer["Hero", 5]):
    # Construction succeeded
else:
    # Construction failed - level out of range
```

Constructor functions cannot use the `<suspends>` effect. Construction must complete synchronously to maintain object consistency.

### Overloading Constructors

You can provide multiple constructor functions with different parameter signatures, allowing flexible object creation:

```verse
entity := class:
    Name:string
    var Health:int = 100
    Position:vector3

# Constructor with all parameters
MakeEntity<constructor>(Name:string, Health:int, Position:vector3) := entity:
    Name := Name
    Health := Health
    Position := Position

# Constructor with defaults
MakeEntity<constructor>(Name:string, Position:vector3) := entity:
    Name := Name
    Health := 100
    Position := Position

# Constructor for origin placement
MakeEntity<constructor>(Name:string) := entity:
    Name := Name
    Health := 100
    Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}

# Each overload can be called based on arguments
Enemy1 := MakeEntity("Goblin", 50, SpawnPoint)
Enemy2 := MakeEntity("Guard", PatrolPoint)
NPC := MakeEntity("Shopkeeper")
```

### Delegating Constructors

Constructor functions can delegate to other constructors, enabling code reuse and constructor chaining. This is particularly important for inheritance hierarchies where subclass constructors need to initialize superclass fields.

When delegating to a parent class constructor from a subclass, you must initialize the subclass fields first, then call the parent constructor using the qualified `<constructor>` syntax within the archetype:

```verse
entity := class:
    Name:string
    var Health:int

MakeEntity<constructor>(Name:string, Health:int) := entity:
    Name := Name
    Health := Health

character := class(entity):
    Class:string
    Level:int

# Subclass constructor delegates to parent constructor
MakeCharacter<constructor>(Name:string, Class:string, Level:int) := character:
    # Initialize subclass fields first
    Class := Class
    Level := Level
    # Then delegate to parent constructor
    MakeEntity<constructor>(Name, Level * 100)

Hero := MakeCharacter("Aldric", "Warrior", 5)
```

Constructor functions can also forward to other constructors of the same class:

```verse
player := class:
    Name:string
    var Score:int

# Primary constructor
MakePlayer<constructor>(Name:string, Score:int) := player:
    Name := Name
    Score := Score

# Convenience constructor forwards to primary
MakeNewPlayer<constructor>(Name:string) := player:
    # Delegate to another constructor of the same class
    MakePlayer<constructor>(Name, 0)

NewPlayer := MakeNewPlayer("Alice")
```

When delegating to a constructor of the same class, the delegation replaces all field initialization—any fields you initialize before the delegation are ignored. When delegating to a parent class constructor, your subclass field initializations are preserved, and the parent constructor initializes the parent fields.

### Order of Execution

Understanding execution order is crucial for correct initialization:

1. **Archetype expression:** Field initializers execute in the order they're written in the archetype
2. **Delegating constructor:** Subclass fields are initialized first, then the parent constructor runs
3. **Class body blocks:** When using direct archetype construction, blocks in the class definition execute before field initialization

For delegating constructors to parent classes:

```verse
base := class:
    BaseValue:int

MakeBase<constructor>(Value:int) := base:
    block:
        Print("Base constructor")
    BaseValue := Value

derived := class(base):
    DerivedValue:int

MakeDerived<constructor>(Base:int, Derived:int) := derived:
    # This executes first
    DerivedValue := Derived
    # Then parent constructor executes
    MakeBase<constructor>(Base)

# Prints: "Base constructor"
# Results in: derived{BaseValue := 10, DerivedValue := 20}
Instance := MakeDerived(10, 20)
```

For classes with mutable fields, initialization sets starting values that can change during the object's lifetime. Immutable fields must be initialized during construction and cannot be modified afterward. This distinction makes the construction phase critical for establishing invariants that will hold throughout the object's existence.

## Shadowing and Qualification

Verse has strict rules about name shadowing to prevent ambiguity and maintain code clarity. Understanding these rules and the qualification syntax is essential for working with inheritance hierarchies, multiple interfaces, and nested modules.

In most contexts, you **cannot redefine names** that already exist in an enclosing scope. This applies to functions, variables, classes, interfaces, and modules:

```verse
# ERROR 3532: Function at module level shadows class method
# F(X:int):int = X + 1
# c := class:
#     F(X:int):int = X + 2  # ERROR - shadows outer F
```

This prohibition extends across various contexts:

```verse
# ERROR 3532: Cannot shadow classes
# Something := class {}
# M := module:
#     Something := class {}  # ERROR

# ERROR 3532: Cannot shadow variables
# Value:int = 1
# M := module:
#     Value:int = 2  # ERROR

# ERROR 3532: Cannot shadow data members
# c := class { A:int }
# A():void = {}  # ERROR - order doesn't matter

# ERROR 3532: Module and function cannot share name
# id():void = {}
# id := module {}  # ERROR
```

The shadowing prohibition exists **regardless of definition order** - it doesn't matter whether the outer name is defined before or after the inner scope.

To define methods with the same name in different contexts, use **qualified names** with the syntax `(ClassName:)MethodName`:

```verse
# Module-level function
F80(X:int):int = X + 1

# Class with qualified method of same name
c80 := class:
    (c80:)F80(X:int):int = X + 2

# Call the module-level function
F80(10)  # Returns 11

# Call the class method
c80{}.F80(10)  # Returns 12

# Explicit qualification (optional here)
c80{}.(c80:)F80(10)  # Returns 12
```

The `(c80:)` qualifier indicates this `F80` is defined specifically in the `c80` class context, distinguishing it from the module-level `F80`. This allows the same name to coexist without shadowing errors.

### Methods with Same Name

Using qualifiers, you can define **new methods** with the same name as inherited methods, creating multiple distinct methods in the same class:

```verse
c50 := class<abstract> { F(X:int):int }

c51 := class(c50):
    F<override>(X:int):int = X + 1

c52 := class(c51):
    # NEW method with same name, not an override
    (c52:)F(X:int):int = X + 2

# c52 now contains BOTH methods:
# - (c50:)F inherited from c51
# - (c52:)F newly defined in c52

o52 := c52{}
o52.(c50:)F(10)  # Returns 11 (inherited from c51's override)
o52.(c52:)F(10)  # Returns 12 (new method in c52)
```

**Key distinction:**

- `F<override>` without qualifier: Overrides the inherited `F`
- `(c52:)F` without `<override>`: Defines a **new** `F` specific to `c52`

This allows a class to have multiple methods with the same name, differentiated by their qualifiers, each serving different purposes in the class hierarchy.

### `(super:)` Qualified

The `(super:)` qualifier works with qualified method names to call the parent class's implementation:

```verse
i60 := interface { F(X:int):int }

c61 := class(i60):
    (i60:)F<override>(X:int):int = X + 1
    (c61:)F(X:int):int = X + 2

c62 := class(c61):
    # Override both inherited methods, calling super implementations
    (i60:)F<override>(X:int):int = 100 + (super:)F(X)
    (c61:)F<override>(X:int):int = 200 + (super:)F(X)

o62 := c62{}
o62.(i60:)F(10)  # Returns 111 (100 + c61's 11)
o62.(c61:)F(10)  # Returns 212 (200 + c61's 12)
```

`(super:)F(X)` within the qualified method calls the parent class's implementation of that same qualified method. This enables you to extend behavior for multiple method variants independently.

### Interface Collisions

When implementing multiple interfaces with methods of the same name, qualifiers disambiguate which interface's method you're implementing:

```verse
interface_A := interface:
    B(X:int):int

interface_B := interface:
    B(X:int):int

collision := class(interface_A, interface_B):
    # Implement both B methods separately
    (interface_A:)B<override>(X:int):int = 20 + X
    (interface_B:)B<override>(X:int):int = 30 + X

Obj := collision{}
Obj.(interface_A:)B(1)  # Returns 21
Obj.(interface_B:)B(1)  # Returns 31
```

Without qualifiers, the compiler cannot determine which interface's method you're implementing, resulting in an error. The qualification makes your intent explicit.

**Complex interface hierarchies:**

```verse
interface_C := interface:
    C(X:int):int

interface_A := interface(interface_C):
    A(X:int):int

interface_B := interface(interface_C):
    B(X:int):int
    # interface_B redefines C
    (interface_B:)C(X:int):int

multi := class(interface_A, interface_B):
    A<override>(X:int):int = 10 + X
    B<override>(X:int):int = 20 + X
    # Must implement C from both inheritance paths
    (interface_C:)C<override>(X:int):int = 30 + X
    (interface_B:)C<override>(X:int):int = 40 + X

Obj := multi{}
Obj.(interface_C:)C(1)  # Returns 31
Obj.(interface_B:)C(1)  # Returns 41
```

When an interface redefines a method from a parent interface using qualification `(interface_B:)C`, implementing classes must provide separate implementations for both variants.

### Nested Module Qualification

Modules can be nested, and deeply qualified names reference members through the entire hierarchy:

```verse
TopLevel := module:
    (TopLevel:)module_a<public> := module:
        (TopLevel.module_a:)Value<public>:int = 1
        (TopLevel.module_a:)Function<public>(X:int):int = X + 10

        (TopLevel.module_a:)module_a<public> := module:
            (TopLevel.module_a.module_a:)Value<public>:int = 3
            (TopLevel.module_a.module_a:)Function<public>(X:int):int = X + 100

using { TopLevel.module_a }
using { TopLevel.module_a.module_a }

# Access with full qualification
(TopLevel.module_a:)Function(0)          # Returns 10
(TopLevel.module_a.module_a:)Function(0) # Returns 100

# Access via path
TopLevel.module_a.Function(1)          # Returns 11
TopLevel.module_a.module_a.Function(1) # Returns 101
```

Nested modules can have the same simple name (e.g., both `module_a`) when qualified with their full path, allowing hierarchical organization without naming conflicts.

### Restrictions

Qualifiers can only be used in appropriate contexts. You cannot use class qualifiers for local variables:

```verse
# ERROR 3612: Class qualifier not valid for local variable
# C := class:
#     f():void =
#         (C:)X:int = 0  # ERROR - wrong context
```

Certain qualifiers are not supported. Function qualifiers for local variables are not allowed:

```verse
# ERROR 3506: Function qualifier not supported
# C := class:
#     f():void =
#         (C.f:)X:int = 0  # ERROR - unsupported pattern
```

Similarly, using module function paths as qualifiers is not supported:

```verse
# ERROR 3612: Should use (local:) instead (not yet supported)
# M := module:
#     f():void =
#         (M.f:)X:int = 0  # ERROR
```

Local variables cannot shadow class members:

```verse
# ERROR 3588, 3532: Local shadows member
# A := class:
#     I:int
#     F(X:int):int =
#         I:int = 5  # ERROR - shadows member I
#         I
```

Currently, there is no `(local:)` qualifier to disambiguate, so this pattern is not supported. You must use different names for local variables and members.

### External Package Shadowing

Shadowing rules are **relaxed for external packages** (those marked with `?Role:=External`), except when two symbols shadow each other in the same scope:

```verse
# Normal package - ERROR 3532
# vpackage(P, /Root) {
#     A<public>:int = 0
#     m := module { A<public>:int = 0 }  # ERROR - shadowing
# }

# External package - OK
vpackage(P, /Root, ?Role:=External) {
    snippet {
        A<public>:int = external{}
        m := module { A<public>:int = external{} }  # OK - shadowing allowed
    }
}
```

This relaxation allows external packages to have internal naming conflicts without causing errors in consuming code, since external implementations are provided separately.

**Still an error in same scope:**

```verse
# ERROR 3532 even in external package
# vpackage(P, /Root, ?Role:=External) {
#     snippet {
#         A<public>:int = external{}
#         A<public>:int = external{}  # ERROR - same scope
#     }
# }
```

## Parametric Classes

Parametric classes, also known as generic classes, allow you to define classes that work with any type. Rather than writing separate container classes for integers, strings, players, and every other type, you write one parametric class that accepts a type parameter.

A parametric class takes one or more type parameters in its definition:

```verse
# Simple container that holds a single value
container(t:type) := class:
    Value:t

# Can be instantiated with any type
IntContainer := container(int){Value := 42}
StringContainer := container(string){Value := "hello"}
PlayerContainer := container(player){Value := player{}}
```

The syntax `container(t:type)` defines a class that is parameterized by type `t`. Within the class definition, `t` can be used anywhere a concrete type would appear—in field declarations, method signatures, or return types.

**Multiple type parameters:**

Classes can accept multiple type parameters:

```verse
pair(t:type, u:type) := class:
    First:t
    Second:u

# Different types for each parameter
Coordinate := pair(int, int){First := 10, Second := 20}
NamedValue := pair(string, float){First := "score", Second := 99.5}
```

**Type parameters in methods:**

Type parameters are available throughout the class, including in methods:

```verse
optional_container(t:type) := class:
    var MaybeValue:?t = false

    Set(Value:t):void =
        set MaybeValue = option{Value}

    Get()<decides>:t =
        MaybeValue?

    Clear():void =
        set MaybeValue = false
```

Methods automatically know about the type parameter from the class definition—you don't redeclare it in method signatures.

### Instantiation and Identity

When you instantiate a parametric class with specific type arguments, Verse creates a concrete type. Critically, **multiple instantiations with the same type arguments produce the same type**:

```verse
container(t:type) := class:
    Value:t

# These are the same type
Type1 := container(int)
Type2 := container(int)
Type3 := container(int)

# All three are equal - they're the same type
```

This type identity is guaranteed across the program:

```verse
# Create instances
C1 := container(int){Value := 1}
C2 := container(int){Value := 2}

# Both have the same type: container(int)
# Type checking treats them identically
```

The instantiation process is **deterministic and memoized**. The first time you write `container(int)`, Verse generates a concrete type. Every subsequent use of `container(int)` refers to that same type, not a new copy.

This matters for:

- **Type compatibility**: Two values of `container(int)` can be used interchangeably
- **Memory efficiency**: Not creating duplicate type definitions
- **Semantic correctness**: Same type arguments always mean the same type

Even when generating types in loops, the same instantiation yields the same type:

```verse
# Generate the same type four times
Types := for (I := 1..4) { container(int) }

# All four are the same type
Types[0] = Types[1]  # Same type
Types[1] = Types[2]  # Same type
Types[2] = Types[3]  # Same type
```

While the same type arguments always produce the same type, different type arguments produce distinct, incompatible types:

```verse
container(t:type) := class:
    Value:t

IntContainer := container(int){Value := 42}
StringContainer := container(string){Value := "text"}

# These are different types and cannot be mixed
# IntContainer = StringContainer  # Type error!
```

`container(int)` and `container(string)` are completely different types, with no subtype relationship. They happen to share the same structure (both defined from `container`), but that doesn't make them compatible.

While different instantiations of a parametric class are distinct types, Verse allows certain instantiations to be used in place of others based on **variance**. Variance determines when `parametric_class(subtype)` can be used where `parametric_class(supertype)` is expected (or vice versa).

The variance of a parametric type depends on how the type parameter is used within the class definition:

#### Covariant

When a type parameter appears only in **return positions** (method return types, field types being read), the parametric class is **covariant** in that parameter (see [Types](11_types.md#understanding-subtyping) for details on variance). This means instantiations follow the same subtyping direction as their type arguments:

```verse
# Base class hierarchy
entity := class:
    ID:int

player := class(entity):
    Name:string

# Covariant class - type parameter only in return position
producer(t:type) := class:
    Value:t

    Get():t = Value  # Returns t - covariant position

# Covariance allows subtype → supertype
PlayerProducer:producer(player) = producer(player){Value := player{ID := 1, Name := "Alice"}}
EntityProducer:producer(entity) = PlayerProducer  # Valid!

# Can use producer(player) where producer(entity) expected
ProcessProducer(P:producer(entity)):int = P.Get().ID
Result := ProcessProducer(PlayerProducer)  # Works!
```

**Why this is safe:** If you expect to get an `entity` from a producer, receiving a `player` (which is a subtype of `entity`) is always valid—a `player` has all the properties of an `entity`.

**Direction:** `producer(player)` → `producer(entity)` ✓ (follows subtype direction)

#### Contravariant

When a type parameter appears only in **parameter positions** (method parameters being consumed), the parametric class is **contravariant** in that parameter (see [Types](11_types.md#understanding-subtyping) for details on variance). This means instantiations follow the **opposite** subtyping direction:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Contravariant class - type parameter only in parameter position
consumer(t:type) := class:
    Process(Item:t):void = {}  # Accepts t - contravariant position

# Contravariance allows supertype → subtype
EntityConsumer:consumer(entity) = consumer(entity){}
PlayerConsumer:consumer(player) = EntityConsumer  # Valid!

# Can use consumer(entity) where consumer(player) expected
ProcessPlayers(C:consumer(player)):void =
    C.Process(player{ID := 1, Name := "Bob"})

ProcessPlayers(EntityConsumer)  # Works!
```

**Why this is safe:** If you have a function that accepts any `entity`, it can certainly handle the more specific `player` type. A `consumer(entity)` can consume anything a `consumer(player)` can consume, plus more.

**Direction:** `consumer(entity)` → `consumer(player)` ✓ (opposite of subtype direction)

#### Invariant

When a type parameter appears in **both parameter and return positions**, the parametric class is **invariant** in that parameter. No subtyping relationship exists between different instantiations:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Invariant class - type parameter in both positions
transformer(t:type) := class:
    Transform(Input:t):t = Input  # Both parameter and return

# No variance - cannot convert in either direction
EntityTransformer:transformer(entity) = transformer(entity){}
PlayerTransformer:transformer(player) = transformer(player){}

# Invalid: Cannot use one where the other is expected
# X:transformer(entity) = PlayerTransformer  # ERROR 3509
# Y:transformer(player) = EntityTransformer  # ERROR 3509
```

**Why this is necessary:** If a `transformer(player)` could be used as a `transformer(entity)`, you could pass any `entity` to its `Transform` method, which expects specifically a `player`. This would be unsafe.

**Direction:** No conversion allowed in either direction

#### Bivariant

When a type parameter is not used in any method signatures (only in private implementation details or not at all), the parametric class is **bivariant**. Any instantiation can be converted to any other:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Bivariant class - type parameter not used in public interface
container(t:type) := class:
    DoSomething():void = {}  # Doesn't use t at all

# Bivariant allows conversion in both directions
EntityContainer:container(entity) = container(entity){}
PlayerContainer:container(player) = container(player){}

# Both directions work
X:container(entity) = PlayerContainer  # Valid
Y:container(player) = EntityContainer  # Also valid
```

**Why this works:** Since the type parameter doesn't affect the observable behavior, the instantiations are interchangeable.

#### Common Pitfalls

**Attempting invalid conversions:**

```verse
# Invariant parameter - neither direction works
ref(t:type) := class:
    var Value:t
    Get():t = Value
    Set(V:t):void = set Value = V

PlayerRef:ref(player) = ref(player){Value := player{ID := 1, Name := "Test"}}

# Invalid: ref is invariant
# EntityRef:ref(entity) = PlayerRef  # ERROR 3509
```

**Confusing variance direction:**

```verse
# Common mistake: thinking contravariance works like covariance
consumer(t:type) := class:
    Accept(Item:t):void = {}

EntityConsumer := consumer(entity){}

# Invalid: Wrong direction for contravariance
# PlayerConsumer:consumer(player) = consumer(entity){}  # ERROR 3509

# Valid: Contravariance goes opposite direction
PlayerConsumer:consumer(player) = EntityConsumer  # Correct!
```

### Parameter Constraints

You can constrain type parameters to require certain properties:

```verse
# Only comparable types allowed
sorted_list(t:type where t:subtype(comparable)) := class:
    var Items:[]t = array{}

    Add(Item:t):void =
        # Can compare because t is comparable
        set Items = InsertSorted(Items, Item)

    Contains(Item:t):logic =
        for (Element : Items):
            if (Element = Item):
                return true
        false

# Valid: int is comparable
IntList := sorted_list(int){}

# Invalid: regular classes aren't comparable by default
# PlayerList := sorted_list(player){}  # Error if player isn't comparable
```

The `where` clause specifies requirements on the type parameter. Common constraints include:

- `t:subtype(comparable)` - requires equality comparison
- `t:subtype(SomeClass)` - requires inheriting from a specific class
- `t:type` - any type (the default if no constraint specified)

### Restrictions

Parametric classes have certain limitations:

**Cannot be `<castable>`:**

Parametric classes cannot use the `<castable>` specifier because runtime type checks require knowing the concrete type:

<!--NoCompile-->
```verse
# Invalid: parametric classes cannot be castable
container(t:type) := class<castable>:  # Error!
    Value:t
```

However, specific instantiations can be used where `<castable>` types are needed:

```verse
component := class<castable>{}

container(t:type) := class:
    Value:t

# Valid: concrete instantiation of parametric type
ProcessComponent(Comp:component):void =
    if (Wrapped := container(component)[Comp]):
        # Wrapped is container(component)
```

**Cannot cast between different parametric instantiations:**

Even when instantiations are fixed (non-parametric), you cannot use cast syntax to convert between different instantiations of the same parametric class or interface. This restriction is enforced at compile time:

```verse
container(t:type) := class:
    Value:t

X := container(int){Value := 42}

# Invalid: Cannot cast between different instantiations
# if (Y := container(float)[X]):     # ERROR 3502
#     # This will not compile
# if (Z := container([]int)[X]):     # ERROR 3502
#     # This also will not compile
```

Different instantiations like `container(int)` and `container(float)` are completely distinct types with no subtype relationship, so cast expressions between them are disallowed. The compiler rejects these casts with error 3502 even though both are concrete types.

This restriction extends to parametric class hierarchies:

```verse
base := class:
    Property:int

parametric_child(t:type) := class(base):
    GetProperty():int = Property

# Cannot cast between different instantiations of parametric_child
Foo:base = parametric_child(float){Property := 42}

# Invalid: Different type parameters prevent casting
# if (FooChild := parametric_child(int)[Foo]):  # ERROR 3502
#     # Cannot cast parametric_child(float) to parametric_child(int)
```

Even though both `parametric_child(int)` and `parametric_child(float)` inherit from `base`, you cannot cast between them because they are different instantiations of a parametric type.

**Parametric interfaces also cannot be used in casts:**

Cast expressions involving parametric interfaces with type parameters are disallowed:

```verse
parametric_interface(t:type) := interface:
    Foo():t

child := class:
    pass

impl := class(child, parametric_interface(float)):
    Foo<override>():float = 42.42

# Invalid: Cannot cast to parametric interface with type parameter
X:child := impl{}
# if (X_Casted := parametric_interface(float)[X]):  # ERROR 3502
#     # Parametric interface casts not allowed
```

However, specialized (non-parametric) interfaces derived from parametric interfaces can be used in casts:

```verse
parametric_interface(t:type) := interface:
    Foo():t

# Specialized interface fixes the type parameter
specialized_interface := interface(parametric_interface(float)){}

impl := class(specialized_interface):
    Foo<override>():float = 42.42

# Valid: specialized_interface is no longer parametric
X := impl{}
if (X_Casted := specialized_interface[X]):
    X_Casted.Foo()  # Works!
```

**Valid casting scenarios:**

While casts between different parametric instantiations fail, the following patterns work:

1. **Non-parametric class hierarchies** support normal casting:

```verse
base := class<castable>:
    ID:int

child := class(base):
    Name:string

B:base = child{ID := 1, Name := "Test"}
if (C := child[B]):
    # Valid: Normal class hierarchy cast
    Print(C.Name)
```

2. **Fixed parametric instantiations** where the type parameter is locked in the subclass:

```verse
parametric_base(t:type) := class:
    Property:t

# Child fixes the type parameter to int
int_child := class(parametric_base(int)):
    GetProperty():int = Property

Foo:parametric_base(int) = int_child{Property := 42}
if (FooChild := int_child[Foo]):
    # Valid: Type parameter is fixed to int in both
    FooChild.Property = 42
```

3. **Empty parametric classes** (parametric syntax but no actual type parameters):

```verse
empty_parametric() := class(base):
    Foo():int = 100

X:base = empty_parametric(){Property := 42}
if (X_Casted := empty_parametric()[X]):
    # Valid: No type parameters involved
    X_Casted.Foo()
```

**Future behavior:**

The current implementation treats parametric instantiation casts as compile-time errors (error 3502). However, this behavior may evolve in future versions to allow the cast syntax at compile time while failing at runtime if the types don't match. This would enable more dynamic patterns while maintaining type safety.

**Cannot be `<persistable>` directly:**

While you can define parametric classes, making them persistable requires special consideration for how the type parameter is serialized. Specific instantiations with persistable types may work depending on the implementation.

### Recursive Parametric Types

Parametric classes can reference themselves in their field types, enabling recursive generic data structures like linked lists, trees, and graphs. However, Verse imposes specific restrictions on how recursion can occur.

The most common form of recursive parametric type is when a class references itself with **the same type parameter**:

```verse
# Linked list node
list_node(t:type) := class:
    Value:t
    Next:?list_node(t)  # Same type parameter 't'

# Usage
IntList := list_node(int){
    Value := 1
    Next := option{list_node(int){
        Value := 2
        Next := false
    }}
}

# Helper to create lists
Cons(Head:t, Tail:?list_node(t) where t:type):list_node(t) =
    list_node(t){Value := Head, Next := Tail}

# Sum a linked list
SumList(List:?list_node(int)):int =
    if (Head := List?):
        Head.Value + SumList(Head.Next)
    else:
        0
```

Binary trees work similarly:

```verse
tree_node(t:type) := class:
    Value:t
    var Left:?tree_node(t) = false   # Same parameter
    var Right:?tree_node(t) = false  # Same parameter

# Create a tree
Root := tree_node(int){
    Value := 5
    Left := option{tree_node(int){Value := 3}}
    Right := option{tree_node(int){Value := 7}}
}
```

**Why this works:** Each instantiation creates a complete, consistent type. `list_node(int)` always contains `int` values and references other `list_node(int)` nodes. The type system can verify this recursion is well-formed.

**Disallowed: Direct Type Alias Recursion**

You cannot define a parametric type that directly aliases to a structural type containing itself:

```verse
# Invalid: Direct array recursion
# t(u:type) := []t(u)  # ERROR 3502

# Invalid: Direct map recursion
# t(u:type) := [int]t(u)  # ERROR 3502

# Invalid: Direct optional recursion
# t(u:type) := ?t(u)  # ERROR 3502

# Invalid: Direct function recursion
# t(u:type) := u->t(u)  # ERROR 3502
# t(u:type) := t(u)->u  # ERROR 3502
```

These fail because they create infinite type expansion—the compiler cannot determine the actual structure of the type.

**Valid alternative:** Wrap in a class:

```verse
# Valid: Indirect recursion through class
nested_list(t:type) := class:
    Items:[]nested_list(t)  # OK - wrapped in class

Tree := nested_list(int){
    Items := array{
        nested_list(int){Items := array{}},
        nested_list(int){Items := array{}}
    }
}
```

**Disallowed: Polymorphic Recursion**

Polymorphic recursion occurs when a parametric type references itself with a **different type argument**:

```verse
# Invalid: Type parameter changes
# my_type(t:type) := class:
#     Next:my_type(?t)  # ERROR 3509 - ?t is different from t

# Invalid: Alternating type parameters
# bi_list(t:type, u:type) := class:
#     Value:t
#     Next:?bi_list(u, t)  # ERROR 3509 - parameters swapped
```

**Why this is disallowed:** Polymorphic recursion makes type inference undecidable and can create infinitely complex types. When you instantiate `my_type(int)`, it would need `my_type(?int)`, which needs `my_type(??int)`, and so on forever.

**Current limitation:** While polymorphic recursion is theoretically sound in some type systems, Verse currently does not support it to keep type checking tractable.

**Disallowed: Mutual Recursion**

Mutual recursion between multiple parametric types is not supported:

```verse
# Invalid: Mutual recursion
# t1(t:type) := class:
#     Next:?t2(t)  # References t2
#
# t2(t:type) := class:
#     Next:?t1(t)  # References t1
#
# # ERROR 3509, 3509
```

**Why this is disallowed:** Similar to polymorphic recursion, mutual recursion complicates type inference and can create circular dependencies that are difficult for the compiler to resolve.

**Workaround:** Combine into a single type:

```verse
# Valid: Single type with multiple cases
node_type := enum:
    TypeA
    TypeB

combined_node(t:type) := class:
    Type:node_type
    Value:t
    Next:?combined_node(t)
```

**Disallowed: Inheritance Recursion**

You cannot inherit from a type variable or create recursive inheritance through parametric types:

```verse
# Invalid: Inheriting from parametric self
# t(u:type) := class(t(u)){}  # ERROR 3590

# Invalid: Inheriting from type variable
# inherits_from_variable(t:type) := class(t){}  # ERROR 3590
```

**Why this is disallowed:** Inheritance requires knowing the parent's structure, but with parametric recursion, this structure would be self-referential before being defined.

<!-- #### Practical Patterns for Recursive Types

**Linked list with helper functions:**

```verse
list(t:type) := ?list_node(t)

list_node(t:type) := class:
    Head:t
    Tail:list(t)

Nil(t:type):list(t) = false

Cons(Head:t, Tail:list(t) where t:type):list(t) =
    option{list_node(t){Head := Head, Tail := Tail}}

Length(L:list(t) where t:type):int =
    if (Node := L?):
        1 + Length(Node.Tail)
    else:
        0

# Usage
Numbers := Cons(1, Cons(2, Cons(3, Nil(int))))
Length(Numbers)  # Returns 3
```

**Binary search tree:**

```verse
bst_node(t:subtype(comparable)) := class:
    Value:t
    var Left:?bst_node(t) = false
    var Right:?bst_node(t) = false

    Insert(NewValue:t):void =
        if (NewValue < Value):
            if (LeftChild := Left?):
                LeftChild.Insert(NewValue)
            else:
                set Left = option{bst_node(t){Value := NewValue}}
        else:
            if (RightChild := Right?):
                RightChild.Insert(NewValue)
            else:
                set Right = option{bst_node(t){Value := NewValue}}

    Contains(SearchValue:t)<decides>:void =
        if (SearchValue = Value):
            return
        if (SearchValue < Value, LeftChild := Left?):
            LeftChild.Contains[SearchValue]
        if (SearchValue > Value, RightChild := Right?):
            RightChild.Contains[SearchValue]
        false
```

**Graph with adjacency list:**

```verse
graph_node(t:type) := class<unique>:
    Data:t
    var Neighbors:[]graph_node(t) = array{}

    AddEdge(Target:graph_node(t)):void =
        set Neighbors = Neighbors + array{Target}

# Unique ensures identity-based equality
Node1 := graph_node(string){Data := "A"}
Node2 := graph_node(string){Data := "B"}
Node1.AddEdge(Node2)  # Create edge A -> B
```
-->

### Parametric Interfaces

While parametric classes get most of the attention, interfaces can also be parametric, enabling abstract contracts that work with any type:

```verse
# Generic equality interface
equivalence(t:type, u:type) := interface:
    Equal(Left:t, Right:u)<transacts><decides>:t

# Generic collection interface
collection(t:type) := interface:
    Add(Item:t):void
    Remove(Item:t)<decides>:void
    Contains(Item:t):logic
```

Classes implement parametric interfaces by providing concrete types for the parameters:

```verse
equivalence(t:type, u:type) := interface:
    Equal(Left:t, Right:u)<transacts><decides>:t

# Implement with specific types
int_equivalence := class(equivalence(int, comparable)):
    Equal<override>(Left:int, Right:comparable)<transacts><decides>:int =
        Left = Right

# Or with type parameters matching the class
comparable_equivalence(t:subtype(comparable)) := class(equivalence(t, comparable)):
    Equal<override>(Left:t, Right:comparable)<transacts><decides>:t =
        Left = Right

# Usage
Eq := comparable_equivalence(int){}
Eq.Equal[5, 5]  # Succeeds
```

Parametric interfaces follow the same variance rules as parametric classes:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Covariant interface - returns t
producer_interface(t:type) := interface:
    Produce():t

player_producer := class(producer_interface(player)):
    Produce<override>():player = player{ID := 1, Name := "Test"}

# Covariant subtyping works
EntityProducer:producer_interface(entity) = player_producer{}
```

You can create specialized (non-parametric) interfaces from parametric ones:

```verse
generic_handler(t:type) := interface:
    Handle(Item:t):void

# Specialize to a concrete type
int_handler := interface(generic_handler(int)):
    # Inherits Handle(Item:int):void
    # Can add more methods here

int_processor := class(int_handler):
    Handle<override>(Item:int):void =
        Print("Handling: {Item}")

# Can use in casts now (specialized interfaces are non-parametric)
Base := int_processor{}
if (Handler := int_handler[Base]):
    Handler.Handle(42)
```

#### Multiple Type Parameters

Interfaces can have multiple type parameters with independent variance:

```verse
converter_interface(input:type, output:type) := interface:
    Convert(In:input):output
    # input is contravariant, output is covariant

entity := class:
    ID:int

player := class(entity):
    Name:string

# Implement with specific types
player_to_entity := class(converter_interface(player, entity)):
    Convert<override>(In:player):entity = entity{ID := In.ID}

# Variance allows flexible usage
C:converter_interface(entity, entity) = player_to_entity{}
```

### Advanced Parametric Types

#### First-Class Parametrics

Parametric type definitions can be used as first-class values, allowing dynamic type application:

```verse
# Parametric class
container(t:type) := class:
    Value:t

# Store parametric type as value
TypeConstructor := container

# Apply type argument dynamically
IntContainer := TypeConstructor(int)

# Construct instance
Instance := IntContainer{Value := 42}
Instance.Value = 42  # Success
```

This enables powerful patterns for generic factories and type-driven programming:

```verse
# Factory that works with any parametric container
CreateContainer(TypeCtor:type, Value:t where t:type) :=
    TypeCtor(t){Value := Value}

# Can work with different container types
container1(t:type) := class:
    Value:t

container2(t:type) := class:
    Data:t

X := CreateContainer(container1, 42)  # container1(int)
Y := CreateContainer(container2, "hello")  # container2(string)
```

**Conditional type selection:**

```verse
my_class(t:type) := class:
    Property:t

base := class:
    ID:int

derived := class(base):
    Name:string

# Choose type at runtime based on condition
ChosenType := if (SomeCondition?):
    base
else:
    derived

# Use the chosen type
Container := my_class(ChosenType){Property := ChosenType{}}
```

#### Effects

Parametric types can have effect specifiers that apply to all instantiations:

```verse
# Parametric class with effects
async_container(t:type) := class<computes>:
    Property:t

# All instantiations inherit the effect
X:async_container(int) = async_container(int){Property := 1}  # <computes> effect

# Multiple effects
transactional_container(t:type) := class<transacts><suspends>:
    Property:t

# Constructor inherits effects
Y:transactional_container(int) = transactional_container(int){Property := 2}
```

**Allowed effects:**

- `<computes>` - Allows non-terminating computation
- `<decides>` - Can fail
- `<suspends>` - Can suspend execution
- `<transacts>` - Participates in transactions
- `<reads>` - Reads mutable state
- `<writes>` - Writes mutable state
- `<allocates>` - Allocates resources

**Not allowed:**

- `<converges>` - Would conflict with parametric instantiation (error 3565)

**Effect propagation:**

```verse
# Effect on parametric type propagates to constructor
my_type(t:type) := class<computes>:
    Property:t

# This requires <computes> in the context
CreateInstance():my_type(int)<computes> =
    my_type(int){Property := 1}
```

The effect becomes part of the type's contract—all code constructing or working with instances must account for these effects.

#### Natives

Parametric types can be marked `<native>` for implementation in the underlying platform:

```verse
# Native parametric type
native_container<native>(t:type) := class:
    Get():t
    Set(Value:t):void

# Native implementation in C++/Blueprint
# Provides efficient platform-specific implementation
```

Native parametric types enable:

- Performance-critical generic containers
- Platform-specific optimizations
- Integration with existing native code

**Usage:**

```verse
# Use like any parametric type
Container := native_container(int){}
Container.Set(42)
Value := Container.Get()  # Returns 42
```

Native parametric types must be implemented by the runtime, but from Verse's perspective they work identically to regular parametric types.

#### Aliases

You can create type aliases that simplify complex parametric type expressions:

```verse
# Alias for map type
string_map(t:type) := [string]t

# Use the alias
PlayerScores:string_map(int) = map{
    "Alice" => 100,
    "Bob" => 95
}

# Alias for optional array
optional_array(t:type) := []?t

# Simplifies type signatures
FilterValid(Items:optional_array(int)):[]int =
    for (Item : Items; Value := Item?):
        Value
```

**Composing parametric aliases:**

```verse
# Nested parametric aliases
map_alias(k:type, v:type) := [k]v
array_alias(t:type) := []t

# Compose them
nested(t:type) := array_alias(map_alias(string, t))

# Usage: []([string]t)
Data:nested(int) = array{
    map{"a" => 1, "b" => 2},
    map{"c" => 3}
}
```

**Structural type aliases:**

```verse
# Function type aliases
transformer(input:type, output:type) := input -> output
predicate(t:type) := t -> logic

# Tuple type aliases
pair(t:type, u:type) := tuple(t, u)
triple(t:type) := tuple(t, t, t)

# Use in signatures
ApplyTransform(T:transformer(int, string), Value:int):string =
    T(Value)

CheckCondition(P:predicate(int), Value:int):logic =
    P(Value)
```

Type aliases improve readability and maintainability for complex generic types.

#### Advanced Type Constraints

Beyond basic `subtype` constraints, parametric types support specialized constraints:

**Subtype constraints:**

```verse
# Constrain to subtype of a class
bounded_container(t:subtype(entity)) := class:
    Value:t

    GetID():int = Value.ID  # Can access entity members

# Valid: player is subtype of entity
PlayerContainer := bounded_container(player){}

# Invalid: int is not subtype of entity
# IntContainer := bounded_container(int){}  # Type error
```

**Castable subtype constraints:**

```verse
# Requires castable subtype
dynamic_handler(t:castable_subtype(component)) := class:
    Handle(Item:component):void =
        if (Typed := t[Item]):
            # Typed has the specific subtype
            ProcessTyped(Typed)
```

**Multiple constraints:**

```verse
# Combine multiple requirements
sorted_unique(t:type where t:subtype(comparable)) := class<unique>:
    var Items:[]t = array{}

    Add(Item:t):void =
        # Can use comparison because t:subtype(comparable)
        if (not Contains(Item)):
            set Items = Sort(Items + array{Item})

    Contains(Item:t):logic =
        for (Element : Items):
            if (Element = Item):
                return true
        false
```

**Constraint propagation:**

```verse
# Constraints propagate through function calls
wrapper(t:subtype(comparable)) := class:
    Data:t

Process(W:wrapper(t) where t:subtype(comparable)):logic =
    # Compiler knows t is comparable here
    W.Data = W.Data
```

When defining parametric functions that work with parametric types, the constraints must be compatible:

```verse
base_class := class:
    ID:int

constrained(t:subtype(base_class)) := class:
    Data:t

# Valid: Constraint matches
UseConstrained(C:constrained(t) where t:subtype(base_class)):int =
    C.Data.ID

# Invalid: Missing or incompatible constraint
# UseConstrained(C:constrained(t) where t:type):int =  # ERROR 3509
#     C.Data.ID
```

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

### Concrete

The `<concrete>` specifier enforces that all fields have default values, allowing construction with an empty archetype:

<!--NoCompile-->
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

### Unique

The `<unique>` specifier creates classes and interfaces with reference semantics where each instance has a distinct identity. When a class or interface is marked as `<unique>`, instances become comparable using the equality operators (= and <>), with equality based on object identity rather than field values.

Classes marked with `<unique>` compare by identity, not by value:

<!--verse
vector3:=struct{X:float,Y:float,Z:float}
entity := class<unique>:
   Name : string
   Position : vector3
F()<decides>:void={
E1 := entity{Name := "Guard", Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}}
E2 := entity{Name := "Guard", Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}}
E3 := E1

E1 = E2  # Fails - different instances despite identical field values
E1 = E3  # Succeeds - same instance
}
<#
-->
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
<!--verse
#>
-->

Without `<unique>`, class instances cannot be compared for equality at all—the language prevents meaningless comparisons. With `<unique>`, you gain the ability to use instances as map keys, store them in sets, and perform identity checks, essential for tracking specific objects throughout their lifetime.

#### Interfaces

Interfaces can also be marked with `<unique>`, which makes all instances of classes implementing that interface comparable by identity:

```verse
component := interface<unique>:
    Update():void
    Render():void

physics_component := class(component):
    Update<override>():void = {}
    Render<override>():void = {}

# Instances are comparable because component is unique
P1 := physics_component{}
P2 := physics_component{}

P1 <> P2  # true - different instances
P1 = P1   # true - same instance
```

The `<unique>` property propagates through interface inheritance. If a parent interface is marked `<unique>`, all child interfaces and classes implementing those interfaces automatically become comparable:

```verse
base_component := interface<unique>:
    Update():void

# Child interface inherits <unique> from parent
advanced_component := interface(base_component):
    AdvancedUpdate():void

# Classes implementing any interface in the hierarchy become comparable
player_component := class(advanced_component):
    Update<override>():void = {}
    AdvancedUpdate<override>():void = {}

C1 := player_component{}
C2 := player_component{}
C1 <> C2  # true - comparable due to base_component being unique
```

When a class implements multiple interfaces, comparability is determined by whether ANY of the inherited interfaces is `<unique>`:

```verse
updateable := interface:  # Not unique
    Update():void

renderable := interface<unique>:  # Unique
    Render():void

game_object := class(updateable, renderable):
    Update<override>():void = {}
    Render<override>():void = {}

# game_object is comparable because renderable is unique
G1 := game_object{}
G2 := game_object{}
G1 <> G2  # true - comparable due to renderable interface
```

Even if most interfaces are non-unique, a single `<unique>` interface in the hierarchy makes the entire class comparable.

#### Unique in Default Values

When a `<unique>` class appears in a field's default value, each containing object receives its own distinct instance. This guarantee applies even when the unique class is nested within complex parametric types:

```verse
token := class<unique>:
    ID:int = 0

container := class:
    MyToken:token = token{}

C1 := container{}
C2 := container{}
C1.MyToken <> C2.MyToken  # true - each container has its own unique token
```

This behavior extends to `<unique>` instances within arrays, optionals, tuples, and maps:

```verse
item := class<unique>{}

# Each class instantiation creates fresh unique instances in default values
with_array := class:
    Items:[]item = array{item{}}

with_optional := class:
    MaybeItem:?item = option{item{}}

with_map := class:
    ItemMap:[int]item = map{0 => item{}}

A := with_array{}
B := with_array{}
A.Items[0] <> B.Items[0]  # true - different unique instances

C := with_optional{}
D := with_optional{}
if (ItemC := C.MaybeItem?, ItemD := D.MaybeItem?):
    ItemC <> ItemD  # true - different unique instances
```

The same principle applies when parametric classes contain unique instances in their fields:

```verse
entity := class<unique>{}

registry(t:type) := class:
    DefaultEntity:entity = entity{}
    Data:t

R1 := registry(int){}
R2 := registry(int){}
R1.DefaultEntity <> R2.DefaultEntity  # true

R3 := registry(string){}
R3.DefaultEntity <> R1.DefaultEntity  # true - even across different type parameters
```

This guarantee ensures that identity-based operations remain reliable. If you store objects in maps keyed by unique instances, or maintain sets of unique objects, each container genuinely owns distinct instances rather than sharing references. The language prevents subtle bugs where multiple objects might unexpectedly share the same identity.

#### Overload Resolution

Types marked with `<unique>` are subtypes of the built-in `comparable` type. This can create overload ambiguity:

```verse
# Valid: non-unique interface doesn't conflict with comparable
regular_interface := interface:
    Method():void

Process(A:comparable, B:comparable):void = {}
Process(A:regular_interface, B:regular_interface):void = {}  # OK - no conflict

# Invalid: unique interface conflicts with comparable
unique_interface := interface<unique>:
    Method():void

Handle(A:comparable, B:comparable):void = {}
Handle(A:unique_interface, B:unique_interface):void = {}  # ERROR - ambiguous!
```

Since `unique_interface` is a subtype of `comparable`, both overloads could match when called with `unique_interface` arguments, causing a compilation error. When designing overloaded functions, be aware that `<unique>` types participate in the `comparable` type hierarchy.

#### Use Cases

The `<unique>` specifier is ideal for:

**Game Entities:** Where each entity in the world must be distinguishable regardless of current state

```verse
entity := class<unique>:
    var Health:int = 100
    var Position:vector3

# Can track specific entities in collections
var ActiveEntities:[entity]logic = map{}
```

**Component Interfaces:** Where you need identity-based equality for interface types

```verse
component := interface<unique>:
    Owner:entity
    Update():void

# Can use interface references as map keys
var ComponentRegistry:[component]string = map{}
```

**Session Objects:** Where identity matters more than current property values

```verse
player_session := class<unique>:
    PlayerID:string
    var ConnectionTime:float

# Track specific sessions
var ActiveSessions:[player_session]connection_info = map{}
```

**Resource Handles:** Where you need to track specific instances rather than equivalent values

```verse
texture_handle := class<unique>:
    ResourceID:int
    FilePath:string

# Manage resource lifecycle
var LoadedTextures:[texture_handle]gpu_resource = map{}
```

The `<unique>` specifier enables these patterns by providing identity-based equality semantics, making it possible to use instances as map keys, maintain sets of unique objects, and distinguish between different instances even when their data is identical.

### Abstract

The `<abstract>` specifier marks classes that cannot be instantiated directly — they exist solely as base classes  for inheritance. When you declare a class with `<abstract>`, you're creating a template that defines structure and behavior for subclasses to inherit and implement.

Abstract classes serve as architectural foundations in a type hierarchy. They define contracts through abstract methods that subclasses must implement, while potentially providing concrete methods and fields that subclasses inherit. This creates a powerful pattern for code reuse and polymorphic behavior.

```verse
  vehicle := class<abstract>:
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

### Castable

The `<castable>` specifier enables runtime type checking and safe downcasting for classes. When a class is marked with `<castable>`, you can use dynamic type tests and casts to determine if an object is an instance of that class or its subclasses at runtime.

Without `<castable>`, Verse's type system operates purely at compile time. The `<castable>` specifier adds runtime type information, allowing code to inspect and react to actual object types during execution. This bridges the gap between static type safety and dynamic polymorphism.

Verse provides two forms of type casting: **fallible casts** (which can fail at runtime) and **infallible casts** (which are verified at compile time).

**Fallible casts** use bracket syntax `Type[Value]` and return an optional result. These are runtime checks that succeed only if the value is actually an instance of the target type:

```verse
component := class<abstract><castable>:
    Name:string

physics_component := class(component):
    Name<override>:string = "Physics"
    Velocity:vector3

render_component := class(component):
    Name<override>:string = "Render"
    Material:string

ProcessComponent(Comp:component):void =
    # Attempt to cast to physics_component
    if (PhysicsComp := physics_component[Comp]):
        # Cast succeeded - PhysicsComp has type physics_component
        Print("Physics component with velocity: {PhysicsComp.Velocity}")
    else if (RenderComp := render_component[Comp]):
        # Cast succeeded - RenderComp has type render_component
        Print("Render component with material: {RenderComp.Material}")
    else:
        # Neither cast succeeded
        Print("Unknown component type")
```

The cast expression has the `<decides>` effect—it fails if the object is not an instance of the target type. This integrates naturally with Verse's failure handling:

```verse
GetPhysicsComponent(Comp:component)<decides>:physics_component =
    # Returns physics_component or fails
    physics_component[Comp]

# Use with failure handling
if (Physics := GetPhysicsComponent[SomeComponent]):
    UpdatePhysics(Physics)
```

**Infallible casts** use parenthesis syntax `Type(Value)` and only work when the compiler can verify the cast is safe—that is, when the value type is a subtype of the target type:

```verse
base := class:
    ID:int

derived := class(base):
    Name:string

GetDerived():derived = derived{ID := 1, Name := "Test"}

# Infallible upcast - derived is a subtype of base
BaseRef:base = base(GetDerived())  # Always safe
```

Attempting an infallible downcast (from supertype to subtype) is a compile error, as the compiler cannot guarantee safety:

```verse
# This would be an error:
# DerivedRef := derived(BaseRef)  # ERROR: not a subtype relationship
```

#### Castable and Inheritance

The `<castable>` property is inherited by all subclasses. When you mark a class as `<castable>`, every class that inherits from it automatically becomes castable as well:

```verse
base := class<castable>:
    Value:int

child := class(base):
    # Automatically castable - inherits from castable base
    Name:string

grandchild := class(child):
    # Also automatically castable
    Extra:string

# Can cast through the hierarchy
ProcessBase(Instance:base):void =
    if (AsChild := child[Instance]):
        Print("It's a child: {AsChild.Name}")
    if (AsGrandchild := grandchild[Instance]):
        Print("It's a grandchild: {AsGrandchild.Extra}")
```

**Important constraint:** Parametric types cannot be `<castable>`. This prevents type erasure issues at runtime:

```verse
# Valid: non-parametric castable class
valid_castable := class<castable>:
    Data:int

# Invalid: parametric classes cannot be castable
# invalid_castable(t:type) := class<castable>:  # ERROR
#     Data:t
```

However, a non-parametric class can be `<castable>` even if it inherits from or contains parametric types:

```verse
container(t:type) := class:
    Value:t

# Valid: concrete instantiation of parametric type
int_container := class<castable>(container(int)):
    Extra:string
```

#### Using castable_subtype

The `castable_subtype` type constructor works with `<castable>` classes to enable type-safe filtered queries and dynamic type dispatch:

<!--NoCompile-->
```verse
  component<public> := class<abstract><unique><castable>:
      Parent<public>:entity

  entity<public> := class<concrete><unique><transacts><castable>:
      FindDescendantEntities(entity_type:castable_subtype(entity)):generator(entity_type)
```

When you call `FindDescendantEntities(player)`, the function returns only entities that are actually player instances or subclasses thereof, verified at runtime through the castable mechanism. The type parameter ensures type safety—the returned values have the specific subtype you requested.

#### Permanence of Castable

Once a class is published with `<castable>`, this decision becomes permanent. You cannot add or remove the `<castable>` specifier after publication because doing so would break existing code that relies on runtime type checking. Code that performs casts would suddenly fail or behave incorrectly if the castable property changed.

This permanence is enforced through the versioning system—attempting to change the `<castable>` status of a published class will result in a compatibility error.

### Final

The `<final>` specifier prevents inheritance, creating a terminal point in a class hierarchy. When you mark a class  with `<final>`, no other class can inherit from it. For methods, `<final>` prevents overriding in subclasses, locking  the implementation at that level of the hierarchy.

Classes marked with `<final>` serve as concrete implementations that cannot be extended. This is particularly important for persistable classes, which require `<final>` to ensure their structure remains stable for serialization:

<!--verse
player_stats:=struct<persistable>{}
-->
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
      GetName():string = "Entity"

  game_object := class(base_entity):
      GetName<override><final>():string = "GameObject"
      # Any subclass of game_object cannot override GetName
```

The related `<final_super>` specifier marks classes as terminal base classes — they can be inherited from but their subclasses cannot be further extended.  `<final_super_base>` marks a class as the ultimate root of a restricted inheritance tree. Classes with this   specifier can be inherited from, but their subclasses automatically become final — they cannot be further  extended. This creates a two-level inheritance limit starting from the base:

<!-- TODO  DOES NOT WORK -->

```verse
component<native><public> := class<abstract><unique><castable><final_super_base>:
      Parent:entity

  # Can inherit from component (first level)

physics_component := class(component):  # implicitly final_super
      Mass:float = 1.0

 # Cannot inherit from physics_component - it's implicitly final

# gravity_component := class(physics_component): # COMPILE ERROR
```

So, `<final_super>` marks a class that inherits from a `<final_super_base>` class, explicitly declaring it as the final inheritance point. While classes inheriting from `<final_super_base>` are implicitly final, using `<final_super>`  makes this finality explicit and self-documenting.
<!-- :

TODO REVISIT

```verse
  # Explicitly marking as final_super (though implicitly final anyway)
  name_component := class<final_super>(component):
      Name:string = ""

  copter_camera_component := class<final_super>(copter_camera_component_director_version):
      # Terminal implementation
```
-->

This pattern is particularly valuable in component architectures where you want a base component interface that  various concrete components implement, but don't want those implementations to spawn their own inheritance  subtrees. The base class defines the contract, immediate subclasses provide  implementations, and inheritance stops  there — clean, controlled, and predictable.

This design enforces architectural discipline, preventing the "inheritance explosion" that can occur when every class becomes a potential base for further specialization. By limiting inheritance depth, these specifiers promote composition over deep inheritance, leading to more maintainable and understandable code structures.

### Persistable

The `<persistable>` specifier marks types that can be saved and restored across game sessions, enabling permanent storage of player progress, achievements, and game state. This specifier transforms ephemeral gameplay into  lasting progression, creating the foundation for meaningful player investment.

Persistence  works through module-scoped `weak_map(player, t)` variables, where `t` is any persistable type.  These special maps automatically synchronize with backend storage — when players join, their data loads; when they leave or data changes, it saves. The system handles all serialization, network transfer, and storage management transparently.

<!--verse
player:=string
-->
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

Interfaces define contracts that classes can implement, specifying both the data and behavior that implementing classes must provide. Unlike many traditional languages where interfaces only declare method signatures, Verse interfaces are rich contracts that can include fields, default method implementations, and even custom accessor logic.

An interface can declare method signatures, provide default implementations, and define data members:

```verse
damageable := interface:
    # Abstract method - implementing classes must provide
    TakeDamage(Amount:int):void

    # Method with default implementation
    GetHealth():int = 100

    # Data member - implementing classes inherit or must provide
    MaxHealth:int = 100

    IsAlive():logic = GetHealth() > 0

healable := interface:
    Heal(Amount:int):void
    GetMaxHealth():int
```

Interfaces establish contracts that can be purely abstract (method signatures only), partially concrete (some default implementations), or fully implemented (complete behavior that classes inherit). Any class implementing an interface must provide implementations for abstract methods, but inherits concrete implementations and default field values.

### Implementing Interfaces

Classes implement interfaces by inheriting from them and providing concrete implementations where required:

<!--verse
damageable:=interface{}
healable:=interface{}
-->
```verse
character := class(damageable, healable):
    var Health : int = 100
    MaxHealth : int = 100

    TakeDamage<override>(Amount:int):void =
        set Health = Max(0, Health - Amount)

    GetHealth<override>():int = Health

    Heal<override>(Amount:int):void =
        set Health = Min(MaxHealth, Health + Amount)
```

A class can implement multiple interfaces, effectively achieving multiple inheritance of both behavior contracts and data specifications. This provides more flexibility than single class inheritance while maintaining type safety.

### Interface Fields

Interfaces can declare data members that implementing classes must provide or inherit. These fields can be either immutable or mutable, and may include default values:

```verse
# Interface with various field types
entity_properties := interface:
    # Immutable field with default - classes inherit this value
    EntityID:int = 0

    # Mutable field with default
    var Health:float = 100.0

    # Field without default - classes must provide a value
    Name:string

    # Field that can be overridden
    MaxHealth:float = 100.0

player_entity := class(entity_properties):
    # Must provide Name (no default in interface)
    Name<override>:string = "Player"

    # Can override to change default
    MaxHealth<override>:float = 150.0

    # Inherits EntityID and Health with their defaults
```

When an interface field has a default value, implementing classes automatically inherit that default unless they override it. Fields without defaults must be provided either by the implementing class or through construction parameters.

### Default Implementations

Interfaces can provide complete method implementations that implementing classes inherit automatically:

```verse
animated := interface:
    var CurrentFrame:int = 0
    TotalFrames:int = 10

    # Concrete implementation provided by interface
    NextFrame():void =
        set CurrentFrame = (CurrentFrame + 1) % TotalFrames

    # Can access interface fields
    ProgressPercent():float =
        CurrentFrame / TotalFrames

sprite := class(animated):
    TotalFrames<override>:int = 20
    # Automatically inherits NextFrame and ProgressPercent implementations
```

Classes inherit these implementations without modification, allowing interfaces to provide reusable behavior. Implementing classes can override these methods if they need specialized behavior, but the interface provides a working default.

### Overriding Members

Classes can override both fields and methods from interfaces to provide specialized implementations:

```verse
base_stats := interface:
    BaseHealth:int = 100

    CalculateFinalHealth():int = BaseHealth

warrior := class(base_stats):
    # Override field with different default
    BaseHealth<override>:int = 150

    # Override method for specialized calculation
    CalculateFinalHealth<override>():int =
        BaseHealth * 2  # Warriors get double health

mage := class(base_stats):
    BaseHealth<override>:int = 75

    CalculateFinalHealth<override>():int =
        BaseHealth + MagicBonus

    MagicBonus:int = 25
```

Field overrides can provide different default values or specialize to subtypes. Method overrides replace the interface's implementation entirely. All overrides must maintain type compatibility—fields can only be overridden with subtypes, and method signatures must match exactly.

### Multiple Interfaces with Sharing

When a class implements multiple interfaces that declare fields or methods with the same name, you must use qualified names to disambiguate:

```verse
magical := interface:
    Power:int = 50
    GetPowerLevel():int = Power

physical := interface:
    Power:int = 75
    GetPowerLevel():int = Power * 2

hybrid := class(magical, physical):

UseHybridPowers():void =
    MagicPower := (magical:)Power         # Access magical's Power
    PhysicalPower := (physical:)Power     # Access physical's Power

    MagicLevel := (magical:)GetPowerLevel()
    PhysicalLevel := (physical:)GetPowerLevel()
```

The qualified name syntax `(InterfaceName:)MemberName` specifies which interface's member you're accessing. Each interface maintains its own instance of the field, allowing the class to support both contracts simultaneously without conflict.

### Interface Hierarchies

Interfaces can extend other interfaces, creating hierarchies of contracts that combine data and behavior requirements:

<!--NoCompile-->
```verse
combatant := interface(damageable, healable):
    var AttackPower:int = 10

    Attack(Target:damageable):void =
        Target.TakeDamage(AttackPower)

    GetAttackPower():int = AttackPower

boss := interface(combatant):
    Phase:int = 1

    UseSpecialAbility():void
    GetPhase():int = Phase
```

A class implementing `boss` inherits all fields and methods from the entire hierarchy—`boss`, `combatant`, `damageable`, and `healable`. Diamond inheritance (where an interface is inherited through multiple paths) is fully supported, with fields properly merged so each field exists only once in the implementing class.

**Important:** A class cannot directly inherit the same interface multiple times (e.g., `class(interface1, interface1)` is an error), but can inherit it indirectly through diamond inheritance. This means `class(interface2, interface3)` is valid even if both `interface2` and `interface3` inherit from the same base interface.

### Fields with Accessors

Interfaces can define fields with custom getter and setter logic, encapsulating complex behavior behind simple field access syntax:

```verse
subscribable_property := interface:
    # External field with accessor methods
    var Value<getter(GetValue)><setter(SetValue)>:int = external{}

    # Internal storage
    var Storage:int = 100

    # Getter adds computation
    GetValue(:accessor):int = Storage + 10

    # Setter adds validation
    SetValue(:accessor, NewValue:int):void =
        if (NewValue >= 0):
            set Storage = NewValue

tracked_value := class(subscribable_property):

UseTrackedValue():void =
    Object := tracked_value{}

    # Uses getter - returns 110 (Storage + 10)
    Current := Object.Value

    # Uses setter - validates and updates Storage
    set Object.Value = 150
```

The `external{}` keyword indicates the field has no direct storage—all access goes through the accessor methods. This pattern is powerful for implementing property change notifications, validation, computed properties, and other scenarios requiring logic around field access.

**Important:** Fields with accessors defined in interfaces cannot be overridden in implementing classes. The accessor implementation is fixed by the interface.
