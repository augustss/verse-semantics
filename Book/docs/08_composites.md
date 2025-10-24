# Composite Types

Composite types allow you to create custom data structures that model the entities and concepts in your game world. Rather than working solely with primitive types like integers and strings, you can define rich, structured types that represent players, weapons, game states, and any other domain-specific concepts your game requires.

Verse provides four fundamental composite type constructors, each serving a distinct purpose in your type architecture. Classes provide object-oriented programming with inheritance and polymorphism, enabling you to model complex hierarchies of game entities. Interfaces define contracts that classes must fulfill, promoting loose coupling and enabling multiple inheritance of behavior specifications. Structs offer lightweight, value-oriented data containers perfect for simple data aggregation without the overhead of object-oriented features. Enums represent fixed sets of named values, ideal for modeling game states, item types, or any domain with a known set of alternatives.

## Classes

Classes form the backbone of object-oriented programming in Verse. A class serves as a blueprint for creating objects that share common properties and behaviors. When you define a class, you're creating a new type that bundles data (fields) with operations on that data (methods), encapsulating related functionality into a cohesive unit.

**Important:** Class definitions must occur at module scope. You cannot define a class inside another class, struct, interface, or function. Classes are top-level type definitions that establish the type system's structure:

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

This class definition establishes several important concepts. Fields without the `var` modifier are immutable after construction—once you create a character with a specific name, that name cannot change. Fields marked with `var` are mutable and can be modified after the object is created. Default values provide sensible starting points, making object construction more convenient while ensuring objects start in valid states.

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

### Methods and Behavior

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

**Important:** All methods in non-abstract classes must have implementations. Unlike interfaces (which can declare abstract methods), a concrete class method declaration without an implementation is an error:

```verse
# Valid: method with implementation
valid_class := class:
    Compute():int = 42

# Invalid: method without implementation in concrete class
# invalid_class := class:
#     Compute():int  # ERROR: needs implementation
```

### Block Clauses for Initialization

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

### The Self Identifier

Within class methods, `Self` is a special keyword that refers to the current instance of the class. Each method invocation has its own `Self` that refers to the specific object the method was called on.

#### Basic Self Usage

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

#### Instance-Specific Behavior

Each instance has its own `Self`. When you call a method on different instances, `Self` refers to the specific instance that was used:

```verse
player := class<unique>:
    ID:int

    GetSelf():player = Self

Player1 := player{ID := 1}
Player2 := player{ID := 2}

Player1.GetSelf() = Player1  # True - Self refers to Player1
Player2.GetSelf() = Player2  # True - Self refers to Player2
Player1.GetSelf() = Player2  # False - different instances
```

#### Capturing Self in Nested Archetypes

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

#### Restrictions on Self

`Self` is a reserved keyword with strict usage rules:

**Cannot use Self as an identifier (error 3514):**

Self cannot be used as a name for classes, methods, parameters, variables, or fields:

```verse
# ERROR 3514: Cannot use Self as class name
# Self := class {}

# ERROR 3514: Cannot use Self as method name
# my_class := class:
#     Self():void = {}

# ERROR 3514: Cannot use Self as parameter name
# my_class := class:
#     Process(Self:int):void = {}

# ERROR 3514: Cannot use Self as local variable
# my_class := class:
#     Method():void =
#         Self:int = 42

# ERROR 3514: Cannot use Self as field name
# my_class := class:
#     Self:int
```

**Cannot use Self as a type (error 3548, 3547):**

`Self` cannot appear in type positions:

```verse
# ERROR 3548: Cannot inherit from Self
# my_class := class(Self) {}

# ERROR 3547: Cannot use Self as parameter type
# my_class := class:
#     Compare(Other:Self):logic = true
```

While some languages allow `Self` as a type to refer to "the type of the current class," Verse does not support this pattern.

**Cannot access Self on other instances (error 3506):**

`Self` is instance-specific and cannot be accessed as a member of other instances:

```verse
# ERROR 3506: Self is not a field
# my_class := class:
#     Compare(Other:my_class):logic =
#         Other.Self  # ERROR - Self is not accessible on Other
```

**Cannot use Self in static/class-level context (error 3502):**

`Self` is only available in method bodies, not in field initializers:

```verse
# ERROR 3502: Self not available in field initializer
# my_class := class:
#     Reference:my_class = Self  # ERROR
```

Field initializers execute before the instance exists, so `Self` has no meaning in that context.

#### When to Use Self

**Use `Self` when:**
- Passing the entire instance to another function
- Returning the instance for method chaining (builder pattern)
- Explicitly clarifying that you're accessing the current instance
- Capturing the instance in nested objects or closures

**Direct field access is usually sufficient:**

Within methods, you can access fields directly without `Self`:

```verse
character := class:
    Name:string
    Level:int

    Display():void =
        # Direct access - no Self needed
        Print("Name: {Name}, Level: {Level}")

        # Explicit Self - same result
        Print("Name: {Self.Name}, Level: {Self.Level}")
```

Both forms work identically. Use direct access for brevity, or use `Self.` when you want to make it explicit that you're accessing instance members.

#### Self in Inherited Methods

In subclasses, `Self` refers to the derived instance, not the base instance:

```verse
base := class<unique>:
    ID:int
    GetSelf():base = Self

derived := class<unique>(base):
    Name:string

D := derived{ID := 1, Name := "Test"}
Result := D.GetSelf()
# Result is the derived instance, type is base but refers to the full derived object
```

This ensures that methods inherited from base classes correctly reference the actual instance they're called on.

### Inheritance and Subclassing

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

### The super Keyword

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

#### Calling Parent Methods with (super:)

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

**In Inheritance Chains:**

The `(super:)` syntax works correctly in multi-level inheritance hierarchies, always calling the immediate parent's implementation:

```verse
grandparent := class:
    Method():int = 1

parent := class(grandparent):
    Method<override>():int =
        (super:)Method() + 10  # Calls grandparent, returns 11

child := class(parent):
    Method<override>():int =
        (super:)Method() + 100  # Calls parent, returns 111

Instance := child{}
Result := Instance.Method()  # Returns 111
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

#### Restrictions on (super:)

The `(super:)` syntax has several important restrictions:

**Cannot Capture as Value (Error 3502):**

```verse
base := class:
    Method():void = {}

derived := class(base):
    Method<override>():void =
        # ERROR: Cannot capture (super:) as a value
        # F := (super:)Method
        {}
```

The `(super:)` is a special syntax form, not a value that can be stored or passed around.

**Must Call Same Method Name (Error 3612):**

```verse
base := class:
    Method1():void = {}
    Method2():void = {}

derived := class(base):
    Method1<override>():void =
        # ERROR: Must call Method1, not Method2
        # (super:)Method2()
        {}
```

You can only use `(super:)MethodName()` to call the parent's version of the current method being overridden.

**Cannot Use on Non-Methods (Error 3506, 3560):**

```verse
base := class:
    Value:int = 0

derived := class(base):
    # ERROR: Cannot use (super:) on fields
    # GetValue():int = (super:)Value
    {}
```

The `(super:)` syntax only works with methods, not fields or other class members.

**Cannot Use Without Parent Class (Error 3552):**

```verse
standalone := class:
    Method():void =
        # ERROR: Class has no parent
        # (super:)Method()
        {}
```

**Cannot Use in Block Clauses (Error 3560):**

```verse
base := class:
    Method():void = {}

derived := class(base):
    # ERROR: (super:) not allowed in block clauses yet
    block:
        # (super:)Method()
        {}
```

Currently, `(super:)` is restricted to method bodies and cannot be used in class block clauses.

**Cannot Use in Nested Functions:**

As documented in the Functions chapter, `(super:)` cannot be used within nested function definitions, even if those nested functions are defined within a method:

```verse
base := class:
    Method():void = {}

derived := class(base):
    Method<override>():void =
        NestedFunction():void =
            # ERROR: Cannot use (super:) in nested function
            # (super:)Method()
            {}
        NestedFunction()
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

### Constructor Effects and Behavior

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

**Important:** Constructor functions cannot use the `<suspends>` effect. Construction must complete synchronously to maintain object consistency.

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

### Named and Unnamed Parameters

Constructor functions support both named and unnamed parameters, following the same rules as regular functions:

```verse
character := class:
    Name:string
    var Health:int
    var Mana:int

# Constructor with optional named parameters
MakeCharacter<constructor>(Name:string, ?Health:int, ?Mana:int) := character:
    Name := Name
    Health := Health
    Mana := Mana

# Call with named parameters
Mage := MakeCharacter("Wizard", ?Health := 75, ?Mana := 200)
Warrior := MakeCharacter("Knight", ?Health := 150, ?Mana := 50)
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

### Parametric Constructors

Constructor functions can be parametric, accepting type parameters to create instances of parametric classes. This enables type inference where the constructor determines type parameters from the arguments passed. For comprehensive coverage of parametric classes and their constructors, see the [Parametric Classes](#parametric-classes) section.

### Blocks and Let Expressions in Construction

Both archetype expressions and constructor functions can use `block` and `let` expressions to perform computations during construction:

```verse
transform := class:
    Position:vector3
    Scale:float

# Using let for intermediate calculations
Origin := transform:
    let:
        DefaultScale := 1.0
    Position := vector3{X := 0.0, Y := 0.0, Z := 0.0}
    Scale := DefaultScale

# Using block for side effects during construction
MakeLoggingTransform<constructor>(Pos:vector3) := transform:
    block:
        Print("Creating transform at {Pos}")
    Position := Pos
    Scale := 1.0
```

**The let: Clause Keyword:**

The `let:` clause introduces temporary variable bindings that are available to subsequent field initializers. Variables defined in a `let:` block exist only during construction and cannot be accessed after the object is created:

```verse
player := class:
    Health:int
    MaxHealth:int
    HealthPercent:float

CreatePlayer(Level:int) := player:
    let:
        BaseHealth := 100 + (Level * 10)
    MaxHealth := BaseHealth
    Health := BaseHealth
    HealthPercent := 1.0  # BaseHealth is visible here
    # BaseHealth no longer exists after construction
```

**Multiple Let Blocks:**

You can use multiple `let:` blocks within a single constructor. Each block defines variables that become available to subsequent code:

<!--verse
component := class:
    X:int
    Y:int
    Z:int
-->
```verse
MakeComponent<constructor>(Base:int) := component:
    let:
        DoubledBase := Base * 2
    X := DoubledBase

    let:
        TripleX := X * 3  # Can reference previously initialized field
    Y := TripleX
    Z := X + Y
```

**Restrictions on Let Blocks:**

The `let:` clause has several important restrictions:

1. **Only one variable per line:** You cannot use comma-separated variable lists:

<!--NoCompile-->
```verse
# ERROR: Cannot use comma-separated variables
BadConstructor := my_class:
    let:
        X := 1, Y := 2  # Compile error 3552
    Field := X + Y
```

Instead, define each variable on its own line:

```verse
# OK: Each variable on separate line
GoodConstructor := my_class:
    let:
        X := 1
        Y := 2
    Field := X + Y
```

2. **Must contain bindings:** A `let:` block must define at least one variable, not just arbitrary expressions:

<!--NoCompile-->
```verse
# ERROR: let: must contain bindings
BadConstructor := my_class:
    let:
        1 + 1  # Compile error 3560 - not a binding
    Field := 2
```

3. **Only valid in construction context:** The `let:` keyword can only be used within class or struct construction:

<!--NoCompile-->
```verse
# ERROR: let: outside construction
BadFunction():void =
    let:  # Compile error 3502
        X := 1
    Print("{X}")
```

Use regular bindings or `block:` expressions in non-construction contexts.

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

## Name Shadowing and Qualification

Verse has strict rules about name shadowing to prevent ambiguity and maintain code clarity. Understanding these rules and the qualification syntax is essential for working with inheritance hierarchies, multiple interfaces, and nested modules.

### Error 3532 - Shadowing Prohibition

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

### Class-Qualified Method Names

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

### Override Qualifier Rules

When overriding methods in inheritance hierarchies, you must use the **defining class or interface** as the qualifier, not an intermediate class:

**Error 3523, 3591 - Must use defining class:**

```verse
# ERROR: Using wrong qualifier
c0 := class<abstract> { F(X:int):int }
c1 := class(c0):
    F<override>(X:int):int = X + 1
# c2 := class(c1):
#     (c1:)F<override>(X:int):int = X + 2  # ERROR 3591, 3523
#     # F was defined in c0, not c1!
```

**Correct: Use the defining class:**

```verse
c20 := class<abstract> { F(X:int):int }
c21 := class(c20):
    F<override>(X:int):int = X + 1
c22 := class(c21):
    (c20:)F<override>(X:int):int = X + 2  # OK - c20 is where F was defined

o22 := c22{}
o22.F(10)  # Returns 12
o22.(c20:)F(20)  # Returns 22 (explicit qualification)
```

The qualifier must reference the class or interface where the method was **originally declared**, not where it was previously overridden.

**With interfaces:**

```verse
# ERROR: Wrong qualifier
# i0 := interface { F(X:int):int }
# i1 := interface(i0) {}
# c := class(i1):
#     (i1:)F<override>(X:int):int = X + 2  # ERROR 3523, 3591
#     # F was defined in i0, not i1!

# CORRECT:
i0 := interface { F(X:int):int }
i1 := interface(i0) {}
c := class(i1):
    (i0:)F<override>(X:int):int = X + 2  # OK
```

### Multiple Methods with the Same Name

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

### Using `(super:)` with Qualified Names

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

### Interface Method Collisions

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

### Qualifier Restrictions and Errors

**Error 3612 - Wrong qualifier context:**

Qualifiers can only be used in appropriate contexts. You cannot use class qualifiers for local variables:

```verse
# ERROR 3612: Class qualifier not valid for local variable
# C := class:
#     f():void =
#         (C:)X:int = 0  # ERROR - wrong context
```

**Error 3506 - Unsupported qualifiers:**

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

**Error 3588 - Local variable shadowing member:**

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

### External Package Shadowing Rules

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

### Shadowing vs Assignment Error Hints

When you accidentally shadow a variable that looks like you meant to assign to it, the compiler provides helpful error messages:

```verse
# ERROR 3547, 3532: Suggests using 'set'
# c := class {
#     var A:int = 0
#     A := 1  # ERROR - looks like you meant: set A = 1
# }

# ERROR 3532: Regular shadowing
# F():void = {
#     A:int = 0
#     A := 1  # ERROR - can't redefine
# }

# ERROR 3532: Attempting to redefine immutable
# F():void = {
#     A:int = 0
#     A:int = 1  # ERROR - shadowing, not reassignment
# }
```

The error message for mutable fields specifically suggests using `set` instead of `:=`, helping catch common mistakes where shadowing was unintentional.

### Summary of Qualification Syntax

Verse supports several qualification patterns:

| Qualifier | Usage | Example |
|-----------|-------|---------|
| `(ClassName:)` | Class-specific method | `(c80:)F(X:int):int` |
| `(InterfaceName:)` | Interface member | `(i60:)F<override>(X:int):int` |
| `(ModuleName:)` | Module member | `(my_module:)Value` |
| `(EnumName:)` | Enum enumerator | `(direction:)North` |
| `(super:)` | Parent implementation | `(super:)F(X)` |

**Key rules:**

1. **Shadowing is generally prohibited** (Error 3532) at module/class scope
2. **Qualifiers allow same names** in different contexts without shadowing
3. **Overrides must use defining class** as qualifier (Error 3523, 3591)
4. **Multiple methods possible** with same name using qualifiers
5. **Interface collisions** resolved through qualification
6. **`(super:)` works** with qualified method names
7. **External packages** have relaxed shadowing (except same scope)
8. **Local shadowing members** not supported (would need `(local:)`)

Understanding these rules enables you to work with complex class hierarchies, multiple interface implementations, and nested module structures while maintaining clarity about which definition is being referenced.

## Parametric Classes

Parametric classes, also known as generic classes, allow you to define classes that work with any type. Rather than writing separate container classes for integers, strings, players, and every other type, you write one parametric class that accepts a type parameter.

### Defining Parametric Classes

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

### Type Instantiation and Identity

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

### Different Instantiations Are Different Types

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

### Variance: When Different Instantiations Are Compatible

While different instantiations of a parametric class are distinct types, Verse allows certain instantiations to be used in place of others based on **variance**. Variance determines when `parametric_class(subtype)` can be used where `parametric_class(supertype)` is expected (or vice versa).

The variance of a parametric type depends on how the type parameter is used within the class definition:

#### Covariant: Return Types Only

When a type parameter appears only in **return positions** (method return types, field types being read), the parametric class is **covariant** in that parameter. This means instantiations follow the same subtyping direction as their type arguments:

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

#### Contravariant: Parameter Types Only

When a type parameter appears only in **parameter positions** (method parameters being consumed), the parametric class is **contravariant** in that parameter. This means instantiations follow the **opposite** subtyping direction:

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

#### Invariant: Both Positions

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

#### Bivariant: Unused Type Parameter

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

#### Variance with Multiple Type Parameters

Classes with multiple type parameters have variance independently for each parameter:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Different variance for each parameter
converter(input:type, output:type) := class:
    Convert(In:input):output
    # input is contravariant (parameter position)
    # output is covariant (return position)

# input contravariance: supertype → subtype ✓
# output covariance: subtype → supertype ✓
C1:converter(player, entity) = converter(entity, player){}
# Accepts broader input (entity), returns more specific output (player)
# Can be used where we need:
#   - Input of player (entity can handle player since player is entity)
#   - Output of entity (player is an entity)

C2:converter(entity, player) = C1  # Valid!
```

#### Practical Implications

Understanding variance is crucial for designing flexible, reusable parametric types:

**Producer pattern (covariant):**
```verse
# Generators, repositories, factories - things that produce values
data_source(t:type) := class:
    Fetch():t
    GetAll():[]t

# Can use data_source(player) where data_source(entity) expected
```

**Consumer pattern (contravariant):**
```verse
# Handlers, validators, serializers - things that consume values
validator(t:type) := class:
    Validate(Item:t):logic

# Can use validator(entity) where validator(player) expected
```

**Transformer pattern (invariant):**
```verse
# Codecs, converters, updaters - things that both consume and produce
processor(t:type) := class:
    Process(Item:t):t

# Cannot substitute - must use exact type
```

#### Variance and Inheritance

Variance rules also apply when parametric types are inherited:

```verse
entity := class:
    ID:int

player := class(entity):
    Name:string

# Covariant base class
producer_base(t:type) := class<abstract>:
    Get():t

# Inheriting preserves variance
player_producer := class(producer_base(player)):
    Get<override>():player = player{ID := 1, Name := "Test"}

# Covariant subtyping works
Base:producer_base(entity) = player_producer{}  # Valid
```

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

### Constraints on Type Parameters

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

### Parametric Classes with Methods

Methods in parametric classes can use the type parameters in sophisticated ways:

```verse
stack(t:type) := class:
    var Items:[]t = array{}

    Push(Item:t):void =
        set Items = Items + array{Item}

    Pop()<decides>:t =
        Count := Items.Length
        Count > 0
        Item := Items[Count - 1]
        set Items = Slice(Items, 0, Count - 1)
        Item

    Peek()<decides>:t =
        Items.Length > 0
        Items[Items.Length - 1]

    IsEmpty():logic =
        Items.Length = 0

# Use with any type
IntStack := stack(int){}
IntStack.Push(10)
IntStack.Push(20)
if (Top := IntStack.Pop()):
    # Top is 20
```

The methods automatically work with whatever type `t` is instantiated with. When you create `stack(int)`, `Push` accepts `int`, `Pop` returns `int`, and so on.

### Parametric Constructors

Constructor functions can be parametric, enabling type inference:

```verse
container(t:type) := class:
    Value:t

# Parametric constructor
MakeContainer<constructor>(Value:t where t:type) := container(t):
    Value := Value

# Type inference determines t from the argument
IntContainer := MakeContainer(42)         # container(int)
StringContainer := MakeContainer("text")  # container(string)
```

The constructor infers the type parameter from the value passed, making usage more concise than explicitly writing `container(int){Value := 42}`.

### Restrictions on Parametric Classes

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

### Use Cases for Parametric Classes

**Collections and containers:**

```verse
dynamic_array(t:type) := class:
    var Items:[]t = array{}
    var Capacity:int = 16

    Add(Item:t):void = ...
    Remove(Index:int)<decides>:t = ...
    Get(Index:int)<decides>:t = ...
```

**Result and error wrappers:**

```verse
result(t:type, e:type) := class:
    var Value:?t = false
    var Error:?e = false

    IsSuccess():logic = Value?
    IsError():logic = Error?
```

**Generic data structures:**

```verse
tree_node(t:type) := class:
    Value:t
    var Left:?tree_node(t) = false
    var Right:?tree_node(t) = false
```

Note how `tree_node` references itself with the same type parameter, creating a recursive generic structure.

**Type-safe builders and factories:**

```verse
builder(t:type) := class:
    var Config:[string]any = map{}

    Set(Key:string, Value:any):builder(t) =
        set Config[Key] = Value
        Self

    Build():t = ...
```

Parametric classes eliminate code duplication while maintaining type safety. Instead of copy-pasting container implementations for every type you need, write one parametric version that works universally.

### Recursive Parametric Types

Parametric classes can reference themselves in their field types, enabling recursive generic data structures like linked lists, trees, and graphs. However, Verse imposes specific restrictions on how recursion can occur.

#### Allowed: Same-Parameter Recursion

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

#### Disallowed: Direct Type Alias Recursion

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

#### Disallowed: Polymorphic Recursion

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

#### Disallowed: Mutual Recursion

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

#### Disallowed: Inheritance Recursion

You cannot inherit from a type variable or create recursive inheritance through parametric types:

```verse
# Invalid: Inheriting from parametric self
# t(u:type) := class(t(u)){}  # ERROR 3590

# Invalid: Inheriting from type variable
# inherits_from_variable(t:type) := class(t){}  # ERROR 3590
```

**Why this is disallowed:** Inheritance requires knowing the parent's structure, but with parametric recursion, this structure would be self-referential before being defined.

#### Practical Patterns for Recursive Types

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

#### Summary

Recursive parametric types enable powerful data structures, but with clear limitations:

- ✓ **Same-parameter recursion**: `list_node(t)` containing `?list_node(t)`
- ✗ **Direct alias recursion**: `t(u:type) := []t(u)`
- ✗ **Polymorphic recursion**: `my_type(t)` containing `my_type(?t)`
- ✗ **Mutual recursion**: `t1(t)` and `t2(t)` referencing each other
- ✗ **Inheritance recursion**: `t(u:type) := class(t(u))`

These restrictions ensure the type system remains decidable and type checking stays tractable.

### Parametric Interfaces

While parametric classes get most of the attention, interfaces can also be parametric, enabling abstract contracts that work with any type:

#### Defining Parametric Interfaces

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

#### Implementing Parametric Interfaces

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

#### Variance in Parametric Interfaces

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

#### Specialized Interfaces

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

### Parametric Structs

Structs can also be parametric, providing lightweight generic value types:

```verse
# Generic pair
pair(t:type, u:type) := struct:
    First:t
    Second:u

# Usage
Coordinate := pair(int, int){First := 10, Second := 20}
NameAge := pair(string, int){First := "Alice", Second := 30}

# With parametric function
GetFirst(P:pair(t, u) where t:type, u:type):t = P.First

X := GetFirst(Coordinate)  # Returns 10 (int)
Y := GetFirst(NameAge)     # Returns "Alice" (string)
```

Parametric structs follow all the same rules as parametric classes regarding variance, recursion, and type parameters. They're useful for lightweight data holders that don't need identity or inheritance.

### Advanced Parametric Type Features

#### First-Class Parametric Types

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

#### Effects on Parametric Types

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

#### Native Parametric Types

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

#### Type Aliases for Parametric Types

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

### The Concrete Specifier

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

### The Unique Specifier

The `<unique>` specifier creates classes and interfaces with reference semantics where each instance has a distinct identity. When a class or interface is marked as `<unique>`, instances become comparable using the equality operators (= and <>), with equality based on object identity rather than field values.

#### Unique Classes

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

#### Unique Interfaces

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

#### Mixed Interface Implementation

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

#### Parametric Unique Interfaces

Parametric interfaces can be marked `<unique>`, and each concrete instantiation is independently comparable:

```verse
container(t:type) := interface<unique>:
    Get():t
    Set(Value:t):void

int_container := class(container(int)):
    var Value:int = 0
    Get<override>():int = Value
    Set<override>(NewValue:int):void = set Value = NewValue

string_container := class(container(string)):
    var Value:string = ""
    Get<override>():string = Value
    Set<override>(NewValue:string):void = set Value = NewValue

# Each instantiation is separately comparable
IC1 := int_container{}
IC2 := int_container{}
IC1 <> IC2  # true

SC1 := string_container{}
SC2 := string_container{}
SC1 <> SC2  # true
```

#### Unique Instances in Default Values

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

#### Overload Resolution and comparable

**Important:** Types marked with `<unique>` are subtypes of the built-in `comparable` type. This can create overload ambiguity:

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

#### Use Cases for Unique

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

### The Abstract Specifier

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

### The Castable Specifier

The `<castable>` specifier enables runtime type checking and safe downcasting for classes. When a class is marked with `<castable>`, you can use dynamic type tests and casts to determine if an object is an instance of that class or its subclasses at runtime.

Without `<castable>`, Verse's type system operates purely at compile time. The `<castable>` specifier adds runtime type information, allowing code to inspect and react to actual object types during execution. This bridges the gap between static type safety and dynamic polymorphism.

#### Dynamic Casting Syntax

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

### The Final Specifier

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

### The Persistable Specifier

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

### Default Method Implementations

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

### Overriding Interface Members

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

### Multiple Interfaces with Shared Fields

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

### Interface-Based Programming

Interfaces enable programming to abstractions rather than concrete implementations:

<!--NoCompile-->
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
    CurrentHealth := Target.GetHealth()
    Target.Heal(MaxHealth - CurrentHealth)
```

This approach creates loose coupling between different parts of your code. Functions operating on interface types work with any implementing class, promoting code reuse and flexibility. The interface defines what operations are available, while implementing classes determine how those operations execute.

### Property Patterns

A common pattern uses interfaces to package related properties that multiple classes can share:

```verse
# Interface defines shared state structure
player_properties := class:
    var PlayerHealth:float = 100.0
    var PlayerXP:int = 0

# Interface requires the property bundle
has_player_properties := interface:
    var PlayerProperties:player_properties = player_properties{}

# Multiple classes can implement the same property interface
game_mode := class(has_player_properties):
    # Inherits PlayerProperties field

player_tracker := class(has_player_properties):
    # Also has PlayerProperties field

    UpdateHealth(NewHealth:float):void =
        set PlayerProperties.PlayerHealth = NewHealth
```

This pattern allows different systems to share common data structures without inheritance relationships, enabling composition-based designs where functionality is mixed in through interface implementation rather than class inheritance.

## Structs

Structs provide lightweight data containers without the object-oriented features of classes. They're value types optimized for simple data aggregation, making them perfect for mathematical types, data transfer objects, and any scenario where you need a simple bundle of related values without behavior.

Structs group related data with minimal overhead:

<!--verse
damage_type:= enum:
    Physical
character := struct{}
-->
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

<!--NoCompile-->
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

<!--NoCompile-->
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

<!--NoCompile-->
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

### Basic Syntax

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

Each value in the enum becomes a named constant of that enum type. The compiler ensures that variables of an enum type can only hold one of these defined values. Enums can even be empty:

```verse
placeholder := enum{}  # Valid but rarely useful
```

### Types vs Values: A Critical Distinction

Enums introduce both a type and a set of values, and it's crucial to distinguish between them:

```verse
status := enum:
    Active
    Inactive

# status is the TYPE
# status.Active and status.Inactive are VALUES

CurrentStatus:status = status.Active  # OK - value of type status
```

You cannot use the enum type where a value is expected:

<!--NoCompile-->
```verse
# ERROR: Cannot use type as value
BadAssignment:status = status  # Compile error
set CurrentStatus = status     # Compile error

# CORRECT: Use enum values
GoodAssignment:status = status.Active  # OK
set CurrentStatus = status.Inactive    # OK
```

This distinction prevents confusion and ensures type safety. The enum type defines what values are possible, while enum values are the actual constants you use in your code.

### Syntax Restrictions

Enums have specific syntactic requirements that keep their usage clear and unambiguous:

**Enums must be direct right-hand side of definitions:**

<!--NoCompile-->
```verse
# Valid
Priority := enum:
    Low
    Medium
    High

# Invalid - cannot use enum in expressions
Result := -enum{A, B}      # Compile error
Value := enum{X, Y} + 1    # Compile error
```

**Enums must be module or class-level definitions:**

<!--NoCompile-->
```verse
# Valid
MyEnum := enum:
    Value1
    Value2

# Invalid - cannot define local enums
ProcessData():void =
    LocalEnum := enum{A, B}  # Compile error - no local enums
```

These restrictions ensure enums remain stable, referenceable definitions throughout your codebase rather than ephemeral local values.

### Using Enums

Enums provide type-safe alternatives to error-prone string or integer constants:

<!--verse
game_state := enum:
    MainMenu
    Playing
    Paused
    GameOver
-->
```verse
var CurrentState:game_state = game_state.MainMenu

ProcessInput(Input:string):void =
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

The `case` expression with enums provides powerful pattern matching with exhaustiveness checking that ensures you handle all possible values correctly.

### Open vs Closed Enums

Enums can be marked as open or closed, fundamentally affecting how they can evolve and how they interact with pattern matching:

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

**Closed enums** (the default) commit to a fixed set of values forever. This allows the compiler to verify that case expressions handle all possibilities exhaustively. Use closed enums for truly fixed sets: days of the week, cardinal directions, fundamental game states.

**Open enums** allow new values to be added in future versions. This flexibility comes at a cost: case expressions cannot be exhaustive since future values might exist. Use open enums for extensible sets: item types, enemy types, damage types, or any content that may grow.

### Case Exhaustiveness: Critical Rules

The interaction between enum types and case expressions follows sophisticated rules that prevent bugs while enabling both safety and flexibility. Understanding these rules is essential for working with enums effectively.

**Closed Enums with Full Coverage:**

When your case expression handles every value in a closed enum, no wildcard is needed:

```verse
day := enum:
    Monday
    Tuesday
    Wednesday

# Exhaustive - all values covered
GetDayType(D:day):string =
    case (D):
        day.Monday => "Weekday"
        day.Tuesday => "Weekday"
        day.Wednesday => "Weekday"
    # No wildcard needed - all values handled
```

Adding a wildcard when all cases are covered triggers an unreachable code warning:

<!--NoCompile-->
```verse
# Warning: unreachable wildcard
GetDayType(D:day):string =
    case (D):
        day.Monday => "Weekday"
        day.Tuesday => "Weekday"
        day.Wednesday => "Weekday"
        _ => "Unknown"  # WARNING: unreachable - all values already matched
```

**Closed Enums with Partial Coverage:**

If you don't match all values, you must either provide a wildcard or be in a `<decides>` context:

```verse
day := enum:
    Monday
    Tuesday
    Wednesday
    Thursday

# With wildcard - OK
GetWeekStart(D:day):string =
    case (D):
        day.Monday => "Week start"
        _ => "Mid-week"

# Without wildcard but in <decides> context - OK
GetWeekStart(D:day)<decides>:string =
    case (D):
        day.Monday => "Week start"
        # Missing other days causes failure

# Without either - COMPILE ERROR
GetWeekStart(D:day):string =
    case (D):
        day.Monday => "Week start"
        # ERROR: Missing cases and no wildcard
```

**Open Enums Always Require Wildcard or `<decides>`:**

Open enums can have new values added after publication, so they can never be exhaustive:

```verse
weapon := enum<open>:
    Sword
    Bow
    Staff

# Must have wildcard - OK
GetWeaponClass(W:weapon):string =
    case (W):
        weapon.Sword => "Melee"
        weapon.Bow => "Ranged"
        weapon.Staff => "Magic"
        _ => "Unknown"  # REQUIRED - future values may exist

# In <decides> context without wildcard - OK
GetWeaponClass(W:weapon)<decides>:string =
    case (W):
        weapon.Sword => "Melee"
        weapon.Bow => "Ranged"
        weapon.Staff => "Magic"
        # Can fail for unknown (future) values

# Without either - COMPILE ERROR
GetWeaponClass(W:weapon):string =
    case (W):
        weapon.Sword => "Melee"
        weapon.Bow => "Ranged"
        weapon.Staff => "Magic"
        # ERROR: Open enum requires wildcard or <decides>
```

Even if you match all currently defined values in an open enum, you still need a wildcard or `<decides>` context because new values might be added in future versions.

**Summary of Exhaustiveness Rules:**

| Enum Type | Case Coverage | Wildcard | Context | Result |
|-----------|---------------|----------|---------|--------|
| Closed | Full | No | Any | ✓ Valid - exhaustive |
| Closed | Full | Yes | Any | ⚠ Warning - unreachable wildcard |
| Closed | Partial | Yes | Any | ✓ Valid |
| Closed | Partial | No | `<decides>` | ✓ Valid - unmatched values fail |
| Closed | Partial | No | Non-`<decides>` | ✗ Error - missing cases |
| Open | Any | Yes | Any | ✓ Valid |
| Open | Any | No | `<decides>` | ✓ Valid - unmatched values fail |
| Open | Any | No | Non-`<decides>` | ✗ Error - open enum needs wildcard |

These rules ensure that closed enums provide safety through exhaustiveness while open enums require explicit handling of unknown values.

### Unreachable Case Detection

The compiler actively detects unreachable cases in case expressions, helping you identify dead code and logic errors:

**Duplicate cases** are flagged as unreachable:

<!--NoCompile-->
```verse
status := enum:
    Active
    Inactive
    Pending

# ERROR: Duplicate case is unreachable
GetStatusCode(S:status):int =
    case (S):
        status.Active => 1
        status.Inactive => 2
        status.Pending => 3
        status.Pending => 4  # ERROR: unreachable - already matched above
```

**Cases after wildcards** are always unreachable:

<!--NoCompile-->
```verse
# ERROR: Case after wildcard
GetStatusCode(S:status):int =
    case (S):
        status.Active => 1
        _ => 0  # Wildcard matches everything
        status.Inactive => 2  # ERROR: unreachable - wildcard already matched
```

These errors prevent logic bugs where you think you're handling specific cases but the code will never execute.

### The `@ignore_unreachable` Attribute

Sometimes you intentionally want unreachable cases—for testing, migration, or defensive programming. The `@ignore_unreachable` attribute suppresses unreachable warnings and errors for specific cases:

```verse
status := enum:
    Active
    Inactive

ProcessStatus(S:status):int =
    case (S):
        status.Active => 1
        status.Inactive => 2
        @ignore_unreachable status.Inactive => 3  # No error
        @ignore_unreachable _ => 0  # No unreachable warning
```

This attribute only affects cases it's applied to. Other unreachable cases without the attribute still produce errors:

<!--NoCompile-->
```verse
ProcessStatus(S:status):int =
    case (S):
        status.Active => 1
        status.Inactive => 2
        @ignore_unreachable status.Inactive => 3  # Suppressed
        status.Active => 4  # ERROR: still unreachable without attribute
```

Use `@ignore_unreachable` sparingly, primarily during refactoring or when maintaining multiple code paths for testing purposes.

### Namespace Isolation

Enumerators in different enums exist in separate namespaces and never collide:

```verse
letters := enum:
    A
    B
    C

numbers := enum:
    A      # OK - different namespace from letters
    B
    C

# Qualify to distinguish
LetterValue := letters.A
NumberValue := numbers.A
```

This isolation means you can reuse meaningful names across different enums without conflicts. However, within a single scope, enum type names must be unique just like any other identifier.

### Explicit Qualification

Enumerators can collide with identifiers in parent scopes. When this happens, you can use explicit qualification to disambiguate:

```verse
# Outer scope has 'Start'
Start:int = 0

# Enum wants to use 'Start' as enumerator
game_state := enum:
    (game_state:)Start  # Explicit qualification avoids collision
    Playing
    Paused

# Now both are accessible
OuterStart := Start             # References the int
StateStart := game_state.Start  # References the enum value
```

The syntax `(enum_name:)enumerator` explicitly qualifies the enumerator, preventing conflicts with outer-scope symbols.

**Using Reserved Words as Enum Values:**

Qualification also allows you to use reserved words and keywords as enum values, which would otherwise cause errors:

```verse
# Using reserved words as enum values
keyword_enum := enum:
    (keyword_enum:)public    # OK: reserved word qualified
    (keyword_enum:)for       # OK: keyword qualified
    (keyword_enum:)class     # OK: reserved word qualified
    Regular                  # Normal enum value

# Without qualification - errors
bad_enum := enum:
    public    # Error 3532: reserved word
    for       # Error 3514: reserved keyword
```

This is particularly useful when modeling language constructs, access levels, or any domain where reserved words make natural value names.

**Self-Referential Enum Values:**

You can even use the enum's own name as a value when qualified:

```verse
recursive_enum := enum:
    (recursive_enum:)recursive_enum  # OK: qualified with enum name
    OtherValue

# Without qualification - error
bad_recursive := enum:
    bad_recursive  # Error 3532: shadows the type name
```

### Attributes on Enums

Enums support custom attributes, both on the enum type itself and on individual enumerators:

```verse
# Define attributes with appropriate scopes
@attribscope_enum @attribscope_enumerator
category_attribute := class<computes>(attribute) {}
category<constructor>(Name:string)<computes> := category_attribute{}

# Apply to enum and enumerators
@category("Game States")
game_state := enum:
    @category("Initial")
    MainMenu

    @category("Active")
    Playing

    @category("Paused")
    Paused
```

Attributes must be marked with the appropriate scopes (`@attribscope_enum` for enum types, `@attribscope_enumerator` for individual values) or the compiler will reject them. This provides metadata capabilities for reflection, serialization, or custom tooling.

### Enum Comparison

Enum values are fully comparable, meaning they support both equality (`=`) and inequality (`<>`) operators. This makes them ideal for state tracking and conditional logic:

<!--NoCompile-->
```verse
CurrentWeapon := weapon_type.Sword
if (CurrentWeapon = weapon_type.Sword):
    PlaySwordAnimation()

PreviousState := game_state.Playing
if (CurrentState <> PreviousState):
    OnStateChanged(PreviousState, CurrentState)
```

Enum values from the same enum type can be compared, while values from different enum types are always unequal:

```verse
letters := enum:
    A, B, C

numbers := enum:
    One, Two, Three

letters.A = letters.A    # Succeeds - same value
letters.A <> letters.B   # Succeeds - different values
letters.A <> numbers.One # Succeeds - different enum types
```

Because enums are comparable, they can be used as map keys, stored in sets, and used with generic functions that require comparable types:

```verse
# Enums as map keys
var StateCounters:[game_state]int = map{
    game_state.Menu => 0,
    game_state.Playing => 0,
    game_state.Paused => 0
}

# In generic functions
FindState(States:[]game_state, Target:game_state)<decides>:int =
    for (State:States, Index->State):
        if (State = Target):
            return Index
    -1
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

### Implementation Notes

**Virtual Machine Limits:**

Different Verse virtual machines have different capabilities. The Blueprint VM (BPVM) used in Unreal Engine has a limit of 256 enumerators per enum (values 0-255). Attempting to use enumerators beyond this range will cause a link error in BPVM environments:

```verse
# BPVM: OK for values 0-255
small_enum := enum:
    Value000
    Value001
    # ... up to ...
    Value255  # Last valid value in BPVM

# BPVM: ERROR for value 256+
large_enum := enum:
    Value000
    # ... 255 more values ...
    Value256  # Link error in BPVM
```

The native Verse VM has no practical limit and can handle enums with thousands of enumerators. If you're targeting BPVM, keep your enums under 256 values or split large enums into multiple smaller ones.

**Language Evolution:**

Enum semantics have evolved across Verse versions. In versions before 30.00 (Fortnite version 29.30), the compiler was more permissive:

- Duplicate enumerators within an enum didn't error until referenced
- Enumerators didn't collide with parent-scope symbols

From version 30.00 onwards, these became compilation errors:

- Duplicate enumerators are immediately flagged as errors
- Enumerators that shadow parent-scope symbols cause errors (use explicit qualification to resolve)

If you're maintaining code across versions, be aware that older code may have patterns that newer compilers reject. Use explicit qualification `(enum_name:)enumerator` to resolve naming conflicts that arise from the stricter rules.

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

**For enums specifically:**

Choose closed enums (the default) whenever the set of values is truly fixed and will never grow. This enables exhaustive pattern matching and catches missing cases at compile time. Reserve open enums for extensible sets where new values are likely to be added in future updates.

Design case expressions to leverage exhaustiveness checking. For closed enums with complete coverage, omit the wildcard to let the compiler verify all cases are handled. For open enums or partial matches, always provide either a wildcard or use a `<decides>` context to handle unknown values safely.

Prefer descriptive enumerator names over terse abbreviations. `weapon_type.TwoHandedSword` is clearer than `weapon_type.THS`, even if longer. The extra clarity prevents misinterpretation and makes code self-documenting.

Keep enums focused on a single concept. An enum named `entity_state` should only contain entity states, not mixed concerns like `Active`, `Dormant`, `PlayerControlled`, `EnemyType_Zombie`. Split mixed concerns into separate enums that each represent one category.
