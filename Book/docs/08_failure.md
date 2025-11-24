# Failure

Most programming languages treat control flow as a matter of true or false, yes or no, one or zero. They evaluate boolean conditions and branch accordingly, creating a world of binary decisions that often requires checking conditions twice - once to see if something is possible, and again to actually do it. Verse takes a different approach. Instead of asking "is this true?", Verse asks "does this succeed?"

This distinction might seem subtle, but it changes how programs are written and reasoned about. Failure isn't an error or an exception - it's a first-class concept that drives control flow. When an expression fails, it doesn't crash your program or throw an exception that needs to be caught. Instead, failure is a normal, expected outcome that your code handles gracefully through the structure of the language itself.

Consider the simple act of accessing an array element. In traditional languages, you might write:

<!--NoCompile-->
```verse
# Traditional
# if (index < array.length) {
#     value = array[index]
#     process(value)
# }
```

This approach checks validity separately from access, creating opportunities for bugs if the check and access become separated or if the array changes between them. In Verse, validation and access are unified:

<!--NoCompile-->
```verse
if (Value := MyArray[Index]):
    Process(Value)
```

The array access either succeeds and binds the value, or it fails and the code moves on. There's no separate validation step, no possibility of the check and access becoming inconsistent, and no undefined behavior from accessing invalid indices.

## Failable Expressions

A failable expression is one that can either succeed and produce a value, or fail and produce nothing. This isn't the same as returning null or an error code - when an expression fails, it literally produces no value at all. The computation stops at that point in that particular path of execution.

Many operations are naturally failable. Array indexing fails when the index is out of bounds. Map lookups fail when the key doesn't exist. Comparisons fail when the values aren't equal. Division fails when dividing by zero. Even simple literals can be made to fail:

<!--NoCompile-->
```verse
42      # Always succeeds with value 42
false?  # Always fails - the query of false
true?   # Always succeeds - the query of true
```

The query operator `?` turns any value into a failable expression. When applied to `false`, it always fails. When applied to any other value, it succeeds with that value. This simple mechanism provides immense power for controlling program flow.

You can create your own failable expressions through functions marked with the `<decides>` effect:

<!--verse
-->
```verse
ValidateAge(Age:int)<decides>:int =
    Age >= 0    # Fails if age is negative
    Age <= 150  # Fails if age is unrealistic
    Age         # Returns the age if both checks pass
```
<!--verse
Main()<decides>:void=
    ValidateAge[10]
-->

This function doesn't just check conditions - it embodies them. If the age is invalid, the function fails. If it's valid, it succeeds with the age value. The validation and the value are inseparable.

## Failure Contexts

Not every part of a program can execute failable expressions. They can only appear in failure contexts - places where the language knows how to handle both success and failure. Each failure context defines what happens when expressions within it fail.

The most common failure context is the condition of an `if` expression:

<!--NoCompile-->
```verse
if (Player := GetPlayerByName[Name], Score := GetPlayerScore[Player], Score > 100):
    Print("High scorer: {Name} with {Score} points!")
```

This `if` condition contains three potentially failable expressions. All must succeed for the body to execute. If any fails, the entire condition fails, and control moves to the `else` branch (if present) or past the `if` entirely. The beauty is that each expression can use the results of previous ones - `Score` is only computed if we successfully found the `Player`.

The `for` expression creates a failure context for each iteration:

<!--NoCompile-->
```verse
for (Item : Inventory, IsWeapon[Item], Damage := GetDamage[Item], Damage > 50):
    Print("Powerful weapon: {Item} with {Damage} damage")
```

Each iteration attempts the failable expressions. If they all succeed, the body executes for that item. If any fails, that iteration is skipped, and the loop continues with the next item. This creates a natural filtering mechanism without explicit conditional logic.

Functions marked with `<decides>` create a failure context for their entire body:

<!--verse
item:=struct{}
IsWeapon(i:item)<computes><decides>:void={}
GetDamage(i:item)<computes><decides>:int=0
-->
```verse
FindBestWeapon(Inventory:[]item)<decides>:item =
    var BestWeapon:?item = false
    var MaxDamage:int = 0

    for (Item : Inventory, IsWeapon[Item], Damage := GetDamage[Item]):
        if (Damage > MaxDamage):
            set BestWeapon = option{Item}
            set MaxDamage = Damage

    BestWeapon?  # Fails if no weapon was found
```
<!--verse
Main()<decides>:void=return
-->
The function body is a failure context, allowing failable expressions throughout. The final line extracts the value from the option, failing if no weapon was found.

## Speculative Execution

When you execute code in a failure context, changes to mutable variables are provisional—they only become permanent if the entire context succeeds. Functions that modify state in failure contexts must use the `<transacts>` effect specifier (see [Effects](13_effects.md)):

<!-- TODO MUTABLE PARAMETERS ARE NOT YET IMPLMENTED -->

<!--NoCompile-->
```verse
AttemptPurchase(var PlayerGold:int, Cost:int)<transacts><decides>:void =
    set PlayerGold = PlayerGold - Cost  # Provisional change
    PlayerGold >= 0                     # Check if still valid
    # If this fails, PlayerGold reverts to original value
```

If the check fails, the subtraction is automatically rolled back. You don't need to manually restore the original value or check conditions before modifying state. The `<transacts>` effect provides special failure semantics—heap modifications are rolled back on failure.

<!--NoCompile-->
```verse
ComplexTransaction(var State:game_state)<transacts><decides>:void =
    ModifyHealth(State.Player)         # All these operations
    UpdateInventory(State.Inventory)   # are provisional
    ChargeResources(State.Resources)   # until all succeed
    ValidateFinalState[State]         # If this fails, everything rolls back
```

This transactional behavior makes complex state updates safe and predictable. Either everything succeeds and all changes are committed, or something fails and nothing changes.

## The Logic of Failure

Verse provides logical operators that work with failure, creating a rich algebra for combining failable expressions.

The `not` operator inverts success and failure:

<!--NoCompile-->
```verse
if (not (Enemy := GetNearestEnemy[])):
    Print("Coast is clear!")  # Executes when GetNearestEnemy fails
```

The `or` operator provides alternatives:

<!--NoCompile-->
```verse
Weapon := PrimaryWeapon[] or SecondaryWeapon[] or DefaultWeapon
```

This tries each option in order, stopping at the first success. It's not evaluating boolean conditions - it's attempting computations and taking the first one that succeeds.

You can combine these operators to create sophisticated control flow:

<!--NoCompile-->
```verse
ValidatePlayer(Player:player)<decides>:void =
    IsAlive[Player]
    not IsStunned[Player]
    HasAmmunition[Player] or HasMeleeWeapon[Player]
```

This function succeeds only if the player is alive, not stunned, and has either ammunition or a melee weapon. Each line is a separate failable expression that must succeed.

## Expression in Decides Contexts

One of Verse's most subtle but powerful features is how expressions behave in decides contexts. When a comparison appears in a context that can handle failure, it doesn't just test a condition—it produces a value.

Consider this function:

```verse
ValidatePositive(X:int)<decides>:int =
    X > 0
```
<!--verse
Main()<decides>:void=
    ValidatePositive[1]
-->

This looks like it just checks a condition, but there's more happening. When `X > 0` succeeds (when the comparison is true), it returns the value of `X`. When the comparison fails (when it's false), the function fails. The comparison is both a test and a value-producing expression.

This behavior applies to all comparison operators in decides contexts:

```verse
GetIfNotEqual(X:int, Y:int)<decides>:int =
    X <> Y  # Returns X when X ≠ Y, fails when X = Y

GetIfLessOrEqual(X:int, Limit:int)<decides>:int =
    X <= Limit  # Returns X when X ≤ Limit, fails otherwise

GetIfGreaterThan(X:int, Threshold:int)<decides>:int =
    X > Threshold  # Returns X when X > Threshold, fails otherwise
```
<!--verse
Main()<decides>:void=
    GetIfNotEqual[1,2]
    GetIfGreaterThan[11,2]
    GetIfLessOrEqual[1,2]
-->

A comparison expression `A op B` in a decides context returns the left operand `A` when the comparison succeeds, and fails when the comparison is false.

This creates concise validation functions:

```verse
ValidateInRange(Value:int, LwrBnd:int, UprBnd:int)<decides>:int =
    Value >= LwrBnd
    Value <= UprBnd
    Value  # This final line is actually redundant!
```
<!--verse
Main()<decides>:void=
    ValidateInRange[5,0,10]
-->

The last line is redundant because `Value <= UprBnd` already returns `Value` when it succeeds. However, it can make the intent clearer. You could write this more concisely as:

```verse
ValidateInRange(Value:int, LwrBnd:int, UprBnd:int)<decides>:int =
    Value >= LwrBnd
    Value <= UprBnd
```
<!--verse
Main()<decides>:void=
    ValidateInRange[5,0,10]
-->

The function returns `Value` from the last successful comparison.

This pattern extends beyond comparisons to any expression that produces a value while potentially failing:

<!--verse
player := string
FindPlayer(Name:player)<decides>:player={
    return Name
}
IsAlive(Name:string)<decides>:string={
    Name = Name
}
-->
```verse
GetValidatedPlayer(Name:string)<decides>:player =
    Player := FindPlayer[Name]  # Fails if player not found
    IsAlive[Player]             # Fails if player is dead
    Player                      # Returns the validated player
```
<!--verse
Main()<decides>:void=
    GetValidatedPlayer["PlayerOne"]
-->

Understanding this semantic is crucial for writing idiomatic Verse code. It's why you often see comparison chains without explicit return statements—the comparisons themselves produce the values.

## Options and Failure

The option type and failure are intimately connected. An option either contains a value or is empty (represented by `false`). The query operator `?` converts between options and failure:

<!--verse
F()<computes><decides>:void={
-->
```verse
MaybeValue:?int = option{42}
Value := MaybeValue?  # Succeeds with 42

Empty:?int = false
Other := Empty?  # Fails
```
<!--verse
}
Main()<decides>:void=return
# the Other := Empty? line panics betaverse.
-->

The `option{}` constructor works in reverse, converting failure to an empty option:

<!--verse
RiskyComputation()<computes><decides>:int=1
F()<decides>:void={
-->
```verse
Result := option{RiskyComputation[]}
# Result is option{value} if computation succeeds
# Result is false if computation fails
```
<!--verse
}
Main()<decides>:void=
    F[]
-->

This bidirectional conversion makes options and failure interchangeable, allowing you to choose the most appropriate representation for your specific use case.

## Option Type Reference

The option type `?T` represents values that may or may not be present. This section provides a comprehensive reference for option type syntax, operations, and constraints.

The question mark appears *before* the type, not after:

```verse
ValidSyntax:?int = option{42}      # Correct
# InvalidSyntax:int? = option{42}  # ERROR 3549 - wrong position
```
<!--verse
Main():void=
    ValidSyntax
    return
-->

This is the only valid syntax for declaring optional types. The `?` prefix applies to any type:

<!--verse
player := struct{}
F()<transacts><decides>:void={
-->
```verse
MaybeNumber:?int = option{42}
MaybeText:?string = option{"hello"}
MaybePlayer:?player = option{player{}}
```
<!--verse
}
Main():void=
    F[] or "F died"
-->

Use the `option{}` constructor to wrap a value:

<!--
# TODO: this example does not work. A decides computation cannot be used to initialize
# data. RiskyComputation is decides due to the [], but is being used to initialize
# Result.
-->
```verse
# Filled option
Filled:?int = option{42}

# Empty option using false
Empty:?int = false

# Option from failable expression
Result:?int = option{RiskyComputation[]}  # false if computation fails
```
<!--verse
Main():void=
    Filled
    Empty
    Result
    return
-->

Empty options and `false` are equivalent—an empty option *is* `false`:

<!--verse
Go()<transacts><decides>:void=
-->
```verse
EmptyOption:?int = false
EmptyOption = false  # This comparison succeeds
```
<!--verse

Main():void=
    Go[] or "Don't go"
-->


**Expression semantics in `option{}`**: The option constructor evaluates its contents as a sequence or tuple depending on separators:

<!--verse
Go()<transacts><decides>:void=
-->
```verse
# Semicolon creates sequence - last value is used
option{1; 2}? = 2

# Comma creates tuple
option{1, 2}? = (1, 2)

# Single expression
option{42}? = 42
```
<!--verse

Main():void=
    Go[] or "Don't go"
-->

### Unwrapping Options

The query operator `?` extracts values from options, failing if the option is empty:

<!--verse
Go()<transacts><decides>:void={
-->
```verse
MaybeValue:?int = option{42}
Value := MaybeValue?  # Succeeds with 42

Empty:?int = false
Other := Empty?  # Fails - cannot unwrap empty option
```
<!--verse
}
Main():void=
    Go[] or "Go should fail so use this"
-->


Unwrapping is only allowed in failure contexts:

<!--verse
MaybeInt:?int = option{42}
UseItem(I:int):void={return}
ProcessItem(I:int):?int=option{3}
Items:[]int = array{1,2,3}
Go()<transacts><decides>:void=
# TODO:
# this example cannot work. We cannot catch the error and furthermore we cannot
# wrap the if and for in a function _and exclude_ GetRequired and RegularFunction
-->
```verse
# Valid: In if condition (failure context)
if (Value := MaybeInt?):
    Print("Got {Value}")

# Valid: In for loop (failure context)
for (Item : Items, ValidItem := ProcessItem[Item]?):
    UseItem(Item)

# Valid: In <decides> function body (failure context)
GetRequired(Maybe:?int)<decides>:int =
    Maybe?  # Fails if Maybe is empty

# ERROR 3512: Not in failure context
RegularFunction(Maybe:?int):void =
    Value := Maybe?  # ERROR - function body is not a failure context
```

### Nested Options

Options can be nested to represent multiple layers of absence:

<!--verse
Go()<transacts><decides>:void=
-->
```verse
# Double-nested option, notice the ?? type
DoubleNested:??int = option{option{42}}

# Single unwrap gets outer option
if (Inner := DoubleNested?):
    # Inner has type ?int
    if (TheValue := Inner?):
        # TheValue has type int, equals 42

# Double unwrap gets the value directly
DirectValue := DoubleNested??  # Fails if either layer is empty
```
<!--verse
Main():void=
    Go[] or "Don't Go"
-->

Helper functions also work with nested options:

```verse
UnpackNested(MaybeValue:??int):?int =
    if (Inner := MaybeValue?):
        Inner
    else:
        option{-1}  # Default for outer empty

DirectUnpack(MaybeValue:??int):int =
    if (Value := MaybeValue??):
        Value
    else:
        -1  # Default for any level empty
```
<!--verse
Main():void=
    UnpackNested(option{option{1}})
    DirectUnpack(option{option{2}})
-->
### Chained Member Access

The `?.` operator provides safe member access on optional values:

<!--
## TODO: this one is also tangled. I think we should remove the linked list example.
-->
```verse
entity := class:
    Name:string = "Unknown"
    Health:int = 100

MaybeEntity:?entity = option{entity{}}

# Safe field access
if (Name := MaybeEntity?.Name):
    Print("Entity: {Name}")  # Succeeds

# Safe method call
MaybeEntity?.TakeDamage(10)  # Only calls if entity present

# Chaining through multiple optionals
linked_list := class:
    Value:int = 0
    Next:?linked_list = false

Head:?linked_list = option{linked_list{Value := 1}}
SecondValue := Head?.Next?.Value  # Fails if any link is empty
```

The `?.` operator short-circuits—if the option is empty, the entire expression fails without evaluating the member access.

### Providing Defaults with `or`

Use the `or` operator to provide fallback values for empty options:
<!--verse
Go()<transacts><decides>:void=
-->
```verse
# Simple default
MaybeValue:?int = false
Value := MaybeValue? or 42  # Results in 42

# Chaining multiple options
Primary:?string = false
Secondary:?string = option{"backup"}
Default:string = "default"

Result := Primary? or Secondary? or Default  # Results in "backup"
```
<!--verse
Main():void=
    Go[] or "Hey a default!"
-->

The `or` operator tries each alternative in order, using the first one that succeeds:

<!--verse
weapon := string
player := class:
    EquippedWeapon:?string = option{"Lancer"}
    HolsteredWeapon:?string = option{"Hammer of Dawn"}

DefaultWeapon:string = "Melee"
-->
```verse
GetPreferredWeapon(Player:player):weapon =
    Player.EquippedWeapon? or Player.HolsteredWeapon? or DefaultWeapon
```
<!--verse
Main():void=
    Marcus := player{}
    GetPreferredWeapon(Marcus)
-->

### Type System Rules

Option types are disjoint from non-option types. Verse does not allow implicitly
convert between `T` and `?T`:

```verse
# ERROR 3510: Cannot pass T where ?T expected
RegularValue:int = 42
# OptionalVariable:?int = RegularValue  # ERROR

# ERROR 3510: Cannot pass ?T where T expected
OptionalValue:?int = option{42}
# RequiredVariable:int = OptionalValue  # ERROR

# Correct: Explicitly wrap
RequiredToOptional:?int = option{42}

# Correct: Unwrap in failure context
GetFromOptional(Opt:?int)<decides>:int = Opt?
```
<!--verse
Main():void=
    # TODO: GetFromOptional Errors out because its <no_rollback>
    # the fix is to annotate it as <transacts> but then this would be in the
    # example
    GetFromOptional[OptionalValue] or RegularValue
-->

This disjointness prevents bugs by making the presence or absence of values explicit:

<!--verse
# TODO: this example is also tangled.
-->
```verse
entity := class:
    MaybeOwner:?player = false

# ERROR 3509: Cannot pass ?player where player expected
# ProcessPlayer(MaybeOwner)  # ERROR

# Correct: Handle both cases
if (Owner := MaybeOwner?):
    ProcessPlayer(Owner)  # Owner has type player
else:
    Print("No owner")
```

### Common Errors and Restrictions

**Wrong type syntax**:

<!--NoCompile-->
```verse
# ERROR 3549: ? goes before the type, not after
# entity := class:
#     Owner?:player  # ERROR - should be Owner:?player
```

**Invalid option construction**:

<!--NoCompile-->
```verse
# ERROR 3502: Wrong delimiters
# option(42)   # ERROR - use braces {}
# option[42]   # ERROR - use braces {}

# ERROR 3622: Empty option{} is invalid
# option{}     # ERROR - use false instead

# ERROR 3559: Cannot have do clause
# option{42} do { Print("Done") }  # ERROR
```

**Non-type expressions as option types**:

<!--verse
Main():void=
-->
```verse
# ERROR 3547: Can only make options of types, not values
# MaybeValue:?42 = option{42}      # ERROR
# MaybeFloat:?3.14 = option{3.14}  # ERROR
# MaybeText:?"hello" = false       # ERROR

# Correct: Use the type
MaybeInt:?int = option{42}
MaybeFloat:?float = option{3.14}
MaybeString:?string = option{"hello"}
```


**Unwrapping non-options**:

<!--verse
Main():void=
-->
```verse
# ERROR 3509: Can only unwrap option types
RegularValue:int = 42
# ExtractedValue := RegularValue?  # ERROR - int is not an option
```

**Undefined type errors don't cascade**:

<!--NoCompile-->
```verse
# ERROR 3506: Undef is not defined, but ?Undef doesn't create additional errors
# Function(Param:?Undef):void = {}  # Single error for Undef, not for ?Undef
```

### Options with Other Types

<!-- TODO: this should show option with a user defined type -->
Options work with all Verse types:

```verse
# Primitives
MaybeInt:?int = option{42}
MaybeFloat:?float = option{3.14}
MaybeString:?string = option{"text"}

# Collections
MaybeArray:?[]int = option{array{1, 2, 3}}
MaybeMap:?[string]int = option{map{"a" => 1}}

# Tuples
MaybePair:?tuple(int, string) = option{(42, "answer")}

# Classes
entity := class:
    ID:int
MaybeEntity:?entity = option{entity{ID := 1}}

# Enums
direction := enum{North, South, East, West}
MaybeDirection:?direction = option{direction.North}
```
<!--verse
# TODO: tangled, class defaults to transacts which is not allowed at top-level
Main():void=
    MaybeInt
    MaybeFloat
    MaybeString
    MaybeArray
    MaybeMap
    MaybeEntity
    MaybeDirection
-->
### Comparison and Equality

Empty options equal `false`, and filled options equal their unwrapped values when compared properly:

<!--verse
Go()<transacts><decides>:void=
-->
```verse
EmptyOption:?int = false
EmptyOption = false  # Succeeds

FilledOption:?int = option{1}
FilledOption? = 1  # Succeeds - unwrap then compare
```
<!--verse
Main():void=
    Go[] or 1
-->

However, you cannot directly compare optional and non-optional values without unwrapping:

<!--verse
Main():void=
-->
```verse
Opt:?int = option{42}
Regular:int = 42

# Must unwrap to compare
if (Opt? = Regular):
    Print("Equal")
```

## Multi-Layer Failure with Optionals

Combining decides functions with optional return types, creates a system with
multiple layers of failure. This pattern enables expressing complex conditions
concisely while maintaining clarity.

A function can fail at two levels:

- *Function-level failure*: The entire function fails using `<decides>`
- *Value-level failure*: The function succeeds but returns an empty option

<!--verse
player := string
IsActive(S:string)<transacts><decides>:string=""
LookupPlayer(S:string)<transacts><decides>:string="player"
# TODO: FindEligiblePlayer must have <transacts> to compile
-->
```verse
FindEligiblePlayer(Name:string)<decides>:?player =
    Name <> ""           # Layer 1: Fail if name is empty
    Player := LookupPlayer[Name]  # Layer 1: Fail if player not found
    option{IsActive[Player]}      # Layer 2: Empty option if player inactive
```
<!--verse
Main():void=
    FindEligiblePlayer["Someone"] or "Default"
-->

This function has three possible outcomes:

- *Function fails*: Empty name or player not found
- *Function succeeds with empty option*: Player found but inactive
- *Function succeeds with filled option*: Player found and active

Calling this function demonstrates the layered failure:

<!--verse
FindEligiblePlayer(S:string)<transacts><decides>:?string=option{S}
Go()<transacts><decides>:void=
-->
```verse
# Function-level failure
Result1 := FindEligiblePlayer[""]  # Fails, Result1 never assigned

# Function succeeds, returns empty option
if (Player := FindEligiblePlayer["InactiveUser"]?):
    # Won't execute - function succeeds but ? query fails
else:
    # Executes here

# Function succeeds, returns filled option
if (Player := FindEligiblePlayer["ActiveUser"]?):
    # Executes with Player bound to the active player
```
<!--verse
Main():void=
    Go[] or "Default"
-->

This pattern is particularly powerful for validation with different failure modes:

```verse
ValidateScore(Score:int)<decides>:?int =
    Score >= 0           # Layer 1: Reject negative scores (invalid input)
    option{Score <= 100} # Layer 2: Reject high scores (out of range)
```
<!--verse
# TODO: ValidateScore needs <transacts> to compile
Main():void=
    ValidateScore[2] or 2
-->

Testing:

<!--verse
ValidateScore(Score:int)<transacts><decides>:?int =
    Score >= 0           # Layer 1: Reject negative scores (invalid input)
    option{Score <= 100} # Layer 2: Reject high scores (out of range)
Main():void=
-->
```verse
ValidateScore[-1] or 1
ValidateScore[50]? or 2
ValidateScore[150]? or 3
```

The distinction between function-level and value-level failure lets you express different kinds of errors. Function-level failure typically means "this operation couldn't complete" while value-level failure means "the operation completed but the result doesn't meet the expected criteria."

You can query optionals at different points:

<!--verse
FindValidScore(S:string):?int=option{2}
-->
```verse
GetScoreClass(Name:string)<decides>:string =
    Score := FindValidScore[Name]  # Returns ?int
    if (Score?):
        if (Score? > 90):
            "Excellent"
        else:
            "Good"
    else:
        "No score"
```
<!--verse
# TODO: GetScoreClass needs a <transacts> to compile
Main():void=
    GetScoreClass["hello"] or "Nope"
-->

Or chain them naturally:

<!--verse
FindValidScore(S:string):?int=option{2}
-->
```verse
GetTopScore(Name:string)<decides>:int =
    Score := FindValidScore[Name]   # Returns ?int
    ExtractedScore := Score?        # Fails if Score is empty
    ExtractedScore > 50             # Fails if score too low
```
<!--verse
# TODO: GetTopScore needs a <transacts> to compile
Main():void=
    GetTopScore["PlayerOne"] or 10
-->

<!-- TODO: This should end with advice, e.g., "We recommend...", on when to use
this pattern rather than claims -->
This multi-layer approach creates expressive APIs where different failure modes have different meanings, and callers can choose how deeply to inspect the results.

## Dynamic Casts as Decides

Type casting in Verse is integrated into the failure system. A dynamic cast
behaves just like a `<decides>` function call and similarly uses bracket
syntax. For example `Type[value]` attempts to cast `value`'s type to `Type` and
fails if unsuccessful.

<!-- TODO: link to chapter 10?  -->
This is also works with user defined types which must specify `<castable>`:

```verse
component := class<castable>:
    Name:string = "Component"

physics_component := class<castable>(component):
    Velocity:float = 0.0

# Casting as a decides operation
TryGetPhysics(Comp:component)<decides>:physics_component =
    physics_component[Comp]  # Succeeds if Comp is actually a physics_component
```
<!--verse
# TODO: TryGetPhysics needs a <transacts> to compile
Main():void=
    TryGetPhysics[component{}] or component{}
-->

This makes type-based dispatch easily expressible:

<!--verse
component := class<castable>:
    Name:string = "Component"
physics_component := class<castable>(component):
    Velocity:float = 0.0
render_component := class<castable>(component):
    Renderer:string = "RayTrace"
UpdatePhysics(P:physics_component):void=return
UpdateRendering(R:render_component):void=return
UpdateGeneric(G:component):void=return
-->
```verse
ProcessComponent(Comp:component):void =
    if (Physics := physics_component[Comp]):
        UpdatePhysics(Physics)
    else if (Render := render_component[Comp]):
        UpdateRendering(Render)
    else:
        # Unknown component type
        UpdateGeneric(Comp)
```
<!--verse
Main():void=
    C := component{}
    ProcessComponent(C)
-->

The cast itself is the condition—no separate type checking needed. When the cast succeeds, you have both confirmed the type and obtained a properly-typed reference.

You can chain casts with other decides operations:

<!--verse
component := class<castable>:
    Name:string = "Component"
physics_component := class<castable>(component):
    Velocity:float = 0.0
render_component := class<castable>(component):
    Renderer:string = "RayTrace"
UpdatePhysics(P:physics_component):void=return
UpdateRendering(R:render_component):void=return
UpdateGeneric(G:component):void=return
entity := class:
    GetComponent()<transacts><decides>:component=
        component{}
IsActive(c:component)<transacts><decides>:logic=true
# TODO: GetActivePhysicsComponent needs <transacts> to compile
-->
```verse
GetActivePhysicsComponent(Entity:entity)<decides>:physics_component =
    Comp := Entity.GetComponent[]  # Fails if no component
    Physics := physics_component[Comp]  # Fails if not physics
    IsActive[Physics]  # Fails if inactive
    Physics
```
<!--verse
Main():void=
    e := entity{}
    GetActivePhysicsComponent[e] or "No entity"
-->

Each step must succeed for the function to return a value. This creates self-documenting validation chains where type requirements are explicit.

Casts work with the `or` combinator for fallback types:

```verse
GetInteractable(Entity:entity)<decides>:component =
    physics_component[Entity] or
    trigger_component[Entity] or
    scripted_component[Entity]
```

This tries each cast in order, returning the first successful one. It's type-safe because all options share the common `component` base type.

## Composition and Call Chains

Decides functions compose naturally, allowing complex operations to be built from simple, reusable pieces. When a decides function calls another decides function, failures propagate automatically.

```verse
ValidatePositive(X:int)<decides>:int =
    X > 0

Double(X:int)<decides>:int =
    Validated := ValidatePositive[X]  # Fails if X ≤ 0
    Validated * 2
```

If `ValidatePositive` fails, `Double` fails immediately. The validated value flows through the chain.

**Multi-level validation:**

```verse
ValidateAge(Age:int)<decides>:int =
    Age >= 0
    Age <= 150

ValidateAdult(Age:int)<decides>:int =
    Validated := ValidateAge[Age]  # First level: is it a valid age?
    Validated >= 18                # Second level: is it adult age?

ValidateSenior(Age:int)<decides>:int =
    Adult := ValidateAdult[Age]    # Reuses adult validation
    Adult >= 65
```

Each function builds on the previous, creating layers of increasingly specific validation.

**Transforming results:**

```verse
FindPlayer(Name:string)<decides>:player = ...
GetPlayerScore(P:player)<decides>:int = ...

FindPlayerScore(Name:string)<decides>:int =
    Player := FindPlayer[Name]      # Fails if player not found
    GetPlayerScore[Player]           # Fails if player has no score
```

The chain threads values through multiple stages, each of which might fail.

**Combining with `or` for fallbacks:**

```verse
GetPrimaryWeapon(Player:player)<decides>:weapon = ...
GetSecondaryWeapon(Player:player)<decides>:weapon = ...
GetMeleeWeapon(Player:player)<decides>:weapon = ...

GetAnyWeapon(Player:player)<decides>:weapon =
    GetPrimaryWeapon[Player] or
    GetSecondaryWeapon[Player] or
    GetMeleeWeapon[Player]
```

Each alternative is tried in order until one succeeds.

**Parallel validation:**

You can validate multiple things in sequence, all of which must succeed:

```verse
ValidateTransaction(Buyer:player, Seller:player, Price:int)<decides>:void =
    Buyer.Gold >= Price        # Buyer can afford it
    not IsSamePlayer[Buyer, Seller]  # Different players
    ValidatePrice[Price]       # Price is reasonable
    not IsBanned[Buyer]        # Buyer not banned
    not IsBanned[Seller]       # Seller not banned
```

Any failure stops the chain, and the transaction is invalid.

**Preserving failure context:**

When calling decides functions in non-decides contexts, you must handle failure explicitly:

```verse
# This won't compile - ProcessPlayer doesn't have <decides>
ProcessPlayer(Name:string):void =
    Player := FindPlayer[Name]  # ERROR: Unhandled failure

# Handle with if
ProcessPlayer(Name:string):void =
    if (Player := FindPlayer[Name]):
        UsePlayer(Player)

# Handle with or
ProcessPlayer(Name:string):void =
    Player := FindPlayer[Name] or GetDefaultPlayer()
    UsePlayer(Player)
```

Understanding composition helps you build complex validation logic from simple, testable pieces.

## Optional Indexing and Tuple Access

When working with optional containers, you can access their contents using specialized query syntax that combines optional checking with element access.

Optional tuples support direct element access through the query operator:

```verse
MaybePair:?tuple(int, string) = option{(42, "answer")}

# Access first element
if (FirstValue := MaybePair?(0)):
    # FirstValue is 42 (type: int)
    Print("First: {FirstValue}")

# Access second element
if (SecondValue := MaybePair?(1)):
    # SecondValue is "answer" (type: string)
    Print("Second: {SecondValue}")
```

The syntax `Option?(index)` simultaneously:

- Queries whether the option is non-empty
- Accesses the tuple element at the given index
- Binds the element value if both succeed

This fails if either:

- The option is empty (`false`)
- The index is out of bounds (though type-checked tuples prevent this)

Compare this to the two-step approach:

```verse
# Two-step: extract then index
if (Pair := MaybePair?):
    FirstValue := Pair(0)
    # Use FirstValue

# One-step: index directly
if (FirstValue := MaybePair?(0)):
    # Use FirstValue
```

The one-step form is more concise when you only need a specific element.

**Chaining with other operations:**

Optional tuple indexing works in any decides context:

```verse
ProcessFirstElement(Data:?tuple(int, int))<decides>:int =
    Value := Data?(0)  # Fails if Data is empty
    Value > 0          # Fails if value not positive
    Value * 2

GetLargerElement(Data:?tuple(int, int))<decides>:int =
    First := Data?(0)
    Second := Data?(1)
    if (First > Second) then First else Second
```

This integrates seamlessly with the failure system, treating empty options and missing elements uniformly as failure.

**Optional arrays:**

While not specifically tested, the pattern extends conceptually to optional arrays:

```verse
MaybeArray:?[]int = option{array{1, 2, 3}}

# Would access first element if array exists and has elements
# (Specific syntax may vary based on implementation)
```

Understanding optional indexing helps when working with heterogeneous data structures where containers might be absent or empty.

## Failure Patterns: Idioms and Techniques

As you work with failure, certain patterns emerge that solve common problems elegantly.

The validation chain pattern uses sequential failures to ensure all conditions are met:

<!--NoCompile-->
```verse
ProcessAction(Action:action)<decides>:void =
    Player := GetActingPlayer[Action]
    IsValidTurn[Player]
    HasRequiredResources[Player, Action]
    Location := GetTargetLocation[Action]
    IsValidLocation[Location]
    ExecuteAction[Action]
```

Each line must succeed for execution to continue. This creates self-documenting code where preconditions are explicit and checked in order.

The first-success pattern tries alternatives until one works:

<!--NoCompile-->
```verse
FindPath(Start:location, End:location)<decides>:path =
    DirectPath[Start, End] or
    PathAroundObstacles[Start, End] or
    ComplexPathfinding[Start, End]
```

This naturally expresses trying simple solutions before complex ones.

The filtering pattern uses failure to select items:

<!--NoCompile-->
```verse
GetEliteEnemies(Enemies:[]enemy):[]enemy =
    for (Enemy : Enemies, Level := GetLevel[Enemy], Level >= 10):
        Enemy
```

Only enemies that have a level and whose level is at least 10 are included in the result.

The transaction pattern groups related changes:

<!--NoCompile-->
```verse
TradeItems(var PlayerA:player, var PlayerB:player, ItemA:item, ItemB:item)<transacts><decides>:void =
    RemoveItem(PlayerA, ItemA)
    RemoveItem(PlayerB, ItemB)
    AddItem(PlayerA, ItemB)
    AddItem(PlayerB, ItemA)
    ValidateTrade(PlayerA, PlayerB)
```

Either the entire trade succeeds, or nothing changes.

## Runtime Errors

While failure (`<decides>`) represents normal control flow with transactional rollback, **runtime errors** represent unrecoverable conditions that terminate execution. Runtime errors propagate up the call stack, bypassing normal failure handling, and cannot be caught or recovered within Verse code.

The `Err()` function explicitly triggers a runtime error with an optional message:

```verse
ValidateInput(Value:int):int =
    if (Value < 0):
        Err("Negative values not allowed")
    Value

ProcessData(Data:[]int):void =
    for (Item : Data):
        ValidatedItem := ValidateInput(Item)  # Runtime error if negative
        # Processing continues only if validation succeeds
```

Runtime errors differ fundamentally from failures:

| Aspect | Failure (`<decides>`) | Runtime Error (`Err()`) |
|--------|---------------------|------------------------|
| Recovery | Can be handled with `or`, `else` | Cannot be caught or handled |
| Transactions | Rolls back effects | Terminates execution |
| Use case | Expected alternatives | Unrecoverable problems |
| Flow | Continues in alternative path | Stops execution |

```verse
# Failure - recoverable
GetValue(Key:int)<decides>:int =
    Map[Key]  # Fails if key not found

Result := GetValue(42) or 0  # Provides alternative

# Runtime error - unrecoverable
CheckInvariant(Condition:logic):void =
    if (not Condition):
        Err("Invariant violated")  # Cannot be caught

CheckInvariant(IsValid())  # Terminates if invariant fails
```

### Stack Unwinding

When a runtime error occurs, execution unwinds through the call stack, terminating the current operation:

<!--verse
Log(Message:string)<transacts>:void = {}
-->
```verse
DeepFunction()<transacts>:int =
    Log("C")
    Err("Fatal error")  # Runtime error here
    Log("D")            # Never executes
    return 1

MiddleFunction():int =
    Log("B")
    Result := DeepFunction()  # Error propagates through here
    Log("E")                  # Never executes
    return Result

TopFunction():void =
    Log("A")
    Value := MiddleFunction()  # Error propagates to here
    Log("F")                   # Never executes

# Execution order: A, B, C, then terminates
# Output: "ABC"
```

The runtime error propagates immediately, bypassing all subsequent code in the call chain.

### Async Contexts

Runtime errors propagate through asynchronous operations, terminating spawned tasks:

<!--verse
Log(Message:string)<transacts>:void = {}
WaitTicks(Count:int)<suspends>:void = {}
-->
```verse
AsyncOperation()<suspends>:int =
    Log("Start")
    WaitTicks(1)
    Err("Async error")  # Runtime error during async execution
    WaitTicks(1)        # Never executes
    return 1

# Error propagates out of spawned task
spawn { AsyncOperation() }  # Task terminates with runtime error
```

When a spawned task encounters a runtime error, that specific task terminates. The runtime error does not automatically propagate to the spawning context.

### Constructors

Runtime errors during class construction prevent object creation:

<!--verse
Log(Message:string)<transacts>:void = {}
ValidateData()<transacts>:int = Err("Validation failed")
-->
```verse
resource := class:
    Data:int

manager := class:
    var Resource:resource = resource{Data := 0}
    block:
        # Constructor block with validation
        set Resource = resource{Data := ValidateData()}

# Construction fails with runtime error
# Manager := manager{}  # Runtime error, no object created
```

The runtime error prevents the object from being constructed, unwinding any partial initialization.

### Infinite Loop

The Verse VM detects infinite loops and terminates them as runtime errors to prevent program hangs:

<!--verse
NoopSuspend()<suspends>:void = {}
-->
```verse
# Infinite loop - will be detected and terminated
InfiniteLoop()<suspends>:void =
    loop:
        NoopSuspend()  # Suspends but never breaks

# VM terminates this with runtime error after limit exceeded
spawn { InfiniteLoop() }
```

The VM tracks iteration counts and suspension time. When limits are exceeded, it triggers a runtime error with messages like "LoopIterationLimit" or "HangTimeLimit".

### Failure Contexts

Runtime errors can occur within failure contexts (like `if` conditions), and they terminate execution rather than flowing to the else branch:

```verse
CheckAndProcess()<decides>:int =
    if:
        Err("Critical failure")  # Runtime error, not failure
        true = true
    then:
        42
    else:
        0  # Never reached - runtime error bypasses this
```

The runtime error propagates immediately, not treating the condition as failed but as terminated.

## The Deeper Meaning of Failure

Failure in Verse represents more than just a control flow mechanism - it embodies a philosophy about how programs should handle uncertainty and partial information. Instead of defensive programming with extensive error checking, Verse encourages optimistic programming where you attempt operations and handle failure naturally.

This approach aligns with how we think about actions in the real world. When you reach for a book on a shelf, you don't first check if the book exists, then check if your hand can reach it, then check if you can grasp it. You simply reach for it, and deal with failure if it occurs. Verse brings this natural way of thinking into programming.

The unification of validation and action eliminates time-of-check to time-of-use bugs. The speculative execution model makes complex state updates safe. The integration with the type system through effects makes failure handling explicit but not burdensome. Together, these features create a programming model that is both powerful and intuitive.

## Failure and Logic Programming

Verse's approach to failure has roots in logic programming, where computations search for solutions rather than executing steps. When a path fails, the computation backtracks and tries alternatives. This non-deterministic model, while powerful, can be hard to reason about in its full generality.

Verse tames this power by making failure contexts explicit and limiting backtracking to specific constructs. You get the benefits of logic programming - declarative code, automatic search, elegant handling of alternatives - without the complexity of full unification and unbounded backtracking.

Consider a simple logic puzzle solver:

<!--NoCompile-->
```verse
SolvePuzzle(Constraints:[]constraint)<decides>:solution =
    var State:solution = InitialState()
    for (Constraint : Constraints):
        ApplyConstraint(State, Constraint)
    ValidateSolution[State]
    State
```

If any constraint can't be satisfied, the entire attempt fails. In a full logic programming language, this might trigger complex backtracking. In Verse, the failure model is simpler and more predictable while still being expressive enough for most problems.

## Living with Failure

Working effectively with failure in Verse requires a shift in mindset. Instead of thinking about error conditions that need to be avoided, think about success conditions that need to be met. Instead of defensive programming that checks everything before acting, write optimistic code that attempts operations and handles failure gracefully.

This perspective makes code more readable and intent more clear. When you see a function marked with `<decides>`, you know it represents a computation that might not have a result. When you see expressions in sequence within a failure context, you know they represent conditions that must all be met. When you see the `or` operator, you know it represents alternatives to try.

Failure in Verse isn't something to be feared or avoided - it's a tool to be embraced. It makes programs safer by eliminating certain categories of bugs. It makes code clearer by unifying validation and action. It makes complex operations simpler by providing automatic rollback. Most importantly, it aligns the way we write programs with the way we think about actions and decisions in the real world.

As you write more Verse code, you'll find that failure becomes second nature. You'll reach for failable expressions naturally when expressing conditions. You'll structure your functions to fail early when preconditions aren't met. You'll compose failures to create sophisticated control flow without nested conditionals. And you'll appreciate how this different way of thinking about control flow leads to code that is both more robust and more expressive than traditional approaches.
