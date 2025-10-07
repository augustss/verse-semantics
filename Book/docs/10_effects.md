# Effects

Every function tells two stories. The first story, told through types, describes what data flows in and what data flows out. The second story, told through effects, describes what the function does along the way — whether it reads from memory, writes to storage, might fail, or could suspend execution. While most languages leave this second story implicit, Verse makes it explicit, turning side effects from hidden surprises into documented contracts.

Think about a simple game function that updates a player's score. In most languages, you'd see a signature like `UpdateScore(player, points)` and have to guess what happens inside. Does it modify the player object? Write to a database? Print to a log? Trigger animations? Without reading the implementation, you can't know. In Verse, effects are part of the signature itself, declaring upfront exactly what kinds of operations the function might perform.

This explicitness might seem like extra work at first, but it fundamentally changes how you reason about code. When you see `<reads>` on a function, you know it observes mutable state. When you see `<writes>`, you know it modifies that state. When you see `<decides>`, you know it might fail. These aren't comments or documentation that might be wrong — they're compiler-enforced contracts that must be accurate.

## Understanding Effects

Effects represent observable interactions between your code and the world around it. Reading a player's health, updating a score, spawning a particle effect, waiting for an animation to complete — all these operations have effects that ripple beyond simple computation. Verse's effect system captures these interactions, making them visible and verifiable.

Consider this simple function that greets a player:

<!--NoCompile-->
```verse
GreetPlayer()<writes>:void =
    set CurrentGreeting = "Hello, adventurer!"
    Print(CurrentGreeting)
```

The `<writes>` effect tells you immediately that this function modifies mutable state. You don't need to read the implementation to know that calling `GreetPlayer()` will change something in your program's memory. The effect is a promise about behavior, checked and enforced by the compiler.

Effects compose naturally through function calls. If function A calls function B, and B has certain effects, then A must declare at least those same effects (with some exceptions we'll explore). This propagation ensures that effects can't be hidden or laundered through intermediate functions — the true nature of an operation is always visible at every level of the call stack.

## Why Effects Matter

Making effects explicit serves both human understanding and compiler optimization. For developers, effects act as documentation that can't lie. When you're debugging why a value changed unexpectedly, you can trace through the call chain looking only at functions with `<writes>`. When you're trying to understand why a function might fail, you look for `<decides>`. This isn't guesswork — it's guaranteed by the type system.

For the compiler, explicit effects enable powerful optimizations and safety guarantees. Pure functions marked `<computes>` can be memoized, their results cached because they'll always return the same output for the same input. Functions without `<writes>` can be safely executed in parallel without locks. Functions without `<decides>` can be called without failure handling.

The effect system also enforces architectural decisions. Want to ensure your math library remains pure? Mark its functions `<computes>`. Building a predictive client system that must run on players' machines? Use `<predicts>` to ensure no server-only operations sneak in. These aren't just conventions — they're compiler-enforced guarantees.

## Effect Families and Specifiers

Verse organizes effects into families, each tracking a specific aspect of computation. Each family contains fundamental effects, and effect specifiers declare which effects a function may perform.

The six effect families are:

* **Cardinality**: Whether and how a function returns
* **Heap**: Access to mutable memory
* **Suspension**: Whether a function may suspend execution
* **Divergence**: Whether a function may run forever
* **Prediction**: Where a function runs
* **Internal**: Reserved for internal use

Some effects have no specifier, while some specifiers imply multiple effects. For instance, `<transacts>` implies `reads`, `writes`, and `allocates`, and belongs to both the Heap and Internal families.

|Fundamental Effect|Effect Specifier|Effect Family|Effects implied by Specifier | Notes |
| ----- | ----------- | ------- | ----- | ---- |
| **succeeds** | | Cardinality | | *No specifier* |
| **fails** | | Cardinality | | *No specifier* |
| | `<decides>` | Cardinality | `{succeeds, fails}` | |
| | `<ambiguates>` | Cardinality | | *Planned* |
| | `<abstracts>` | Cardinality | | *Planned* |
| | `<iterates>` | Cardinality | | *Planned* |
| **reads** | `<reads>` | Heap | `{reads}` | |
| **writes** | `<writes>` | Heap | `{writes}` | |
| **allocates** | `<allocates>` | Heap | `{allocates}` | |
| | `<transacts>` | Heap | `{reads, writes, allocates}` | |
| | `<computes>` | Heap | `{}` | |
| **suspends** | `<suspends>` | Suspension | `{suspends}` | |
| **diverges** | | Divergence | `{diverges}` | *No specifier* |
| | `<converges>` | Divergence | `{}` | |
| **dictates** | | Prediction | `{dictates}` | *No specifier* |
| | `<predicts>` | Prediction | `{}` | |
| **no_rollback** | | Internal | `{no_rollback}` | *To be deprecated* |
| | `<transacts>` | Internal | `{}` | |

There is another planned specifier, `<interacts>`, expected to be used for code with external effects like network communication or user interaction. The `<ambiguates>` and `<abstracts>` specifiers are key to planned logic features, denoting functions that may return different values due to the choice operator. `<diverges>` and `<converges>` will indicate whether a function may not terminate or is guaranteed to return in finite steps.

## How Effects Compose

Think of effect specifiers as setting bits in a bit vector: one bit per fundamental effect. Without any annotation, a function has the following effects:

<!--NoCompile-->
```verse
GameUpdate():void = ...  # No explicit effects specified
```

| dictates | suspends | reads | writes | allocates | succeeds | fails |
| :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| ✔️ | ❌ | ✔️ | ✔️ | ✔️ | ✔️ | ❌ |

This means the function has `dictates`, `reads`, `writes`, `allocates` and `succeeds` effects. It's almost like writing `<dictates><transacts>` except we lack a way to specify that the function cannot fail. A specifier like `<fails>` would have limited use since a function that always fails never returns a value and cannot have observable side effects (they would be undone by failure). The `<succeeds>` specifier is implicit.

Annotating a function only affects the bits in that specifier's family. For example, a function with `<reads>` and `<predicts>`:

<!--NoCompile-->
```verse
CheckPlayerStatus()<reads><predicts>:string = ...
```

| dictates | suspends | reads | writes | allocates | succeeds | fails |
| :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| ❌ | ❌ | ✔️ | ❌ | ❌ | ✔️ | ❌ |

Specifying `<reads><predicts>` clears the `writes` and `allocates` bits, clears the `dictates` bit, and leaves everything else unchanged.

## Effect Families in Detail

### The Cardinality Family: Success and Failure

The cardinality family deals with whether functions return values successfully. Every function either succeeds (returning its declared type) or fails (producing no value). Most functions always succeed — they're deterministic transformations that always produce output. But functions marked with `<decides>` can fail, turning failure into a control flow mechanism.

<!--verse
ValidateHealth(Health:float)<transacts><decides>:void =
    Health > 0.0      # Fails if health is zero or negative
    Health <= 100.0   # Fails if health exceeds maximum
StartCombat():void={}
player:=struct{Health:float}
F(Player:player):void={
if (ValidateHealth[Player.Health]):
    # Health is valid, continue processing
    StartCombat()
}<#
-->
```verse
ValidateHealth(Health:float)<transacts><decides>:void =
    Health > 0.0      # Fails if health is zero or negative
    Health <= 100.0   # Fails if health exceeds maximum

# Usage
    if (ValidateHealth[Player.Health]):
    # Health is valid, continue processing
    StartCombat()
```
<!--verse
#>
-->

The beauty of the decides effect is that it unifies validation with control flow. You don't check conditions and then act on them — the check itself drives the program's path.

### The Heap Family: Memory Access

The heap family governs access to mutable memory. This is perhaps the most important family for understanding program behavior, as it determines whether functions can observe or modify state.

The `<computes>` specifier marks pure functions — those that neither read nor write mutable state. These functions are deterministic: given the same inputs, they always produce the same outputs. They're the mathematical ideal of computation, transforming data without side effects.

```verse
CalculateDamage(BaseDamage:float, Multiplier:float)<computes>:float =
    BaseDamage * Multiplier
```

The `<reads>` effect allows functions to observe mutable state. They can see the current values of variables and mutable fields, but cannot modify them. This is useful for queries and calculations based on current game state.

```verse
player := class:
    Name:string
    var Health:float = 100.0
    var Score:int = 0

GetPlayerStatus(P:player)<reads>:string =
    if (P.Health > 50.0):
        "Healthy"
    else if (P.Health > 0.0):
        "Injured"
    else:
        "Defeated"
```

The `<writes>` effect permits modification of mutable state. Functions with this effect can use `set` to update variables and mutable fields. Note that `<writes>` usually requires `<reads>` as well, since modification often involves reading the current value first. The `reads` effect is needed due to a planned feature: live variables.

<!--verse
player := class:
    Name:string
    var Health:float = 100.0
    var Score:int = 0
-->
```verse
HealPlayer(P:player, Amount:float)<transacts>:void =
    NewHealth := P.Health + Amount
    set P.Health = Min(NewHealth, 100.0)
```

The `<allocates>` effect indicates functions that create observably unique values — either objects marked `<unique>` or values containing mutable fields. Each call to such a function returns a distinct value, even if the inputs are identical.

<!--verse
vector3:=struct{}
GenerateID():int=0
-->
```verse
game_entity := class<allocates>:
    ID:int
    var Position:vector3

CreateEntity(Pos:vector3)<allocates>:game_entity =
    game_entity{ID := GenerateID(), Position := Pos}
```

The `<transacts>` specifier combines `<reads>`, `<writes>`, and `<allocates>`, providing full access to the mutable heap. It's the default for most functions that work with game state. Additionally, `<transacts>` provides special semantics for failure handling — if a transactional function fails, all its heap modifications are rolled back, as if they never happened.

### The Suspension Family: Time and Waiting

The suspension family contains a single effect: `<suspends>`. Functions with this effect can pause their execution and resume later, potentially across multiple game frames. This is essential for operations that take time: animations, cooldowns, waiting for player input, or any multi-frame behavior.

<!--NoCompile-->
```verse
PlayVictorySequence()<suspends>:void =
    PlayAnimation(VictoryDance)
    Sleep(2.0)  # Wait 2 seconds
    PlaySound(VictoryFanfare)
    Sleep(1.0)
    ShowRewardsScreen()
```

The `suspends` effect is viral — any function that calls a suspending function must itself be marked `<suspends>`. This ensures you always know which functions might take time to complete.

### The Prediction Family: Client-Server Execution

The prediction family determines where code runs in a client-server architecture. By default, functions have the `dictates` effect, meaning they run authoritatively on the server. The `<predicts>` specifier allows functions to run predictively on clients for responsiveness, with the server later validating and potentially correcting the results.

<!--NoCompile-->
```verse
HandleJumpInput()<predicts>:void =
    # Runs immediately on the client for responsiveness
    StartJumpAnimation()
    PlayJumpSound()

    # Server will validate and correct if needed
    PerformJump()
```

This enables responsive gameplay even with network latency, as players see immediate feedback for their actions while the server maintains authoritative state.

### The Divergence Family: Termination Guarantees

Currently in planning, the divergence family will track whether functions are guaranteed to terminate. The `<converges>` specifier will mark functions that provably complete in finite time, while functions without it might run forever. This is particularly important for constructors and initialization code.

## Effect Composition and Hiding

Effects generally propagate up the call chain — a function must declare all the effects of the functions it calls. However, certain language constructs can hide specific effects, preventing them from propagating further.

The `if` expression hides the `fails` effect when used for control flow. If a failable expression appears in a condition, the failure doesn't propagate to the enclosing function:

```verse
SafeMod(A:int, B:int)<computes>:int =
    if (V:= Mod[A,B])  then V else 0
```

The `spawn` expression hides the `suspends` effect, allowing immediate functions to start asynchronous operations that continue independently:

<!-- TODO DOES NOT COMPILE -->

<!--verse
Sleep(:float):void={}
GetNextTrack():int=0
PlayTrack(:int)<suspends>:void={}
-->
```verse
StartBackgroundMusic():void =  # Note: no <suspends>
    spawn:
        loop:
            PlayTrack(GetNextTrack())
            Sleep(180.0)  # Suspends effect hidden by spawn
```

The `option` expression converts failure into an optional value, transforming the `fails` effect into a regular value that can be handled without `<decides>`:

<!--verse
item:=struct{}
-->
```verse
TryGetItem(Items:[]item, Index:int):?item =
    option{Items[Index]}  # Array access might fail, option catches it
```

## Effects on Data Types

Classes, structs, and interfaces can be annotated with effect specifiers, which apply to their constructors. This is particularly useful for ensuring that creating certain objects remains pure or has limited effects:

```verse
# Pure data structure - constructor has no effects
vector3 := struct<computes>:
    X:float = 0.0
    Y:float = 0.0
    Z:float = 0.0

# Entity that requires allocation due to unique identity
monster := class<unique><allocates>:
    Name:string
    var Health:float = 100.0
```

Limiting constructor effects helps maintain architectural boundaries. Data transfer objects can be kept pure with `<computes>`, ensuring they're just data carriers. Game entities might require `<allocates>` for unique identity, while service objects might need full `<transacts>` to initialize their state.

## Working with Effects

When designing functions, start with the minimal effects needed and expand only when necessary. Pure functions with `<computes>` are the easiest to test, reason about, and compose. Add `<reads>` when you need to observe state, `<writes>` when you need to modify it, and `<decides>` when you need failure-based control flow.

Effects are part of your API contract. Once published, removing effects is a backwards-compatible change (your function does less than before), but adding effects is breaking (your function now does more than callers might expect). Design your effect signatures thoughtfully, as they become promises to your users.

Remember that over-specifying effects is allowed and sometimes beneficial. A function marked `<reads>` can be implemented as pure `<computes>` internally. This provides flexibility for future changes without breaking existing callers.

<!--verse
weapon:=struct<computes>{Type:weapon_type,Dammage:int}
weapon_type:=enum:
    Sword
-->
```verse
# API promises it might read state
GetDefaultWeapon<public>()<reads>:weapon =
    # But current implementation is pure
    weapon{Type := weapon_type.Sword, Damage := 10}
```

Effect over-specification can future-proof APIs and avoid breaking changes later. For example, marking a currently pure function as `<reads>` allows you to add state observation in the future without breaking compatibility.

## Backwards Compatibility

The effects of a function are part of what is checked for backwards compatibility. When updating a function that is part of a published API, the new version can have "fewer bits" but not more. So, a function that was marked as `<reads>` in a previous version cannot be changed to `<transacts>`, but it can be refined to `<computes>`.

Effects transform side effects from hidden gotchas into visible, verifiable contracts. By making the implicit explicit, Verse helps you write more predictable, maintainable, and correct code. The effect system isn't a burden — it's a tool that helps you express your intent clearly and have the compiler verify that your implementation matches that intent.
