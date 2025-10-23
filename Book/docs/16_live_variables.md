# Live Variables

Live variables represent a reactive programming paradigm in Verse, enabling variables to automatically recompute their values when dependencies change. Rather than requiring explicit callbacks or event handlers, live variables establish dynamic relationships between data, creating a declarative system where changes propagate naturally through your code.

Traditional programming requires manual tracking of dependencies and explicit updates when values change. If variable `A` depends on variable `B`, you must remember to update `A` whenever `B` changes, often through callback functions or observer patterns. Live variables eliminate this bookkeeping by automatically tracking which variables are read during evaluation and re-evaluating when those dependencies change. This creates more maintainable code where the intent—that `A` should always reflect some function of `B`—is expressed directly in the code itself.

Live variables build a foundation for reactive programming constructs in Verse, including `await`, `upon`, and `when`. Understanding live variables is essential for working with Verse's event-driven programming model, particularly for game development scenarios where many values must stay synchronized.

## Core Concepts

### Live Expressions

A live expression establishes a dynamic relationship between a variable and a target expression. Once established, the target is automatically re-evaluated whenever any of its dependencies change, keeping its variable in sync.

```verse
var X:int = 0
var Y:int = 0
set live X = Y  # X now tracks Y
set Y = 5       # X automatically becomes 5
```

The key insight is that Verse tracks which variables the target expression reads during evaluation. When any of those variables change, the target is re-evaluated, and if the result differs from the current value, the variable updates automatically.

### Declaration Forms

Live variables can be declared in several ways, each suited to different use cases:

```verse
# Live variable declaration
var live X:int = Target

# Live assignment to existing variable
var X:int = 0
set live X = Exp

# Immutable live variable
live Y:int = Exp

# Input-output variable pairs
var live In->Out:int = Exp
```

The most common form, `var live X = Exp`, creates a mutable variable whose initial value comes from evaluating the target and subsequently updates whenever dependencies change. The target expression can read other variables, and those reads are tracked to establish the dependency relationship.

The assignment form, `set live X = Exp`, converts an existing variable into a live variable by attaching a target. This is useful when you need to make a variable reactive after initialization or conditionally based on program state.

Immutable live variables, declared with just `live Y = Exp`, cannot be directly written but still update automatically when their dependencies change. This provides a read-only reactive value, useful for derived computations that should never be manually overridden.

### Input-Output Variables

Input-output variable pairs provide bidirectional synchronization with transformation. The syntax `var live In->Out:t = E` creates two related variables where `Out` is writable and `In` tracks assignments to `Out` through the type expression `t`.

```verse
min(Max:int)(Value:int):int = if (Value < Max) then Value else Max

var MaxHealth:int = 100
var BaseHealth->Health:min(MaxHealth) = 50

# Health = 50 (min(50, 100))
set Health = 75    # BaseHealth = 75, Health = 75
set Health = 120   # BaseHealth = 120, Health = 100 (clamped)
set MaxHealth = 60 # BaseHealth = 120, Health = 60 (reclamped)
```

This pattern elegantly handles common game scenarios where values must stay within dynamic constraints. Writing to `Health` updates both `BaseHealth` (the raw value) and `Health` (the constrained value). When constraints change—like `MaxHealth` decreasing—the constrained value automatically adjusts while preserving the base value for future recalculation.

The scope of input and output variables can be controlled independently: `var In<private>->Out<public>:t = E` makes the base value private while exposing the constrained value publicly.

### Type Expressions with Effects

When the type expression in a live variable declaration has the `<reads>` effect, it can perform additional computation during evaluation. This enables sophisticated reactive patterns where the computed value depends not just on the input but on broader program state.

```verse
var Score:int = 0
var Multiplier:int = 2

ApplyMultiplier<reads>(Value:int):int = Value * Multiplier

var BasePoints->Points:ApplyMultiplier = 10

# Points = 20 (10 * 2)
set Multiplier = 3
# Points = 30 (10 * 3)
set Points = 15
# BasePoints = 15, Points = 45 (15 * 3)
```

The `<reads>` effect allows `ApplyMultiplier` to read `Multiplier`, establishing an additional dependency. When `Multiplier` changes, all variables using `ApplyMultiplier` automatically recompute.

## Reactive Constructs

Live variables form the foundation for three reactive constructs that handle asynchronous events without explicit callbacks: `await`, `upon`, and `when`.

### The await Expression

The `await` expression suspends execution until a target expression succeeds, providing a synchronization primitive for asynchronous programming.

<!--verse
using {/Verse.org/Concurrency}
F()<suspends>:void={
-->
```verse
var X:int = 0

# Suspend until X changes from 0
await{X}
Print("X changed to: {X}")
```
<!--verse
}
-->

The target expression is evaluated immediately. If it fails (returns `false` or produces failure), the task suspends. Verse tracks which variables were read during evaluation. Whenever those variables change, the guard is re-evaluated. If it succeeds, execution resumes immediately.

The practical implications are profound. You can write code that naturally expresses "wait for this condition" without manually managing event handlers or callback registration. The code suspends at the await point and resumes exactly when the condition becomes true.

<!--verse
int_ref := class:
    var Contents:int = 0
F(X:int_ref, Y:int_ref)<suspends>:void={
-->
```verse
# Wait for a specific condition
await{X.Contents > 10}
set Y.Contents = X.Contents * 2
```
<!--verse
}
-->

The guard expression must have effects `<reads><computes><decides>`, meaning it can read variables, perform computation, and potentially fail, but cannot write or allocate. This restriction ensures that evaluating the guard has no side effects, allowing Verse to re-evaluate it freely whenever dependencies change.

### The upon Expression

The `upon` expression provides one-shot reactive behavior: when a condition becomes true, execute some code once. Unlike `await`, which resumes the current task, `upon` creates a new concurrent task that runs when triggered.

```verse
var Health:int = 100
var IsDead:logic = false

upon(Health <= 0):
    set IsDead = true
    Print("Player died!")

set Health = 50  # Nothing happens
set Health = 0   # Triggers: prints "Player died!"
set Health = -10 # Nothing happens (already triggered once)
```

The `upon` expression evaluates its guard immediately and records the variables read. It then yields a `task(void)` that represents the pending reactive behavior. When dependencies change, the guard is re-evaluated. If it succeeds, the body executes once in a new concurrent task, and the upon completes.

This one-shot behavior makes `upon` perfect for state transitions and event notifications. When a threshold is crossed, when a resource becomes available, when a timer expires—these scenarios naturally map to `upon`'s "fire once when condition becomes true" semantics.

The body must have the `<transacts>` effect, meaning it can read and write variables (including other live variables), but its execution is guaranteed to be atomic with respect to notifications.

### The when Expression

The `when` expression provides continuous reactive behavior: every time a condition is true, execute some code. This creates a persistent observer that runs whenever its guard succeeds.

```verse
var Score:int = 0
var DisplayedScore:int = 0

when(Score):
    set DisplayedScore = Score
    Print("Score updated to: {Score}")

set Score = 100  # Triggers: prints "Score updated to: 100"
set Score = 100  # No trigger (value unchanged)
set Score = 200  # Triggers: prints "Score updated to: 200"
```

The `when` expression evaluates its guard immediately. If the guard succeeds, the body executes. Then it records the variables read by the guard and yields a `task(void)`. Whenever dependencies change and the guard succeeds, the body executes again, creating a continuous observation loop.

This makes `when` ideal for maintaining derived state and responding to ongoing changes. Synchronizing UI with game state, updating AI behavior based on player actions, or maintaining consistency between related variables all benefit from `when`'s persistent reactivity.

<!--verse
F():void={
-->
```verse
var X:int = 2
var Y:int = 2

when(Y):
    Z := if (Y < 0) then 0 else Y - 1
    if (Z <> X):
        set X = Z

when(X):
    Z := if (X < 0) then 0 else X - 1
    if (Z <> Y):
        set Y = Z

# These when expressions will stabilize at X = -1, Y = 0
```
<!--verse
}
-->

The body executes with the `<transacts>` effect, and the when immediately re-registers after each execution, creating the continuous observation pattern.

### Cancellation

All three reactive constructs—`await`, `upon`, and `when`—return a `task` that can be canceled, allowing dynamic control over reactive behavior.

<!--verse
using {/Verse.org/Concurrency}
F()<suspends>:void={
-->
```verse
var X:int = 0
var Y:int = 0

Task := upon(X > 5):
    set Y = X

Task.Cancel()  # Cancels the reactive behavior
set X = 10     # Y remains 0
```
<!--verse
}
-->

Canceling a task immediately removes all dependency tracking and prevents the associated code from running. This provides fine-grained control over the lifecycle of reactive behaviors, allowing you to enable and disable observations based on game state or user actions.

## The batch Expression

The `batch` expression groups multiple variable updates together, delaying notifications until the entire group completes. This prevents intermediate states from triggering reactive behaviors and ensures observers see consistent snapshots of related changes.

<!--verse
F()<suspends>:void={
-->
```verse
var X:int = 0
var Y:int = 0

spawn:
    await{X > 1}
    Print("Fired!")

batch:
    set X = 2
    Print("Inside batch")

Print("After batch")

# Output order:
# "Inside batch"
# "Fired!"
# "After batch"
```
<!--verse
}
-->

Inside a `batch` block, variable updates occur immediately but notifications to awaiting tasks and reactive constructs are deferred. When the batch completes, all pending notifications fire in the order their triggers occurred, but observers see the final consistent state rather than intermediate values.

If the same notification occurs twice, only the first of them will be delivered.

Batch expressions nest: notifications are delayed until all enclosing batches complete. This composability ensures that no matter how deeply nested your code, you can guarantee atomic updates of related variables.

The body of a batch must not have the `<suspends>` effect—all operations must complete immediately. This ensures batch blocks have well-defined boundaries and can't leave the system in an inconsistent state by suspending mid-update.

## Special Considerations

### Effect Restrictions

Live variable guards cannot have `<writes>` or `<allocates>` effects. This fundamental restriction prevents side effects during guard evaluation, which Verse must be able to perform freely whenever dependencies change.

```verse
# ERROR: guard cannot write
var X:int = 0
var GlobalCounter:int = 0
set live X = block:
    set GlobalCounter += 1  # Not allowed!
    GlobalCounter
```

This restriction also has a subtle implication: since any variable might become live after creation, reading any variable must be assumed to potentially trigger guard evaluation. The effect system accounts for this: the `<writes>` effect implies `<diverges>` because any write might trigger cyclic live variable evaluation.

### Recursive Targets

If a guard expression reads its own target variable, evaluation diverges into infinite recursion. Verse provides the `Old` function to safely access a variable's current value without triggering guard evaluation.

<!--verse
F():void={
-->
```verse
var X:int = 0

# ERROR: infinite recursion
set live X = X + 1

# Correct: use Old to access current value
set live X = Old(X) + 1
```
<!--verse
}
-->

Now each read of `X` increments its value—`X` becomes 1 on first read, 2 on second read, and so on. The `Old` function breaks the recursion by returning the stored value directly.

Using `Old` in a variable declaration is disallowed: `var live X := 1 + Old(X)` is an error because `X` doesn't have a prior value to access.

### Convergence and Stability

Live variables with interdependencies can form cycles. When guards use idempotent operations and proper guards, these cycles naturally converge to fixed points.

<!--verse
F():void={
-->
```verse
var X:int = 2
var Y:int = 2

set live X = if (Y < 0) then 0 else Y - 1
set live Y = if (X < 0) then 0 else X - 1

# Evaluates as: X=1, Y=0, X=-1, Y=0 (stable)
```
<!--verse
}
-->

The guards are re-evaluated until values stabilize. In this example, `X` decrements to -1, `Y` clamps to 0, and `X` would recompute but produces -1 again, so the system stabilizes.

However, cycles without proper termination conditions can diverge. Verse detects common patterns but cannot prevent all divergence—care must be taken when designing interdependent live variables.

### Turning Off Live Behavior

A live variable established through its guard (not its type) can be turned off by a subsequent regular assignment.

```verse
var X:int = 0
var Y:int = 5
set live X = Y  # X is now live, tracking Y

set Y = 10      # X becomes 10
set X = 20      # X is now a regular variable again
set Y = 15      # X remains 20 (no longer tracking Y)
```

This allows temporary reactive behavior that can be disabled when no longer needed. However, variables that are live through their type expression remain live permanently—their reactive behavior is intrinsic to their type.

### API Design Considerations

Public mutable variables can be made live by external code, potentially violating class invariants. Use access modifiers to control this:

<!--verse
ten_counter := class:
    var<private> X<public>:int = 0

    MakeLive():void =
        set live X = if (Old(X) < 10) then Old(X) + 1 else 0
-->
```verse
ten_counter := class:
    var<private> X<public>:int = 0

    MakeLive():void =
        set live X = if (Old(X) < 10) then Old(X) + 1 else 0
```
<!--verse

-->

Here `X` is publicly visible for reading but can only be made live by the class itself through `MakeLive()`. This prevents external code from attaching arbitrary guards that might break the class's invariants.

### Transactional Behavior

Live variable updates and reactive construct triggers participate in Verse's transactional failure system. If a transaction fails, live variable updates within that transaction are rolled back and their notifications are suppressed.

<!--verse
F()<suspends>:void={
-->
```verse
var X:int = 0
var Y:int = 0

spawn:
    upon(X):
        set Y = X

if:
    set live X = 5  # Establishes live relationship
    false?          # Transaction fails

# Live relationship was not established
set Y = 10  # Y remains 0
```
<!--verse
}
-->

This ensures that reactive behaviors only observe committed changes, maintaining consistency even in the presence of speculative execution and failure.

## Common Patterns

### Clamped Values

Live variables excel at maintaining values within dynamic constraints:

```verse
clamp(Min:int, Max:int)(Value:int):int =
    if (Value < Min) then Min
    else if (Value > Max) then Max
    else Value

var MinHealth:int = 0
var MaxHealth:int = 100
var BaseHealth->Health:clamp(MinHealth, MaxHealth) = 50

set Health = 120    # Health = 100
set MaxHealth = 80  # Health = 80 (reclamped)
set Health = 75     # Health = 75
set MinHealth = 60  # Health = 75 (above new minimum)
```

The health value automatically respects both bounds, and changing the bounds automatically adjusts the current value.

### Derived State Synchronization

When multiple UI elements must reflect game state, `when` provides automatic synchronization:

```verse
var PlayerScore:int = 0
var DisplayedScore:int = 0
var ScoreText:string = ""

when(PlayerScore):
    set DisplayedScore = PlayerScore
    set ScoreText = "Score: {PlayerScore}"
```

Every change to `PlayerScore` automatically updates both the numeric display value and the formatted text, keeping the UI consistent without manual coordination.

### Conditional Reactivity

Live variables can track different sources based on conditions:

```verse
var UseAlternate:logic = false
var PrimaryValue:int = 10
var AlternateValue:int = 20
var CurrentValue:int = 0

set live CurrentValue =
    if (UseAlternate) then AlternateValue else PrimaryValue

# CurrentValue = 10
set UseAlternate = true
# CurrentValue = 20
set AlternateValue = 30
# CurrentValue = 30
set PrimaryValue = 15
# CurrentValue = 30 (still tracking AlternateValue)
```

The dependency tracking is dynamic: when the condition changes, the set of tracked variables changes accordingly, allowing flexible reactive routing.

### Resource Loading

Use `upon` for one-time initialization when resources become available:

<!--verse
ResourceManager := class:
    var TextureLoaded:logic = false
    var ModelLoaded:logic = false

    Initialize()<suspends>:void = {}
-->
```verse
ResourceManager := class:
    var TextureLoaded:logic = false
    var ModelLoaded:logic = false

    Initialize()<suspends>:void =
        upon(TextureLoaded and ModelLoaded):
            Print("All resources loaded, starting game")
            StartGame()
```
<!--verse

-->

This pattern eliminates manual tracking of loading state. When both resources finish loading, the game starts automatically.

## Evolution Considerations

In future versions of a system, it is always allowed to remove `live` from a variable definition. This forward compatibility guarantee means that reactive behavior is an implementation detail that can be optimized away without breaking client code.

Converting a regular variable to a live variable in a new version is generally safe if the computed value matches what the previous version maintained manually. However, if external code depends on being able to set arbitrary values, this could break expectations.

The ability to cancel reactive constructs provides an important upgrade path: code that creates `when` or `upon` observers can later be modified to cancel them under different conditions without breaking existing behavior.
