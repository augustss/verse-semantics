# Concurrency

Concurrency is a fundamental aspect of Verse, allowing you to control time flow as naturally as you control program flow. Unlike traditional programming languages that bolt on concurrency as an afterthought, Verse integrates time flow control directly into the language through dedicated expressions and effects.

Game development inherently requires managing multiple simultaneous activities. Think about a typical game scene: NPCs patrol their routes while particle effects play, UI elements animate as cooldown timers count down, and background music fades between tracks. All these activities happen concurrently, overlapping in time. Verse recognizes this reality and provides first-class language constructs to express these parallel behaviors naturally.

The language achieves this through a combination of structured and unstructured concurrency primitives, all built on the concept of async expressions that can suspend and resume across multiple simulation updates. This approach makes concurrent programming feel as natural as writing sequential code, while avoiding the traditional pitfalls of thread-based concurrency like data races and deadlocks.

## Core Concepts

### Immediate vs Async Expressions

Every expression falls into one of two categories: immediate or async. Understanding this distinction is crucial for working with Verse's concurrency model.

Immediate expressions evaluate with no delay, completing entirely within the current simulation update or frame. These include most basic operations you'd expect to happen instantly: arithmetic calculations, variable access, simple function calls, and data structure manipulation. When you write `X := 5 + 3`, the addition happens immediately, the assignment completes instantly, and execution moves to the next statement without any possibility of interruption.

Async expressions, on the other hand, have the possibility of taking time to evaluate, potentially spanning multiple simulation updates. They represent operations that inherently take time in the game world: animations playing out, timers counting down, network requests completing, or simply waiting for the next frame. An async expression might complete immediately if its conditions are already met, or it might suspend execution, allowing other code to run while it waits for the right moment to resume.

### Simulation Updates

A simulation update represents one tick of the game's simulation, typically corresponding to a frame being rendered. Most games target 30 or 60 updates per second, creating the smooth motion players expect. Each update processes input, updates game logic, runs physics simulations, and prepares the next frame for rendering.

In networked games, the relationship between simulation updates and rendering becomes more complex. Multiple simulation updates might occur before rendering to maintain synchronization with the server, or updates might be interpolated to smooth out network latency. Verse's concurrency model abstracts these complexities, allowing you to think in terms of logical time flow rather than platform-specific timing details.

Async expressions naturally align with this update cycle. When an async expression suspends, it yields control back to the game engine, which continues processing other tasks and rendering frames. The suspended expression resumes in a future update when its conditions are met, seamlessly continuing from where it left off. This cooperative model ensures that long-running operations don't block the game's responsiveness.

### The `suspends` Effect

The `suspends` effect marks functions that can perform async operations, serving as the gateway between immediate and async execution contexts. When you mark a function with `<suspends>`, you're declaring that this function might take time to complete and needs the ability to pause and resume its execution.

<!--verse
using {/Verse.org/VerseCLR}
-->
```verse
# Function marked with suspends can use async expressions
MyAsyncFunction()<suspends>:void =
    Sleep(1.0)  # Wait for 1 second
    Print("One second later!")

# Regular functions cannot use async expressions
MyImmediateFunction():void =
    # Sleep(1.0)  # ERROR: Cannot use Sleep without suspends
    Print("This happens immediately")
```

Functions with the `suspends` effect gain powerful capabilities. They can call other suspending functions, creating chains of async operations that flow naturally. They can use concurrency expressions like `sync`, `race`, `rush`, and `branch` to orchestrate multiple simultaneous activities. They have access to timing functions like `Sleep()` that pause execution for specified durations. Most importantly, they cooperatively yield control to other concurrent tasks, ensuring the game remains responsive even during complex operations.

The `suspends` effect propagates through the call chain. If function A calls suspending function B, then A must also be marked with `<suspends>`. This explicit marking helps you understand at a glance which functions might take time and which are guaranteed to complete immediately.

## Structured Concurrency

Structured concurrency represents one of Verse's most elegant design decisions. Rather than spawning threads or tasks that live independently and require manual lifecycle management, structured concurrency expressions have lifespans naturally bound to their enclosing scope. When you enter a structured concurrency block, you know that all concurrent operations within it will be properly managed and cleaned up when the block exits, preventing resource leaks and making code easier to reason about.

This approach mirrors how we think about sequential code. Just as a block of sequential statements has a clear beginning and end, structured concurrent operations have a defined lifetime. You can nest them, compose them, and reason about them using the same mental model you use for regular code blocks.

### The sync Expression

The `sync` expression embodies the simplest concurrent pattern: doing multiple things at once and waiting for all of them to finish. When you have independent operations that can benefit from parallel execution, `sync` provides a clean way to express this parallelism while maintaining deterministic behavior.

<!--verse
using{/Verse.org/VerseCLR}
AsyncOperation1()<suspends>:int=1
AsyncOperation2()<suspends>:int=1
AsyncOperation3()<suspends>:int=1
F():void={
-->
```verse
# All expressions start simultaneously and must all complete
Results := sync:
    AsyncOperation1()  # Returns value1
    AsyncOperation2()  # Returns value2
    AsyncOperation3()  # Returns value3

# Results is a tuple containing (value1, value2, value3)
Print("All operations complete with results: {Results}")
```
<!--verse
}
-->

Inside a `sync` block, all subexpressions begin execution at essentially the same moment. The sync expression then waits patiently for every single subexpression to complete, regardless of how long each takes individually. If one operation finishes in milliseconds while another takes several seconds, sync continues waiting until that last operation completes. Only then does execution continue past the sync block.

The beauty of sync lies in its predictability. You always get results from all subexpressions, always in the same order you wrote them, packaged neatly in a tuple. This makes sync perfect for scenarios where you need multiple pieces of data or need to ensure multiple systems are ready before proceeding. Loading game assets in parallel, initializing multiple subsystems simultaneously, or gathering data from multiple sources all benefit from sync's all-or-nothing approach.

Consider a more sophisticated example that demonstrates sync's composability:

<!--verse
LoadTexture()<suspends>:void={}
ApplyTexture()<suspends>:void={}
LoadSound()<suspends>:void={}
PlaySound()<suspends>:void={}
LoadModel():void={}
ProcessData(:int,:int,:int):void={}
FetchDataA()<suspends>:int={}
FetchDataB()<suspends>:int={}
FetchDataC():int={}
F()<suspends>:void={
sync:
    block:  # Task 1 - sequential operations
        LoadTexture()
        ApplyTexture()
    block:  # Task 2 - parallel to task 1
        LoadSound()
        PlaySound()
    LoadModel()  # Task 3 - parallel to tasks 1 and 2
ProcessData(sync:
    FetchDataA()
    FetchDataB()
    FetchDataC())
}<#
-->
```verse
# Nested blocks for complex operations
sync:
    block:  # Task 1 - sequential operations
        LoadTexture()
        ApplyTexture()
    block:  # Task 2 - parallel to task 1
        LoadSound()
        PlaySound()
    LoadModel()  # Task 3 - parallel to tasks 1 and 2

# Using sync results directly as function arguments
ProcessData(sync:
    FetchDataA()
    FetchDataB()
    FetchDataC())
```
<!--verse
#>
-->

### The race Expression

Where `sync` embodies cooperation, `race` represents competition. The race expression starts multiple async operations simultaneously, but only cares about the first one to cross the finish line. As soon as one subexpression completes, race immediately cancels all the others and continues with the winner's result. This winner-takes-all semantics makes race perfect for timeout patterns, fallback mechanisms, and any situation where you want the fastest possible response.

<!--verse
SlowOperation():int=0
FastOperation()   :int=0
MediumOperation()   :int=0
F():void={
-->
```verse
# First to complete wins, others are canceled
Winner := race:
    SlowOperation()     # Takes 5 seconds
    FastOperation()     # Takes 1 second - wins!
    MediumOperation()   # Takes 3 seconds

Print("Winner result: {Winner}")  # Prints FastOperation's result
```
<!--verse
}
-->

The power of race becomes apparent when you consider real game scenarios. Imagine querying multiple servers for data, where you want to use whichever responds first. Or implementing a player action with a timeout, where either the player completes the action or time runs out. Race elegantly expresses these patterns without complex state management or manual cancellation logic.

Cancellation in race is immediate and thorough. The moment a winner emerges, all losing subexpressions receive a cancellation signal and begin cleanup. This isn't just an optimization; it's crucial for resource management and preventing unwanted side effects from operations that are no longer needed.

The type system handles race elegantly too. Since only one subexpression's result will be returned, the result type of a race is the most specific common supertype of all the subexpressions. This ensures type safety while maintaining flexibility in what kinds of operations you can race against each other.

A pattern involves adding identifiers to determine which subexpression won:

<!--verse
SlowOperation():int=0
FastOperation()   :int=0
MediumOperation()   :int=0
F():void={
-->
```verse
# Adding identifiers to determine which expression won
WinnerID := race:
    block:
        SlowOperation()
        1  # Return 1 if this wins
    block:
        FastOperation()
        2  # Return 2 if this wins
    block:
        loop:
            InfiniteOperation()
        3  # Never returns

case(WinnerID):
    1 => Print("Slow operation won somehow!")
    2 => Print("Fast operation won as expected")
    _ => Print("Impossible!")
```
<!--verse
}
-->

### The rush Expression

The `rush` expression occupies a unique middle ground between `sync` and `race`. Like race, it completes as soon as the first subexpression finishes. Unlike race, it doesn't cancel the losers. This creates an interesting pattern where you can start multiple operations, proceed as soon as one provides a result, while allowing the others to continue their work in the background.

<!--verse
using{/Verse.org/VerseCLR}
LongBackgroundTask():int=0
QuickCheck()   :int=0
MediumTask()   :int=0
F():void={
-->
```verse
# First to complete allows continuation, others keep running
FirstResult := rush:
    LongBackgroundTask()   # Continues after rush completes
    QuickCheck()          # Finishes first
    MediumTask()          # Also continues after rush

Print("First result: {FirstResult}")
# LongBackgroundTask and MediumTask are still running!
```
<!--verse
}
-->

Rush shines in scenarios where you want to be responsive while still completing all operations eventually. Consider preloading game assets: you might start loading multiple levels simultaneously, begin gameplay as soon as the current level loads, while continuing to cache the other levels in the background. Or think about achievement checking, where you want to notify the player as soon as one achievement unlocks while continuing to check for others.

The non-canceling nature of rush requires careful consideration. Those background tasks continue consuming resources and performing their operations even after rush completes. They'll keep running until they naturally complete or until their enclosing async context ends. This makes rush powerful but also potentially dangerous if misused with operations that might never complete or that consume significant resources.

There's an important technical restriction to be aware of: rush cannot be used directly in the body of iteration expressions like `loop` or `for`. The interaction between rush's background tasks and iteration could lead to resource accumulation. If you need rush-like behavior in a loop, wrap it in an async function and call that function from your iteration.

### The branch Expression

The `branch` expression represents fire-and-forget concurrency within a structured context. When you encounter a branch, it immediately starts executing its body as a background task and then, without any pause or hesitation, continues with the next expression. There's no waiting, no result collection, just a task spinning off to do its work while the main flow proceeds unimpeded.

<!--verse
using{/Verse.org/VerseCLR}
AsyncOperation1()<suspends>:int=0
ImmediateOperation()   :int=0
AsyncOperation2() <suspends>  :int=0
F():void={
-->
```verse
branch:
    # This block runs independently
    AsyncOperation1()
    ImmediateOperation()
    AsyncOperation2()

# Execution continues immediately here
Print("Branch started, continuing main flow")
# Branch block is still running in background
```
<!--verse
}
-->

Branch excels at handling side effects that shouldn't interrupt the main game flow. Think about logging player actions to analytics, triggering particle effects that play out over time, or starting background music that fades in gradually. These operations need to happen, but there's no reason to make the player wait for them to complete. Branch lets you express this "start it and move on" pattern directly.

The relationship between a branch and its enclosing scope maintains the structured concurrency guarantee. While the branch task runs independently, it's still tied to the lifetime of its parent async context. If that parent context completes, either naturally or through cancellation, the branch task is automatically canceled too. This prevents orphaned tasks from accumulating and consuming resources indefinitely.

Like rush, branch faces restrictions with iteration expressions. You cannot use branch directly inside a loop or for body, as this could lead to an unbounded number of background tasks. The workaround remains the same: encapsulate the branch in an async function and call that function from your iteration.

## Unstructured Concurrency

### The spawn Expression

While structured concurrency handles most concurrent programming needs elegantly, sometimes you need to break free from the hierarchical task structure. The `spawn` expression is Verse's single concession to unstructured concurrency, allowing you to start an async operation that lives independently of its creating scope. Think of spawn as an emergency escape hatch—powerful when needed, but not your first choice for typical concurrent patterns.

<!--verse
using{/Verse.org/VerseCLR}
LongRunningTask()   :int=0
F():void={
-->
```verse
# Can be used in ANY context (async or immediate)
spawn{LongRunningTask()}
Print("Spawned task continues even after this scope exits")
```
<!--verse
}
-->

What makes spawn unique is its ability to work anywhere. Unlike all the structured concurrency expressions that require an async context, spawn works in immediate functions, class constructors, module initialization—anywhere you can write code. This universality comes with responsibility. The task you spawn becomes a free agent, continuing its work regardless of what happens to the code that created it. There's no automatic cleanup, no parent-child relationship, just an independent task pursuing its goal.

The syntax deliberately constrains spawn to launching a single function call. You can't spawn a block of code with multiple operations; you're limited to spawning one async function. This constraint encourages you to think carefully about what you're spawning and encapsulate complex operations properly in functions rather than creating ad-hoc background tasks.

Spawn finds its place in specific architectural patterns. Global background services that monitor game state throughout the entire session, cleanup tasks that must complete even if the triggering context ends, or integration points where immediate code needs to trigger async operations—these scenarios justify reaching for spawn over the structured alternatives.

The contrast with branch illuminates the design philosophy. Branch gives you structured concurrency's safety within an async context, allowing multiple expressions in its body while maintaining parent-child relationships. Spawn trades these safeguards for the flexibility to work anywhere, but restricts you to a single function call. Each has its place, and choosing between them depends on whether you need structure or freedom.

## Tasks and Task Management

Behind every async operation lies a task—a runtime representation of executing concurrent code. While Verse abstracts away most task management details, understanding the task model helps you reason about concurrent behavior and debug complex scenarios.

When an async expression begins execution, the runtime creates a task to track its progress. This task moves through a well-defined lifecycle, starting in the running state as it actively executes code. When the task encounters a suspension point like `Sleep()` or waits for another async operation, it transitions to the suspended state, freeing up computational resources for other tasks. Eventually, the task either completes successfully, reaching its natural end, or gets canceled due to external factors.

Task cancellation in Verse follows a cooperative model. Rather than forcefully terminating tasks, which could leave resources in inconsistent states, Verse sends cancellation signals that tasks check at suspension points. When a task receives a cancellation signal, it has the opportunity to clean up resources before terminating. This cooperative approach prevents data corruption while ensuring responsive cancellation.

Cancellation cascades through the task hierarchy. When a parent task is canceled, all its child tasks receive cancellation signals too. This cascading behavior maintains the invariant that child tasks don't outlive their parents in structured concurrency, preventing resource leaks and ensuring predictable cleanup. In a race expression, for example, when the winner completes, the race task sends cancellation signals to all losing subtasks, which then cascade to any tasks those losers might have created.

## Timing Functions

The fundamental timing function that suspends execution for a specified duration:

```verse
# Suspend for 1 second
Sleep(1.0)

# Suspend for one frame (smallest possible delay)
Sleep(0.0)
```
<!--verse
ProcessFrame():void={}
F():void={
-->
```verse
# Common patterns
LoopWithDelay()<suspends>:void =
    loop:
        ProcessFrame()
        Sleep(0.033)  # ~30 FPS
```
<!--verse
}
-->

Timing Patterns are:

<!--verse
DoAction():void={}
UpdateLogic:void={}
Lerp(:float,:float,:float):int=0
SetPosition(:int):void={}
-->
```verse
# Delayed action
PerformDelayedAction()<suspends>:void =
    Sleep(2.0)  # Wait 2 seconds
    DoAction()

# Periodic execution
PeriodicUpdate()<suspends>:void =
    loop:
        UpdateLogic()
        Sleep(1.0)  # Update every second

# Animation timing
AnimateMovement(Start:float,End:float)<suspends>:void =
    for (T : 0.0..1.0):
        SetPosition(Lerp(Start, End, T))
        Sleep(0.0)  # One frame
```

## Common Patterns and Best Practices

Implement operations with timeouts using `race`:

<!--verse
ActualOperation():void={}
-->
```verse
PerformWithTimeout()<suspends>:logic =
    race:
        block:
            ActualOperation()
            true  # Success
        block:
            Sleep(5.0)  # 5 second timeout
            false  # Timeout
```

Initialize multiple systems concurrently:

<!--verse
using{/Verse.org/VerseCLR}
LoadAssets():void={}
ConnectToServer():void={}
InitializeUI():void={}
PrepareAudio():void={}
-->
```verse
InitializeGame()<suspends>:void =
    sync:
        LoadAssets()
        ConnectToServer()
        InitializeUI()
        PrepareAudio()
    Print("Game ready!")
```

Start background tasks that don't block gameplay:

<!--verse
MonitorPlayerStats():void={}
UpdateLeaderboards():void={}
ProcessAchievements():void={}
-->
```verse
StartBackgroundSystems()<suspends>:void =
    branch:
        MonitorPlayerStats()
    branch:
        UpdateLeaderboards()
    branch:
        ProcessAchievements()
    # Main game continues while background tasks run
```

Spawn entities with delays:

<!--verse
enemy_class := class:
    Spawn():void={}
-->
```verse
SpawnWave(Enemies:[]enemy_class)<suspends>:void =
    for (Enemy : Enemies):
        spawn{Enemy.Spawn()}
        Sleep(0.5)  # Half second between spawns
```

Animate multiple objects simultaneously:

<!--verse
platform:=class:
    Animate():void={}
-->
```verse
AnimateAllPlatforms(Platforms:[]platform)<suspends>:void =
    sync:
        for (Platform : Platforms):
            branch:
                Platform.Animate()
```

## Error Handling in Concurrent Code

### Failure Propagation

Failures in concurrent expressions propagate differently:

<!--verse
OperationThatSucceeds():void={}
OperationThatFails():void={}
AnotherOperation():void={}
F()<suspends>:void={
-->
```verse
# In sync: all expressions complete, then failure propagates
sync:
    OperationThatSucceeds()
    OperationThatFails()  # Failure occurs
    AnotherOperation()    # Still executes
# Entire sync fails after all complete

# In race: winner determines success/failure
race:
    OperationThatFails()   # If this wins, race fails
    OperationThatSucceeds() # If this wins, race succeeds
```
<!--verse
}
-->

### Defensive Patterns

<!--verse
using{/Verse.org/VerseCLR}
RiskyOperation():void={}
HandleFailure():void={}
-->
```verse
# Safe concurrent operation with fallback
SafeConcurrentOp()<suspends>:void =
    if (race:
        block:
            RiskyOperation()
            true
        block:
            Sleep(10.0)  # Timeout fallback
            false):
        Print("Operation succeeded")
    else:
        Print("Operation failed or timed out")
        HandleFailure()
```

## Performance Considerations

### Granularity

Balance between too many small tasks and too few large tasks:

<!--verse
ProcessItem(:int):void={}
ProcessItemBatch(:[]int):void={}
F()<suspends>:void={
-->
```verse
# Too fine-grained (overhead)
sync:
    for (Item : Items):
        spawn{ProcessItem(Item)}  # Creates many tasks

# Better - batch processing
sync:
    ProcessItemBatch(Items[0..99])
    ProcessItemBatch(Items[100..199])
    ProcessItemBatch(Items[200..299])
```
<!--verse
}
-->

### Resource Management

Be mindful of long-running tasks:

<!--NoCompile-->
```verse
# Potential resource leak
rush:
    InfiniteMonitoring()  # Continues forever
    QuickCheck()

# Better - controllable lifetime
MonitorWithLifetime()<suspends>:void =
    race:
        InfiniteMonitoring()
        Sleep(60.0)  # Maximum 60 second lifetime
```

### Suspension Points

Minimize suspensions in tight loops:

<!--verse
ProcessItem(:int):void={}
F()<suspends>:void={
-->
```verse
# Inefficient - suspends every iteration
for (I := 0..1000):
    ProcessItem(I)
    Sleep(0.0)  # Unnecessary suspension

# Better - batch before suspending
for (Batch := 0..10):
    for (I := Batch*100..(Batch+1)*100):
        ProcessItem(I)
    Sleep(0.0)  # Suspend between batches
```
<!--verse
}
-->

## Debugging Concurrent Code

### Tracing Execution

Add logging to understand execution order:

<!--verse
using{/Verse.org/VerseCLR}
-->
```verse
DebugConcurrency()<suspends>:void =
    sync:
        block:
            Print("Task 1 start")
            Sleep(1.0)
            Print("Task 1 end")
        block:
            Print("Task 2 start")
            Sleep(0.5)
            Print("Task 2 end")
```

## Limitations and Considerations

### Iteration Restrictions

The interaction between iteration and certain concurrency expressions requires careful consideration. Rush and branch cannot be used directly inside loop or for bodies, a restriction that prevents unbounded task accumulation. When you write a loop that might execute hundreds or thousands of times, allowing rush or branch directly would create that many background tasks, potentially overwhelming the system.

<!--verse
Operation1():void={}
Operation2():void={}
F()<suspends>:void={
-->
```verse
# Not allowed
for (I := 0..10):
    rush:  # ERROR: Cannot use rush in loop
        Operation1()
        Operation2()

# Workaround - wrap in function
ProcessWithRush(I:int)<suspends>:void =
    rush:
        Operation1()
        Operation2()

for (I := 0..10):
    ProcessWithRush(I)  # OK
```
<!--verse
}
-->

This restriction forces you to be intentional about creating background tasks in iterations. By wrapping the concurrent operation in a function, you acknowledge the task creation and make it explicit in your code structure. This small friction prevents accidental resource exhaustion while maintaining the flexibility to use these patterns when genuinely needed.

### Abstraction Over Implementation

Verse deliberately abstracts away the underlying threading and scheduling mechanisms. You won't find thread creation APIs, thread-local storage, or explicit synchronization primitives like mutexes or semaphores. This isn't a limitation but a design philosophy. By working with higher-level task abstractions, Verse eliminates entire categories of bugs—no data races, no deadlocks from incorrect lock ordering, no forgotten unlock calls.

The concurrency model is cooperative rather than preemptive. Tasks voluntarily yield control at suspension points rather than being forcibly interrupted by a scheduler. This cooperative nature makes reasoning about concurrent code easier since you know exactly where task switches can occur. It also integrates naturally with game engines' frame-based execution models, where predictable timing is crucial.

### Effect Interactions

The effect system that makes Verse's concurrency safe also introduces some restrictions. The `decides` effect, which marks functions that can fail, cannot be combined with the `suspends` effect. This separation keeps the failure model and the concurrency model orthogonal, preventing complex interactions that would be difficult to reason about. Transactional operations and certain device-specific operations may also have restrictions when used in concurrent contexts, ensuring that operations that must be atomic remain so.
