# Control Flow

Every program has a natural rhythm to its execution, a sequence in which instructions are processed and decisions are made. In Verse, this flow is more than just a mechanical progression through lines of code - it's a carefully orchestrated dance between different types of expressions, each contributing to the overall behavior of your program. Understanding how to control this flow, structure your code into meaningful blocks, and document your thinking through comments is essential to mastering Verse.

## Code Blocks

Code blocks are a fundamental organizational unit, grouping related expressions together and creating new scopes for variables and constants. Unlike many languages where blocks are merely syntactic conveniences, blocks are expressions themselves, meaning they produce values just like any other expression.

The concept of scope is crucial to understanding code blocks. When you create a variable or constant within a block, it exists only within that block's context. This containment ensures that your code remains organized and that names don't accidentally conflict across different parts of your program:

```verse
CalculateReward(PlayerLevel:int):int =
    if (PlayerLevel > 10):
        BonusMultiplier := 2.0  # Only exists within this if block
        BaseReward := 100
        Floor[BaseReward * BonusMultiplier]
    else:
        50  # Different branch, different scope
    # BonusMultiplier doesn't exist here
```

Verse provides three equivalent formats for writing code blocks, each suited to different situations. The spaced format is the most common, using a colon to introduce the block and indentation to show structure:

<!--NoCompile-->
```verse
if (IsPlayerReady[]):
    StartMatch()
    InitializeScoreboard()
    BeginCountdown()
```

The multi-line braced format offers familiarity for programmers coming from C-style languages:

<!--NoCompile-->
```verse
if (IsPlayerReady[])
{
    StartMatch()
    InitializeScoreboard()
    BeginCountdown()
}
```

For simple operations, the single-line dot format keeps code concise:

<!--verse
HasPowerUp()<computes><decides>:void={}
ApplyBoost():void={}
IncrementCounter():void={}
F():void={
-->
```verse
if (HasPowerup[]). ApplyBoost(); IncrementCounter()
```
<!--verse
}
-->

Since everything is an expression, blocks themselves have values. The value of a block is the value of the last expression executed within it. This design enables elegant patterns where complex computations can be encapsulated in blocks that seamlessly integrate with surrounding code:

<!--NoCompile-->
```verse
FinalScore := block:
    BaseScore := CalculateBaseScore()
    TimeBonus := CalculateTimeBonus(CompletionTime)
    AccuracyBonus := Floor(Accuracy * 100.0)
    BaseScore + TimeBonus + AccuracyBonus  # This becomes the block's value
```

## Directing Program Execution

Control flow expressions are how you shape the behavior of your program, making decisions, repeating operations, and handling different scenarios. Verse's approach to control flow is distinctive because it combines traditional imperative constructs with functional programming concepts and its unique failure-based decision making.

### The If Expression

The `if` expression is perhaps the most fundamental control flow construct, but in Verse it works differently than in most languages. Instead of evaluating boolean conditions, `if` uses success and failure to drive decisions. When an expression in the condition succeeds, the corresponding branch executes:

<!--verse
player:=class{
   CanJump()<computes<decides>:void={}
   Jump():void={}
   GetEquippedWeapon()<computes><decides>:weapon=weapon{}
   Idle():void={}
}   
weapon:=class<computes>{
   Fire():void={}
}
ConsumeAmmo():void={}
PlayJumpSound():void={}

-->
```verse
HandlePlayerAction(Player:player, Action:string):void =
    if (Action = "jump", Player.CanJump[]):
        Player.Jump()
        PlayJumpSound()
    else if (Action = "attack", Weapon := Player.GetEquippedWeapon[]):
        Weapon.Fire()
        ConsumeAmmo()
    else:
        # Default action
        Player.Idle()
```

This failure-based approach integrates naturally with `decides` effect system, allowing you to chain conditions that might fail without explicit error handling at each step.

### The Case Expression

When you need to make decisions based on multiple possible values, the `case` expression provides clear, readable branching:

```verse
GetWeaponDamage(WeaponType:string):float =
    case(WeaponType):
        "sword" => 50.0
        "bow" => 35.0
        "staff" => 40.0
        "dagger" => 25.0
        _ => 10.0  # Default damage for unknown weapons
```

The `case` expression excels when you have discrete values to match against, making your intent clearer than a series of `if-else` conditions.

### Repetition and Iteration

Verse provides several constructs for repetition, each suited to different scenarios. The `loop` expression creates an infinite loop that continues until explicitly broken:

<!--verse
UpdatePlayerPositions():void={}
CheckCollisions():void={}
RenderFrame():void={}
GameOver()<computes><decides>:void={}
-->
```verse
GameLoop():void =
    loop:
        UpdatePlayerPositions()
        CheckCollisions()
        RenderFrame()

        if (GameOver[]):
            break
```

The `break` expression exits the loop entirely.

The `for` expression iterates over collections or ranges, providing a more structured approach to repetition:

<!--verse
using { /Verse.org/VerseCLR }
player:=struct{ Name:string }
GetScore(P:player):int=0
-->
```verse
CalculateTotalScore(Players:[]player):int =
    var Total:int = 0
    for (Player : Players):
        PlayerScore := GetScore(Player)
        set Total += PlayerScore

        # Can also get the index
    for (Index -> Player : Players):
        Print("Player {Index}: {Player.Name}")

    Total
```

Verse's `for` expression is particularly powerful when combined with failure contexts, as it can naturally filter elements:

<!--verse
player:=struct{ Name:string }
GetScore(P:player)<computes>:int=0
-->
```verse
GetHighScorers(Players:[]player):[]player =
    for (Player : Players, Score := GetScore(Player), Score > 1000):
        Player  # Only players with score > 1000 are included
```

### The Defer Expression

The `defer` expression ensures that code runs just before exiting the current scope, regardless of how the scope is exited. This makes it perfect for cleanup operations:

<!--verse
OpenFile(P:string)<computes>:?int=false
CloseFile(P:int)<computes>:void={}
ReadFile(P:int)<computes>:?string=false
ProcessContents(P:string)<computes><decides>:void={}
SaveResults()<computes><decides>:void={}
-->
```verse
ProcessFile(FileName:string)<transacts><decides>:void =
    File := OpenFile(FileName)?
    defer:
        CloseFile(File)  # Always runs, even if we fail below

    Contents := ReadFile(File)?
    ProcessContents[Contents]
    SaveResults[]
```

The deferred code executes in reverse order of definition when multiple `defer` expressions exist in the same scope, ensuring proper cleanup of nested resources.

## Performance Profiling

Understanding how your code performs is crucial for optimization, and the `profile` expression measures execution time:

```verse
OptimizedCalculation():float =
    profile("Complex Math"):
        var Result:float = 0.0
        for (I := 1..1000000):
            set Result += Sin(I*1.0) * Cos(I*1.0)
        Result
```

The profile expression wraps around the code you want to measure, logging the execution time to the output. You can add descriptive tags to organize your profiling output, making it easier to identify bottlenecks in complex systems.

Profile expressions pass through their results transparently, meaning you can wrap them around any expression without changing the program's behavior:

<!--NoCompile-->
```verse
PlayerDamage := profile("Damage Calculation"):
    BaseDamage * GetMultiplier() * GetCriticalBonus()
```

## Advanced Patterns and Techniques

The interplay between control flow, code blocks, and scoping creates opportunities for sophisticated programming patterns. One powerful pattern is using blocks to create temporary computation contexts:

<!--verse
item := struct{}
result := struct<allocates>{}
ProcessItems(I:item)<allocates><decides>:result=result{}
-->
```verse
ProcessBatch(Items:[]item)<transacts><decides>:[]result =
    block:
        var Results:[]result = array{}
        var FailureCount:int = 0

        for (Item : Items):
            if (Result := ProcessItems[Item]):
                set Results = Results + array{Result}
            else:
                set FailureCount = FailureCount + 1

        # Fail if too many items failed
        FailureCount < 5
        Results
```

This pattern encapsulates complex logic with multiple mutable variables while presenting a clean interface to the outside world.

Another sophisticated technique involves combining control flow with failure to create elegant error handling:

<!--NoCompile-->
```verse
TryMultipleStrategies(Data:input_data)<decides>:output =
    # Try strategies in order of preference
    if (Result := FastStrategy[Data]):
        Result
    else if (Result := AccurateStrategy[Data]):
        Result
    else if (Result := FallbackStrategy[Data]):
        Result
    else:
        false  # All strategies failed
```

## Thinking in Verse

The key to mastering control flow  is understanding that it's not just about directing execution - it's about composing expressions that produce values. Every `if`, every `loop`, every block contributes to the overall computation. This expression-oriented mindset, combined with the failure-based decision making and the scoping rules of code blocks, creates a programming model that's both powerful and elegant.

As you write more Verse code, you'll find that the boundaries between control flow, data flow, and program structure become fluid. A loop isn't just repetition - it's a value-producing expression that can filter, transform, and aggregate. An `if` isn't just a branch - it's a computation that chooses between possible results based on success and failure. A block isn't just a grouping - it's a scoped computation that produces a value while managing local state.

This unified view of program structure, where everything is an expression and control flow seamlessly integrates with computation, is what makes Verse unique.
