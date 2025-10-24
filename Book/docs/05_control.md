# Control Flow

Every program has a natural rhythm to its execution, a sequence in which instructions are processed and decisions are made. In Verse, this flow is more than just a mechanical progression through lines of code - it's a carefully orchestrated dance between different types of expressions, each contributing to the overall behavior of your program.

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

## Clause Keywords and Syntax Variations

Many expressions in Verse support multiple syntactic forms through **clause keywords**—special identifiers like `then:`, `else:`, `do:`, and `let:` that label different parts of an expression. These keywords provide alternative ways to structure your code:

```verse
# Compact parenthesized form
if (Condition): Action()

# Explicit clause form
if: Condition
then: Action()
```

Both forms are semantically identical—they compile to the same behavior. The clause-based syntax can improve readability for complex multi-part expressions by clearly labeling each section.

Throughout this chapter and the documentation, you'll see clause keywords used with:
- **`if` expressions**: `then:` and `else:` keywords
- **`for` expressions**: `do:` keyword
- **Class/struct construction**: `let:` keyword for temporary variables (see Composite Types chapter)
- **Other constructs**: `block:` for creating scoped code blocks

Choose the syntax that best fits your code's structure and your team's style preferences. All forms are equally valid and idiomatic.

## Directing Program Execution

Control flow expressions are how you shape the behavior of your program, making decisions, repeating operations, and handling different scenarios.

### The If Expression

The `if` expression is a fundamental control flow construct, but in Verse it works differently than in most languages. Instead of evaluating boolean conditions, `if` uses success and failure to drive decisions. When an expression in the condition succeeds, the corresponding branch executes:

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

**Alternative Syntax with then: and else:**

The `if` expression supports an alternative multi-clause syntax using the `then:` and `else:` keywords to explicitly label branches:

```verse
ProcessValue(Value:int):string =
    if:
        Value > 0
        Value < 100
    then:
        "Valid"
    else:
        "Out of range"
```

This syntax can improve readability when you have multiple conditions or want to emphasize the condition-action separation. The parenthesized and clause-based forms are equivalent:

```verse
# Parenthesized style
if (Condition):
    ActionA()
else:
    ActionB()

# Clause style with then: and else:
if:
    Condition
then:
    ActionA()
else:
    ActionB()
```

Both forms support the same features—multiple conditions, variable binding, and speculative execution. Choose based on your preference and code organization needs.

**Conditions Must Be Fallible:**

The condition must contain at least one expression that can fail. This requirement ensures `if` is used for its intended purpose—handling uncertain outcomes:

<!--NoCompile-->
```verse
# Error: condition cannot fail
if (1 + 1):  # Compile error - no fallible expression
    DoSomething()

# Valid: comparison can fail
if (Score > 100):
    AwardBonus()

# Valid: array access can fail
if (FirstItem := Items[0]):
    Process(FirstItem)
```

Empty conditions are also not allowed—every `if` must test something.

**Speculative Execution and Rollback:**

If any expression in the condition fails, control flow proceeds to the `else` branch if present, and any effects performed while evaluating the condition are undone, including side-effects:

```verse
var Counter:int = 0

if:
    set Counter = Counter + 1  # Provisional change
    Score := GetPlayerScore[]  # Might fail
    Score > 100
then:
    # Counter was incremented
else:
    # Counter rolled back to original value - increment undone!
```

This speculative execution makes conditional logic safer—you can perform operations optimistically, knowing they'll be reversed if subsequent conditions fail.

**Variable Scoping:**

Variables defined in the condition are available in the `then` branch but NOT in the `else` branch:

```verse
if:
    Player := FindPlayer[Name]  # Define Player
    Player.Score > 100
then:
    AwardBonus(Player)  # OK - Player available
else:
    # ERROR: Player not available here
    # Penalize(Player)  # Compile error
```

This scoping reflects the logical flow: in the `else` branch, the condition failed, so any variables bound during the condition might not have meaningful values.

**If as an Expression:**

Since `if` is an expression, it produces a value. When all branches return compatible types, the `if` can be used anywhere a value is expected:

```verse
Damage := if (IsCritical):
    BaseDamage * 2
else:
    BaseDamage

# Ternary-style
Status := if (Health > 50). "Healthy" else. "Wounded"
```

When branches have incompatible types, the result is widened to `any`:

```verse
# Different types in branches yields any
Result:any = if (UseNumber) then 42 else "text"
```

All branches must either produce a value or the `if` cannot be used as an expression.

**If Expression Error Codes:**

- **Error 3513:** Condition block is empty or contains no failable expressions
- **Error 3506:** Variable from condition block used in else branch or after if statement
- **Error 3509:** Incompatible types when using if as expression

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

**Supported Types:**

Case expressions work with specific types that support direct value comparison:

- **Primitives**: `int`, `logic`, `char`
- **Strings**: `string`
- **Enums**: Both open and closed enums
- **Refinement types**: Custom types with constraints

**Type restrictions** prevent matching on:

<!--NoCompile-->
```verse
# Not supported: float
case (Value:float):
    1.5 => "one and half"  # Error: cannot match float

# Not supported: classes
case (Obj:my_class):
    my_class{} => "instance"  # Error: cannot match classes

# Not supported: tuples
case (Pair:tuple(int, int)):
    (1, 2) => "pair"  # Error: cannot match tuples
```

These restrictions exist because these types either don't have well-defined equality (float with NaN), lack value semantics (classes are references), or have structural complexity (tuples).

**Exhaustiveness Checking with Enums:**

One of `case`'s most powerful features is exhaustiveness checking with enum types. For closed enums where all values are known, the compiler verifies you've handled all cases:

```verse
direction := enum:
    North
    South
    East
    West

# Exhaustive - no wildcard needed
GetVector(Dir:direction):tuple(int, int) =
    case (Dir):
        direction.North => (0, 1)
        direction.South => (0, -1)
        direction.East => (1, 0)
        direction.West => (-1, 0)
```

If you add a wildcard when all cases are covered, you'll get a warning that the wildcard is unreachable:

<!--NoCompile-->
```verse
# Warning: wildcard unreachable
GetVector(Dir:direction):tuple(int, int) =
    case (Dir):
        direction.North => (0, 1)
        direction.South => (0, -1)
        direction.East => (1, 0)
        direction.West => (-1, 0)
        _ => (0, 0)  # Warning: all cases already covered
```

For incomplete case coverage, you must either provide a wildcard or use a `<decides>` context:

```verse
# With wildcard - OK
GetPrimaryDirection(Dir:direction):string =
    case (Dir):
        direction.North => "Primary"
        _ => "Other"

# Without wildcard in <decides> context - OK
GetPrimaryDirection(Dir:direction)<decides>:string =
    case (Dir):
        direction.North => "Primary"
        # Other directions cause function to fail

# Without either - ERROR
GetPrimaryDirection(Dir:direction):string =
    case (Dir):
        direction.North => "Primary"
        # Compile error: missing cases
```

**Open enums** can have values added after publication, so they can never be exhaustive. They always require either a wildcard or a `<decides>` context:

```verse
item_type := enum<open>:
    Weapon
    Armor
    Consumable

# Must have wildcard or <decides>
GetCategory(Type:item_type):string =
    case (Type):
        item_type.Weapon => "Equipment"
        item_type.Armor => "Equipment"
        item_type.Consumable => "Usable"
        _ => "Unknown"  # Required - future values may exist
```

**Duplicate and Unreachable Cases:**

The compiler detects cases that can never execute:

<!--NoCompile-->
```verse
# Error: duplicate case
case (Value):
    42 => "answer"
    42 => "duplicate"  # Error: unreachable

# Error: case after wildcard
case (Value):
    _ => "default"
    42 => "specific"  # Error: unreachable - wildcard already matched
```

These errors prevent logic bugs where you believe code will execute but it never can.

**Case Expression Error Codes:**

- **Error 3615:** Empty case expression
- **Error 3616:** Duplicate case pattern
- **Error 3617:** Pattern type not supported (e.g., float, tuple)
- **Error 3618:** Pattern type incompatible with domain type
- **Error 3619:** Case expression in non-failable context without wildcard or exhaustive coverage
- **Error 3620:** Multiple expressions in case domain (currently unsupported)
- **Error 2302:** Wildcard pattern with exhaustive enum coverage (redundant)

### Repetition and Iteration

Verse provides several constructs for repetition, each suited to different scenarios.

**The Loop Expression:**

The `loop` expression creates an infinite loop that continues until explicitly broken:

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

The `break` expression exits the loop entirely, terminating iteration. The `break` statement has type "bottom" (never returns), meaning the compiler knows code after `break` in the same block is unreachable.

**The For Expression:**

The `for` expression iterates over collections, ranges, and other iterable types, providing a more structured approach to repetition:

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
    Total
```

**Alternative Syntax with do:**

The `for` expression supports an alternative multi-clause syntax using the `do:` keyword to separate the iteration specification from the body:

```verse
SumNumbers(Numbers:[]int):int =
    var Total:int = 0
    for:
        Num : Numbers
    do:
        set Total += Num
    Total
```

This syntax is particularly useful when you have complex iteration specifications or want to emphasize the separation between iteration setup and the loop body. Both forms are equivalent:

```verse
# Compact colon style
for (X : Collection):
    DoSomething(X)

# Multi-clause do: style
for:
    X : Collection
do:
    DoSomething(X)
```

You can also use the single-line dot syntax for simple operations:

```verse
# Single-line dot style
for (X : Collection). DoSomething(X)
```

All three forms produce identical behavior—choose based on readability and code organization preferences.

**Range Iteration:**

The range operator `..` provides numeric iteration over integer sequences. Ranges are **inclusive on both ends**:

```verse
# Iterates: 1, 2, 3, 4, 5 (both bounds included)
for (I := 1..5):
    Print("Count: {I}")

# Single element range
for (I := 42..42):
    Print("Answer: {I}")  # Prints once: "Answer: 42"

# Empty range (start > end produces no iterations)
for (I := 5..1):
    Print("Never executes")  # Loop body never runs
```

**Important:** The `..` operator is always inclusive. There is no exclusive range syntax like `..=` - that does not exist in Verse.

**Index and Value Pairs:**

When iterating arrays, you can access both the index and the value using the pair syntax `Index -> Value`:

<!--verse
player:=struct{ Name:string }
-->
```verse
PrintRoster(Players:[]player):void =
    for (Index -> Player : Players):
        Print("Player {Index}: {Player.Name}")
```

The index is zero-based, matching Verse's array indexing convention.

**Defining Variables in For Clauses:**

The for loop allows you to define intermediate variables that can be used in subsequent filters or the loop body:

```verse
# Define Y based on X
Doubled := for (X := 1..5, Y := X * 2):
    Y  # Returns array{2, 4, 6, 8, 10}

# Combine with filtering
SafeDivision := for (X := -3..3, X <> 0, Y := Floor(10.0 / X)):
    Y  # Skips X=0, returns array{-3, -5, -10, 10, 5, 3}
```

These intermediate variables are scoped to the iteration and can reference earlier variables in the same clause.

**Multiple Filters:**

You can chain multiple filter conditions using comma-separated expressions. Each filter must be failable, and if any fails, that iteration is skipped:

```verse
# Multiple independent filters
Filtered := for (X := 1..10, X <> 3, X <> 7):
    X  # Returns array{1, 2, 4, 5, 6, 8, 9, 10}

# Filters with intermediate variables
Complex := for (X := 1..5, X <> 2, Y := X * 2, Y < 10):
    Y  # Only includes values where X≠2 and Y<10
```

Each filter condition is evaluated in order, and iteration continues only if all conditions succeed.

**Iterating Over Maps:**

Maps can be iterated over in two ways: values only, or key-value pairs using the pair syntax:

```verse
# Iterate over values only
Scores:[int]int = map{1 => 100, 2 => 200, 3 => 150}
TopScores := for (Score : Scores):
    Score  # Returns array{100, 200, 150}

# Iterate over key-value pairs
PlayerScores:[string]int = map{"Alice" => 100, "Bob" => 200}
for (PlayerName -> Score : PlayerScores):
    Print("{PlayerName} scored {Score}")
```

Maps preserve insertion order, so iteration order matches the order in which keys were added to the map.

**String Iteration:**

Strings can be iterated character by character:

```verse
CountVowels(Text:string):int =
    var Count:int = 0
    for (Char : Text):
        if (Char = 'a' or Char = 'e' or Char = 'i' or Char = 'o' or Char = 'u'):
            set Count += 1
    Count
```

**Nested Iteration (Cartesian Products):**

Multiple iteration sources create nested loops, producing the cartesian product:

```verse
PrintGrid():void =
    for (X := 1..3, Y := 1..3):
        Print("({X}, {Y})")
    # Produces: (1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)
```

**Filtering with Failure:**

Verse's `for` expressions are particularly powerful when they leverage failure contexts, as they can naturally filter:

<!--verse
player:=struct{ Name:string }
GetScore(P:player)<computes>:int=0
-->
```verse
GetHighScorers(Players:[]player):[]player =
    for (Player : Players, Score := GetScore(Player), Score > 1000):
        Player  # Only players with score > 1000 are included
```

When any expression in the iteration header fails, that iteration is skipped. This allows elegant filtering without explicit `if` statements:

<!--verse
item:=struct{Price:float}
-->
```verse
# Filter items under budget and apply transformation
AffordableItems(Items:[]item, Budget:float):[]float =
    for (Item : Items, Item.Price <= Budget):
        Item.Price * 1.1  # Apply 10% markup
```

**For as an Expression:**

Like other control flow constructs, `for` is an expression. When the body produces values, `for` collects them into an array:

<!--verse
player:=struct{Name:string}
-->
```verse
# Collect player names
GetNames(Players:[]player):[]string =
    for (Player : Players):
        Player.Name  # Each iteration produces a string
```

This makes `for` a powerful tool for transforming collections without explicit accumulator variables.

**Breaking from For Loops:**

The `break` statement can exit `for` loops early, just like with `loop`:

```verse
FindFirstNegative(Numbers:[]int):void =
    for (Num : Numbers):
        if (Num < 0):
            Print("Found negative: {Num}")
            break
        Print("Checking: {Num}")
```

**Note on Continue:**

Unlike many languages, Verse does not currently support a `continue` statement to skip to the next iteration. Instead, use conditional logic or failure-based filtering to achieve similar results:

<!--verse
item:=struct{IsValid:logic}
ProcessItem(I:item):void={}
-->
```verse
# Instead of continue, use conditional blocks
ProcessItems(Items:[]item):void =
    for (Item : Items):
        if (Item.IsValid):
            ProcessItem(Item)
        # No continue needed - just structure with conditions

# Or use failure-based filtering in the header
ProcessValidItems(Items:[]item):void =
    for (Item : Items, Item.IsValid):
        ProcessItem(Item)  # Only valid items reach here
```

**For Loop Restrictions:**

The for loop has several important restrictions:

1. **Iteration source must be iterable:** Only ranges (`1..10`), arrays, maps, and strings can be iterated. Other types produce error 3524.

2. **Filters must be failable:** Filter conditions must contain at least one expression that can fail. Using an infallible expression like a constant produces error 3513.

3. **Cannot redefine iteration variables:** You cannot redefine the iteration variable in the same clause (error 3588/3532):
   ```verse
   # Error: X is redefined
   for (X := 1..5, X := 2):
       X
   ```

4. **Cannot define mutable variables:** Using `var` to declare variables in the for clause is not allowed (error 3546).

5. **Type compatibility required:** The iteration variable type must match the collection type (error 3509).

### Range Operator Restrictions

The range operator `..` has strict limitations that distinguish it from other iterable types. Ranges are **not first-class values**—they exist solely as syntactic sugar within for loop iteration clauses.

**Error 3552 - Ranges can only appear in for loops:**

Ranges cannot be used in most contexts where you might expect them to work:

```verse
# ERROR 3552: Cannot store range in variable
# MyRange := 1..10
# for (I := MyRange):

# ERROR 3552: Cannot pass range to function
# ProcessRange(1..10)

# ERROR 3552: Cannot use range as standalone expression
# Result := 1..10

# ERROR 3552: Cannot put range in array
# Ranges := array{1..10}

# ERROR 3552: Cannot index range
# Value := (1..10)(5)

# ERROR 3552: Cannot access members on range
# Length := (1..10).Length

# CORRECT: Range used directly in for loop
for (I := 1..10):
    Process(I)  # This works
```

**Must use `:=` assignment syntax:**

When iterating with ranges, you must use the `:=` binding syntax, not the `:` iteration syntax:

```verse
# ERROR 3552, 3524: Wrong syntax
# for (I : 1..10):

# CORRECT: Use := with ranges
for (I := 1..10):
    Print(I)
```

**Integer-only ranges (Error 3552):**

Ranges work exclusively with the `int` type. Other numeric types, booleans, types, or objects are not supported:

```verse
# ERROR 3552: Cannot create range with float
# for (I := 0.0..10.0):

# ERROR 3552: Cannot create range with float and int
# for (I := 0.0..10):
# for (I := 0..10.0):

# ERROR 3552: Cannot create range with logic
# for (I := false..true):

# ERROR 3552: Cannot create range with types
# for (I := int..float):

# ERROR 3552: Cannot create range with objects
# for (I := Object1..Object2):

# CORRECT: Only int..int works
for (I := 0..10):
    Print(I)
```

**Syntax errors:**

Both bounds are required:

```verse
# ERROR 3101: Missing left bound
# "..10"

# ERROR 3100: Missing right bound
# "10.."

# CORRECT: Both bounds present
"1..10"
```

### Range Evaluation Semantics

**Evaluation order guarantee:**

Range bounds are evaluated in a specific order, and side effects occur predictably:

1. **Left bound evaluated first**, then right bound
2. **Both bounds always evaluated**, even if the range is empty
3. **Side effects happen in order**, regardless of whether iterations occur

```verse
counter := class:
    var Value:int = 0

    GetStart()<transacts>:int =
        set Value += 1
        1

    GetEnd()<transacts>:int =
        set Value += 10
        0  # Results in empty range (1..0)

C := counter{}
for (I := C.GetStart()..C.GetEnd()):
    # Never executes (empty range)

# C.Value = 11 (both methods called: 1 + 10)
```

**Transaction semantics:**

Range bounds participate in Verse's transactional execution model. If a range produces no iterations due to invalid bounds, side effects in the range expressions are rolled back:

```verse
ProcessRange()<transacts>:int =
    var X:int = 100

    for (J:(set X = 200); I := GetInvalidRange()):
        # Loop never executes (invalid range)

    X  # Returns 100, not 200 - assignment rolled back
```

However, if the range is valid (even if empty), side effects are committed:

```verse
ValidEmptyRange()<transacts>:int =
    var X:int = 100

    for (J:(set X = 200); I := 5..1):  # Valid syntax, empty range
        # Loop never executes

    X  # Returns 200 - assignment committed
```

**Creating arrays from ranges:**

While you cannot store ranges as values, you can create arrays using for expressions:

```verse
# This works because for produces an array, not because ranges are storable
Numbers:[]int = for (I := 1..5):
    I * 2
# Numbers = array{2, 4, 6, 8, 10}

# Can then iterate over the array normally
for (N : Numbers):
    Print(N)
```

The range exists only during the for expression evaluation; the resulting array is what gets stored.

### The Return Statement

The `return` statement provides explicit early exits from functions, allowing you to terminate execution and return a value before reaching the end of the function body:

<!--verse
ProcessPayment(Amount:float)<decides>:string={
    if(Amount<0)then{return "invalid"}
    if(Amount>1000)then{return "requires_approval"}
    "processed"
}
-->
```verse
ValidateInput(Value:int)<decides>:string =
    if (Value < 0):
        return "Error: Negative value"

    if (Value > 1000):
        return "Error: Value too large"

    "Valid"  # Implicit return
```

**Tail Position Requirement:**

Return statements can only appear in specific positions within your code—they must be in "tail position," meaning they must be the last operation performed before control exits a scope. This restriction ensures predictable control flow:

```verse
# Valid: return is last operation
ProcessOrder(OrderId:int)<decides>:string =
    Order := GetOrder[OrderId]
    if (not Order.IsValid[]):
        return "Invalid order"
    "Processed"

# Valid: return in both branches
GetStatus(Value:int):string =
    if (Value > 0):
        return "Positive"
    else:
        return "Non-positive"
```

You cannot perform operations after a `return` in the same scope:

<!--NoCompile-->
```verse
# Error: code after return
BadFunction():int =
    return 42
    Print("This never runs")  # Compile error: unreachable code
```

**Implicit vs Explicit Returns:**

Verse functions implicitly return the value of their last expression, so `return` is only needed for early exits:

<!--verse
CalculateBonus(Score:int):int={
    if(Score<100)then{return 0}
    Score*10
}
-->
```verse
# Implicit return
GetValue():int = 42  # Returns 42

# Explicit early return
GetDiscount(Price:float):float =
    if (Price < 10.0):
        return 0.0  # Early exit with no discount

    Price * 0.1  # Implicit return with 10% discount
```

**Returns and Failure:**

In functions with the `<decides>` effect, `return` allows you to provide successful values from early exits, while still allowing other paths to fail:

<!--verse
config:=struct{MaxRetries:int}
GetConfig()<decides>:config=config{MaxRetries:=3}
AttemptOperation(Retry:int)<decides>:string="success"
-->
```verse
RetryableOperation()<decides>:string =
    Config := GetConfig[]

    for (Retry := 1..Config.MaxRetries):
        if (Result := AttemptOperation[Retry]):
            return Result  # Success - exit immediately

    false  # All retries exhausted - fail
```

This pattern is common for search operations where you want to return immediately upon finding a match, but fail if no match is found.

**Return Types Must Match:**

All return paths (both explicit `return` statements and implicit returns) must produce values compatible with the function's declared return type:

<!--NoCompile-->
```verse
# Error: incompatible return types
BadFunction():int =
    if (SomeCondition):
        return 42  # OK: returns int
    else:
        return "text"  # Error: returns string, expected int
```

When return paths have incompatible types, you may need to widen the return type to accommodate all possibilities, potentially using `any` as the common supertype.

### The Break Statement

The `break` statement provides a way to exit loops (`loop` and `for`) before their natural completion. It's one of the fundamental tools for controlling iteration:

```verse
FindFirstNegative(Numbers:[]int):void =
    for (Num : Numbers):
        if (Num < 0):
            Print("Found negative: {Num}")
            break
        Print("Checking: {Num}")
```

**Break Has Bottom Type:**

A unique characteristic of `break` is that it has "bottom" type—a type that represents a computation that never returns normally. Since the bottom type is a subtype of all other types, `break` can be used in any type context:

```verse
var X:int = 0
loop:
    set X = if (ShouldExit[]). break else. ComputeValue()
    # break is compatible with int type because bottom ⊆ int
```

This allows `break` to be used flexibly in expressions where a value is expected, since the compiler knows that path never produces a value.

**Simple Syntax:**

The `break` statement takes no arguments or clauses:

```verse
# Valid
loop:
    if (Done[]):
        break

# Error 3508: break takes no arguments
loop:
    if (Done[]):
        break(42)  # Compile error
```

**Scope and Nesting:**

When `break` appears in nested loops, it exits only the innermost enclosing loop:

```verse
var OuterCount:int = 0
var InnerCount:int = 0

loop:
    set OuterCount += 1

    loop:
        set InnerCount += 1
        if (InnerCount = 5):
            break  # Exits inner loop only

    if (OuterCount = 10):
        break  # Exits outer loop
```

**Restrictions:**

1. **Must be inside a loop:** Using `break` outside a `loop` or `for` produces error 3581:
   ```verse
   # Error 3581: break not in loop
   ProcessData():void =
       if (ShouldStop[]):
           break  # Compile error
   ```

2. **Must be in code block:** The `break` statement must appear in a code block, not as part of a complex expression (error 3658).

3. **Empty loops not allowed:** A loop must contain at least one non-break statement (error 3579):
   ```verse
   # Error 3579: empty loop
   loop:
       break  # Compile error - loop body is effectively empty
   ```

**Break with Async:**

The `break` statement works seamlessly with async loops:

```verse
WaitForCondition()<suspends>:void =
    loop:
        WaitForNextFrame()
        if (ConditionMet[]):
            break
```

### The Defer Expression

The `defer` expression schedules code to run just before successfully exiting the current scope. This makes it invaluable for cleanup operations like closing files, releasing resources, or logging:

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
        CloseFile(File)  # Runs on success or early exit

    Contents := ReadFile(File)?
    ProcessContents[Contents]
    SaveResults[]
```

**Execution on Success and Early Exits:**

Deferred code executes when the scope exits successfully or through explicit control flow like `break` or `return`:

<!--verse
OpenConnection()<transacts>:int=0
CloseConnection(Id:int)<transacts>:void={}
Query(Id:int)<transacts><decides>:string="result"
ProcessResult(R:string)<transacts>:void={}
-->
```verse
ProcessQuery()<transacts><decides>:void =
    ConnId := OpenConnection()
    defer:
        CloseConnection(ConnId)  # Cleanup always needed

    for (Attempt := 1..5):
        if (Result := Query[ConnId]):
            ProcessResult(Result)
            return  # defer executes before return

    false  # defer executes before failure
```

**Critical: Defers Do Not Execute on Failure:**

This is a subtle but crucial point: if a function fails due to speculative execution, deferred code does NOT execute. This is because failure triggers a rollback that undoes all effects, including the scheduling of defer blocks:

<!--verse
AcquireResource()<transacts><decides>:int=0
ReleaseResource(Id:int)<transacts>:void={}
RiskyOperation(Id:int)<transacts><decides>:void={}
-->
```verse
ExampleWithFailure()<transacts><decides>:void =
    ResourceId := AcquireResource[]
    defer:
        ReleaseResource(ResourceId)  # Scheduled...

    if (false):  # This fails!
        RiskyOperation[ResourceId]
    # defer does NOT run - entire scope was speculative and rolled back
```

When the `if (false)` fails, the entire function fails, and speculative execution undoes everything—including the defer registration. The resource cleanup never happens because the resource acquisition itself is rolled back.

This behavior ensures consistency: if a function fails, it's as if it never ran, including any cleanup code that was scheduled.

**Execution Order:**

When multiple `defer` expressions exist in the same scope, they execute in reverse order of definition (last-in, first-out), mimicking the stack-based cleanup of nested resources:

<!--verse
OpenDatabase()<transacts>:int=0
CloseDatabase(Id:int)<transacts>:void={}
BeginTransaction(Id:int)<transacts>:int=0
CommitTransaction(Id:int)<transacts>:void={}
DoWork()<transacts><decides>:void={}
-->
```verse
DatabaseTransaction()<transacts><decides>:void =
    DbId := OpenDatabase()
    defer:
        CloseDatabase(DbId)  # Executes second (outer resource)

    TxnId := BeginTransaction[DbId]
    defer:
        CommitTransaction(TxnId)  # Executes first (inner resource)

    DoWork[]  # Work happens with both resources active
    # Defers execute: CommitTransaction, then CloseDatabase
```

**Defers and Async Cancellation:**

Deferred code also executes when async operations are cancelled, such as when a `race` completes or a `spawn` is interrupted:

<!--NoCompile-->
```verse
ProcessWithTimeout()<suspends><transacts>:void =
    race:
        block:
            Resource := AcquireResource()
            defer:
                ReleaseResource(Resource)  # Runs if cancelled

            LongRunningTask(Resource)

        block:
            Sleep(10.0)  # Timeout
    # If timeout wins, first block is cancelled and defer runs
```

This ensures cleanup happens even when concurrency control interrupts your code.

**Nested Defers:**

Defer statements can be nested within other defer blocks, creating a cascade of cleanup operations:

<!--verse
Log(S:string)<transacts>:void={}
-->
```verse
ProcessWithCleanup():void =
    Log("A")
    defer:
        Log("B")
        defer:
            Log("inner")  # Runs after B
        Log("C")
    Log("D")
    # Output: A D B C inner
```

The execution order follows the LIFO principle at each nesting level—inner defers execute after the outer defer's code, maintaining the stack-like cleanup order.

**Defers in Control Flow:**

Defers work correctly within all control flow constructs:

<!--verse
Log(S:string)<transacts>:void={}
-->
```verse
ProcessLoop():void =
    for (I := 0..2):
        Log("Start")
        defer:
            Log("Cleanup")  # Runs after each iteration
        Log("End")
    # Output: Start End Cleanup Start End Cleanup Start End Cleanup

ProcessWithIf(Condition:logic):void =
    if (Condition):
        defer:
            Log("Then cleanup")
        Log("Then body")
    else:
        defer:
            Log("Else cleanup")
        Log("Else body")
```

Each control flow path executes its own defers independently.

**Defer Restrictions and Error Codes:**

The defer statement has important restrictions to ensure predictable behavior:

1. **Cannot be empty:** Defer blocks must contain at least one expression (error 2001):
   ```verse
   # Error 2001: defer cannot be empty
   defer:
       # Nothing here
   ```

2. **Cannot be used as expression:** Defer cannot be used in positions where a value is expected (error 3567):
   ```verse
   # Error 3567: defer not allowed here
   X := defer { 42 }

   # Error 3567: defer in array
   array{defer{Log("A")}, Log("B")}
   ```

3. **Cannot cross boundaries:** Defer blocks cannot contain `return`, `break`, or other control flow that would exit the defer's scope (error 3566):
   ```verse
   # Error 3566: return crosses defer boundary
   defer:
       if (ShouldExit[]):
           return  # Compile error
   ```

4. **Cannot fail:** Expressions in defer blocks cannot fail (error 3512):
   ```verse
   # Error 3512: defer cannot contain failable expressions
   defer:
       Value := Array[99]  # Compile error - might fail
   ```

5. **Cannot suspend directly:** Defer blocks cannot contain suspend expressions (error 3512), but they can use `branch` or `spawn` for fire-and-forget async operations:
   ```verse
   # Error 3512: cannot suspend in defer
   defer:
       WaitForFrame()  # Compile error

   # Valid: spawn for async cleanup
   defer:
       spawn { AsyncCleanup() }  # OK - fires and forgets
   ```

These restrictions ensure that defers execute quickly and predictably without introducing failure or control flow complexity.

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
