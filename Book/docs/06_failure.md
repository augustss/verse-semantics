# Failure

Most programming languages treat control flow as a matter of true or false, yes or no, one or zero. They evaluate boolean conditions and branch accordingly, creating a world of binary decisions that often requires checking conditions twice - once to see if something is possible, and again to actually do it. Verse takes a radically different approach. Instead of asking "is this true?", Verse asks "does this succeed?"

This distinction might seem subtle, but it fundamentally changes how programs are written and reasoned about. Failure isn't an error or an exception - it's a first-class concept that drives control flow. When an expression fails, it doesn't crash your program or throw an exception that needs to be caught. Instead, failure is a normal, expected outcome that your code handles gracefully through the structure of the language itself.

Consider the simple act of accessing an array element. In traditional languages, you might write:

<!--NoCompile-->
```verse
# Traditional approach (not Verse)
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

## Failable Expressions: The Building Blocks

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

```verse
ValidateAge(Age:int)<decides>:int =
    Age >= 0  # Fails if age is negative
    Age <= 150  # Fails if age is unrealistic
    Age        # Returns the age if both checks pass
```

This function doesn't just check conditions - it embodies them. If the age is invalid, the function fails. If it's valid, it succeeds with the age value. The validation and the value are inseparable.

## Failure Contexts: Where Magic Happens

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
IsWeapon(i:item)<decides>:void={}
Damage(i:item)<decides>:int=0
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

The function body is a failure context, allowing failable expressions throughout. The final line extracts the value from the option, failing if no weapon was found.

## Speculative Execution: Try Before You Commit

One of Verse's most powerful features is speculative execution within failure contexts. When you execute code in a failure context, changes to variables are provisional - they only become permanent if the entire context succeeds.

```verse
AttemptPurchase(var PlayerGold:int, Cost:int)<transacts><decides>:void =
    set PlayerGold = PlayerGold - Cost  # Provisional change
    PlayerGold >= 0                     # Check if still valid
    # If this fails, PlayerGold reverts to original value
```

If the player doesn't have enough gold, the subtraction is rolled back automatically. You don't need to manually restore the original value or check conditions before modifying state. This transactional behavior eliminates entire categories of bugs related to partial state updates.

The `<transacts>` effect is required for any function that might be called in a failure context and modifies state. This makes the transactional behavior explicit in the function signature:

<!--NoCompile-->
```verse
ComplexTransaction(var State:game_state)<transacts><decides>:void =
    ModifyHealth(State.Player)         # All these operations
    UpdateInventory(State.Inventory)   # are provisional
    ChargeResources(State.Resources)   # until all succeed
    ValidateFinalState[State]         # If this fails, everything rolls back
```

This approach makes complex state updates safe and predictable. Either everything succeeds and all changes are committed, or something fails and nothing changes.

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

## Options and Failure: Two Sides of the Same Coin

The option type and failure are intimately connected. An option either contains a value or is empty (represented by `false`). The query operator `?` converts between options and failure:

<!--verse
F()<decides>:void={
-->
```verse
MaybeValue:?int = option{42}
Value := MaybeValue?  # Succeeds with 42

Empty:?int = false
Other := Empty?  # Fails
```
<!--verse
}
-->

The `option{}` constructor works in reverse, converting failure to an empty option:

<!--verse
RiskyComputation()<decides>:int=1
F():void={
-->
```verse
Result := option{RiskyComputation[]}
# Result is option{value} if computation succeeds
# Result is false if computation fails
```
<!--verse
}
-->

This bidirectional conversion makes options and failure interchangeable, allowing you to choose the most appropriate representation for your specific use case.

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
