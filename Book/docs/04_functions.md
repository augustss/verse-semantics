# Functions

Functions are reusable code blocks that perform actions and produce outputs based on inputs. Think of them as abstractions for behaviors, much like ordering food from a menu at a restaurant. When you order, you tell the waiter what you want from the menu, such as `OrderFood("Ramen")`. You don't need to know how the kitchen prepares your dish, but you expect to receive food after ordering. This abstraction is what makes functions powerful - you define the instructions once and reuse them in different contexts throughout your code.

Verse takes a unique approach to functions by supporting three programming paradigms simultaneously: functional programming with lambdas and immutable data structures, imperative programming with mutable variables and pointers, and logic programming where programs describe equations to solve. This multi-paradigm approach makes functions particularly versatile.

## Abstraction

Consider this simple function:

```verse
F1(X:int):int = X + 1
```

This is an open-world function. The type annotation between the colon and equals sign (`:int =`) tells us that this function promises to take at least integers as arguments and return at most integers as results. The implementation could change in the future, perhaps to perform additional operations or optimizations, as long as it maintains these type constraints. This flexibility makes open-world functions ideal for public APIs where you want to preserve the freedom to evolve the implementation without breaking existing code.

Now consider a slightly different version:

```verse
F2(X:int) := X + 1
```

The use of `:=` instead of a type annotation makes this a closed-world function. This syntax creates a forever guarantee - the right-hand side will remain exactly the same throughout the lifetime of your code. The Verse compiler treats closed-world functions specially: when checking code that calls a closed-world function, it actually executes the function at compile-time. This transparency allows for powerful compile-time computations.

Here's a practical example that demonstrates this difference:

```verse
f3(t:type) := t
X:f3(int) = 42  # This works!

f4(t:type):type = t
Y:f4(int) = 42  # ERROR! Checker can't guarantee 42 works for all possible types
```

In the first case, the checker executes `f3(int)` at compile-time and knows it returns `int`, so it can verify that 42 is a valid integer value. In the second case, the checker must assume `f4` could return any type, making it impossible to guarantee that 42 would be valid for all possible return types.

## Parameters and Arguments

Verse functions can accept any number of parameters, from none at all to as many as needed. The syntax follows a straightforward pattern where each parameter has an identifier and a type, separated by commas:

```verse
ProcessData(Name:string, Age:int, Score:float):string =
    "{Name} is {Age} years old with a score of {Score}"
```

Verse also supports named arguments, which provide additional flexibility in how functions are called. Named arguments are prefixed with a question mark and can include default values:

```verse
CalculateBonus(BaseSalary:int, ?Multiplier:float, ?YearsOfService:int = 1):float =
    BaseSalary * Multiplier * (1.0 + YearsOfService * 0.1)
```

When calling this function, named arguments can appear in any order after the positional arguments:

```verse
CalculateBonus(50000, ?Multiplier := 1.5, ?YearsOfService := 3)
CalculateBonus(50000, ?YearsOfService := 5, ?Multiplier := 2.0)  # Order doesn't matter
```

An interesting aspect of Verse is that functions accepting multiple arguments are indistinguishable from functions accepting a single tuple argument. These two definitions are equivalent:

```verse
Second(:any, X:t where t:type):t = X
Second(X:tuple(any, t) where t:type):t = X(1)
```

Both can be invoked with either separate arguments or a tuple, demonstrating Verse's flexible approach to function parameters.

## Return Values and Control Flow

Verse functions automatically return the value of the last executed expression, which often eliminates the need for explicit return statements. This design choice leads to cleaner, more concise code:

```verse
GetStatus(Score:int):string =
    if (Score >= 90):
        "Excellent"
    else if (Score >= 70):
        "Good"
    else:
        "Needs Improvement"
```

However, when you need to exit a function early or when the control flow becomes complex, explicit return statements provide clarity:

```verse
FindFirstNegative(Numbers:[]int):?int =
    for (Number : Numbers):
        if (Number < 0):
            return option{Number}
    false  # No negative found
```

Functions with void return types are special in Verse - they always return the value `false`, regardless of what expressions appear in their body. This consistent behavior simplifies the handling of void functions in logical contexts.

## The Power of Effects: Understanding Failure

Effects in Verse describe additional behaviors that functions can exhibit beyond simply computing a result. The most important effect is `decides`, which indicates that a function can fail in a way that callers must handle.

When a function has the `decides` effect, it fundamentally changes how you interact with it. Consider this example:

```verse
ValidateInput(X:int)<decides>:int =
    X > 0  # Fails if X <= 0
    X * 2  # Only executes if validation passes
```

The `decides` effect requires special calling syntax. In failure contexts, you must use square brackets instead of parentheses:

```verse
ProcessValue(Input:int):string =
    if (ValidatedValue := ValidateInput[Input]):
        "Processed: {ValidatedValue}"
    else:
        "Invalid input"
```

This syntactic distinction makes it immediately clear when you're dealing with potentially failing operations. The square bracket syntax mirrors array indexing, which also carries the `decides` effect since accessing an array element can fail if the index is out of bounds.

Functions with `decides` can be combined in sophisticated ways. For instance, you might want to find the first element in an array that satisfies a condition:

```verse
First(Array:[]t, Test(:t)<decides>:void where t:type)<decides>:t =
    var Result:?t = false
    for (Element : Array, Test[Element], not Result?):
        set Result = option{Element}
    Result?
```

Or ensure all elements meet a criterion:

```verse
All(Array:[]t, Test(:t)<decides>:void where t:type)<decides>:void =
    for (Element : Array):
        Test[Element]
```

The interaction between `decides` and control structures like `for` expressions creates powerful patterns. When a `decides` function appears in a `for` expression, the loop continues only as long as the function succeeds, providing natural filtering behavior.

## Function Types and Overloading

Every function in Verse has a type that captures its parameter types, effects, and return type. The type syntax uses an underscore as a placeholder for the function name:

```verse
type{_(:int, :string)<decides>:float}
```

This represents any function that takes an integer and a string, might fail (has the `decides` effect), and returns a float when successful.

Verse allows multiple functions to share the same name through overloading, as long as their signatures don't create ambiguity. The compiler can distinguish between overloads based on the argument types:

```verse
Transform(X:int):string = "{X}"
Transform(X:float):string = "{X:0.2f}"
Transform(X:string):string = "String: {X}"

Result1 := Transform(42)        # Calls int version
Result2 := Transform(3.14)      # Calls float version
Result3 := Transform("Hello")   # Calls string version
```

However, overloading has limitations. You cannot create overloads where a single argument could satisfy multiple function signatures. This restriction prevents ambiguity and ensures that function calls can always be resolved unambiguously at compile time.

For classes and interfaces, Verse takes a different approach. Instead of overloading, it uses method overriding through inheritance and interface implementation. This design choice reflects the fact that class relationships can evolve over time, and what seems like non-overlapping types today might develop a subtyping relationship tomorrow.

## Practical Patterns and Best Practices

Working with functions in Verse often involves recognizing and applying common patterns. The option return pattern is particularly useful when you need to handle potential failure while still returning meaningful values:

```verse
SearchArray(Array:[]t, Target:t where t:type)<decides>:int =
    var FoundIndex:?int = false
    for (Index -> Element : Array):
        if (Element = Target):
            set FoundIndex = option{Index}
            break
    FoundIndex?
```

This pattern uses an option type to accumulate a result, failing only if no result was found. It's more elegant than using special sentinel values like -1 for "not found."

Another valuable pattern is using failure as a guard condition. By placing assertions early in a function, you can ensure that subsequent code only executes when preconditions are met:

```verse
CalculateRatio(Numerator:float, Denominator:float)<decides>:float =
    Denominator <> 0.0  # Guard against division by zero
    Numerator / Denominator
```

When designing functions, consider whether they should be open-world or closed-world. Use open-world functions for public interfaces where you want flexibility to evolve the implementation. Reserve closed-world functions for situations where the implementation is truly fundamental and should never change, or when you need compile-time computation.

Remember that Verse's multi-paradigm nature means you can combine functional, imperative, and logic programming styles within your functions. A function might use mutable variables internally for efficiency while presenting a pure functional interface to its callers, or it might use logic programming constructs to express complex constraints while computing imperative results.

## Advanced Considerations

As you become more comfortable with Verse functions, you'll discover that they interact with the language's other features in sophisticated ways. The type system's support for dependent types means that functions can compute types at compile-time, enabling patterns that would be impossible in many other languages. The metaverse-first design philosophy means that functions can transparently work with distributed resources as if they were local, hiding complexity that would typically require extensive networking code.

The distinction between compile-time and runtime in Verse is more fluid than in traditional languages. Closed-world functions blur this boundary by executing during compilation, while the abstract execution model used during checking allows you to write code that verifies complex properties without separate proof obligations.

Functions in Verse are not just subroutines that compute values; they're the fundamental building blocks for expressing computation, abstraction, and even distributed behavior in a unified way. Whether you're writing simple utility functions or complex system behaviors, understanding the full depth of Verse's function system will help you write more elegant, maintainable, and powerful code.

The journey from simple function definitions to mastering effects, overloading, and compile-time computation represents a progression in understanding not just Verse's syntax, but its underlying philosophy of unifying different programming paradigms into a coherent whole. As you explore these concepts, you'll find that functions in Verse offer both the familiarity of traditional programming and the power to express ideas that go beyond conventional language boundaries.
