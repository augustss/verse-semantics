# Functions

Functions are reusable code blocks that perform actions and produce outputs based on inputs. Think of them as abstractions for behaviors, much like ordering food from a menu at a restaurant. When you order, you tell the waiter what you want from the menu, such as `OrderFood("Ramen")`. You don't need to know how the kitchen prepares your dish, but you expect to receive food after ordering. This abstraction is what makes functions powerful - you define the instructions once and reuse them in different contexts throughout your code.

Verse takes a unique approach to functions by supporting three programming paradigms simultaneously: functional programming with lambdas and immutable data structures, imperative programming with mutable variables and pointers, and logic programming where programs describe equations to solve. This multi-paradigm approach makes functions particularly versatile.

## Parameters and Arguments

Functions can accept any number of parameters, from none at all to as many as needed. The syntax follows a straightforward pattern where each parameter has an identifier and a type, separated by commas:

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

## Return Values

Functions return the value of the last executed expression, which often eliminates the need for explicit return statements. This design choice leads to more concise code:

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

Functions with void return types are special - they always return the value `false`, regardless of what expressions appear in their body. This consistent behavior simplifies the handling of void functions in logical contexts.

## Understanding Failure

Effects describe additional behaviors that functions can exhibit beyond simply computing a result. An important effect is `decides`, which indicates that a function can fail in a way that callers must handle.

When a function has the `decides` effect, it fundamentally changes how you interact with it. Consider this example:

```verse
ValidateInput(X:int)<decides>:int =
    X > 0  # Fails if X <= 0
    X * 2  # Only executes if validation passes
```

The `decides` effect requires special calling syntax, you must use square brackets instead of parentheses:

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

## Types and Overloading

Every function has a type that captures its parameters, effects, and return value. The type syntax uses an underscore as a placeholder for the function name:

```verse
type{_(:int, :string)<decides>:float}
```

This represents any function that takes an integer and a string, might fail (has the `decides` effect), and returns a float when successful.

Multiple functions may share a name through overloading, as long as their signatures don't create ambiguity. The compiler can distinguish between overloads based on the argument types:

```verse
Transform(X:int):string = "{X}"
Transform(X:float):string = "{X:0.2f}"
Transform(X:string):string = "String: {X}"

Result1 := Transform(42)        # Calls int version
Result2 := Transform(3.14)      # Calls float version
Result3 := Transform("Hello")   # Calls string version
```

However, overloading has limitations. You cannot create overloads where a single argument could satisfy multiple function signatures. This restriction prevents ambiguity and ensures that function calls can always be resolved unambiguously at compile time.

## Best Practices

Working with functions often involves recognizing and applying common patterns. The option return pattern is particularly useful when you need to handle potential failure while still returning meaningful values:

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

## Publishing Functions and Transparency

Publishing a function is a promise of backwards compatibility between the function and its clients. Consider this simple function:

```verse
F1<public>(X:int):int = X + 1
```

The type annotation (`X:int):int`) tells us that this function promises that given any integer it will always return an integer. That contract cannot be broken in future versions of the code. The implementation could change in the future, perhaps to perform additional operations or optimizations, as long as it maintains these type constraints.

Now consider a slightly different version:

```verse
F2<public>(X:int) := X + 1
```

The type of this function is inferred from its body. This implies a very different promise: this syntax creates a forever guarantee - the right-hand side will remain exactly the same throughout the lifetime of your code.  Sometimes functions like these are referred to as *transparent*, this transparency allows for powerful compile-time computations.
