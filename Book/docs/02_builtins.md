# Built-in Data Types

Verse provides a rich set of built-in types that cover the full spectrum of programming needs. The numeric types `int`, `float`, and `rational` handle mathematical operations, counters, and measurements. The `logic` type represents boolean values for conditions and flags. Text is handled through `char`, `char32`, and `string` types for character data, player names, and messages. Container types like arrays, maps, optionals, and tuples manage collections and structured data. Two special types, `any` and `void`, serve unique roles in the type hierarchy as the supertype of all types and the empty type respectively.

Let's explore each built-in type in detail, starting with the numeric types that form the backbone of game logic.

## Integers

The `int` type represents integer, non-fractional values. An `int` can contain a positive number, a negative number, or zero. Supported integers range from `-9,223,372,036,854,775,808` to `9,223,372,036,854,775,807`, inclusive.

You can include `int` values within your code as literals.

<!--verse
# The scary number does not compile due to a front end issue. It should work
F():void={
-->
```verse
A :int= -42                                 # civilian size
B := 42424242424242424242424242424242424242424242424242 # scary 

AnswerToTheQuestion :int= 42               # A variable that never changes
CoinsPerQuiver :int= 100                   # A quiver costs this many coins
ArrowsPerQuiver :int= 15                   # A quiver contains this many arrows

var Coins :int= 225                        # The player currently has 225 coins
var Arrows :int= 3                         # The player currently has 3 arrows
var TotalPurchases :int= 0                 # Track total purchases
```
<!--verse
}
-->

You can use the four basic math operations with integers: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

<!--verse
F(MyInt:int,MyHugeInt:int):void={
-->
```verse
var C :int= (-MyInt + MyHugeInt - 2) * 3   # arithmetic
set C += 1                                 # like saying, set C = C + 1
set C *= 2                                 # like saying, set C = C * 2
```
<!--verse
}
-->

For integers, the operator `/` is failable, and the result is a `rational` type if it succeeds.

## Rationals

`rational` represents the result of integer division. Unlike `int` or `float`, you cannot write a `rational` literal directly. Instead, rationals arise only as intermediate results when dividing integers with the `/` operator.  
Because rational numbers are not meant to be used as general-purpose values, their role is intentionally limited. They serve as an intermediate type that can be rounded to an integer when needed.  

<!--verse
F()<decides>:void={
-->
```verse
X := 7 / 3    # type of X is rational
```
<!--verse
}
-->

Here, `X` is not an `int` and not a `float`. It is a `rational`, representing the exact ratio `7 ÷ 3`.  

Since rationals are mainly useful for rounding, two functions consume them:  

- `Floor[]` — rounds a rational down to the nearest integer.  
- `Ceil[]` — rounds a rational up to the nearest integer.  

<!--verse
F()<decides>:void={
-->
```verse
Quotient1 :int = Floor(7 / 3)   # Quotient1 = 2
Quotient2 :int = Ceil(7 / 3)    # Quotient2 = 3
```
<!--verse
}
-->

These functions are the only way to convert a `rational` to an `int` directly.  

Rationals are often used in game logic to determine how many items a player can afford or carry when resources are limited.  

<!--verse
F()<decides>:void={
-->
```verse
Coins :int = 225
CoinsPerQuiver :int = 100
ArrowsPerQuiver :int = 15

if (NumberOfQuivers := Floor(Coins / CoinsPerQuiver)):
    TotalArrows :int = NumberOfQuivers * ArrowsPerQuiver
```
<!--verse
}
-->

Here, the rational `Coins / CoinsPerQuiver` represents the exact division of coins into quivers. Applying `Floor` converts it into the number of whole quivers the player can actually buy.  

## Floats

The `float` type represents all non-integer numerical values. It can hold large values and precise fractions.

The float type is used for storing and handling floating point numbers, such as `1.0`, `-50.5`, and `3.14159`. A float is an IEEE 64-bit float, which means it can contain a positive or negative number that has a decimal point in the range `[-2^1024 + 1, … , 0, … , 2^1024 - 1]`, or has the value `NaN` (Not a Number).

The implementation for float differs from the IEEE standard in the following ways:

- There is only one `NaN` value.
-`NaN` is equal to itself.
- Every number is equal to itself. If two numbers are equal, then no pure code can observe the difference between them.
- `0` cannot be negative.

You can include predefined float values within your code as float literals:

<!--verse
F()<transacts><decides>:void={
-->
```verse
A:float = 1.0
B := 2.14
MaxHealth : float = 100.0

var C:float = A + B
C = 3.14              # succeeds
set C -= 3.14
C = 0.0               # succeeds
C = 0                 # compile error; 0 is not a `float` literal
```
<!--verse
}
-->

You can use the four basic math operations with floats: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

There are also combined operators for doing the basic math operations (addition, subtraction, multiplication, and division), and updating the value of a variable:

<!--verse
F()<transacts>:void={
-->
```verse
var CurrentHealth : float = 100.0
set CurrentHealth /= 2.0    # Halves the value of CurrentHealth
set CurrentHealth += 10.0   # Adds 10 to CurrentHealth
set CurrentHealth *= 1.5    # Multiplies CurrentHealth by 1.5
```
<!--verse
}
-->

To convert an `int` to a `float`, multiply it by `1.0`: `MyFloat := MyInt * 1.0`.

## Booleans

The `logic` type represents the Boolean values `true` and `false`.

<!--verse
F()<decides>:void={
-->
```verse
A:logic = true
B := false

A = B             # fails
A?                # succeeds
B?                # fails

true?             # succeeds
false?            # fails
```
<!--verse
}
-->

The `logic` type only supports query operations and comparison operations.

Query expressions use the query operator `?` to check if a logic value is true and fail if the logic value is `false`.

For comparison operations, use the failable operator `=` to test if two logic values are the same, and `<>` to test for inequality.

Many programming languages find it idiomatic to use a type like `logic` to signal the success or failure of an operation. In Verse, we use success and failure instead for that purpose, whenever possible. The conditional only executes the `then` branch if the guard succeeds:

<!--verse
ShowTargetLockedIcon():void={}
F(TargetLocked:?int):void={
-->
```verse
 if (TargetLocked?):
    ShowTargetLockedIcon()
```
<!--verse
}
-->

To convert an expression that has the `<decides>` effect to `true` on success or `false` on failure, use
`logic{ exp }`:

<!--verse
using{ /Verse.org/Random }
F(Frequency:int)<decides>:void={
-->
```verse
GotIt := logic{GetRandomInt(0, Frequency) <> 0}   # if success
GotIt?                                            # then this succeeds
GotIt = false                                     # and this fails
not GotIt?                                        # and this fails too
```
<!--verse
}
-->

## Characters and Strings

Text is represented in terms of characters and strings.  

A `char` is a single **UTF-8 code unit** (not a full Unicode code point). A string is therefore an array of characters, written as `[]char`. For convenience, the type alias `string` is provided for `[]char`:  

<!--verse
F():void={
-->
```verse
MyName :string = "Joseph"
MyAlterEgo := "José"
```
<!--verse
}
-->

UTF-8 is used as the character encoding scheme. Each UTF-8 code unit is one byte. A Unicode code point may require between one and four code units. Code points with lower values use fewer bytes, while higher values require more.  

For example:  

- `"a"` requires one byte (`{0o61}`),  
- `"á"` requires two bytes (`{0oC3}{0oA1}`),  
- `"🐈"` (cat emoji) requires four bytes (`{0u1f408}`).  

Thus, strings are sequences of code units, not necessarily sequences of Unicode characters in the abstract sense.  

Because strings are arrays of `char`, you can index into them with `[]`. Indexing has the `<decides>` effect: it succeeds when the index is valid and fails otherwise.  

<!--verse
F(MyName:string):void={
-->
```verse
TheLetterJ := MyName[0]     # succeeds
TheLetterJ = 'J'            # succeeds
MyName[100]                 # fails
```
<!--verse
}
-->

The length of a string is the number of UTF-8 code units it contains, accessed via `.Length`. Note that this is *not the same as the number of Unicode characters*:  

<!--verse
F()<decides>:void={
-->
```verse
"José".Length = 5           # succeeds; 5 UTF-8 code units
"Jose".Length = 4           # succeeds; 4 UTF-8 code units
```
<!--verse
}
-->

Because `string` is just `[]char`, strings declared as `var` can be mutated:  

<!--verse
F()<decides>:void={
-->
```verse
var OuterSpaceFriend :string = "Glorblex"
set OuterSpaceFriend[0] = 'F'
```
<!--verse
}
-->

Strings can be concatenated using the `+` operator:  

<!--verse
F(MyName:string,MyAlterEgo:string):void={
-->
```verse
MyAttemptAtFormatting := "My name is " + MyName + " but my alter ego is " + MyAlterEgo + "."
```
<!--verse
}
-->

Verse also supports string interpolation for more readable formatting:  

<!--verse
F(MyName:string,MyAlterEgo:string):void={
-->
```verse
Formatting := "My name is {MyName} but my alter ego is {MyAlterEgo}."
```
<!--verse
}
-->

Interpolation works for any value that has a `ToString()` function in scope.  

Literal characters are written with single quotes. The type depends on whether the character falls within the ASCII range (`U+0000`–`U+007F`) or not:  

- `'e'` has type `char`,  
- `'é'` has type `char32`.  

<!--verse
F():void={
-->
```verse
A :char = 'e'                       # ok
B :char32 = 'é'                     # ok
# C :char = 'é'                     # error: type of 'é' is char32
# D :char32 = 'e'                   # error: type of 'e' is char
```
<!--verse
}
-->

Character literals can also be written using numeric escape sequences:  

<!--verse
F():void={
-->
```verse
E :char = 0o65                      # ok; same as 'e'
F :char32 = 0u00E9                  # ok; same as 'é'
```
<!--verse
}
-->

- `char` represents a single UTF-8 code unit (one byte, `0oXX`).  
- `char32` represents a full Unicode code point (`0uXXXX`).  

Unlike some languages, Verse does not allow implicit conversion between characters and integers.  

Strings can be compared using the failable operators `=` (equality) and `<>` (inequality). Comparison is done by code point, and is case sensitive.  

Equality depends on exact code unit sequences, not visual appearance. Unicode allows multiple encodings for the same abstract character. For example, `"é"` may appear as the single code point `{0u00E9}`, or as the two-code-point sequence `"e"` (`{0u0065}`) plus a combining accent (`{0u0301}`). These two strings look the same, but they are not equal in Verse.  

Checking whether a player has selected the correct item:  

<!--verse
F()<transacts>:logic={
-->
```verse
ExpectedItemInternalName :string = "RedPotion"
SelectedItemInternalName :string = "BluePotion"

if (SelectedItemInternalName = ExpectedItemInternalName):
    return true 
return false
```
<!--verse
}
-->

Padding a timer with leading zeros:  

<!--verse
F()<transacts>:void={
-->
```verse
SecondsLeft :int = 30
SecondsString :string = SecondsLeft    # convert int to string

var Combined :string = "Time Remaining: "
if (SecondsString.Length > 2):
    set Combined += "99"               # clamp to maximum
else if (SecondsString.Length < 2):
    set Combined += "0{SecondsString}" # pad with zero
else:
    set Combined += SecondsString
```
<!--verse
}
-->

Certain characters have special meaning inside strings and must be escaped:  

- `{` and `}` are used for interpolation and code points, so literal braces must be written as `"\{\}"`.  
- `"\n"` represents a newline.  

## Optionals

An optional is an immutable container that either holds a value of type `t` or nothing at all. The type is written `?t`. Optionals are useful whenever a value may or may not be present, such as when looking up a key in a map or calling a function that can fail. By making this possibility explicit in the type, Verse allows programmers to handle “no result” situations directly and consistently, instead of relying on ad hoc error codes or special values.

You can create a non-empty optional with `option{...}`, which wraps a value into an optional. For example:

<!--verse
F():void={
-->
```verse
A:?int = option{42}    # an optional containing the integer 42
```
<!--verse
}
-->

If you want to represent “no value,” you use the special constant `false`. This is how Verse spells the empty optional:

<!--verse
F()<decides>:void={
-->
```verse
var B:?int = false     # this optional has no element
B = false              # still empty
```
<!--verse
}
-->

To extract the element of an optional, you write `?` after the optional expression. This produces a `<decides>` expression that succeeds if the optional has an element and fails otherwise. For example:

<!--verse
F(A:?int)<decides>:void={
-->
```verse
S := A? + 2            # succeeds with 44 because A contains 42
```
<!--verse
}
-->

If `A` had been `false`, then the attempt to use `A?` would fail and so would the whole computation. A failing case makes this clearer:

<!--NoCompile-->
```verse
T := B? + 1            # fails, because B is false and has no element
```

This shows how Verse integrates optionals tightly with the effect system: the presence or absence of a value can cause an entire computation to succeed or fail.

The `option{...}` form also works in the opposite direction. When you have a computation with the `<decides>` effect, wrapping it in `option{...}` converts it to an optional. On success you get a non-empty optional; on failure you get `false`:

<!--NoCompile-->
```verse
MaybeAFloat := option{GetAFloatOrFail[]}
```

This symmetry is important. The `?` operator unwraps an optional into a `<decides>` expression, while `option{...}` wraps a `<decides>` expression into an optional. Together they provide a smooth bridge between computations that may fail and values that may be absent.

Although an optional value itself is immutable, you can keep one in a variable and change which optional the variable points to. The keyword `set` is used for this:

<!--verse
F()<decides>:void={
-->
```verse
var C:?int = false
set C = option{2}      # C now refers to an optional containing 2
C? = 2                 # succeeds, since C is not empty
```
<!--verse
}
-->

This ability is useful whenever you want to track success or failure over time, such as gradually computing a result and updating the variable only when you succeed.

A common use case is searching for something that may or may not be there. Imagine a function `Find` that looks through an array of integers and returns the index of the element you want. If the element exists, the function returns `option{index}`; if not, it returns `false`. The caller can then safely decide what to do:

<!--verse
Find(N:[]int, X:int):?int =
    for {I := 0..N.Length} do
        if (N[I] = X) then return option{I}
    return false

F()<decides>:void=
    var Numbers:[]int = array{10, 20, 30}
    Idx:?int = Find[Numbers, 20]    # succeeds with option{1}
    Y := Idx?                       # succeeds with 1
<#
-->
```verse
var Numbers:[]int = array{10, 20, 30}

Find[N:[]int, X:int]:?int =
    for {I := 0..N.Length} do
        if N[I] = X then return option{I}
    return false

Idx:?int = Find[Numbers, 20]    # succeeds with option{1}
Y := Idx?                       # succeeds with 1
```
<!--verse
#>
-->

Here the optional signals the possibility of failure directly in the type. The `?` operator makes it easy to use the result in an expression, while `option{...}` allows you to turn conditional computations back into optionals. The effect is that the idea of “maybe a value, maybe not” becomes a first-class part of the language, rather than an afterthought, and programmers are encouraged to handle the absence of values in a disciplined way.

## Tuple

A tuple is a container that groups two or more values. Unlike arrays, which can only contain elements of one type, tuples allow you to combine values of mixed types and treat them as a unit. The elements of a tuple appear in the order in which you list them, and you access them by their position, called the index. Because the number of elements is always known at compile time, a tuple is both simple to create and safe to use.

The term *tuple* is a back formation from *quadruple*, *quintuple*, *sextuple*, and so on. Conceptually, a tuple is like an unnamed data structure with ordered fields, or like a fixed-size array where each element may have a different type.

A tuple literal is written by enclosing a comma-separated list of expressions in parentheses. For example:

<!--NoCompile-->
```verse
(1, 2, 3)
```

The order of elements matters, so `(3, 2, 1)` is a completely different value. Since tuples allow mixed types, you might write:

<!--NoCompile-->
```verse
(1, 2.0, "three")
```

Tuples can also nest inside each other:

<!--verse
X:tuple(int,tuple(int,float,string),string)=
-->
```verse
(1, (10, 20.0, "thirty"), "three")
```

Tuples are useful when you want to return multiple values from a function or when you want a lightweight grouping of values without the overhead of defining a struct or class. The type of a tuple is written with the `tuple` keyword followed by the types of the elements, but in most cases it can be inferred. For instance, you can write `MyTuple : tuple(int, float, string) = (1, 2.0, "three")`, or simply `MyTuple := (1, 2.0, "three")` and let the compiler deduce the type.

The elements of a tuple are accessed using a zero-based index operator written with parentheses. If `MyTuple := (1, 2.0, "three")`, then `MyTuple(0)` is the integer `1`, `MyTuple(1)` is the float `2.0`, and `MyTuple(2)` is the string `"three"`. Because the compiler knows the number of elements in every tuple, tuple indexing cannot fail: any attempt to use an out-of-bounds index results in a compile-time error.

Another feature of tuples is *expansion*. When a tuple is passed to a function as a single argument, its elements are automatically expanded as if the function had been called with each element separately. For example:

```verse
F(Arg1:int, Arg2:string):void =
    Print("{Arg1}, {Arg2}")

G():void =
    MyTuple := (1, "two")
    F(MyTuple)   # expands to F(1, "two")
```

Tuples also play a role in structured concurrency. The `sync` expression produces a tuple of results, allowing several computations that unfold over time to be evaluated simultaneously. In this way, tuples provide not only a convenient grouping mechanism but also a foundation for composing concurrent computations.

## Arrays

An array is an immutable container that holds zero or more values of the same type `t`. The elements of an array are ordered, and each can be accessed by a zero-based index. Arrays are written with square brackets in their type, for example `[]int` or `[]float`, and are created with the `array{...}` literal form. For instance, `A : []int = array{}` creates an empty array, while `B : []int = array{1, 2, 3}` creates an array of three integers. Accessing elements by index is a failable operation: `B[0]` succeeds with the value `1`, while `B[10]` fails because the index is out of bounds.

Arrays can be concatenated with the `+` operator, and when declared as `var` they can be extended with the shorthand operator `+=`. For example, `var C:[]int= B + array{4}` gives `C` the value `array{1,2,3,4}`, and `set C += array{5}` updates it to `array{1,2,3,4,5}`. The length of an array is available through the `.Length` member, so `C.Length` here would be `5`. Elements are always stored in the order they are inserted, and indexing starts at `0`. Thus `array{10,20,30}[0]` is `10`, and the last valid index of any array is always one less than its length.

Although arrays themselves are immutable, variables declared with `var` can be reassigned to new arrays, or can appear to have their elements changed. For example, `var D:[]int = array{1,2,3}` allows the update `set D[0] = 3`, after which `D` will hold `array{3,2,3}`. What actually happens is that a brand new array is created under the hood, with the specified element updated. In effect, `set D[0] = 3` is compiled into `set D = array{3,D[1],D[2]}`. The old array continues to exist if another variable was referencing it, which means that if `A` and `B` both start as `array{1}` and we update `A[0]`, then `A` and `B` will diverge: `A[0]` is now `2` while `B[0]` is still `1`.

Arrays are useful whenever you want to store multiple values of the same type, such as a list of players in a game: `Players:[]player = array{Player1,Player2}`. Access is by index, for example `Players[0]` is the first player. Since indexing is failable, it is often combined with `if` expressions or iteration. For instance, the following code safely prints out every element of an array:  

<!--verse
using { /Verse.org/VerseCLR }
F():void={
-->
```verse
ExampleArray : []int = array{10, 20, 30, 40, 50}
for (Index := 0..ExampleArray.Length - 1):
    if (Element := ExampleArray[Index]):
        Print("{Element} in ExampleArray at index {Index}")
```
<!--verse
}
-->

which produces  

```
10 in ExampleArray at index 0
20 in ExampleArray at index 1
30 in ExampleArray at index 2
40 in ExampleArray at index 3
50 in ExampleArray at index 4
```

Because arrays are values, “changing” them always means replacing the old array with a new one. With `var` this feels natural, since variables can be reassigned. For example, you can concatenate arrays and then update an element:  

<!--verse
F():void={
-->
```verse
Array1 : []int = array{10, 11, 12}
var Array2 : []int = array{20, 21, 22}
set Array2 = Array1 + Array2 + array{30, 31}
if (set Array2[1] = 77) {}
```
<!--verse
}
-->

After this code runs, iterating through `Array2` prints `10, 77, 12, 20, 21, 22, 30, 31`.

Arrays can also be nested to form multi-dimensional structures, similar to rows and columns of a table. For example, the following creates a two-dimensional 4×3 array of integers:

<!--verse
F():void={
-->
```verse
var Counter : int = 0
Example : [][]int =
    for (Row := 0..3):
        for (Column := 0..2):
            set Counter += 1
```
<!--verse
}
-->

This array can be visualized as  

```
Row 0:  1  2  3
Row 1:  4  5  6
Row 2:  7  8  9
Row 3: 10 11 12
```

and is accessed with two indices: `Example[0][0]` is `1`, `Example[0][1]` is `2`, and `Example[1][0]` is `4`. You can loop through all rows and columns with nested iteration. Arrays in Verse are not restricted to rectangular shapes: each row can have a different length, producing a jagged structure. For example,  

<!--verse
F():void={
-->
```verse
Example : [][]int =
    for (Row := 0..3):
        for (Column := 0..Row):
            Row * Column
```
<!--verse
}
-->

produces a triangular array with rows of increasing length: row 0 has none, row 1 has a single `0`, row 2 has `0, 2, 4`, and row 3 has `0, 3, 6, 9`.

Arrays in Verse are thus immutable values with predictable behavior, but through `var` they offer the convenience of mutable variables. They can be concatenated, iterated, or nested, and are one of the most flexible and fundamental data structures in the language.

## Maps

Maps are one of the core container types, alongside arrays and optionals. If arrays are ordered sequences indexed by integers, and optionals are the smallest container of all, holding either zero or one value, then Maps generalize both ideas: like arrays, they provide efficient lookup, but instead of being limited to integer indices, they allow any *comparable* type as a key. You can think of a map as an array indexed by arbitrary keys, or as a larger optional that can hold many key–value associations at once.

A map is an immutable associative container that stores zero or more key–value pairs of type `[k]v`, written as `(Key:k, Value:v)`. Maps are the standard way to associate values with other values: you supply a key, and the map returns the value associated with it.

Maps are useful whenever you want to store data that is naturally indexed by something other than an integer position. For example, you might want to store the weights of different objects keyed by their names:  

<!--verse
F():void={
-->
```verse
Empty := map{}

var Weights:[string]float = map{
    "ant" => 0.0001,
    "elephant" => 500.0,
    "galaxy" => 500000000000.0
}
```
<!--verse
}
-->

Looking up a value in a map uses square brackets. The expression succeeds if the key is present and fails if it is not. Lookups are designed to be fast, with amortized *O(1)* time complexity:  

<!--verse
F(Weights:[string]float)<decides>:void={
-->
```verse
0.00001 < Weights["ant"]    # succeeds, since "ant" is a key
Weights["car"]              # fails, since "car" is not a key
```
<!--verse
}
-->

If you want to update a map stored in a variable, you use `set`. This works both for adding a new key–value pair and for changing the value of an existing key. If you try to modify a key that is not present, the operation fails:  

<!--verse
F()<decides><transacts>:void={
-->
```verse
var Friendliness:[string]int = map{"peach" => 1000}

set Friendliness["pelican"] = 17     # add a new key
set Friendliness["peach"] += 2000    # update an existing key
set Friendliness["tomato"] += 1000   # fails; "tomato" is not in the map
```
<!--verse
}
-->

Every map also carries its size, accessible as the `Length` field:  

<!--verse
F(Friendliness:[string]int)<decides>:void={
-->
```verse
Friendliness.Length = 2              # the map has 2 entries
```
<!--verse
}
-->

When constructing a map with duplicate keys, only the last value is kept. This is because a map enforces uniqueness of keys, so earlier entries are silently overwritten:  

<!--verse
F():void={
-->
```verse
WordCount:[string]int = map{
    "apple" => 0,
    "apple" => 1,
    "apple" => 2
}
# WordCount contains only {"apple" => 2}
```
<!--verse
}
-->

Maps can also be iterated over, letting you traverse all key–value pairs exactly in the order they were inserted:  

<!--verse
using { /Verse.org/VerseCLR }
F():void={
-->
```verse
ExampleMap:[string]string = map{
    "a" => "apple",
    "b" => "bear",
    "c" => "candy"
}

for (Key -> Value : ExampleMap):
    Print("{Value} in ExampleMap at key {Key}")
```
<!--verse
}
-->

This produces:  

- “apple in ExampleMap at key a”  
- “bear in ExampleMap at key b”  
- “candy in ExampleMap at key c”  

Sometimes you want to remove an entry from a map. Since maps are immutable, “removing” means creating a new map that excludes the given key. For example, here is a function that removes an element from a `[string]int` map:  

```verse
RemoveKeyFromMap(TheMap:[string]int, ToRemove:string):[string]int =
    var NewMap:[string]int = map{}
    for (Key -> Value : TheMap, Key <> ToRemove):
        set NewMap = ConcatenateMaps(NewMap, map{Key => Value})
    return NewMap
```

The key type of a map must belong to the class `comparable`, which guarantees that two keys can be checked for equality. All basic scalar types such as `int`, `float`, `rational`, `logic`, `char`, and `char32` are comparable, and so are compound types like arrays, maps, tuples, and `struct`s whose components are comparable. Classes and interfaces cannot be used as keys, since their instances do not provide a built-in notion of equality.  

### Weak Maps

The `weak_map` type is a supertype of `map`. It behaves similarly to ordinary maps, but it deliberately restricts what you can do. You cannot ask for its length, you cannot iterate over its entries, and you cannot use `ConcatenateMaps`. These restrictions make `weak_map` lighter and in some contexts more efficient, but at the cost of flexibility.

A `weak_map` is declared with `weak_map(k,v)` and can be initialized from an ordinary `map{}`. Updating and accessing values works the same way:  

<!--verse
F()<decides>:void={
-->
```verse
var MyWeakMap:weak_map(int,int) = map{}

set MyWeakMap[0] = 1
Value := MyWeakMap[0]         # succeeds with 1

set MyWeakMap = map{0 => 2}   # reassignment still works
```
<!--verse
}
-->

Because `weak_map` is a supertype of `map`, you can switch between the two when needed, but you lose the ability to count or iterate once you are working with a weak map.

## Any

The `any` type is the *supertype of all types*. Every type in the language is a subtype of `any`. Because of this, `any` itself supports very few operations: whatever functionality `any` provides must also be implemented by every other type. In practice, there is very little you can do directly with values of type `any`. Still, it is important to understand the type, because it sometimes arises when working with code that mixes different kinds of values, or when the type checker has no more precise type to assign.  

One way `any` appears is when combining values that do not share a more specific supertype. For example:  

```verse
Letters := enum:
    A
    B
    C

letter := class:
    Value : char
    Main(Arg : int) : void =
        X := if (Arg > 0) then:
            Letters.A
        else:
            letter{Value := 'D'}
```

In this example, `X` is assigned either a value of type `Letters` or of type `letter`. Since these two types are unrelated, the compiler assigns `X` the type `any`, which is their lowest common supertype.  

A more useful role for `any` is as the type of a parameter that is required syntactically but not actually used. This pattern can arise when implementing interfaces that require a certain method signature.  

```verse
FirstInt(X:int, :any) : int = X
```

Here, the second parameter is ignored. Because it can be any value of any type, it is given the type `any`.  

In more general code, the same idea can be expressed using *parametric types*, making the function flexible while still precise:  

```verse
First(X:t, :any where t:type) : t = X
```

This version works for any type `t`, returning a value of type `t` while discarding the unused argument of type `any`.  

## Void

The `void` type is the *empty type*. Unlike `any`, which contains all possible values, `void` contains none. It represents the absence of a value and is used in places where no result is returned.  

Because `void` has no values, you can never construct or assign a value of type `void`. This makes it useful as a marker type in function signatures and control flow.  

A function whose purpose is to perform an effect, rather than compute a value, has return type `void`.  

<!--verse
Print(:string):void={}
-->
```verse
LogMessage(Msg:string) : void =
    Print(Msg)
```

Here, `LogMessage` performs an action (printing) but does not return a result. The `void` return type makes that explicit.
