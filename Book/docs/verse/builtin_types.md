# Built-in Data Types

Built-in data types are the basic building blocks of computation. These include numeric types (`int`, `float`,  `rational`), booleans (`logic`), sequences of characters (`char` and `string`). Two handy types are `any`, the supertype of all types, and `void`, the empty type.

## Integers

The `int` type represents integer, non-fractional, values.  An `int` can contain a positive number, a negative number, or zero. Supported integers range from `-9,223,372,036,854,775,808` to `9,223,372,036,854,775,807`, inclusive.

You can include `int` values within your code as literals.

```verse
A :int= -42                                      # civilian size
B := 42424242424242424242424242424242424242424242424242 # scary 

AnswerToTheQuestion :int= 42                     # A variable that never changes
CoinsPerQuiver :int= 100                         # A quiver costs this many coins
ArrowsPerQuiver :int= 15                         # A quiver contains this many arrows

var Coins :int= 225                              # The player currently has 225 coins
var Arrows :int= 3                               # The player currently has 3 arrows
var TotalPurchases :int= 0                       # Track total purchases
```

You can use the four basic math operations with integers in Verse: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

```verse
var C :int= (-MyInt + MyHugeInt - 2) * 3        # arithmetic
set C += 1                                      # like saying, set C = C + 1
set C *= 2                                      # like saying, set C = C * 2
```

For integers, the operator `/` is failable, and the result is a `rational` type if it succeeds.

The following code uses integer division to determine how many arrows the player can buy with their coins:

```verse
if (NumberOfQuiversYouCanBuy := Floor(Coins / CoinsPerQuiver)):
    NumberOfArrowsYouCanBuy :int= NumberOfQuiversYouCanBuy * ArrowsPerQuiver
```

## Rationals

The rational type can only be used as a parameter to the following functions:

- `Floor()`: Rounds the rational value down to the closest integer.
- `Ceil()`: Rounds the rational value up to the closest integer.

## Floats

The float type represents all non-integer numerical values. It can hold large values and precise fractions.

Verse uses float as the type for storing and handling floating point numbers, such as 1.0, -50.5, and 3.14159. A float in Verse is an IEEE 64-bit float, which means it can contain a positive or negative number that has a decimal point in the range [-2^1024 + 1, … , 0, … , 2^1024 - 1], or has the value NaN (Not a Number).

The implementation for float differs from the IEEE standard in the following ways:

- There is only one NaN value.
- NaN is equal to itself.
- Every number is equal to itself. If two numbers are equal, then no pure Verse code can observe the difference between them.
- 0 cannot be negative.

You can include predefined float values within your code as float literals. A float literal is a floating point number in your code:

```verse
A:float = 1.0
B := 2.14
MaxHealth : float = 100.0

var C:float = A + B
C = 3.14                  # succeeds

set C -= 3.14
C = 0.0                   # succeeds

C = 0                     # compile error; 0 is not a `float` literal

```

You can do the four basic math operations with floats: `+` for addition, `-` for subtraction, `*` for multiplication, and `/` for division.

There are also combined operators for doing the basic math operations (addition, subtraction, multiplication, and division), and updating the value of a variable. These combined operators are the same as assigning the result to the first operand of the math operation.

```verse
var CurrentHealth : float = 100.0
set CurrentHealth /= 2.0    # Halves the value of CurrentHealth
set CurrentHealth += 10.0   # Adds 10 to CurrentHealth
set CurrentHealth *= 1.5    # Multiplies CurrentHealth by 1.5
```

To convert an `int` to a `float`, multiply it by `1.0`: `MyFloat := MyInt * 1.0`.

## Booleans

The `logic` type represents the Boolean values `true` and `false`.

```verse
A:logic = true
B := false

A = B                              # fails
A?                                 # succeeds
B?                                 # fails

true?                              # succeeds
false?                             # fails
```

The `logic` type only supports query operations and comparison operations.

Query expressions use the query operator `?` to check if a logic value is true and fail if the logical is `false`.

For comparison operations, use the failable operator `=` to test if two logic values are the same, and `<>` to test for inequality.

Many programming languages find it idiomatic to use a type like `logic` to signal the success or failure of an operation. In Verse, it's considered idiomatic to prefer using the `<decides>` effect instead of `logic` for that purpose, whenever possible. The conditional only executes the `then` branch if the guard succeeds:

```verse
 if (TargetLocked?):
    ShowTargetLockedIcon()
```

To convert an expression that has the `<decides>` effect to `true` on success or `false` on failure, use
`logic{ exp }`:

```verse
GotIt := logic{GetRandomInt(0, Frequency) <> 0}   # if success
GotIt?                                            # then succeeds
GotIt = false                                     # fails
not GotIt?                                        # fails
```

## Characters and Strings

A `char` is a single 8-byte UTF8 code unit (not code-point). Strings in Verse are thus represented as `[]char` (pronounced "array of `char`s"), or by its more common type alias `string`:

```verse
MyName:string = "Joseph"
MyAlterEgo := "José"
```

Verse uses the UTF-8 Unicode character-encoding scheme, a standard developed by the Unicode Consortium to provide comparable support for characters across languages, platforms, and devices. For example, the emoji in this string "🐈" can be represented by its Unicode code point "{0u1f408}".  The UTF-8 code unit is 8-bits (one byte), and encodes characters with code points that are one to four bytes long. Code points with a lower value use fewer bytes than code points with higher values. For example, "a" uses one byte "{0o61}", while "á" uses two bytes "{0oC3}{0oA1}".

The string's individual `char`s are accessed using `[]`. Like all function calls that use `[]`, this has the `<decides>` effect:

```verse
TheLetterJ := MyName[0]            # succeeds
TheLetterJ = 'J'                   # succeeds
MyName[100]                        # fails
```

`string.Length` is just `[]char.Length`, that is, the number of `char`s in the array, or the number of UTF8 _code units_ (not code _points_!):

```verse
"José".Length = 5           # succeeds; 5 utf8 code units (not points)
"Jose".Length = 4           # succeeds; 4 utf8 code units (and points, coincidentally!)
```

Because `string` is `[]char`, strings can be "mutated" through `var`:

```verse
var OuterSpaceFriend:string = "Glorblex"
set OuterSpaceFriend[0] = 'F'

```

To concatenate strings, use the `+` operator:

```verse
MyAttemptAtFormatting := "My name is " + MyName + " but my alter ego is " + MyAlterEgo + "."

```

To make life a little easier, Verse also supports string "interpolation":

```verse
MyAttemptAtFormatting2 := "My name is {MyName} but my alter ego is {MyAlterEgo}."           # ah... much better
MyAttemptAtFormatting = MyAttemptAtFormatting2                                              # succeeds

```

To support Unicode code point literals beyond the ASCII range, the type of `'`_X_`'` is `char` if _X_ is in the range `U+0000` through `U+007F`, or
`char32` otherwise:

 A:char = 'e'                       # ok
 B:char32 = 'é'                     # ok
 C:char = 'é'                       # compile error; the type of 'é' is `char32`
 D:char32 = 'e'                     # compile error; the type of 'e' is `char`

```verse
E:char = 0o145                     # ok; same as 'e'
F:char32 = 0u00E9                  # ok; same as 'é'

```

Unlike some other languages, there is no implicit conversion between characters and integers.

Keep in mind that `[]char`/`string` doesn't automatically validate/guarantee that its contents actually represent a valid UTF8 string; validation is delegated to Verse frameworks and user code.

Strings support concatenation, comparison, indexing, getting the length of the string, and string interpolation.

Concatenation is when one string is appended to another string. You can use the operator + to concatenate strings.
For example, the following code results in the variable Announcement containing the string "...And the winner is: Player One!".

```verse
# The winning player's name:
WinningPlayerName : string = "Player One"
# Build a message announcing the winner.
Announcement : string = "...And the winner is: " + WinningPlayerName + "!"
```

You can inject a value into a string if it has a valid ToString() function defined in the current scope.
For example, the following code results in the variable Announcement containing the string "...And the winner is: Player One!".

```verse
# The winning player's name:
WinningPlayerName : string = "Player One"
# Build a message announcing the winner.
Announcement : string = "...And the winner is: {WinningPlayerName}!"
```

Whether two strings are equal depends on whether they use the same characters.

Comparison of strings in Verse is done by comparing the code points of each character. Comparison of two strings is case sensitive, because uppercase and lowercase characters have different code points.
You can use the failable operator = to test if two strings are equal, and the failable operator <> to test for inequality.
There can be multiple ways to represent the same character in Unicode. For example, "é" is "{0u0049}", but you can also use two code points: "{0u0065}", which is "e", and "{0u0301}", which is a combining accent. This means that if you compare these strings, which both appear to be the character "é" but the strings use different code points, the strings will not be equal. "{0u0049}" is not the same as "{0u0065}{0u0301}".

The following example would check to see if the player has used the correct item to make progress in an adventure/puzzle game:

```verse
# This is the item the puzzle requires to unlock the next step:
ExpectedItemInternalName : string = "RedPotion"
# This is the item that the player has selected:
SelectedItemInternalName : string = "BluePotion"
# Check to see if the player has the right item selected.
if (SelectedItemInternalName = ExpectedItemInternalName):
    # They do! Report that the puzzle can proceed to the next step.
    return true
# They do not. Report that this item does not advance the puzzle.
return false
```

You can get the number of UTF-8 code units in a string by accessing the member Length on the string. For example, "hey".Length is 3.

The length of a string accounts for the amount of data it takes to represent the string in UTF-8 code units. For example, "héy".Length is 4, because it takes an extra UTF-8 code unit to represent the character é, even though the string appears to have three characters. The following code displays a "seconds" timer with two digits. It will pad the display with a leading zero if needed.

```verse
# SecondsRemaining is assumed to be non-negative
SecondsRemaining : int = 30
# Automatically convert the int representation to a string:
SecondsString:string = SecondsRemaining
# Set up the timer display string.
var Combined : string = "Time Remaining: "
# If the string is too long, replace it the maximum two-digit value, 99.
if (SecondsString.Length > 2):
    # Too much time on the clock! Set the string to a hard-coded max value.
    set Combined += "99"
else if (SecondsString.Length < 2):
    # Pad the display with a leading zero.
    set Combined += "0{SecondsString}"
else:
    # The string is already the exact length, so add it.
    set Combined += SecondsString
```

You can access the UTF-8 code unit at a specific index of the string. The first UTF-8 code unit in a string has an index of 0, and each subsequent code unit index increases in number.
For example, "cat"[0] is "c" and "cat"[1] is "a".

In cases where a string has characters that are represented by more than one code unit, there will be an index for each code unit. For example, "á" is represented by two UTF-8 code units "{0oC3}{0oA1}", so "cát"[1] is "{0oC3}" and "cát"[2] is "{0oA1}".

The last index in a string is one less than the length of the string. For example, "cat".Length is 3 and the index for "t" in "cat" is 2.

Alternate Representations of Characters

Some characters have alternate representations when they’re used in a string. For example, "{}" can be used for string interpolation or for the code points of characters, but they can also be used as the brace characters {} themselves in text.
To be able to use an alternate representation of a character in a string, you must add the escape character "\" before the character in the string. For example, "\{\}" is rendered as {} in text, and "\n" starts a new line in text.

The string type is a type alias of []char, an array of UTF-8 code units. Because string is a type alias for an array, string has the same behavior as arrays.
There are two primitive types for characters, depending on their size and code point format — char and char32. The only capabilities of char and char32 in Verse are for comparison, and to access their values.

Primitive Type Description Supported Formats

char

A primitive type that represents a single UTF-8 code unit (one byte), up to the value 256 (0off).
Code units of the form 0oXX. For example, 0o52.

char32

A primitive type that represents a Unicode code point.

Code points of the form 0uXXXX. For example, 0u0041.

You can also express literals with single quotes. Whether the primitive type of the string in single quotes is char or char32 depends on the UTF-8 code units used for the character. For example, 'e' is char, and 'é' is char32.

## `?t`: Optional

An immutable container that holds zero or one elements of type `t`, spelled `?t`:

```verse
A:?int = option{42}                            # the `option` macro is used to construct a nonempty `option(t)` value

```

An empty optional is just `false`:

```verse
var B:?int = false                             # use `false` to mean "this optional has no element"
B = false                                      # succeeds

```

To convert an optional to a `<decides>` expression that produces the optional's element (or fails if the optional is unset) use `?` like this:

```verse
S := A? + 2                                    # `A?` succeeds with 42

```

To convert an expression with the `<decides>` effect into a nonempty `option(t)` on success and `false` otherwise, use the `option` macro:

```verse
MaybeAFloat := option{GetAFloatOrFail[]}

```

Use `set` to change what optional value a `var`s is referring to:

```verse
var C:?int = false
set C = option{2}                              # C now refers to a brand new option(int) with a 2 inside
```

 C? = 2                                         # succeeds
 C = option{2}                                  # succeeds

## `[]t`: Array

An immutable container that holds zero or more values of `t`, and lets you address these values with a `0`-based index:

```verse
A:[]int = array{}
B:[]int = array{1, 2, 3}

```

Accessing elements:

```verse
B[0] = 1                                       # succeeds; there's an element at index 0, and its value is 1
B[10]                                          # fails; there's no element at index 10

```

Concatenating arrays:

```verse
var C:[]int = B + array{4}                     # concatenate arrays using `+`
C = array{1, 2, 3, 4}                          # succeeds

set C += array{5}                              # shorthand for `set C = C + array{5}`

```

Getting the length of an array:

```verse
C.Length = 5                                   # succeeds

```

Although `array(t)` is immutable, we can still "mutate" arrays through `var`:

```verse
var D:[]int = array{1, 2, 3}                   # notice the `var`
set D[0] = 3                                   # succeeds; `D` has an element at index 0
D = array{3, 2, 3}                             # succeeds

```

This works because the `var` itself is mutable (whoa): it's a mutable reference to an immutable array. Thus when we write

```verse
set D[0] = 3

```

... the compiler treats it as if we wrote something like:

```verse
set D = array{3, D[1], D[2]}                   # element 0 has the "new" value we specified, the others stay the same

```

Now `D` refers to a _brand new immutable array_ whose elements are `3, 2, 3`. We can verify that the array is in fact brand new:

```verse
var A:[]int = array{1}                         # a single-element array
var B:[]int = A                                # A and B now refer to the same array
set A[0] = 2                                   # now A refers to a brand new array
A[0] = 2                                       # succeeds
B[0] = 1                                       # succeeds

```

## `[k]v`: Map

An immutable associative container (like a dictionary) that holds zero or more `(`_Key_`:k,` _Value_`:v)` pairs:

```verse
Empty := map{}

var Weights:[string]float = map{
    "ant" => 0.0001,
    "elephant" => 500.0,
    "galaxy" => 500000000000.0
}

```

A key's value can be looked up using `[]` in amortized _O(1)_ time:

```verse
0.00001 < Weights["ant"]                       # succeeds
Weights["car"]                                 # fails; no "car" key

```

To (re)associate a value with a key, use `set` through a `var`:

```verse
var Friendliness[string]int = map{"peach" => 1000}

set Friendliness["pelican"] = 17                            # add a new value
set Friendliness["peach"] += 2000                           # ... or change an existing one
set Friendliness["tomato"] += 1000                          # fails; no "tomato" key

```

The number of key/value pairs can be obtain by calling `Length`:

```verse
Friendliness.Length = 2                                     # succeeds

```

Any subclass of the `comparable` class may be used as a map's key type. This includes: `int`, `float`, `rational`, `logic`, `char`, `char8`, `char32`, and any combination of arrays, maps, tuples, or `struct`s consisting of other `comparable` values, but not instances of `class`es or `interface`s.

TODO: maybe worth mentioning what iterating over maps looks like? "since we're here"

## `tuple(t1, ...)`: Tuple

TODO: fill this in

# Any

Any is the supertype of all types, meaning whatever behavior is defined for it is also defined for all the any subtypes.
Verse has a special type, any, that is the supertype of all types (all other types are subtypes of any). Because of this, any supports very few operations, as all other types must be able to provide the same functionality that any provides.
For example, if any were to define a comparison operation (which it doesn't), then all other types would also have to define a comparison operation (which they don't).
There is very little that you can do with an any type. but it's good to be aware of this type as it may come up when writing code that produces an error.
But there are ways that you can use any:

```verse
Letters := enum:
    A
    B
    C
letter := class:
    Value : char
    Main(Arg : int) : void =
        X := if (Arg > 0)
            Letters.A
        else
            letter{Value := 'D'}
```

In the code example above, X is given the type any, as that is the lowest supertype of both Letters and letter.
More usefully, any can be used as the type for a parameter to a function that is ignored (but might be required as an argument for a method of an implemented interface).
For example:

```verse
FirstInt(X:int, :any) : int = X
```

The second argument to FirstInt is ignored, and can be of any type, so it is given the any type. FirstInt can be more generally written using parametric types. For example:

```verse
First(X:t, :any where t:type) : t = X
```
