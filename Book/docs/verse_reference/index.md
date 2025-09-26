# Imports the folder containing base_module and its submodule


persistence_example := class(creative_device):
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void =
        InitialSavedPlayerData:int = 0
        Players := GetPlayspace().GetPlayers()
        for (Player : Players):
            if:
                not MySavedPlayerData[Player]
                set MySavedPlayerData[Player] = InitialSavedPlayerData
```

Module-scoped variables using the player type as the key have the following limitations:

Accessing the player’s persistent data is only allowed when the player is in the current game.
If a player leaves a game or is not in the current session, you can no longer store or access their data in that game session. If the player returns or plays the same game again, then you can access and update their data.
The module-scoped weak_map cannot be completely read or written to, so it's not possible to read or override values for all players.
You cannot iterate through the values of a weak_map or see how many players have a record in the weak_map, because a weak_map has no length.
A single island can have up to two persistent variables, that is two weak_map variables with player as the key type.
At least one persistent variable's weak_map value must be a class if the limit for max persistent variables has been met.
A single weak_map record has a maximum data size of 128 kilobytes (KB) per player.
For more details on persistence in Verse, check out Using Persistable Data in Verse.

# coins if there is no check that the player has enough coins in advance.
set Coins -= CoinsPerQuiver
# Give arrows to the player.
set Arrows += ArrowsPerQuiver
# Count this as a purchase. We do not have a variable for this.
set TotalPurchases += 1
```

The operator /= is not supported for int, since the result of integer division is a rational type and therefore cannot be assigned to an int.

### Signed Integers

A signed integer is a value that can be positive, or negative, or zero. The operator - can be used to negate an integer if - appears before the integer, for example -3. You can also use the operator + before an integer to help align your code visually, but it won’t change the value of the integer.

```verse
# This is an alternate way to sell arrows to the player. It is
# functionally identical to the code in the Math section above.
set Coins += -CoinsPerQuiver
set Arrows += +ArrowsPerQuiver
set TotalPurchases += +1
```

### Comparison

You can use the failable operator = to test if two integers are equal, and the failable operator <> to test for inequality.
Since numbers are ordered, you can use the failable operator < to test if one integer is less than another integer, and the failable operator > to test if one integer is greater than another.
You can use the failable operator <= to test if one integer is less than or equal to another integer, and the failable operator >= to test if one integer is greater than or equal to another integer.

```verse
# Check that the player can afford this purchase.

if (Coins >= CoinsPerQuiver):
    # They can! Proceed with the purchase
    set Coins -= CoinsPerQuiver
    set Arrows += ArrowsPerQuiver
    set TotalPurchases += 1
```

### Standard Library2

The standard library provides functions to help with creating and using integers, and common math structures and functions. Refer to the Verse API Reference for more details on these functions.
Alternate Representations of Integers

You can also use the hexadecimal numeral system to represent integers, which is base-16, instead of the decimal numeral system which uses base-10. This means that hexadecimal values are represented with the digits 0-9 and the letters A-F. For example, 0x7F is the same as 127, and 0xFACE is the same as 64206.

### Implementation Details

In a future update, int will semantically represent an integer of any size, but currently, an int in Verse is implemented as a signed 64-bit integer. Until this update, an int must be in the range [-2^63, … , -1, 0, 1, … , 2^63 - 1], and integers, including the results of math operations that are outside this range for int sizes, will produce a runtime error called an integer overflow.
Although integers currently have these restrictions, Verse code with integers that you write today will semantically be the same as when integers can have arbitrary precision.

### Persistable Type2

Integer values are persistable, which means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.

var CurrentHealth : float = 75.0
# Reduce it to half
set CurrentHealth *= 0.5
# CurrentHealth is now 37.5.
```

### Signed Floating Point Numbers

A signed float is a value that can be positive, or negative, or zero. The operator - can be used to negate a float if - appears before the float, for example -3.2. You can also use the operator + before a float to help align your code visually, but it won’t change the value of the float. In the following code, a "life drain" attack heals the attacker for one eighth of the damage inflicted on the target.

```verse
# Set up the parameters that describe the situation
DamageInflicted : float = 10.0
LifeDrainMultiplier : float = 0.125
var CurrentAttackerHealth : float = 99.0
# Increase current health based on damage inflicted.

set CurrentAttackerHealth += DamageInflicted * HealingMultiplier
# CurrentAttackerHealth is now 100.25.
```

### Comparison3

You can use the failable operator = to test if two floats are equal, and the failable operator <> to test for inequality.
Since numbers are ordered, you can use the failable operator < to test if one float is less than another float, and the failable operator > to test if one float is greater than another float.
You can use the failable operator <= to test if one float is less than or equal to another float, and the failable operator >= to test if one float is greater than or equal to another float.
NaN is comparable like other floats, and NaN is larger than all other floats and equal to itself.

```
# Set up the parameters that describe the situation
DamageInflicted : float = 10.0
LifeDrainMultiplier : float = 0.125
var CurrentAttackerHealth : float = 99.0
MaxAttackerHealth : float = 100.0

# Increase current health based on damage inflicted
set CurrentAttackerHealth += DamageInflicted * HealingMultiplier
# Ensure that the attacker does not heal beyond their maximum health
```

### Standard Library3

The standard library provides functions to help with creating and using floats, and common math structures and functions. Refer to the Verse API Reference for more details on these functions.
Persistable Type Floating point values are persistable, which means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.

WinningPlayerName : string = "Player One"
# Build a message announcing the winner.
Announcement : string = "...And the winner is: " + WinningPlayerName + "!"
```

### String Interpolation

You can inject a value into a string if it has a valid ToString() function defined in the current scope.
For example, the following code results in the variable Announcement containing the string "...And the winner is: Player One!".

```verse
# The winning player's name:
WinningPlayerName : string = "Player One"
# Build a message announcing the winner.
Announcement : string = "...And the winner is: {WinningPlayerName}!"
```

### Comparison4

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

### Length

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

### Index

You can access the UTF-8 code unit at a specific index of the string. The first UTF-8 code unit in a string has an index of 0, and each subsequent code unit index increases in number.
For example, "cat"[0] is "c" and "cat"[1] is "a".

### Index

0
1
2
Character
"c"
"a"
"t"
Code Unit
"{0o63}"
"{0o61}"
"{0o74}"

In cases where a string has characters that are represented by more than one code unit, there will be an index for each code unit. For example, "á" is represented by two UTF-8 code units "{0oC3}{0oA1}", so "cát"[1] is "{0oC3}" and "cát"[2] is "{0oA1}".

### Index

0
1
2
3
Character
"c"
"á"
"t"
Code Unit
"{0o63}"
"{0oC3}"
"{0oA1}"
"{0o74}"

The last index in a string is one less than the length of the string. For example, "cat".Length is 3 and the index for "t" in "cat" is 2.

### Standard Library4

The standard library provides functions to help with creating and using strings. Refer to the Verse API Reference for more details on these functions.
Alternate Representations of Characters

Some characters have alternate representations when they’re used in a string. For example, "{}" can be used for string interpolation or for the code points of characters, but they can also be used as the brace characters {} themselves in text.
To be able to use an alternate representation of a character in a string, you must add the escape character "\" before the character in the string. For example, "\{\}" is rendered as {} in text, and "\n" starts a new line in text.

### Implementation Details2

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

### Persistable Type3

String, char, and char32 values are all persistable, which means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.

DistanceScaling : float = Max(1.0, Pow(PlayerDistance, 2.0))
# The farther the explosion is, the less damage the player takes
var ExplosionDamage : float = BaseDamage / DistanceScaling
# Reduce the damage by armor
set ExplosionDamage -= Armor
# Avoid negative damage values so that explosions can't heal very high armor players.
set ExplosionDamage = Max(0.0, ExplosionDamage)
```

Using grouping, you could rewrite the example above as:

```verse
BaseDamage : float = 100
Armor : float = 15
DistanceScaling : float = Max(1.0, Pow(PlayerDistance, 2.0))
ExplosionDamage : float = Max(0.0, (BaseDamage / DistanceScaling) - Armor)
```

Grouping expressions can also improve the readability of your code.

ModuleA<public> := module:
    ModuleB<public> := module:
        # Internal to ModuleB.
        class_b1 := class{}
        # Allows access from anywhere inside ModuleA.
        class_b2<scoped{ModuleA}> := class {}
```

Class Specifiers

Class specifiers define certain characteristics of classes or their members, such as whether you can create a subclass of a class.

### Specifier Description Example

abstract

When a class or a class method has the abstract specifier, you cannot create an instance of the class. Abstract classes are intended to be used as a superclass with partial implementation or as a common interface. This is useful when it doesn't make sense to have instances of a superclass but you don't want to duplicate properties and behaviors across similar classes.

```verse
pet := class<abstract>():
    Speak() : void
cat := class(pet):
    Speak() : void = {}
castable
```

Indicates that this type is dynamically castable. The <castable> specifier has a backward compatibility restriction on its use. Once a class or interface is published, the <castable> attribute can be neither added nor removed. Doing so can introduce unsafe casting behaviors, so this is disallowed.

The castable_subtype type functions very similarly to subtype, but requires that any types used with it are also marked <castable>. This increases code safety in places where dynamic casts are used.

```verse
my_base := class {}
my_castable_type := class<castable>(my_base) {}
my_child_type := class(my_castable_type) {}
MySubtypeFunction(t:castable_subtype(my_base)):void=
    return
Main()<decides>:void =
```

concrete

When a class has the concrete specifier, you can construct an instance of the class with an empty archetype, which means that every field of the class must have a default value. Every subclass of a concrete class is implicitly concrete. A concrete class can only inherit directly from an abstract class if both classes are defined in the same module.

```verse
cat := class<concrete>():
     # field must be initialized because the class is concrete
    Name : string = "Cat"
```

unique

A unique class in Verse is assigned a unique identity for each instance. This means that, even if two instances of the same unique class have the same field values, they are not equal since they are distinct instances. This allows instances of unique classes to be compared for equality by comparing their identities. Classes without the unique specifier don't have any such identity, and so can only be compared for equality based on the values of their fields. This means that unique classes can be compared with the = and <> operators, and are subtypes of the comparable type.

```verse
unique_class := class<unique>:
    Field : int
Main()<decides> : void =
    X := unique_class{Field := 1}
    X = X # X is equal to itself
    Y := unique_class{Field := 1}
    X <> Y # X and Y are unique and therefore not equal
```

final

You can only use the final specifier on classes and members of classes, with the following restrictions:
When a class has the final specifier, you cannot create a subclass of the class.
When a field has the final specifier, you cannot override the field in a subclass.
When a method has the final specifier, you cannot override the method in a subclass.

```verse
cat := class<final>():
```

final_super

The final_super specifier is only applicable to class definitions and requires that the class definition derives from a parent class or interface. This specifier imposes a future compatibility constraint that the given class will always derive from its parent directly; for this and all future published versions of this class definition.
This is necessary in Scene Graph for immediate subtypes of component to limit the number of instances to exactly zero or one per Scene Graph entity. This limit extends to subtypes of those types as well.

```verse
component := class {}
my_final_class := class<final_super>(component) {}
# Not allowed since my_final_class has the final_super specifier.
my_subclass_type := class(my_final_class) {}
```

### Persistence Specifier

When a custom type, such as a class, has the <persistable> specifier, it means that you can use it in your module-scoped weak_map variables and have its values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.
You can use the persistable specifier with the following types. Follow the links for more details.

class

enum
struct

Open and Closed Specifiers

Currently usable only with enums. The <open> and <closed> specifiers determine how you can change the definition of the enum once your island is published.
You can use the open and closed specifiers with the following types. Follow the links for more details.

enum

Specifier Description Example

Open

A specifier that currently applies to enums only.
You can add or reorder enum values in an open enum, or change it to a <closed> enum.
Open enums are best used when you expect the number of values in your enum may increase in the future. For example, an enum of weapon types.

Enums are <closed> by default so you must explicitly define the enum as an open enum with the <open> specifier

```verse
my_enum := enum<open>{Value1, Value2, Value3}

```

Closed

A specifier that currently applies to enums only.

Enums are closed by default.

Closed enums are best used for cases where your values are expected to stay the same, like days of the week.

```verse
# Enums are <closed> by default so the specifier is not required.
my_enum := enum{Value1, Value2, Value3}
## You can also explicitly define the enum as closed by adding the <closed> specifier
my_enum := enum<closed>{Value1, Value2, Value3}
```

Implementation Specifiers

It's not possible to use implementation specifiers when writing code, but you will see them when looking at the UEFN APIs.

Specifier Description Example

native

Indicates that the definition details of the element are implemented in C++. Verse definitions with the native specifier auto-generate C++ definitions. A Verse developer can then fill out its implementation. You can see this specifier used on:
class
interface
enum
method
data

```verse
GetCreativeObjectsWithTag<native><public>(Tags:tag)<transacts>:[]creative_object_interface
native_callable
```

Indicates that an instance method is both native (implemented in C++) and may be called by other C++ code. You can see this specifier used on an instance method. This specifier doesn’t propagate to subclasses and so you don’t need to add it to a definition when overriding a method that has this specifier

```verse
creative_device<native><public> := class<concrete>:
    OnBegin<public>()<suspends>:void = external {}
    OnEnd<native_callable><public>():void = external {}
```

Attributes

Attributes in Verse describe behavior that is used outside of the Verse language (unlike specifiers, which describe Verse semantics). Attributes can be added on the line of code before definitions.

Attribute syntax uses the at sign (@) followed by the keyword.

Attribute Description Example

editable

Indicates this field is an exposed property that can be changed directly from UEFN so you don't need to modify the Verse code to change its value. For more details, see Customize Device Properties.
Verse

```verse
@editable
Platform : color_changing_tiles_device = color_changing_tiles_device{}
```

editable_text_box

An editable string that displays as a text box in the editor. Editable text boxes currently do not support
tooltips or categories.  For more details, see Customize Device Properties.

```verse
# An editable string that displays as a text box in the editor.
# Editable text boxes currently do not support tooltips or categories.
@editable_text_box:
    # Whether this text can span multiple lines.
    MultiLine := true
    # The maximum amount of characters this text block can display.
    MaxLength := 32
MessageBox:string = "This is a short message!"
```

editable_slider

An editable slider that uses the float type. You can drag the slider in the editor to increase or decrease the value. For more details, see Customize Device Properties.

```verse
# An editable slider that uses the float type. You can drag the slider in the editor to increase
# or decrease the value.
@editable_slider(float):
    # The categories this editable belongs to.
    Categories := array{FloatsCategory}
    # The tool tip for this editable.
    ToolTip := SliderTip
    # The minimum value of each component. You cannot set an editable value for this number lower
```

editable_number

An editable number with minimum and maximum.  For more details, see Customize Device Properties.

```verse
# An editable number with minimum and maximum
@editable_number(int):
    # The tool tip for this editable.
    ToolTip := EditableIntTip
    # The category this editable belongs to.
    Categories := array{IntsCategory}
    # The minimum value of each component. You cannot set an editable value for this number lower
```

editable_vector_slider

An editable vector slider. You can drag to change the values of each of the vector components.  For more details, see Customize Device Properties.

```verse
# An editable vector slider. You can drag to change the values of each of the vector components.
@editable_vector_slider(float):
    # The tool tip for this editable.
    ToolTip := VectorSliderTip
    # The categories this editable belongs to.
    Categories := array{FloatsCategory}
    # Shows the option to preserve the ratio between vector values in the editor.
    ShowPreserveRatio := true
```

editable_vector_number

An editable vector number, which can be a vector2, vector2i, or vector3.  For more details, see Customize Device Properties.

```verse
# An editable vector number, which can be a vector2, vector2i, or vector3.
@editable_vector_number(float):
    # The categories this editable belongs to.
    Categories := array{FloatsCategory}
    # The tool tip for this editable.
    ToolTip := VectorFloatTip
    # Shows the option to preserve the ratio between vector values in the editor.
```

editable_container

An editable container of values. Currently, this only supports arrays.  For more details, see Customize Device
Properties.

```Verse
#An editable container of values. Currently, this only supports arrays.
@editable_container:
    # The category this editable belongs to.
    Categories := array{IntsCategory}
    # The tool tip for this editable.
    ToolTip := IntArrayTip
    # Whether dragging elements to reorder this container is allowed.
```

if (PlayerFallHeight > 3.0):
    DealDamage()
# Reset the player’s fall height
ZeroPlayerFallHeight()
```

In this example, if PlayerFallHeight is greater than three meters, then the condition succeeds and DealDamage() is executed before the player’s fall height is reset. Otherwise, the condition fails so the player doesn’t take any damage but the player’s fall height is reset.

if

The player fall height example would use the following syntax:

```Verse
expression0
if (test-arg-block):
    expression1
expression2
```

After executing expression0, the Verse program enters the if-block. If the test-arg-block succeeds, then the Verse program executes expression1, which can be one expression or a block of expressions. Otherwise, if the test-arg-block fails, the Verse program skips expression1 and only executes expression2.
Diagram of how if expressions work with the example of fall height
Flow diagram for if-block logic.

if ... else

You can also specify an expression to execute when the if expression fails.
For example, the player should gain a double-jump ability if they fall less than three meters and if their jump meter is at 100 percent. But if they fall more than three meters or their jump meter isn’t at 100 percent, then the character’s arms will flap to let the player know they cannot double jump.

```verse
var PlayerFallHeight : float = CalculatePlayerFallHeight()
    if (PlayerFallHeight < 3.0 and JumpMeter = 100):
    # Perform a double jump.
        ActivateDoubleJump()
    # Reset the player’s fall height.
        ZeroPlayerFallHeight()
    else:
    # Flap the character’s arms to tell the player they
    # cannot double jump right now!
```

In this example, the condition of if evaluates whether PlayerFallHeight is less than three meters and if JumpMeter is equal to 100 percent. If the condition succeeds, ActivateDoubleJump() and ZeroPlayerFallHeight() are executed before SetDoubleJumpCooldown().

If the if condition fails, then the expression ActivateFlapArmsAnimation() following else is executed before SetDoubleJumpCooldown().
Syntactically, the if-else example looks like this:

```Verse
expression0
if (test-arg-block):
    expression1
else:
    expression2
expression3
```

Example of how if/else expressions work using logic for jumping and fall heights

if ... else if ... else

If a player has 100 percent shields when they fall more than three meters, they should take maximal damage but still survive. And let’s modify the rule that gives players a double-jump ability, such that players will only gain double-jump if they fall less than three meters and if their jump meter is greater than 75 percent.

```verse
var PlayerFallHeight : float = CalculatePlayerFallHeight()
    if (PlayerFallHeight > 3.0 and shields = 100):
        DealMaximalDamage()
        return false
    else if (PlayerFallHeight < 3.0 and JumpMeter > 75):
        ActivateDoubleJump()
        return false
    else:
        return true
    # Reset the player’s fall height
    ZeroPlayerFallHeight()
```

Syntactically, the if-else if-else example looks like this:

```Verse
expression0
    if (test-arg-block0):
        expression1
    else if (test-arg-block1):
        expression2
    else:
        expression3
    expression4
```

An example of if/else if/else expression in Verse using shield and jumping variables

if ... then

You can write any of the if conditions in the previous examples on multiple lines without changing how they work:

```Verse
expression0
if:
 test-arg-block
then:
    expression1
expression2
```

The code block test-arg-block can contain one or more lines of conditions but they must all succeed to execute expression1 before expression2, otherwise only expression2 will be executed.
The example from the if ... else section rewritten in this format looks like:

```verse
var PlayerFallHeight : float = CalculatePlayerFallHeight()
if:
    PlayerFallHeight < 3.0
    JumpMeter = 100
then:
    # Perform a double jump.
    ActivateDoubleJump()
    # Reset the player’s fall height.
    ZeroPlayerFallHeight()
else:
    # Flap the character’s arms to tell the player they
    # cannot double jump right now!
    ActivateFlapArmsAnimation()

# Set the double-jump cooldown so rapidly pressing Jump does

# not cause the "flap arms" animation to play inappropriately.

SetDoubleJumpCooldown()
```

Single-Line Expression

You can write an if else as a single-line expression, similar to ternary operators in other programming languages. For example, if you want to assign a maximum or minimum Recharge value based on a player’s ShieldLevel, you can write the following Verse code:

```verse
Recharge : int = if(ShieldLevel < 50) then GetMaxRecharge() else GetMinRecharge()
```

Predicate Requirements

The predicate of the if, which is the expression between the parentheses (), is unlike other programming languages, in that it is not expected to return a Boolean (called logic in Verse). Instead, the predicate is expected to have the decides effect (note that though subtyping normally allows for a subset of effects in places allowing a set of effects, if requires the overall effect of the predicate to include decides). The effect is removed from the surrounding scope. That is to say, the decides effect from all operations in the if predicate is consumed by the if construct. For example, in the code below, Main does not have the decides effect, though it invokes Foo, which does.

```verse
Foo()<transacts><decides> : void = {}
Bar() : void = {}
Main() : void =
    if (Foo[]):
        Bar()
```

This is because, rather than using a logic input to if to choose which branch is taken, the success of the operations contained in the predicate of the if is used to decide the appropriate branch - the then branch if all operations succeed, the else branch (if present) if any operations fail. Note that this means arbitrary operations can be used in the if predicate, including introducing constants. For example:

```Verse
Main(X : int) : void =
    Y = array{1, 2, 3}
    if:
        Z0 := Y[X]
        Z1 := Y[X + 1]
    then:
        Use(Z0)
        Use(Z1)
```

Put another way, the scope of the then branch includes any names introduced in the if predicate.

Transactional Behavior

Another deviation of if with respect to other programming languages is the transactional behavior of the predicate to if. The predicate to if must not have the no_rollback effect (implicitly used by all functions that do not explicitly specify transacts, varies, or computes). This is because in the event the predicate fails, all operations taken during the execution of the predicate (short of any operation impacting resources outside of the runtime, such as file I/O, or writing to console) are undone before execution of the else branch. For example:

```verse
int_ref := class:
    var Contents : int
    Incr(X : int_ref)<transacts> : void =
        set X.Contents += 1
Foo(X : int) : int =
    Y := int_ref{Contents := 0}
    if:
        Incr(Y)
        X > 0
    then:
        Y.Contents
    else:
        Y.Contents

```

The function Foo(-1) will return 0, while Foo(1) will return 1. This is because, though the call to Incr occurs before the test of X > 0, the mutation of Y it causes is undone before execution of the else branch. Note that Incr had to manually specify the transacts effect. By default, transactional behavior is not provided, indicated by the implicit no_rollback effect, but it can be added by specifying the transacts effect manually (overriding the implicit no_rollback effect).

### Case

The case expression is how you make a decision, from a list of choices, about what expressions should be executed next.

With case expressions, you can control the flow of a program from a list of choices. The case statement in Verse is a way to test one value against multiple possible values (as though you were using =), and running code based on which one matches.

The use of case expressions can be found in all kinds of applications, like in games where there is a non-playable character (NPC).

For example, let's say you use the Guard Spawner device to spawn a guard with its patrol option enabled. After the guard spawns into the game, it has a few possible active states, including Idle, Patrol, Alert, Attack, and Harvest. A high-level state-transition diagram for this could look like:

In-game state transition in Verse

You can observe these state transitions in-game.

In this video, the guard has its patrol option enabled as the default behavior.

In the video, the guard transitions from patrolling the science base to harvesting some resources. Then the guard spots the player, which sends the guard into an alert state (indicated by the hovering question mark) before entering its attack state (indicated by the hovering exclamation mark).

Depending on the state the guard is in, it will exhibit certain behaviors, and these behaviors are typically coded as functions that are called when the program chooses to enter a specific state.
As code, this high-level guard-state transition could look like this:

```Verse
case(GuardStateVariable):
        idle_state =>
            RunIdleAnimation()
            SearchPlayerCharacter()
        harvest_state =>
            GatherResources()
        alert_state=>
            RunAlertAnimation()
            PlayAlertSound()
            DisplayAlertUIElement()
```

This case expression passes a label that tells the program which functions to run if the guard enters a specific state.

In this expression, the guard's patrol_state is the default case because a guard with patrol enabled should run its default patrol behavior.

Syntactically, this is the same as:

```Verse
expression0
    case (test-arg-block):
    label1 =>
        expression1
    label2 =>
        expression2
    _ =>
        expression3 for the default case
    expression4
```

Each pattern in the case block, such as label1 and label2, must use the form constant => block, where the constant can be an integer, logic, string, char, or enum constant. So case statements only work with int, logic, string, char, and enums.

Structure

Structurally, the Verse case expression runs code based on input of the GuardStateVariable test argument block, and it functionally works the same as a series of if expressions.
Example of running expression3, the alert_state

In this example, the Verse program runs expression3 if GuardStateVariable resolves to alert_state. If the program passes in patrol_state, Verse structurally jumps to the default case, and runs expression5.
Example of running the default state, expression5

Using Case with Other Control Flow

The blocks in a case statement are allowed to break and continue if the case statement is inside of a loop. Blocks of case statements are also allowed to return from the function they are in.
For example:

```verse
loop:
        case (x):
            42 => break
            _ => {}
```

This absurd loop will either complete immediately if x = 42 or loop forever.

Another example:

```Verse
Foo(x : int) : int =
        case (x):
            100 => return 200
            _ => return 100
```

This example is equivalent to:

```Verse
Foo(x : int) : int =
        case (x):
            100 => 200
            _ => 100
```

This is because the case statement is the last expression of the function.

Default Case

Case statements that do not have a *=> case (a default case) will fail if none of the cases match. It's fine to use such case statements in failure contexts (such as functions with the decides effect).
Case statements that match all of the cases of an enumeration will be non-failing even if they do not have a*=> case.

### Loop and Break

The loop expression repeats the expressions in its code block. End the loop with either a break or return.
With the loop expression, the expressions in the loop block are repeated for every iteration of the loop.
The GIF below of the Fortnite Emote Clean Sweep is an example of how a loop works. The GIF plays to the end, then repeats from the beginning, and the player emoting is like the expressions in a loop block.
GIF of a character sweeping

```verse
## GIF
    loop:
        DoCleanSweepEmote()
```

Like a GIF, a loop block will repeat forever unless instructed to do otherwise. This is called an infinite loop.
Infinite loops are not very useful in most cases since they will block progress for the program, so Verse provides a way to end and / or suspend.

End: You can end a loop by exiting with either break or return.

Suspend: You can suspend a loop if it's used in an async expression. See Concurrency Overview for more details.
It's also possible to do both in the same loop. In this example, the loop block repeats until the random number that's generated is less than twenty.

```verse
loop:
        # generate random number
        RandomNumber : int = GetRandomInt(0, 100)
        # check if random number is less than twenty
        if (RandomNumber < 20):
            # exit loop
            break
```

Syntactically, this is the same as:

```Verse
expression0
    loop:
        expression-block
        if (test-arg-block):
            break
        expression-block
    expression2
```

Loop flow diagram

Unlike some of the other control flow expressions, the loop expression returns void, so it may not be useful in cases where you want an expression to return a result. If the loop is inside a function, then it's possible to return a value with return, but this will exit not only out of the loop but also out of the function.

Nested Loop Expressions

You can nest one loop inside another loop. The first loop is sometimes called the outer loop, and the second loop is called the inner loop. When the break expression is executed in an inner loop, it only breaks out of the inner loop.

In the example below, the outer loop continues to expression3, then the if expression after the inner loop exits and can execute expression1 and the inner loop again.

```Verse
expression0
    # outer loop
    loop:
        expression1
        # inner loop
        loop:
            expression2
            if (test-arg-block0):
                # exit inner loop
                break
        expression3
        if (test-arg-block1):
            # exit outer loop
            break
    expression4
Nest loop block diagram
loop and break
loop block
```

### For

The for expression iterates over a bounded number of items and repeats the expressions in its code block the same number of times.

The for expressions, sometimes called for loops, are the same as loop expressions, except that for expressions iterate over a bounded number of items. This means the number of iterations is known before the for loop is executed, and decisions on when to exit the loop are automated for you.

The Pulse Trigger device is an example of a for loop with bounded iterations when you set the Pulse Trigger device Looping setting to a number. The Pulse Trigger's pulse repeats as many times as specified by the device
Looping setting.

Using Verse to program the Pulse Trigger Device in UEFN

In this example, two Trigger devices are in the Pulse Trigger's path. When the Pulse Trigger's pulse reaches a Trigger device, the device sends a signal to display text on one of the Billboard devices and repeats three times.

As code, this example could look like:

```verse
for (X := 0..2):
    TriggerDevice1.Transmit()
    TriggerDevice2.Transmit()
```

The for expression contains two parts:

Iteration specification: The expressions within the parentheses and the first expression must be a generator. In this example, it is (X := 0..2).

Body: The expressions after the parentheses. In this example, that is the two lines with Transmit().
For flow diagram in Verse

Generator

A generator produces a sequence of values, one at a time, and gives the value a name. In this example, the generator is X := 0..2, so each iteration of the loop, the generator produces the next value and gives the value the name X. When the generator reaches the end of the sequence, the for loop ends. This decision flow of checking if the loop variable has a valid value is built into the for expression. Generators only support ranges, arrays, and maps.

Iterating over a Range

The range type represents a series of integers; for example, 0..3, and Min..Max.

The start of the range is the first value in the expression — for example 0 — and the end of the range is the value following .. in the expression — for example, 3. The range contains all the integers between, and including, the start and end values. For example, the range expression 0..3 contains the numbers 0, 1, 2, and 3. Range expressions only support int values, and can only be used in for, sync, race, and rush expressions.

```verse
for (Number := 0 .. 3):
    Log("{Number}")
```

The result will add four lines to the log containing the numbers 0, 1, 2, and 3.

A for expression can return the results from each iteration in an array. In the following example, Numbers is an immutable array with the int values -1 to -10.

```verse
Numbers := for (Number := 1..10):
    -Number
```

Iterating over an Array or a Map

Iterations over arrays and maps can be just the values, or the key-value pair for maps and the index-value pair for arrays.

In this case, only the values of the array are used, and Values is an immutable array with the int values 2, 3, and 5.

```verse
Values := for (X : array{1, 2, 4}):
    X+1
```

The same can be done with a map, and Values is, in this case, an immutable array with the int values 3,7.

```verse
Values :=  for  (X := map{ 1=>3,  0=>7 }):
    X
```

The X->Y pattern can be used to deconstruct an index-value or key-value pair. The index (or key) is bound to the left part (X) and the value is bound to to the right part (Y). An example of Index-value pairs from an array, Values is an immutable array with the int values 1, 3, and 6.

```verse
Values := for ( X -> Y : array{1, 2, 4}) :
    X + Y
```

An example of Index-value pairs from a map, Values is an immutable array with the int values 4, and 7.

```verse
Values  :=  for ( X->Y := map{ 1=>3,  0=>7 }):
    X + Y
```

Filter

You can add failable expressions to the for expression to filter out values from the generator. If the filter fails, then there's no result for that iteration, and for skips to the next value produced by the generator.
For example, the filter Num <> 0 is added to the for expression to exclude 0 from the returned results.

```verse
NoZero := for (Number := -5..5, Number <> 0):
    Number
```

Syntactically, this is the same as:

```Verse
expression0
for (Item : Collection, test-arg-block):
    expression1
expression2
```

For with Condition diagram in Verse

Definition

You can also add named expressions to the iteration specification, and the name can be used in both the iteration specification and the body.

```verse
Values := for ( X := 1..5; Y:=SomeFunction(X); Y < 10):
    Y
```

Result: an array with at most 5 items where all values are less than 10.

Nested For

You can nest a for loop inside another for loop. There are two ways to do this:
Single For Expression: Specified by multiple generators. The result is a one-dimensional array.

Multiple For Expressions: Separate for blocks. The result is a multidimensional array.

The sections below describe these further.

Single For Expression

You can have multiple loops in a single for expression by adding more generators. The result of a single for expression with multiple generators is a one-dimensional array.

In this example, Values is an immutable array with the int values 13, 14, 23 and 24.

```verse
Values := for(X:=1..2, Y:=3..4):
        X * 10 + Y
```

Semantically, this is the same as:

```Verse
expression0
for (Item : Collection, Item2 : Collection2):
    expression1
expression2
```

Multiple For Expressions

You can also nest a for expression in another for-loop body. Since one for expression returns a one-dimensional array, nesting a for expression returns a two-dimensional array.

In this case, Values is an immutable array with two immutable int arrays. The first array contains the values 13, and 14, and the second array contains 23 and 24. This can be written as array{ array{13, 14}, array{23, 24} }.

```verse
Values := for ( X := 1..2 ):
    for (Y := 3..4):
        X * 10 + Y
```

Failure

If anything fails inside the iteration specification, then any changes due to that iteration will be rolled back.

```verse
for(X := 1..5; S := IncrementSomeVariable(); X < 3):
    X
```

The result of this for expression is array{1,2}, with only two calls to IncrementSomeVariable after the evaluation of the for loop because the other calls were rolled back when the filter X < 3 failed.

### Defer

Use the defer expression to execute code just before exiting the current scope.
The defer expression delays the execution of code until the current scope exits. You can use the defer expression to handle cleanup tasks like resetting variables. Even when there is an early exit (such as return or break) from the current scope, the expressions in a defer block will run as long as defer is encountered before the exit.

The following code shows how to use defer to reset a variable to zero while still using that same variable as a return value. In this function, RoundScore is returned and the expressions in the defer block run immediately after.

This means you do not need to create a temporary variable to save the value of RoundScore before it gets reset to zero.

```verse
OnRoundEnd<public>() : void =
var ScoreThisRound : int = AddRoundScoreToTotalScore()
Print("Points scored this round: {ScoreThisRound}")
<# Adds RoundScore to TotalScore and resets RoundScore to 0.
Returns the RoundScore added. #>
AddRoundScoreToTotalScore<public>() : int =
        defer:
                set RoundScore = 0
                UpdateUI()
```

Defer Expression Use

You can use a defer expression within any sequential code block such as a block, loop, for, if, branch, or even another defer.

Expressions within a defer block must be immediate (and not async) — with one exception. Async expressions can still be used within a defer if they are made immediate by using:
spawn

branch (if the defer is within an async block such as in a coroutine)

A defer has no result, and cannot be used as an argument or an assignment value.

defer defer before an exit

```Verse
expression0
defer:
    expression1
    expression2
expression3
```

```Verse
name() : type =
    expression0
    defer:
        expression1
        expression2
    return expression3
```

A defer expression will only execute if it is encountered before an early exit occurs.
defer with early return defer with a canceled async expression

```Verse
expression0
if (conditions):
    return
defer:
    expression1
expression2
```

```Verse
expression0
race:
    block: # canceled during slow-async-expression
        slow-async-expression
        defer:
            expression1
        expression2
     block: # finishes first
         fast-async-expression
         defer:
             expression3
         expression4
expression5
```

Multiple defer expressions appearing in the same scope accumulate. The order they are executed is the reverse order they are encountered — first-in-last-out (FILO) order. Since the last encountered defer in a given scope is executed first, expressions inside that last encountered defer can refer to context (such as variables) that will be cleaned up by other defer expressions that were encountered earlier and executed later.

Verse does not have deterministic destruction, but defer allows behavior similar to RAII to ensure cleanup.
Multiple defer expressions in a code block Multiple defer expressions in different code blocks

```Verse
expression0
defer:
    expression1
expression2
defer:
    expression3
expression4
```

```Verse
expression0
if (conditions):
    expression1
    defer:
        expression2
    expression3
expression4
defer:
    expression5
expression6
```

Exiting early is allowed within a defer block as long as the exit does not transfer control outside the scope of the defer. For example, using a loop with break is allowed within a defer, but that break must keep the code execution within the defer block. It cannot refer to a loop outside of the defer block.
Any variables that have been encountered in the outer nesting scope of a defer can be used within that defer expression.

Remember that defer runs last at the time of scope exit. This means that it uses whatever the state of the program is (including variable values) at that time, not at the time when the defer is encountered. The code below will print 10 because defer runs immediately after MyScore is set to 10.

```verse
var MyScore = 5
defer:
 Print(MyScore)
set MyScore = 10
```

Using a defer expression as the last expression within a scope is the same as not using it at all. For example, these two sets of expressions will run in exactly the same order, so defer is not needed:
Without defer With defer

```Verse
expression0
expression1
expression2
```

```Verse
expression0
expression1
defer:
    expression2
expressions
defer
```

Npc := Player.MoveToNearestNPC()
#Only called after MoveToNearestNPC() completes
Print("Moved to {Npc}")
```

Any code block that is within an async context (inside the body of an async function) may have any mix of immediate and async expressions.

If any expressions in a code block are async, then the whole code block is considered to be async.
If all expressions in a code block are immediate, then the whole code block is considered to be immediate.
All the expressions in the example below are async expressions, so the overall code block is async:

```verse
Sleep(2.0)  # waits 2 seconds
Boss.TauntEmote() # waits until TauntEmote() completes
Player.MoveToNearestNPC() # waits until MoveToNearestNPC() completes
```

All the expressions in the example below are immediate expressions, so the overall code block is immediate:

```verse
Print("Reset after explosion")
Platform.Show()
set SecondsUntilExplosion = 12.0
```

The expressions in the example below are a mix of async and immediate expressions, so the overall code block is async:

```Verse
Print("Started")
var Seconds := 1.0
Sleep(Seconds)
Print("Waited {Second} seconds")
set Second += 1.0
Sleep(Seconds)
Print("Waited {Second} seconds")
set Second += 1.0
Sleep(Seconds)
Print("Waited {Second} seconds")
```

Immediate expressions stick together on their own. All adjacent immediate (non-async) expressions are considered to be atomic — their code is guaranteed to run without interruption within the same update, and without preemption or context switching. It is as though such code had an automatic mutual-exclusion primitive wrapped around them.
So from the code example above, these immediate expressions are treated atomically:

```verse
# These two expressions are always kept together
Print("Started")
var Seconds := 1.0
Sleep(Seconds)
# These two expressions are always kept together
Print("Waited {Second} seconds")
set Second += 1.0
```

Like any other code block, the last expression in an async code block is used as a result.

Concurrency Expressions

Verse uses concurrency expressions to determine whether expressions execute concurrently (at the same time), or in sequence, one after another. An async expression is executed or invoked over time, so these concurrency expressions can be especially useful when you’re using async expressions.

Structured Concurrency

An async expression will block other expressions from executing if it takes a long time to execute. For example, using Sleep(90.0) will cause the program to wait 90 seconds, blocking the next expression until Sleep(90.0) is fully executed.

Structured concurrency expressions are used to specify async logical time flow, and to modify the blocking nature of async expressions with a lifespan that is logically constrained to a specific async context scope (such as an async function body).
This is similar to structured flow control such as block, if, for, and loop that constrain to their associated scope.

Verse async expressions do not use the yield and await primitives used by async implementations in other languages. The same mechanisms are accomplished by using Verse concurrency expressions and internal mechanisms.
For more on structured concurrency, see Sync, Race, Rush, and Branch.

Unstructured Concurrency

There is only one unstructured concurrency expression — spawn. This expression has a lifespan that is not logically constrained to a specific async context scope, but that potentially can extend beyond the scope where it was executed.

Unstructured concurrency is like an emergency escape hatch — you shouldn't use it on a regular basis although sometimes it is your best and only option.
Structured concurrency expressions (sync, race, rush and branch) should be used before unstructured concurrency (spawn) expressions whenever possible.

For more on unstructured concurrency, see Spawn.

Tasks for Tracking Currently Executing Async Expressions

An async expression has a task associated with it.
A task is an object that represents an async function that has started to execute, but has suspended to allow another task to complete.

The task can be used to check the status of an async expression and to cancel the async expression, if desired.
For more on tasks, see Task.

### Sync

Run two or more async expressions concurrently using a sync expression.
sync

 In a sync expression, all expressions in the sync block run concurrently and must complete before the sync expression yields control back. sync is a structured concurrency expression.
You can use the sync expression to run two or more async expressions at the same time. For example:

```verse
# All three async functions effectively start at the same time
Results = sync:
    AsyncFunction1()  # task 1
    AsyncFunction2()  # task 2
    AsyncFunction3()  # task 3
# Called after all three tasks complete (regardless of order)
MyLog.Print("Done with list of results: {Results}")
```

The following code shows the syntax for the sync expression with an accompanying diagram that shows the execution flow of the expressions.

```verse
Verse
expression0
sync:
    slow-expression
    mid-expression
    fast-expression
expression1
```

Sync Expression Use

Where you can use a sync expression

Async contexts

Invocation time of the sync expression

Async

Requirements for sync code block

The body of the sync expression must have at least two expressions that are async; otherwise, you have no need
to run the expressions simultaneously.

What the sync expression does

Executes all expressions in its code block concurrently and waits for them all to finish before executing the
next expression after the sync.
When the sync expression completes

When all the expressions in the sync code block have completed.

When the next expression after sync starts

Result of the sync expression

Its result is a tuple of results from each expression in the order that the top-level expressions were
specified. The result types of the expressions can be of any type, and each tuple element will have the type of
its corresponding expression.
At least two top-level expressions must be async.

```Verse
sync:
    AsyncFunction1()
    MyLog.Print("Second top level expression")
    AsyncFunction2()
    MyLog.Print("Third top level expression")
sync:
    AsyncFunction1()
    # Error: expected at least two top-level expressions
```

Top-level expressions can be compound expressions, such as nested code blocks:

```verse
# sync may also have compound expressions
# with each top-level expression its own task
sync:
    block: # task 1
        # Evaluated in serial order
        AsyncFunction1a()
        AsyncFunction1b()
    block: # task 2
        AsyncFunction2a()
        AsyncFunction2b()
        AsyncFunction2c()
    AsyncFunction3() # task 3

# AsyncFunction1a(), AsyncFunction2a() and AsyncFunction3() all start essentially at the same time
```

Since tuples can be used as self-splatting arguments, sync expressions can be used directly as arguments since they have a tuple result. This allows async arguments to evaluate simultaneously, and the function they are being passed to is called when all the expressions in the sync code block are completed.

```verse
# All three coroutine arguments start their evaluation at the same time
DoStuff(sync{AsyncFunctionArg1(); AsyncFunctionArg2(); AsyncFunctionArg3()})
# Not every argument needs to be async - a minimum of two justifies the use of sync
DoOtherStuff(sync{AsyncFunctionArg1(); 42; AsyncFunctionArg2(); AsyncFunctionArg3()})
```

### Race

Use a race expression to run two or more async expressions concurrently and cancel whichever expressions don't finish first.

The race expression is used to run a block of two or more async expressions concurrently (simultaneously). When the fastest expression completes, it “wins the race”. Any remaining “losing” expressions are canceled, then any expression that follows the race is evaluated.

```verse
set WinnerResult = race:
    # All three async functions start at the same time
    AsyncFunctionLongTime()
    AsyncFunctionShortTime()  # This will win and its result is used
    AsyncFunctionMediumTime()

# Next expression is called after the fastest async function completes
# / when the fastest/shortest async function task (AsyncFunctionShortTime()) completes
# and all other async function tasks (AsyncFunctionLongTime(), AsyncFunctionMediumTime()) are canceled.

NextExpression(WinnerResult)
```

The following code shows the syntax for the race expression.

```Verse
expression0
race:
    slow-expression
    mid-expression
    fast-expression
expression1
```

Race Expression Use

Where you can use a race expression

Async contexts

Invocation time of the race expression

Async

Requirements for race code block
The body of the race expression must have at least two expressions, and all the expressions must be async.

What the race expression does

Similar to sync, but cancels all but the “winning” subexpression. If any other expressions complete at the same
simulation time as the earlier expression, the first (earlier) expression “wins” and breaks any tie. Any “losing” expression tasks are canceled.

When the race expression completes

The race is completed when the “winning” expression in the code block has completed. This refers to the fastest,
shortest length, first completed, or least amount of time to complete.
When the next expression after race starts

Any expression that follows the race expression is started once the first expression finishes.
Result of the race expression

The result of a race is the result of the first completed expression. The result type is the most common
compatible type of all expressions in the code block.

This might seem simple, but race is one of the most useful and powerful expressions in the Verse arsenal. It is
key to stopping other arbitrarily complex async code in a structured fashion — a form of early exit. It does this in a very clean way by keeping whatever tests are needed to determine when to stop separated from the code

that is to be stopped.

Any async expression can be canceled.
Some async expressions, such as an endless loop or Sleep(Inf) will never complete. The only way they can be stopped is to cancel them. This can be a strong strategy when paired with one or more race expressions.

Async expressions will not have a result if they are canceled, so any variable or other expression that depends
on a canceled async expression would not be bound.

Need to stop some complex behavior after some amount of time or after some complex sequence of events trigger?
Without race, you would normally need to sprinkle tests, such as polling all throughout your complex behavior.
With race, you only need to add all stop conditions as sibling subexpressions to the complex behavior.

```Verse
race:
    ComplexBehavior() # Could be simple or as complex as a whole game
    Sleep(60.0)       # Timeout after one minute
    EventTrigger()    # Some other arbitrary test that can be used to stop
```

A race result can be used to determine which subexpression finished first, or won the race.

```verse
# Adding a unique result to subexpressions so it can
# be used to determine which subexpression won
Winner := race:
    block:        # task 1
        AsyncFunction1()
        1
    block:        # task 2
        AsyncFunction2a()
        AsyncFunction2b()
        AsyncFunction2c()
        2
    loop:         # task 3
        # endless loop which could never win
        AsyncFunction3()
        3
MyLog.Print("The winning subexpression was: {Winner}")
```

### Rush

Use a rush expression to run two or more async expressions without canceling the slower expressions.
In a rush expression, all expressions in the rush block run concurrently, but as soon as one expression completes, the rush expression yields control back and the other expressions continue to run independently. rush is a structured concurrency expression.

The rush expression is used to run a block of two or more async expressions concurrently (simultaneously).
When the fastest subexpression completes, any expression that follows the rush is evaluated, and any remaining subexpressions continue to evaluate.

```verse
set WinnerResult = rush:
    # All three async functions start at the same time
    AsyncFunctionLongTime()
    AsyncFunctionShortTime()  # This will win and its result is used
    AsyncFunctionMediumTime()

# Next expression is called after the fastest async function (AsyncFunctionShortTime()) completes.
# All other subexpression tasks (AsyncFunctionLongTime(), AsyncFunctionMediumTime()) continue.
NextExpression(WinnerResult)
AsyncFunction4()

# If any rush subexpression tasks are still running when AsyncFunction4 completes
# then they are now canceled.
```

The following code shows the syntax for the rush expression.

```Verse
expression0
rush:
    slow-expression
    mid-expression
    fast-expression
expression1
```

Rush Expression Use

Where you can use a rush expression

Async contexts

Invocation time of the rush expression
Async

Requirements for rush code block

The body of the rush expression must have at least two expressions, and all of the expressions must be async.

What the rush expression does

Is similar to race, but expressions that complete after first completion continue. If any expressions effectively complete at the same simulation update, then the earlier encountered expression that completes breaks any tie. Any incomplete expressions continue to evaluate until they complete, or until the enclosing async context completes, at which point, any remaining losing expressions are canceled — whichever occurs first.

When the rush expression completes

The rush expression completes when the first expression in the code block has completed. This could be the fastest, shortest length, first completed, or least amount of time to complete.
When the next expression after rush starts

Any next expression that follows the rush expression is started when the completed expression finishes.

Result of the rush expression

The result of a rush expression is the result of the first completed expression. The result type is the most common compatible type of all expressions in the code block.

A rush expression cannot currently be used in the body of an iteration expression like loop or for. If it must
be used, then wrap it in an async function and have the iteration expression call that function.

### Branch

Use a branch expression to start one or more async expressions, then immediately execute following expressions.
A branch expression starts a block of one or more async subexpressions, and any expression that follows after is executed immediately, without waiting for the branch expressions to complete.
You can use branch essentially to treat any async block of code as though it were fire-and-forget immediate, but it still must be called within an async context.

```Verse
branch:
    # This block continues until completed
    AsyncFunction1()    # Starts effectively the same time as AsyncFunction3()
    Method1()  # Block can be mixed with immediate expressions
    AsyncFunction2()
AsyncFunction3()  # Starts effectively the same time as AsyncFunction1()
# If branch block task is still running when AsyncFunction3 completes
# then any remaining branch task is canceled
```

The following code shows the syntax for the branch expression.

```Verse
expression0
branch:
    slow-expression
    mid-expression
    fast-expression
expression1
```

It is similar to the unstructured concurrency spawn expression, but branch allows for any arbitrary block of code, and is only permissible within, and bounded by, an enclosing async context. Because of this, branch is preferred over spawn whenever possible.

Branch Expression Use

Where you can use a branch expression

Async contexts
Invocation time of the branch expression

Immediate

Requirements for branch code block

The branch expression must have at least one async expression.
What the branch expression does

The body of the branch expression is started as soon as it is encountered. The body of the branch expression continues to evaluate until the code block completes or the enclosing async context completes — whichever occurs
first — at which point the branch code block task is canceled.

When the branch expression completes

The branch expression completes immediately.
When the next expression after branch starts

Any expression that follows the branch expression is started immediately.

Result of the branch expression

A branch expression has no result, so its result type is void.

A branch expression may not currently be used in the body of an iteration expression such as loop or for. If it must be used then wrap it in an async function and have the iteration expression call that function.

### Spawn

Use a spawn expression to start one async expression in any context, then immediately execute the following expressions.

The spawn expression starts one async function invocation, and any expression that follows the spawn is executed immediately while the started async function task continues independently until it completes.

```verse
# Continues until completed without blocking
spawn{AsyncFunction1()}  # Started at same time as expression0
expression0         # Started at same time as AsyncFunction1()
```

The following code shows the syntax for the spawn expression.

```Verse
expression0
spawn{ expression1 }
expression2
```

While similar to branch, the spawn body is limited to a single async function call. It is also allowed outside of an async context, so it can be called within both non-async and async functions.

A spawn expression should be treated like an emergency escape hatch, while branch should be used in place of

spawn whenever possible.

Spawn Expression Use

Where you can use a spawn expression
Any context.

Invocation time of the spawn expression

Immediate.

Requirements for spawn code block

The body of the spawn expression is started as soon as it is encountered. It must have at least one async expression.

What the spawn expression does

The body of a spawn creates an async context like the body of an async function. However, only a single async
function call is allowed within the spawn body. The async function of the spawn is started as soon as it is encountered, and evaluates as much as possible until it encounters something suspending or blocking. The spawned async function continues to evaluate until it completes without any further connection to the location where it was spawned.

When the spawn expression completes

The spawn expression completes immediately.

When the next expression after spawn starts
Any next expression that follows the spawn expression is started immediately.

Result of the spawn expression

A spawn has a task result.

### Task

A task is an object that represents the state of a currently-executing async function.

A task is an object used to represent the state of a currently-executing async function. Task objects are used
to identify where an async function is suspended, and the values of local variables at that suspend point.

Tasks execute concurrently in a cooperatively multitasked environment.
A task can be durational, based on a lifespan of one or more updates before it completes.

Tasks can be sequential, overlapped, staggered, and so on, in any logical order.
The sequence and overlapping flow of tasks is specified through the use of structured or unstructured concurrency expressions.

Each task can be concurrently arranged sequentially, overlapped, staggered, and so on, in any logical order of time. Internally, a task could have a caller (or even several callers), and zero or more dependent sub-tasks that form a call graph (as opposed to a call stack).

A task is similar to a thread, but has the advantage over threads in that context switching between tasks does not involve any system calls, expensive context-state saving, or processor-blocking calls, and a processor can be 100% utilized). You don’t need synchronization such as mutexes or semaphores to guard critical sections, and there is no need for support from the operating system.

The task(t:type) class allows direct programmatic querying and manipulation of tasks in an unstructured manner, though it is generally recommended that tasks be manipulated through structured concurrency expressions for greater clarity, power and efficiency.

Currently, the only exposed function for task is Await(), which waits until the current task has completed. This essentially anchors a task and adds a caller for it to return to at the call point.

```Verse
spawn{AsyncFunction3()}
# Get task to query / give commands to
# starts and continues independently
Task2 := spawn{Player.MoveTo(Target1)}
Sleep(1.5) # Wait 1.5 Seconds
MyLog.Print("1.5 Seconds into Move_to()")
Task2.Await() # wait until MoveTo() completed
Wait(0.5)     # Wait 0.5 Seconds
# Explicit start and wait until completed
# Task1 could still be running
Target1.MoveTo(Target2)
```

Similar to the example above, the one below uses structured concurrency expressions:

```Verse
sync:
    AsyncFunction3()  # Task 1
    block:
        Player.MoveTo(Target1)  # Task 2
        Sleep(0.5)  # Wait 0.5 Seconds
        Target1.MoveTo(Target2)
    block:  # Task 3
        Sleep(1.5)  # Wait 1.5 Seconds
        MyLog.Print("1.5 Seconds into Move_to()")
```

Array1 : []int = array{10, 11, 12}
# Array2 is an array variable of integers
var Array2 : []int = array{20, 21, 22}
# we concatenate Array1, Array2, and a new array of integers
# and assign that to the Array2 variable
set Array2 = Array1 + Array2 + array{30, 31}
# we assign the integer 77 to index 1 of Array2
if (set Array2[1] = 77) {}
for (Index := 0..Array2.Length - 1):
    if (Element := Array2[Index]):
        Print("{Element} at index {Index}")
```

This code will print:

```Verse
10 at index 0
    77 at index 1
    12 at index 2
    20 at index 3
    21 at index 4
    22 at index 5
    30 at index 6
    31 at index 7
```

Multi-Dimensional Arrays

The arrays in the previous examples were all one-dimensional, but you can also create multi-dimensional arrays. Multi-dimensional arrays have another array, or arrays, stored at each index, similar to columns and rows in a table.

For example, the following code produces a two-dimensional (2D) array, visualized in the following table:

```verse
var Counter : int = 0
Example : [][]int =
    for (Row := 0..3):
        for(Column := 0..2):
            set Counter += 1
```

  Column 0 Column 1 Column 2
Row 0
1
2
3
Row 1
4
5
6
Row 2
7
8
9
Row 3
10
11
12

To access elements in a 2D array, you must use two indices. For example, Example[0][0] is 1, Example[0][1] is 2, and Example[1][0] is 4.

The following code shows how to use a for expression to iterate through the Example 2D array.

```verse
if (NumberOfColumns : int = Example[0].Length):
```

```verse
    for(Row := 0..Example.Length-1, Column := 0..NumberOfColumns):
         if (Element := Example[Row][Column]):
             Print("{Element} at index [{Row}][{Column}]")
```

This code will print:

```Verse
1 at index [0][0]
    2 at index [0][1]
    3 at index [0][2]
    4 at index [1][0]
    5 at index [1][1]
    6 at index [1][2]
    7 at index [2][0]
    8 at index [2][1]
    9 at index [2][2]
    10 at index [3][0]
    11 at index [3][1]
    12 at index [3][2]
```

The number of columns in each row is not required to be constant.

For example, the following code produces a two-dimensional (2D) array, visualized in the following table, where the number of columns in each row is greater than the previous row:

```verse
Example : [][]int =
    for (Row := 0..3):
        for(Column := 0..Row):
            Row * Column
```
  
  Column 0 Column 1 Column 2 Column 3
Row 0

0

Row 1
1
Row 2
0
2
4
Row 3
0
3
6
9

### Persistable Type

An array is persistable if the type of elements in the array are persistable, which means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.
arrays

### Map

A map is a container where you can store values associated with other values, called key-value pairs, and access
the elements by their unique keys.

A map is a container type that holds key-value pairs, which are mappings from one value to another value.
Elements in a map are ordered based on the order of key-value pairs when you create the map, and you access
elements in the map using the unique keys you define.

For example, if you want to keep a count of how many times you encounter a word, you can create a map using the word as the key and its count as the value.

```verse
WordCount : [string]int = map{"apple" => 11, "pear" => 7}
```

If you use the same key multiple times when initializing a map, the map will only keep the last value provided for that key. In the following example, WordCount will only have the "apple" => 2 key-value pair. The "apple" => 0 and "apple" => 1 pairs are discarded.

```verse
WordCount : [string]int = map{"apple" => 0, "apple" => 1, "apple" => 2}
```

Supported Key Types

Key-value pairs can be of any type as long as the key type is comparable, because there needs to be a way to check if a key already exists for a map.
The following types can be used as keys:
char

enum

A class, if it’s comparable

An option, if the element type is comparable

An array, if the element type is comparable

A map if both the key and the value types are comparable
A tuple if all elements in the tuple are comparable

### Map Length

You can get the number of key-value pairs in a map by accessing the field Length on the map. For example, map{"a" => "apple", "b" => "bear", "c" => "candy"}.Length returns 3.

Accessing Elements in a Map

You can access an element in a map by using a key, for example WordCount["apple"].
Accessing an element in a map is a failable expression and can only be used in a failure context, such as an if expression. For example:

```verse
ExampleMap : [string]string = map{"a" => "apple", "b" => "bear", "c" => "candy"}
for (Key->Value : ExampleMap):
    Print("{Value} in ExampleMap at key {Key}")
```

Key
"a"
"b"
"c"
Value
"apple"
"bear"
"candy"

Adding and Modifying Elements in a Map

You can add elements to a map variable by setting the key in a map to a specific value. For example set ExampleMap["d"] = 4. Existing key-value pairs can be updated by similarly assigning a value to a key that already exists in the map. Adding an element to a map is a failable expression and can only be used in a failure context, such as an if expression. For example:

```verse
var ExampleMap : [string]int = map{"a" => 1, "b" => 2, "c" => 3}
# Modifying an existing element
if (set ExampleMap["b"] = 3, ValueOfB := ExampleMap["b"]):
    Print("Updated key b in ExampleMap to {ValueOfB}")
# Adding a new element
if (set ExampleMap["d"] = 4, ValueOfD := ExampleMap["d"]):
    Print("Added a new key-value pair to ExampleMap with value {ValueOfD}")
```

Removing Elements from a Map

Elements can be removed from a map variable by creating a new map that excludes the key you want to remove. An example of a function that provides removal from [string]int maps is provided below.

```verse
# Removes an element from the given map and returns a new map without that element
RemoveKeyFromMap(ExampleMap:[string]int, ElementToRemove:string):[string]int=
    var NewMap:[string]int = map{}
    # Concatenate Keys from ExampleMap into NewMap, excluding ElementToRemove
```

```verse
    for (Key -> Value : ExampleMap, Key <> ElementToRemove):
        set NewMap = ConcatenateMaps(NewMap, map{Key => Value})
    return NewMap
```

Weak Map

The type weak_map is a supertype of the map type. You would use a weak_map in a similar way to how you’d use the map type in most cases, but with the following exceptions:

You cannot query how many elements a weak_map contains because weak_map does not have a Length member.

You cannot iterate through the elements of a weak_map.

You cannot use ConcatenateMaps() on a weak_map.

Another difference is that the type definition for a weak_map requires you to define the key-value pair types using the weak_map function, such as MyWeakMap:weak_map(string, int) = map{}, which defines a weak map named MyWeakMap that will have a string key paired with an integer value. Since weak_map is a supertype of map, you can initialize it with a standard map{}.

The following shows an example of creating a weak_map variable, and accessing an element in the weak map:

```verse
ExampleFunction():void=
```

```verse
    var MyWeakMap:weak_map(int, int) = map{} # Supertype of the standard map, so it can be assigned from the standard map
    if:
        set MyWeakMap[0] = 1 # Same means of mutation of a particular element as the standard map
    then:
        if (Value := MyWeakMap[0]):
            Print("Value of map at key 0 is {Value}")
    set MyWeakMap = map{0 => 2} # Same means of mutation of the entire map as the standard map
```

Persistable Type

A map is peristable if both the key and value types are persistable. When a map is persistable, it means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.
map

### Tuple

A tuple is a container where you can group two or more expressions of mixed types and access the elements in the tuple by their position.

A tuple is a grouping of two or more expressions that is treated as a single expression.

The elements of a tuple are in the order you insert them into the tuple, and you can access the elements by their position in the tuple, called their index. Because the expressions in a tuple are grouped, they can be treated as a single expression.

The word tuple is a back formation from quadruple, quintuple, sextuple, and so on. Compare to array.
A tuple literal has multiple expressions between (), with the elements separated by commas:

```Verse
(1, 2, 3)
```

The order of the elements in a tuple is important. The following tuple is different than the previous tuple example:

```Verse
(3, 2, 1)
```

The same expression can also be in multiple positions in a tuple:

```Verse
("Help me Rhonda", "Help", "Help me Rhonda")
```

Tuple expressions can be of any type, and can contain mixed types (unlike arrays which can only have elements of one type):

```Verse
(1, 2.0, "three")
```

Tuples can even contain other tuples:

```Verse
(1, (10, 20.0, "thirty"), "three")
```

If you are familiar with these terms, a tuple is like:

An unnamed data structure with unnamed ordered elements

A fixed-size array where each element can be a different type

Tuples are especially useful for:

Returning multiple values from a function.

A simple in-place grouping that is more concise than the overhead of making a fully-described, reusable data structure (such as a struct or class).

### Specifying a Variable with a Tuple Type

To specify the type of variable as a tuple, the tuple prefix is used before comma-separated types enclosed in ():

```verse

MyTupleInts : tuple(int, int, int) = (1, 2, 3)
MyTupleMixed : tuple(int, float, string) = (1, 2.0, "three")
MyTupleNested : tuple(int, tuple(int, float, string), string) = (1, (10, 20.0, "thirty"), "three")
```

Tuple types can also be inferred:

```verse
MyTupleInts   := (1, 2, 3)
MyTupleMixed  := (1, 2.0, "three")
MyTupleNested := (1, (10, 20.0, "thirty"), "three")
```

Tuple type specifiers can be used in data members and function type signatures for parameters or a return Type:

```Verse
ExampleFunction(Param1 : tuple(string, int), Param2 : tuple(int, string)) : tuple(string, int) =
    # Using parameter as result
    Param1
```

Tuple Element Access

The elements of a tuple can be accessed with a non-failing, zero-based index operator that takes an integer. The index operator cannot fail (unlike an array index operator [index] which can fail) because the compiler always knows the number of elements of any tuples and so any out-of-bounds index will be a compile-time error:

```verse
MyTuple := (1, 2.0, "three")
MyNestedTuple := (1, (10, 20.0, "thirty"), "three")
var MyInt: int = MyTuple(0)
var MyFloat: float = MyTuple(1)
var MyString: string = MyTuple(2)
Print("My variables: {MyInt}, {MyFloat}, {MyString}")
Print("My nested tuple element: {MyNestedTuple(1)(2)}")
```

Tuple Array Coercion

Tuples can be passed wherever an array is expected, provided that the type of the tuple elements are all of the same type as the array. Arrays cannot be passed where a tuple is expected.
Tuple Expansion

A tuple passed as a single element to a function will be as though that function were called with each of that tuple's elements separately. This is called tuple expansion or splatting.

```Verse
F(Arg1 : int, Arg2 : string) : void =
    DoStuff(Arg1, Arg2)
G() : void =
    MyTuple := (1, "two")
    F(MyTuple(0), MyTuple(1))  # Accessing elements
    F(MyTuple)                 # Tuple expansion
```

The sync structured concurrency expression has a tuple result that allows several arguments that evaluate over time to be evaluated simultaneously. For more information, see Concurrency.

Persistable Type

A tuple is persistable if every element type in the tuple is persistable. When a tuple is persistable, it means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.
tuple

class2 := class<concrete>:
    Property : int
# Error: class3 must also have the <concrete> specifier since it inherits from class1
class3 := class(class1):
    Property : int = 0
```

A concrete class can only inherit directly from an abstract class if both classes are defined in the same module. However, it does not hold transitively — a concrete class can inherit directly from a second concrete class in another module where that second concrete class inherits directly from an abstract class in its module.

### Unique Specifier

The unique specifier can be applied to a class to make it a unique class. To construct an instance of a unique class, Verse allocates a unique identity for the resulting instance. This allows instances of unique classes to be compared for equality by comparing their identities. Classes without the unique specifier don't have any such identity, and so can only be compared for equality based on the values of their fields.
This means that unique classes can be compared with the = and <> operators, and are subtypes of the comparable type.

For example:

```verse
unique_class := class<unique>:
    Field : int
Main()<decides> : void =
    X := unique_class{Field := 1}
    X = X # X is equal to itself
    Y := unique_class{Field := 1}
    X <> Y # X and Y are unique and therefore not equal
```

### Final Specifier

You can only use the final specifier on classes and fields of classes.
When a class has the final specifier, you cannot create a subclass of the class. In the following example, you cannot use the pet class as a superclass, because the class has the final specifier.

```verse
pet := class<final>():
    …
cat := class(pet): # Error: cannot subclass a “final” class
    …
```

When a field has the final specifier, you cannot override the field in a subclass. In the following example, the cat class can’t override the Owner field, because the field has the final specifier.

```verse
pet := class():
    Owner<final> : string = “Andy”
cat := class(pet):
    Owner<override> : string = “Sid” # Error: cannot override “final” field
```

When a method has the final specifier, you cannot override the method in a subclass. In the following example, the cat class can’t override the GetName() method, because the method has the final specifier.

```verse
pet := class():
    Name : string
    GetName<final>() : string = Name
cat := class(pet):
    …
    GetName<override>() : string =  # Error: cannot override “final” method
        …
```

Block Expressions in a Class Body

You can use block expressions in a class body. When you create an instance of the class, the block expressions are executed in the order they are defined. Functions called in block expressions in the class body cannot have the NoRollback effect.

As an example, let’s add two block expressions to the cat class body and add the transacts effect specifier to the Meow() method because the default effect for methods has the NoRollback effect.

```verse
cat := class():
    Name : string
    Age : int
    Sound : string
    Meow()<transacts> : void =
        DisplayOnScreen(Sound)
    block:
            Self.Meow()
```

When the instance of the cat class, OldCat, is created, the two block expressions are executed: the cat will
first say “Rrrr”; then “Garfield” will print to the output log.

Interfaces

Interfaces are a limited form of classes that can only contain methods that don't have a value. Classes can only inherit from a single other class, but can inherit from any number of interfaces.

Persistable Type

A class is persistable when:

Defined with the persistable specifier.
Defined with the final specifier, because persistable classes cannot have subclasses.

Not unique.

Does not have a superclass.

Not parametric.

Only contains members that are also persistable.

Does not have variable members.

When a class is persistable, it means that you can use them in your module-scoped weak_map variables and have
their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable

Data in Verse.

The following Verse example shows how you can define a custom player profile in a class that can be stored, updated, and accessed later for a player. The class player_profile_data stores information for a player, such as their earned XP, their rank, and quests they’ve completed.

```verse
player_profile_data := class<final><persistable>:
    Version:int = 1
    Class:player_class = player_class.Villager
    XP:int = 0
    Rank:int = 0
    CompletedQuestCount:int = 0
    QuestHistory:[]string = array{}
```

### Enum

An enum is a type used to store named sets of things.

Enum is short for enumeration, which means to name or list a series of things, called enumerators. This is a type in Verse that can be used for things like days of the week or compass directions.

Creating an enum in Verse

Closed and Open Enums

Verse uses the <open> and <closed> attribute specifiers on enums to determine how you can change the definition of the enum once your island is published.

Enums are closed by default. With closed enums, you cannot add or reorder enum values or change a closed enum to
an open one once your island has been published.

Closed enums are best used for cases where your values are expected to stay the same, like days of the week.
With open enums, you can:

Add new enum values.

Reorder enum values.

Change an open enum to a closed enum.

Open enums are best used when you expect the number of values in your enum may increase in the future. For
example, an enum of weapon types.

Open enums cannot be used in a case statement without a default case. Closed enums can be used in case
statements without a default case only if all enumeration values have a case.

Creating an Enum

Creating closed enums:

Enums are closed by default.

Use the keyword enum followed by {}. If you want to specify initial elements in the enum, add the enumerators between the {}, separated by ,.

You can explicitly define the enum as closed by including the <closed> specifier after the enum keyword.

```verse
# If not specified, enums are closed by default.
direction := enum{Up, Down, Left, Right}
# The same as the previous enum, where its closed nature is explicit.
direction := enum<closed>{Up, Down, Left, Right}
```

Creating open enums:

You must explicitly define an open enum by including the <open> specifier after the enum keyword.

```verse
## You can add and reorder enum values, or change this to a <closed> enum
direction := enum<open>{Up, Down, Left, Right}
Accessing an enumerator: Use . on the enum, followed by the enumerator you want to use. For example direction.Up.
```

Persistable Type

An enum is persistable when it is defined with the <persistable> specifier. This means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions.
For more details on persistence in Verse, check out Using Persistable Data in Verse.

Non-persistent enums cannot be used with persistable data.

The following is an example of a closed persistable enum for the days of the week that can be stored, updated, and accessed later for a player.

```verse
day := enum<persistable>:
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
```

If not specified, all enums are closed by default.

Published Enums

Once you have published your island, certain aspects of closed and open enums with the <persistable> specifier are fixed.

Closed enums:

Cannot be updated to become <open>.

Cannot add, rename, reorder or remove enum values.

If not specified, you can add the <closed> specifier.

Open enums:
Can be updated to become a closed enum with the <closed> specifier.

Can add and reorder enum values.

Cannot rename or remove enum values.

Cannot be used in case statements without a default case.

### Struct

A struct is a way to group several related variables together.

Struct is short for structure, and is a way to group several related variables together. Any variables can be grouped, including variables of different types.

Instantiating a struct in Verse

```verse
coordinates := struct:
    X : float = 0.0
    Y : float = 0.0
```

Creating a struct: Use the keyword struct followed by a code block. Definitions in the struct’s code block define the fields of the struct.

```verse
Position := coordinates{X := 1.0, Y := 1.0}
```

Instantiating a struct: You can construct an instance of a struct from an archetype. An archetype defines the values of a struct’s fields.

```Verse
Position.X
```

Accessing fields on a struct: You can access a struct’s fields to get their value by adding . between the struct instance and the field name.

Persistable Type

A struct is persistable when:

Defined with the persistable specifier.

Not parametric.

Only contains members that are also persistable.

When a struct is persistable, it means that you can use them in your module-scoped weak_map variables and have
their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.

You cannot alter a persistable struct once you’ve published your island. For this reason, we recommend using
persistable structs only when the schema is known to be constant.

The following is an example of a persistable struct X, Y coordinates that can be stored, updated, and accessed later for a player.

```verse
coordinates := struct<persistable>:
    X:float = 0.0
    Y:float = 0.0
```

## Subclass

A subclass is a class that extends the definition of another class by adding or modifying the fields and methods of the other class.

In Verse, you can create a class that extends the definition of another class by adding or modifying the fields and methods of the other class. This is often called subclassing or inheritance, because one class inherits definitions from the other class.

Let’s look at the Class Designer device as an example of subclassing. With the Class Designer device, you can create character classes for player characters that let you define the attributes and inventories specific to a character class, such as a tank or DPS (damage per second) character.

DPS character class created with the Class Designer device

Tank character class created with the Class Designer device

In Verse, you could create a tank class and a dps class like this:

```verse
tank := class():
    StartingShields : int
    MaxShields : int
    AllowOvershield : logic
    DamageReduction : int
dps := class():
    StartingShields : int
    MaxShields : int
    AllowOvershield : logic
    MovementMultiplier : float
```

Because some of the fields in the two classes are the same, you can reduce duplication with a superclass that holds the shared properties and behaviors of the classes. Let’s call this superclass player_character, and make tank and dps subclasses of player_character:

```verse
player_character := class():
    StartingShields : int
    MaxShields : int
    AllowOvershield : logic
dps := class(player_character):
    MovementMultiplier : float
tank := class(player_character):
    DamageReduction : int
```

Since the tank and dps classes are subclasses of player_character, they automatically inherit the fields and methods of the player_character class, so you only need to specify what’s different in this class from the superclass.

For example, the dps class only adds the Movement Multiplier field, and the tank class only adds the DamageReduction field. This setup is useful if you change the shared behaviors of the two classes later because you’ll only need to change it in the superclass.

Diagram showing inheritance relationship between the superclass player_character and the subclasses dps and tank

With Verse, you can add more changes to differentiate the tank and dps classes by adding methods to the subclasses.

A useful effect of subclassing is that you can use the relationship between a superclass and its subclasses. Because of inheritance, an instance of tank is a specialized player_character, and an instance of dps is a specialized player_character, which is referred to as an is-a relationship. Since tank and dps are both subclasses of the same superclass and diverge from their shared superclass, tank does not have a relationship with dps.

### Override Specifier

To create instances of classes with initial values, a common practice is to have a function that generates the instances. For example:

```verse
CreateDPSPlayerCharacter() : dps =
    return dps{StartingShields := 0, MaxShields := 0, AllowOvershield := false, MovementMultiplier := 1.9}
CreateTankPlayerCharacter() : tank =
    return tank{StartingShields := 100, MaxShields := 200, AllowOvershield := true, DamageReduction := 50}
```

The CreateTankPlayerCharacter() and CreateDPSPlayerCharacter() functions create the instances with the appropriate initial values. Alternatively, you can override the fields from the superclass and assign initial values, so you don’t need to provide so many initial values when creating an instance.

For example, the tank class from the previous section could look like this with overrides on the fields:

```verse
tank := class(player_character):
    StartingShields<override> : int = 100
    MaxShields<override> : int = 200
    AllowOvershield<override> : logic = true
    DamageReduction : int = 50
CreateTankPlayerCharacter() : tank =
    return tank{}
```

Diagram showing overrides in the inheritance relationship between the superclass player_character and the subclasses dps and tank

You can also override methods in the subclass, which means you can use the overriding method everywhere the overridden method can be used. This means:

The method must accept at least any argument accepted by the overridden method, so the parameter type must be a
supertype of the overridden function's parameter type.

The method must not return a value that the overridden method couldn't have, so the return type must be a subtype of the overridden method's return type.

The method must not have more effects than the overridden method, so the effect specifier must be a subtype of the overridden method's effect specifier.

Super

Similar to Self, you can use (super:) to access the superclass implementations of fields and methods. To be able
to use (super:), the field or method must be implemented in the superclass definition.

```verse
pet := class():
    Sound : string
    Speak() : void =
        Log(Sound)
cat := class(pet):
    Sound<override> : string = "Meow"
    Speak<override>() : void =
```

Block Expressions in a Subclass Body

Any block expressions that are in a subclass body will be executed after the block expressions specified in the superclass body. For example, in the following code, when the instance of the cat class named MrSnuffles is created, Speak() is executed first, then Purr().

```verse
pet := class():
    Speak() : void =
    ...
    block:
        Speak()
cat := class(pet):
    Purr() : void =
    ...
    block:
        Purr()
MrSnuffles := cat{}
```

### Abstract Specifier

When a class or a class method has the abstract specifier, you cannot create an instance of the class. Abstract classes are intended to be used as a superclass with partial implementation, or as a common interface. This is useful for when it doesn’t make sense to have instances of a superclass but you don’t want to duplicate properties and behaviors across similar classes.

In the following example, because pet is an abstract concept, an instance of the pet class isn’t specific enough, but a pet cat or pet dog does make sense, so those subclasses aren’t marked as abstract.

```verse
pet := class<abstract>():
    Speak() : void
cat := class(pet):
    Speak() : void =
    ...
dog := class(pet):
    Speak() : void =
    ...
```

### Interface

An interface provides a contract for how to interact with any class that implements the interface.
The interface type provides a contract for how to interact with any class that implements the interface. An interface cannot be instantiated, but a class can inherit from the interface and implement its methods. An interface is similar to an abstract class, except that it does not allow partial implementation or fields as part of the definition.
For example, let’s create an interface for anything that you can ride on, such as a bicycle or a horse:

```verse
rideable := interface():
    Mount()<decides> : void
    Dismount()<decides> : void
```

Any classes that inherit the interface must implement the interface’s functions and add the override specifier:

```verse
bicycle := class(rideable):
    ...
    Mount<override>()<decides> : void =
        ...
    Dismount<override>()<decides> : void =
        ...
horse := class(rideable):
    ...
    Mount<override>()<decides> : void =
        ...
    Dismount<override>()<decides> : void =
        ...
```

An interface can extend another interface. For example, you can specify that anything that you can ride should also be able to move.

```verse
moveable := interface():
    MoveForward() : void
rideable := interface(moveable):
    Mount()<decides> : void
    Dismount()<decides> : void
```

A class can inherit from an interface and another class. For example, you can define a horse, and differentiate it from one that has a saddle you can ride on:

```verse
horse := class(moveable):
    ...
    MoveForward()<decides> : void =
        ...
saddle_horse := class(horse, rideable):
    ...
    Mount<override>()<decides> : void =
        ...
    Dismount<override>()<decides> : void =
        ...
```

A class can inherit from multiple interfaces.

```verse
lockable := interface():
    Lock() : void =
        ...
    Unlock() : void =
        ...
bicycle := class(rideable, lockable):
    …
    Mount<override>()<decides> : void =
        ...
    Dismount<override>()<decides> : void =
        ...
    Lock<override>() : void =
        ...
    Unlock<override>() : void =
        ...
    MoveForward<override>() : void =
        ...
```

### Constructor

A constructor is a special function that creates an instance of the class that it’s associated with. It can be used to set initial values for the new object.

You can add a constructor for a class by adding the constructor specifier on the function name. Instead of specifying a return type on the function, the function is assigned the class name followed by any initialization of fields. A class can have more than one constructor.

```verse
class1 := class:
    Property1 : int
MakeClass1<constructor>(Arg1:int) := class1:
    Property1 := Arg1
Main():void =
    X := MakeClass1(1)
    F := MakeClass1()
    Z := F(2)
```

Defining a constructor for a class: You can add a constructor for a class by adding the <constructor> specifier on the function name. Instead of specifying a return type on the function, the function is assigned the class name followed by any initialization of fields. A class can have more than one constructor.

```verse
MakeOtherClass1<constructor>(Arg1 : int) := class1:
    let:
        OnePlusArg1 := Arg1 + 1
    block:
        DoSomething(OnePlusArg1)
    Property1 := OnePlusArg1
    block:
        DoOtherStuff()
```

Adding variables and executing code in the constructor: You can execute expressions within a constructor with the block expression, and introduce new variables with the keyword let.

```verse
MakeClass1Plus1<constructor>(Arg1 : int) := class1:
    MakeClass1<constructor>(Arg1 + 1) # Note use of <constructor> on invocation

# The base type constructor can be invoked in any order with respect to properties,
# but the properties "win"

MakeOtherClass2<constructor>(Arg1 : int, Arg2 : int) := class2:
    Property2 := Arg2
    MakeClass1<constructor>(Arg1)
    # Note that effects are still ordered as they appear in the code
```

Calling other constructors in a constructor: You can call other constructors from a constructor. You can also call constructors for the superclass of the class from a constructor of the class as long as all fields are initialized. When a constructor calls another constructor and both constructors initialize fields, only the values provided to the first constructor are used for the fields. The order of evaluation for expressions between the two constructors will be in the order the expressions are written (as far as side effects are concerned), but only the values provided to the first constructor are used.

# This means that we need to account for failure as it is a failable expression.
# This results in the following
# var WoodInt:int = failable expression or the value if it fails.
var WoodCollectedFloat:float = 10.5
var WoodInt:int = Int[WoodCollectedFloat] or 0
Print("Printing WoodInt Value (10): {WoodInt}")
# Similar to Int[], Floor[], Ceil[], and Round[] also have the <decides> effect
# So we must account for failure.

var StoneCollectedFloat:float = 12.9
var StoneInt:int = Floor[StoneCollectedFloat] or 0
Print("Printing StoneInt Floor (12): {StoneInt}")
var GoldCollectedFloat:float = 19.1
var GoldInt:int = Ceil[GoldCollectedFloat] or 0
Print("Printing GoldInt Floor (20): {GoldInt}")
var FoodCollectedFloat:float = 25.4
var FoodInt:int = Round[FoodCollectedFloat] or 0
Print("Printing FoodInt Round (25): {FoodInt}")
```

In this example, the if expression creates the failure context for these failable functions and set assigns the values to variables of type int.

```verse
var WoodCollected:int = 0
var StoneCollected:int = 0
var GoldCollected:int = 0
var FoodCollected:int = 0
if:
    # This block is the condition of the if expression
    # Which creates the failure context
    # If any fail, the entire chain of execution is rolled back
    # And the else branch, if it exists, is executed
    # WoodCollected is now 2
    TempWoodInt:int = Round[1.6]
    set WoodCollected = TempWoodInt
    # StoneCollected is now 1
    TempStoneInt:int = Floor[1.9]
    set StoneCollected = TempStoneInt
    # GoldCollected is now 2
    TempGoldInt:= Ceil[1.2]
    set GoldCollected = TempGoldInt
    # FoodCollected is now 1
    TempFoodInt:= Int[1.56]
    set FoodCollected = TempFoodInt
then:
    # If the operations in the if expression succeed
    # Also perform the operations in the then block
    Print("WoodCollected: {WoodCollected}")
    Print("StoneCollected: {StoneCollected}")
    Print("GoldCollected: {GoldCollected}")
    Print("FoodCollected: {FoodCollected}")
else:
    # The else block represents operations executed in the case of failure
    Print("Failure when attempting Float to Int conversion!")
```

Converting Int to Float

The multiply operator (*) converts the integer to a floating-point number before performing the multiplication.  The way to convert from an int to a float data type is to multiply the integer by 1.0.
This code converts the int variable StartingPositionX into a float through multiplication so it can be used in the declaration of a vector3 variable. The data type vector3 requires float type values for its X, Y, and Z fields.

```verse
# Required for the vector3 type
using { /UnrealEngine.com/Temporary/SpatialMath}
var StartingPositionX:int = 960
# CurrentX = 960.0
var CurrentX:float = StartingPositionX * 1.0
```

```verse
var CurrentPosition:vector3 = vector3{X := CurrentX, Y := 0.0, Z := 0.0}
Print("CurrentX: {CurrentX}")
```

Converting to a String

You can convert multiple data types to a string using either a ToString() function or string interpolation, which calls a ToString() function. Currently, the following types have built-in ToString() functions in Verse.

[]char
char
vector2
vector3
rotation

In this example, you can see variables being converted to a string through string interpolation and ToString() functions. Both methods have the same result because string interpolation calls ToString().

```verse
var WoodCollected:int = 100
# Convert using string interpolation
Print("WoodCollected: { WoodCollected }")
# or ToString() function
Print("WoodCollected: " + ToString(WoodCollected))
var InitialDistance:float = 3.625
# Convert using string interpolation
Print("InitialDistance: { InitialDistance }")
# or ToString() function
```

Converting a Custom Data Type to a String

Custom data types can also be converted to strings by implementing a ToString(custom_type) function for the data type. If a ToString(custom_type) function exists, string interpolation will use it to automatically convert data types to strings.
Here is an example of a custom ToString() function for an enum of fruits.

```verse
fruit := enum:
    Apple
    Banana
    Strawberry
ToString(Fruit: fruit):string =
    case(Fruit):
        fruit.Apple => "Apple"
        fruit.Banana => "Banana"
        fruit.Strawberry => "Strawberry"
PickUpFruit():void =
    # Examples of using string interpolation to convert data to strings
    var FruitItem:fruit = fruit.Banana
    # Picked up: Banana
    Print("Picked up: {FruitItem}")
    set FruitItem = fruit.Apple
    # Picked Up: Apple
    Print("Picked up: {FruitItem}")
```

Here is an example of a custom ToString() function for a custom class. Notice that the ToString() function is declared outside of the waypoint class. In the SetDestination() function, the string interpolation of Destination is calling the custom ToString() function.

```verse
# Custom class with constructor and a ToString() function
waypoint := class():
    DisplayName:string
    Position:vector3 = vector3{}
MakeWaypoint<constructor>(Name:string, X:float, Y:float, Z:float) := waypoint:
    DisplayName := Name
    Position := vector3{X := X, Y := Y, Z := Z}
ToString(Waypoint: waypoint):string =
    return "{Waypoint.DisplayName} at {Waypoint.Position}"
SetDestination():void =
    Destination:waypoint = MakeWaypoint("River", 919.0, 452.0, 545.0)
    # River at {x=919.0, y=452.0, z=545.0}
    Print("Destination: {Destination}")

### Converting an Object Reference to a Different Type
```

You can explicitly convert references to objects (or type cast) to different classes or interfaces using the following syntax:

```verse
if (NewObjectReference := object_type_to_cast_to[ObjectReference]) {}
```

The object_type_to_cast_to represents the class or interface that you are attempting to convert the reference to. This is a failable expression because the type conversion will fail if the object can't be converted to the specified type. Attempting to convert an object reference to a class will fail if the class does not match the object's type, the type of a superclass, or an interface that the object's class implements.

This code declares an interface positionable, an abstract class shape that inherits from positionable, and two subclasses of shape: triangle and square. It then creates an object of type square called MyShape and attempts to type cast it to three other types. Here is a breakdown of the results.

square Type Cast To Result

square

succeeds because MyShape is a square

triangle

fails because triangle is not a superclass of square, and triangle is not an interface that square implements
positionable

succeeds because square is a subclass of shape, and all subclasses of shape must implement positionable.

```verse
## Class and interface definitions
positionable := interface() {}
shape := class<abstract>(positionable) {}
triangle := class(shape) {}
square := class(shape) {}

## Create a square object referenced using the superclass type shape
MyShape:shape = square{}
## This will succeed since MySquare is a square object
```

In the last example, type casting will work but is not necessary. This code will have the same result:

```Verse
MyDrawable:positionable = MyShape
```

Examples Using Type Conversion

One use case for object type casting in UEFN is finding actors of a certain type and calling functions based on the type. To find out how to do this, see Finding Actors with a Gameplay Tag in Gameplay Tags.
type cast

### Type Aliasing

You can use a type alias to give a type a unique name without creating a new type.

Verse supports giving a type another name that can be used to refer to the same underlying type. This is known as a type alias. The syntax is similar to constant initialization as it is basically the same thing, but using types instead of values.

For example, to give an alias to float the following syntax could be used:

```verese
number := float
```

You can use this to shorten some type signatures. For example, instead of the code below,

```verse
RotateInts(X : tuple(int, int, int)) : tuple(int, int, int) =
    ( X(3), X(1), X(2))
```

an alias could be introduced for tuple, like this:

```verse
int_triple := tuple(int, int, int)
RotateInts(X : int_triple) : int_triple =
    (X(3), X(1), X(2))
```

This is particularly useful in combination with function types. For example,

```verse
int_predicate := type{_(:int)<transacts><decides> : void}
Filter(X : []int, F : int_predicate) : []int =
    for (Y : X, F[Y]):
        Y
```

Note that Verse does not currently support parametric type aliases.

For example,

```verse
predicate(t : type) := type{_(:t)<transacts><decides> : void}
```

is not supported.

### Parametric Types

Parametric types refer to any type that can take a parameter. You can use parametric types in Verse to define generalized data structures and operations. There are two ways to use parametric types as arguments: either in functions as explicit or implicit type arguments, or in classes as explicit type arguments.

Events are a common example of parametric types and are used extensively throughout devices in UEFN. For instance, the Button device has the InteractedWithEvent, which occurs whenever a player interacts with the button. To see a parametric type in action, check out the CountdownEndedEvent from the Custom Countdown Timer tutorial.

Explicit Type Arguments

Consider a box that takes two arguments. The first_item initializes an ItemOne, and the second_item initializes an ItemTwo, both of type type. Both first_item and second_item are examples of parametric types that are explicit arguments to a class.

```verse
box(first_item:type, second_item:type) := class:
    ItemOne:first_item
    ItemTwo:second_item
```

Because type is the type argument for first_item and second_item, the box class can be created with any two types. You could have a box of two string values, a box of two int values, a string and an int, or even a box of two boxes!

For another example, consider the MakeOption() function, which takes any type and returns an option of that type.

```Verse
MakeOption(t:type):?t = false
IntOption := MakeOption(int)
FloatOption := MakeOption(float)
StringOption := MakeOption(string)
```

You could modify the MakeOption() function to instead return any other container type, such as an array or a map.

Implicit Type Arguments

Implicit type arguments for functions are introduced using the where keyword. For example, given a function ReturnItem(), which simply takes a parameter and returns it:

```Verse
ReturnItem(Item:t where t:type):t = Item
```

Here, t is an implicit type parameter of the function ReturnItem(), which takes an argument of type type and immediately returns it. The type of t restricts what type of Item we can pass to this function. In this case since t is of type type, we can call ReturnItem() with any type. The reason to use implicit parametric types with functions is that it allows you to write code that works regardless of the type passed to it.
For example, instead of having to write:

```Verse
ReturnInt(Item:int):int = Item
ReturnFloat(Item:float):float = Item
```

The single function could be written instead.

```Verse
ReturnItem(Item:t where t:type):t = Item
```

This comes with the guarantee that ReturnItem() doesn't need to know what particular type the t is — whatever operation it performs, it will work regardless of the type of t.
The actual type to be used for t depends on how ReturnItem() is used. For example, if ReturnItem() is called with argument 0.0, then t is a float.

```verse
ReturnItem("t") # t is a string
ReturnItem(0.0) # t is a float
```

Here "hello" and 0.0 are the explicit arguments (the Item) passed to ReturnItem(). Both of these will work because the implicit type of Item is t, which can be any type.

For another example of a parametric type as an implicit argument to a function, consider the following MakeBox() function which operates on the box class.

```verse
box(first_item:type, second_item:type) := class:
    ItemOne:first_item
    ItemTwo:second_item
MakeBox(ItemOneVal:ValOne, SecondItemVal:ValTwo where ValOne:type, ValTwo:type):box(ValOne, ValTwo) =
    box(ValOne, ValTwo){ItemOne := ItemOneVal, ItemTwo := SecondItemVal}
Main():void =
    MakeBox("A", "B")
    MakeBox(1, "B")
    MakeBox("A", 2)
    MakeBox(1, 2)
```

Here the MakeBox() function takes two arguments, FirstItemVal and SecondItemVal, both of type type, and returns a box of type (type, type). Using type here means we’re telling MakeBox that the returned box could be made up of any two objects; it could be an array, a string, a function, etc. The MakeBox() function passes both arguments to Box, uses them to create a box, and returns it. Note that both box and MakeBox() use the same syntax as a function call.

A built-in example of this is the function for the Map container type, given below.

```verse
Map(F(:t) : u, X : []t) : []u =
    for (Y : X):
        F(Y)
```

### Type Constraints

You can specify a constraint on the type of an expression. The only currently supported constraint is subtype, and only for implicit type parameters. For example:

```verse
int_box := class:
    Item:int
MakeSubclassOfIntBox(NewBox:subtype_box where subtype_box:(subtype(int_box))) : tuple(subtype_box, int) = (NewBox, NewBox.Item)
```

In this example, MakeSubclassOfIntBox() will only compile when passed a class that subclasses from IntBox, since SubtypeBox has the type (subtype(IntBox)). Note that type can be seen as shorthand for subtype(any). In other words, this function accepts any subtype of any, which is every type.

nCovariance and Contravariance

Covariance and Contravariance refer to the relationship of two types when the types are used in composite types or functions. Two types that are related in some way, such as when one subclasses from the other, are either covariant or contravariant to each other depending on how they are used in a particular piece of code.

Covariant: Using a more specific type when the code expects something more generic.

Contravariant: Using a more general type when the code expects something more specific.

For instance, if we we could use an int in a situation where any comparable would be accepted (such as a float), our int would be acting covariantly, since we’re using a more specific type when a more generic one is expected. On the reverse, if we could use any comparable when normally an int would be used, our comparable would be acting contravariantly, since we’re using a more generic type when a more specific one is expected.

An example of covariance and contravariance in a parametric type might look like the following:

```Verse
MyFunction(Input:t where t:type):logic = true
```

Here t is used contravariantly as the input to the function, and logic is used covariantly as the output to the function.

It is important to keep in mind that the two types are not inherently covariant or contravariant to each other, rather whether they’re acting as covariant or contravariant depends on how they’re used in the code.

Covariant

Covariance means to use something more specific when you expect something generic. Usually this is for output from a function. All type uses that aren’t inputs to functions are covariant uses. A generic parametric type example below has payload acting covariantly.

```verse
DoSomething():int =
    payload:int = 0
```

For instance, suppose we have a class animal, and a class cat that subclasses animal. We also have a class pet_sanctuary that adopts out pets with the function AdoptPet(). Since we don’t know what kind of pet we’re going to get, AdoptPet() returns a generic animal.

```verse
animal := class:
cat := class(animal):
pet_sanctuary := class:
    AdoptPet():animal = animal{}
```

Suppose we have another pet sanctuary that only deals with cats. This class, cat_sanctuary, is a subclass of pet_sanctuary. Since this is a cat sanctuary, we override AdoptPet() to only return a cat instead of an animal.

```verse
cat_sanctuary := class(pet_sanctuary):
    AdoptPet<override>():cat = cat{}
```

In this case, the return type cat of AdoptPet() is covariant to animal. We’re using a more specific type when the original used a more general one.

This can also apply to composite types. Given an array of cat, we can initialize an array of animal using the cat array. The opposite does not work since animal cannot be converted to its subclass cat. The array of cat is covariant to the array of animal, because we’re treating a narrower type as a more generic type.

```verse
CatArray:[]cat = array{}
AnimalArray:[]animal = CatArray
```

Inputs to functions cannot be used covariantly. The following code will fail because the assignment of AnimalExample(), to CatExample(), is of type cat, which is too specific to be the return type of AnimalExample(). Reversing this order by assigning CatExample() to AnimalExample would work due to cat subtyping from animal.

```Verse
CatExample:type{CatFunction(MyCat:cat):void} = …
AnimalExample:type{AnimalFunction(MyAnimal:animal):void} = CatExample
```

An additional example follows where the variable t is only used covariantly.

```verse
# The line below will fail because t is used only covariantly.
MyFunction(:logic where t:type):?t = false
```

Contravariant

Contravariance is the opposite of covariant, and means to use something more generic when you expect something specific. This is usually input to a function. A generic parametric type example below has payload acting contravariantly.

```Verse
DoSomething(Payload:payload where payload:type):void
```

Say our pet sanctuary has a specific procedure for handling new cats. We add a new method to pet_sanctuary called RegisterCat().

```verse
pet_sanctuary := class:
    AdoptPet():animal = animal{}
    RegisterCat(NewAnimal:cat):void = {}
```

For our cat_sanctuary, we’re going to override this method to accept an animal as a type parameter because we already know that every cat is an animal.

```verse
cat_sanctuary := class(pet_sanctuary):
    AdoptPet<override>():cat = cat{}
    RegisterCat<override>(NewAnimal:animal):void = {}
```

Here animal is contravariant to cat, since we’re using something more generic when something more specific would work.

Using an implicit type introduced by a where clause covariantly produces an error. For example, payload here is used contravariantly, but errors out due to not being defined as an argument.

```Verse
DoSomething(:logic where payload:type) : ?payload = false
```

To fix this, this could be rewritten to exclude a type parameter:

```Verse
DoSomething(:logic) : ?false = false
```

Contravariant-only uses do not result in an error, but can be rewritten using any instead of false. For example:

```Verse
ReturnFirst(First:first_item, :second_item where first_item:type, second_item:type) : first_item = First
```

Since second_item was of type type and was not returned, we can replace it with any in the second example and avoid doing type checking on it.

```Verse
ReturnFirst(First:first_item, :any where first_item:type) : first_item = First
```

Replacing the type first_item with either any or false loses precision. For example, the following code will fail to compile:

```Verse
ReturnFirst(First:any, :any) :any = First
Main() : void =
    FirstInt:int = ReturnFirst(1, "ignored")
```

Known Limitations

Explicit type parameters for data types may only be used with classes, and not interfaces or structs. Inheritance related to parametric types is also disallowed.

```verse
OriginalBox(item:type) := class:
    Item:type = item
## InheritingBox cannot inherit from OriginalBox
## because Parametric types cannot inherit
InheritingBox(item : type) := class(OriginalBox):
    Item:type = item
```

Parametric types can reference themselves recursively as long as the recursion is direct. Parametric types cannot recursively reference other parametric types.

```verse
## Will compile
box_with_a_box(FirstItem : type) := class:
    ItemOne : FirstItem
    SecondThing : box_with_a_box(FirstItem)

## Will not compile
box_with_a_box(FirstItem : type) := class:
    ItemOne : FirstItem
    SecondThing : ListOfBoxes(FirstItem)
```

Currently, classes only support immutable parametric type data. For example, this code would not compile because ItemOne is a variable.

```verse
box(first_item:type, second_item:type) := class:
    var ItemOne:first_item
    ItemTwo:second_item
```

Explicit type parameters can be freely combined with a class, just as implicit type parameters can be combined with a function.

```verse
OptionBox(FirstItem : type) := class:
    Item:?FirstItem
Flatten(Box1:?OptionBox(item) where item:type)<decides><transacts>:?item =
    Box1?.Item
Main() : void =
    Box1 := OptionBox(int){Item := option{1}}
    if(Flatten[option{Box1}] = Box1.Item):
        Print("Retrieved the item from Box1")
```

### Type Macro

Verse has a special construct that can be used to get the type of an arbitrary expression (similar to decltype in modern C++): type. It can be used anywhere a type can be used. For example,

```verse
Foo() : int = 0
Bar(X : type{Foo()}) : type{Foo()} = X
```

It is particularly useful to describe the types of functions, and it is required to give the result type of a function as another function with non-default effects. For example,

```verse
comparison := enum:
    LT
    EQ
    GT
Less(X : int, Y : int)<decides> : int =
    X < Y
Equal(X : t, Y:comparable where t:subtype(comparable))<decides> : t =
    X = Y
Greater(X : int, Y : int)<decides> : int =
    X > Y
Comparison(Arg : comparison) : type{_(:int, :int)<decides> : int} =
    case (Arg):
        comparison.LT => Less
        comparison.EQ => Equal
        comparison.GT => Greater
```

Here, the comparison expression converts a comparison enumeration to the comparison operation each particular enumeration value corresponds to. This example also makes use of the special _ identifier, which can be used in type in places where an identifier is expected without having to actually provide a name that is otherwise unused.


using { module_folder }
using { base_module }
using { submodule }

```

Access of Definitions in a Module

The access of a module and its contents from other Verse files are set using access specifiers, such as public and internal.

By default, the access for definitions are internal, which means they're only discoverable within their own module. This is also true for modules introduced by folders in a project.
Because the default access specifier is internal, you can't access module members outside of the module without making them public. For instance:

```verse
# This module and its members are not accessible from other Verse Files.
private_module := module:
    SecretInt:int = 1
    ...

# But this module, its submodule, and its members are.
public_module<public> := module:
    public_submodule<public> := module:
        PublicInt<public>:int = 1
        ...
```

Note both the module and its members need to be public to access them in a different scope.


These pages describe the Verse programming language and its syntax. Spend time getting familiar with the language, then use these pages as reference.
If this is your first time using Verse, or if you're learning programming for the first time, make sure to check out Programming with Verse to help you get started. You'll also find a useful onboarding guide.
What Is Verse?
Verse is a programming language developed by Epic Games that you can use to create your own gameplay in Unreal Editor for Fortnite, including customizing your devices for Fortnite Creative.
Verse’s primary design goals:
Simple enough to learn as a first-time programmer.
General enough for writing any kind of code and data.
Productive in the context of building, iterating, and shipping a project in a team setting, and integrating code and content.
Statically verified to catch as many categories of runtime problems as possible at compile time.
Performant for writing real-time, open-world, multiplayer games.
Complete so that every feature of the language supports programmer abstraction over that feature.
Timeless — built for the needs of today, and for foreseeable future needs, without being rooted in the past artifacts of other languages.
The design goals above informed key features of the Verse programming language:
Strongly typed to minimize opportunities for uncaught errors in development or deployment and support static checking.
Multi-paradigm to use the best of functional programming, object-oriented programming, and imperative programming, such as being as deterministic as possible. One example of this is that data is immutable by default, and given the same code and data, results will always be exactly the same.
There is no distinction between statements and expressions. In Verse, everything is an expression, which means that everything has a result.
Failure is control flow. Instead of using true / false values to change the flow of your program (such as with decision points), Verse uses failable expressions, which produce a value if they succeed or don’t if they fail. Failable expressions can only be executed in failure contexts, such as if expressions.
The ability to do speculative execution within failure contexts, meaning you can try out actions without committing them. When an expression succeeds, the effects of the expression are committed, but if the expression fails, the effects of the expression are rolled back as though the expression never happened. This way, you can execute a series of actions that accumulate changes, but those actions will be undone if a failure occurs in the failure context.

Concurrency at the language level so you don’t need to rely on system-level threads across multiple processors to perform actions simultaneously. You can author time flow the same as you do control flow by using built-in concurrency expressions in the language.
Epic Games is continuing to develop the Verse programming language and add more features. For Verse code that you write today, you can expect Verse to provide backward compatibility and continue to work with future updates to the language.

Explore the Language

* Verse Language Version 1 Updates and Deprecations:  Learn about the new updates and deprecations in Version 1 of the Verse Language.

* Expressions: Everything in Verse is an expression and has a result. This page describes all the kinds of expressions in Verse.

* Comments: A code comment explains something about the code. Comments are ignored when the program runs.

* Constants and Variables: Variables and constants can store information, or values, that your program uses.

* Common Types: Common types support the fundamental operations that most programs use.

* Operators: Operators are special functions defined in the Verse programming language to perform actions such as the math operations for addition and multiplication.

* Grouping: Group your Verse expressions to specify order of evaluation and improve readability.

* Code Blocks: A code block is a group of expressions, and introduces a new scope for variables and constants.

* Functions: A function is reusable code that performs an action and produces different outputs based on the input you provide.

* Failure: Failure is a way to control the sequence in which a program performs actions, called the control flow.

* Specifiers and Attributes: Learn about specifiers and attributes, and how to apply additional semantics and behavior to your Verse code.

* Control Flow: Control flow is the order in which a computer executes instructions. Verse has a number of ways to change the control flow of your program.

* Time Flow and Concurrency: You can author time flow the way you author control flow, by executing expressions simultaneously using built-in concurrency expressions in Verse.

* Container Types: Store multiple values together by using a container type.

* Composite Types: Create your own unique type from a composite type.

* Working with Verse Types: Learn how to do more with types in Verse.

* Modules and Paths:  A Verse module is an atomic unit of code that can be redistributed and depended upon, and that you can import into your Verse file to use code definitions from other Verse files.
