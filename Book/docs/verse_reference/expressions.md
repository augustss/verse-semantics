# Expressions

Everything in Verse is an expression and has a result. This page describes all the kinds of expressions in Verse.
An expression is the smallest unit of code that has a result when evaluated. In Verse everything is an expression, which means everything evaluates to a value.

An example is an if ... else expression, which in Verse evaluates to a value that depends on the content of the expression blocks. The following code evaluates to a string value, containing either “Big!” or “Small!” depending on whether MyNumber was greater than 5:

```verse
if (MyNumber > 5):
    “Big!”
else
    “Small!”
```

This means you can use an if ... else directly as input to functions instead of storing a result and using that result as input.

While this example is simple, there are contexts where this becomes more powerful. For example, loops evaluate to arrays of values, so you can quickly create new arrays based on existing ones.
In the following code, MyArray will contain all the values from NumberArray that are less than 5.

```verse
MyArray : []int = for(Number := NumberArray, Number < 5):
    Number
```

### Failable Expressions

A failable expression is an expression that may succeed and produce a value, or fail and return no value. Failable expressions can only be executed in a failure context because that context will define what happens in the event that the expression fails.
Examples of failable expressions include indexing into an array because an invalid index will fail, and using operators such as comparing two values. For more on failable expressions in Verse, see Failure.

### List of Expressions in Verse

The following table describes the different kinds of expressions in Verse. Follow the links to learn more about each expression.

Expression Description Is the Expression Failable?

Literals

A literal is a fixed value in your code, such as a number or a character. In Verse, there are literals for the following types:

option

enum

Function Calls

A function call is an expression, and can have two forms: FunctionName() and FunctionName[]. The result type of the function call expression is defined in the function signature. Refer to Function for more details.
Only when the function call has the form FunctionName[], and the function definition has the <decides> specifier.

Comparison

A comparison expression compares two things using one of the comparison operators:
<
>
<=
>=
<>
=

Refer to Operators for more details.

Assignment

An assignment expression stores a value at a mutable location, such as when initializing a constant or changing the value of a variable. Refer to Variables and Constants for more details.

Math

A math expression performs computations using the operators: +  -  * /

All of these operators also have assignment variants that can be used with pointers. Refer to Operators for more details.

Only for integer division.

Decision

A decision expression uses the operators not, and, and or to give you control over the success and failure decision flow. Refer to Operators for more details.

Query

A query expression uses the operator ? and checks whether a logic or option value is true. Otherwise, the expression fails. Refer to Operators for more details.

Class and Struct Instantiation

Creating an instance of a class or struct is an expression. Refer to Class and Struct.

Control Flow

Control flow is the order in which a computer executes instructions. You can use expressions such as if and loop to change that flow. Some control flow expressions, such as loop, only return void and so may not be useful everywhere you can use an expression. The following are control flow expressions in Verse:

if
case
for
loop
sync
race
rush
branch
spawn

Array

An array is a container where you can store elements of the same type. The elements of an array are in the order you insert them into the array, and you can access the elements by their position in the array, called their index. For more info, see Array.
Only when indexing into an array.

Tuple

A tuple is a container where you can store elements of one or more types. The elements of a tuple are in the order you insert them into the tuple and you can access the elements by their position in the tuple, called their index.. For more info, see Tuple .

#### Map

A map is a container where you can store values associated with another value, called key-value pairs. Key-value pairs can be any combination of types as long as the key type is comparable. The elements of a map are in the order you insert the key-value pairs into the map, and you can access the elements by their unique keys. For more info, see Map.

#### Option

An option is a container that can have one or no value of a type. For more info, see Option.

#### Range

Range expressions contain all the numbers between and including the two specified values with .. between, for example 1..5. Range expressions can only be used in some places, such as in for expressions. See Range for more details.
expressions
