# Working with Verse Types

There are several ways you can work with types in Verse, shown in the pages below.

* Type Casting and Conversion: Use type casting to convert from one data type to another.
* Type Aliasing: You can use a type alias to give a type a unique name without creating a new type.
* Parametric Types: Use parametric types as explicit type arguments to classes or functions, or as implicit type arguments to functions.
* Type Macro: The type macro allows you to get the type of an expression. It can be used anywhere a type can be used.

### Type Casting and Conversion

Use type casting to convert from one data type to another.

When working with data, it is often necessary to convert variables from one data type to another. For example, displaying the result of a calculation requires converting from a float to a string.

All type conversion within Verse is explicit, which means that you must use a function like ToString() or use an operator like multiply (*) to convert an object to a different data type. Explicit conversion of one type to another is also called type casting.

Converting Float to Int

Converting from a float to an int requires a function that explicitly specifies how it will convert from a floating point number to an integer. The following functions all handle the conversion, but they all work differently. It's up to you to decide which one works best in a given situation.

Round[]
Floor[]
Ceil[]
Int[]

Conversion functions such as Round[] require a finite argument and will fail if the argument passed to them is NaN or Inf.

In this example, different functions convert four float literal values into int values using the or operator to create a failure context. Next, set assigns the values to variables of type int.

```verse