# Data types

Programmers can define new data types in several ways: by creating aliases to existing types, declaring enumerations, defining structures, or introducing classes and interfaces.

## Aliases

A type alias is simply another name for an existing type. Aliases can be declared with minimal ceremony, either at the module level or within a function. The compiler treats variables of an alias type exactly as if they had the original type.

```
i := int            # Alias for int

Fun() :=
   A :i= 2          # A is an int
   j := int         # Local alias for int   
   B :j= 4          # B is an int
   C :=  B+A        # C is an int
   k := j           # Alias to an alias; still int
   var l := int     # Compiler error! 
```

Aliases must be immutable to preserve type consistency. Otherwise, one could change the type of an existing variable, which would break the compiler’s guarantees.

A common example is the built-in alias string, which stands for []char.

!!! question
    Are parameterized aliases supported?

Enumerations

An enumeration defines a fixed set of values. Enumerations are introduced with `enum`, followed by a set of disjoint labels.

For example, here is an enumeration describing device states, along with a function that performs case analysis:
```
@doc("The state of progress of a device")
progress<public> := enum<open>:
    @doc("This device is being prepared")
    Preparing
    @doc("This device is ready for use")
    Ready

Fun(Status: progress):string =
    case(Status):
        progress.Preparing => "Wait"
        progress.Ready => "Play"
        _ => "Error"
```
Here, the enumeration is marked `<open>`, allowing future extensions.
Unlike in some languages, enumerations cannot be implicitly converted to or from integers.

!!! question
    Is the declaration order of enumeration values significant?

### Evolution

Enumerations may be declared as `<open>` or `<closed>` (the default).
  * _Open enumerations_ can be extended in later versions by adding new cases (but not removing existing ones). Open enumeration can be closed subsequent versions.
  * _Closed enumerations_ are fixed once defined.
Case analysis over open enumerations requires a default branch (`_`) to account for possible future values.
 