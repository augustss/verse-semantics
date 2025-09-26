# Comparable

A subtype of any, comparable adds the requirement that any value of this type can be compared to any other value of this type.
The comparable type is used to compare values of this type to other values of the same type.
The = and <> operators make use of this type to define their signatures.

Verse
operator'='(:t, :comparable where t:subtype(comparable)):t
operator'<>'(:t, :comparable where t:subtype(comparable)):t
Each of these functions is defined as taking a first argument that is an arbitrary subtype of comparable and a second argument that is also an arbitrary subtype, and that returns the same type as the first argument.
The comparable type has many subtypes that you can use. These subtypes can be compared both with other values of the same type type, and other subtypes of comparable. These subtypes include:
char
char32
array if all contained types are subtypes of comparable
option if all contained types are subtypes of comparable
tuple if all contained types are subtypes of comparable
map if all contained types are subtypes of comparable
Classes can also be made to be subtypes of comparable with the unique specifier. Instances of classes with this specifier are only equal to themselves, even if the contained members are equal. For example:

```verse

int_ref := class<unique>:
    Contents:int
Main()<decides> : void =
    X := int_ref{Contents := 0}
    Y := int_ref{Contents := 0}
    X = X # Succeeds
    X = Y # Fails
    X <> Y # Succeeds

```

Currently float, option, and classes (regardless of the presence of unique) cannot be used as keys of maps, meaning map keys are required to be a subtype of comparable that is not given a name (and therefore, map key types cannot be parametric types, as the required subtyping cannot be described in Verse code).
Note that because a value of a subtype of comparable can be compared to any other value of another subtype of comparable, some unexpected results can occur. For example:
Verse
0 = 0.0
This example will fail because 0 is of type int, while 0.0 is of type float, and int and float do not share a subtyping relationship.
comparable
