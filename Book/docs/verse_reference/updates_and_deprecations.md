# Verse Language Version 1 Updates and Deprecations

Starting November 1, 2024 in the 32.00 release, we are stabilizing the Verse language to Version 1. We recommend you that upgrade your project both for stability and so that it’ll be easier to upgrade your code incrementally when future Verse language updates arrive. This upgrade is optional; your current version of the project will always continue to work at an older Verse version. That being said, there may be a point in the future where you will need to upgrade your project to a newer Verse version if you want to upload a newer iteration of it.
Since its first public release, we have continued to and evolve the Verse language, with those changes transparently rolling out to users without requiring an upgrade to a new language version. We anticipate that this will continue, with most language changes being backwards compatible with previous versions of the language and rolled out to users with new releases of UEFN.

However, some language changes are not backwards compatible, and may require changes to your code in order for the code to compile. Such backwards incompatible changes will only be triggered if the user upgrades their project to target the new language version.

Warnings will appear any time you save code that is not backwards compatible and indicate your code behavior is deprecated in newer versions of the language. For example, continuing to use coding practices that aren’t compatible with the new Verse Language version result in warnings about your code.
If your project compiles in V0 without any deprecation warnings, you can upgrade to V1 without any changes to the behavior of your code. When you open a V0 project in UEFN 32.00, and it compiles without any errors or warnings, the editor will ask if you'd like to upgrade.
When you open a V0 project in UEFN 32.00, and it compiles without any errors or warnings, the editor will ask if you'd like to upgrade.
If you'd like to upgrade or downgrade later for any reason, you can do so in your project settings.
Upgrade your Verse Version from Project Settings.
You can also open project files from the Fortnite Projects folder and change the Verse Version of the .uplugin files.
Upgrade your Verse Version from the .uplugin file in your Fortnite Projects.
Most of the deprecations clear the way for future language improvements, and don’t yet provide any benefit to users. Two exceptions are local qualifiers and struct comparison.

### Changes in V1

### Failure in Set

The expression that a set executes is no longer allowed to fail. Previously, the code:

```verse
F():void=
    var X:int = 0
    set X = FailableExpression[]
```

Was allowed, and X would have been set to whatever FailableExpression evaluated to with a warning. In V1, this is disallowed.
In order to fix your code, you will need to make sure the expression cannot fail. One possible way to do this is with the following modification:

```verse
var X:int = 0
Value:= FailableExpression[] or DefaultValue
set X = Value
```

Failure in Map Literal Keys
Previously, you could have literal keys in a map fail:

```verse
map{ExpressionThatCannotFail=>Value}
```

A simple example being map{ (0 = 0) =>0 }, where 0 = 0 fails. This is no longer allowed.
Mixing Semicolon/Comma/Newline Separators for Blocks
Previously, mixing semicolons/commas/newlines to separate sub-expressions was allowed, and resulted in code as displayed below:

```verse
A,
B
for (A := 0..2):
    # more code here
```

Internally being desugared to code as displayed below:

```verse
block:
    A
    B
for (A := 0..2):
    # more code here
```

This meant that both definitions of A in the code block did not conflict with one another, as there was an implicit block created that had its own separate scope.
However, this is incorrect behaviour and is fixed in the latest Verse language version. Now, the same code treats each sub-expression separately, resulting in the following:

```verse
A
B
for (A := 0..2):
    # more code here
```

This means that the first A and the second definition of A := 0..2 now shadow one another and the meaning is ambiguous.
In order to fix this, both creators (and anyone who relies on this behaviour) must stop mixing semicolon/commas/newlines to separate sub-expressions, across all their Verse code.
Example:

```verse
PropPosition := Prop.GetTransform().Translation,
```

```verse
if(Round[PropPosition.Z] = Round[ROOT_POSITION.Z]) { break }
Sleep(0.0)
```

Should be modified to:

```verse
PropPosition := Prop.GetTransform().Translation # note the trailing comma here has been removed
```

```verse
if(Round[PropPosition.Z] = Round[ROOT_POSITION.Z]) { break }
Sleep(0.0)
```

Previously, a warning was produced whenever any mixed separators were detected starting back in 28.20. This is now disallowed in the latest Verse language version.

### Unique Specifier Changes

Classes with the `<unique>` specifier now require the `<allocates>` construction effect. For example, `class<unique><computes>` is no longer allowed.

### Function-Local Qualifiers

The (local:) qualifier can be applied to identifiers within functions to disambiguate them from other identifiers.
For example:

```verse
ExternallyDefinedModuleB<public> := module:
    ShadowX<public>:int = 10 # added only after `ModuleC` was published
ModuleC := module:
    using{ExternallyDefinedModuleB}
    FooNoLocal():float=
        ShadowX:float = 0.0
        ShadowX
```

The code above would produce a shadowing error, since ShadowX is ambiguous (is it from ExternallyDefinedModuleB.ShadowX or the ShadowX within FooNoLocal?)
To solve this, you can use the (local:) qualifier to be clear about which ShadowX is being referred to, like in the example below:

```verse
ExternallyDefinedModuleA<public> := module:
    ShadowX<public>:int = 10 # added only after `ModuleB` was published
ModuleB := module:
    using{ExternallyDefinedModuleA}
    FooLocal():float=
        (local:)ShadowX:float = 0.0 #The `local` qualifier can be used here to disambiguate
        (local:)ShadowX
```

Previously, we produced a warning when we detected you were using the word local as a data definition identifier since this is now a reserved keyword. In the latest language version, the use of local results in an error, and if you want to use the local identifier as a normal data definition identifier, its usage must be explicitly qualified.
Here is another example of a local qualifier:

```verse
MyModule := module:
    X:int = 1
    Foo((local:)X:int):int = (MyModule:)X + (local:)X
```

In this example, if we didn't specify the (local:) qualifier, the X in Foo(X:int) would shadow the X:int = 1 definition directly above, since both X identifiers are in the same scope. Thus, using the (local:) qualifier allows us to disambiguate the two, by making the X in Foo's argument parameter clause specific to only within the scope of Foo. The same applies to the X identifiers in the body of Foo as well.

### Public Fields in Structs

All fields in structs must now be public. By default, this is also now the case as of V1. (using <public> is no longer necessary).
V1 also adds the ability to compare two structs. If all fields of a struct are comparable, then you can use = to compare two instances of the struct field-by-field. For example:

```verse
vector3i := struct{X:int, Y:int, Z:int}
```

```verse
vector3i{X:=0, Y:=0, Z:=0} = vector3i{X:=0, Y:=0, Z:=0} # succeeds
vector3i{X:=0, Y:=0, Z:=0} = vector3i{X:=0, Y:=0, Z:=1} # fails because Z is different between the two instances
```
