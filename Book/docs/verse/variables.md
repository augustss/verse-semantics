# Defining Variables


Let's start with a simple variable definition in Verse:

```verse
X:int = 42

```

This defines `X` to be an immutable variable such that:

- Its value *right now*, on this execution, is `42`.

- All uses of `X` must work for any `int`. In other words, users of `X` cannot strongly assume that `X` will be `42` (even though it is right now), but they can assume that it'll be some `int`.

What does it mean to be an `int`? Verse does not have types in the traditional sense. Instead, Verse has functions, an `int` is an identity function that only accepts integers. The `int` function fails for any value that is not an integer. You can use that as a type check, for example:

```verse
if (int[X]):
    SomeFunction()

```

This will run `SomeFunction()` if `X` is indeed an `int`. In our running example, `X` is statically known to be an `int` so we statically know that `int[X]` always succeeds. But, we could have written this snippet for an `X` declared to have some broader type, like `comparable`. Note that we will refer to `int` and `comparable` as types, but that doesn't change the fact that under the hood, they are just identity functions that succeed for values of their type.

Let's consider other examples of declaring `X`.

```verse
X:int

```

This declares `X` to be an immutable variable such that:

- Its value is not known right now, but can be determined by applying constraints to it. Verse will require us to adequately constrain `X`, otherwise it will be *stuck*, which leads to a runtime error. The Verse checker will reject any program that could get stuck. This means that for `X:int` to be a valid statement, we will need to constrain it, but that could be in another statement.

- All uses of `X` must work for any `int` and whatever its value is eventually constrained to be, it has to be an `int`.

Having declared `X` this way, we can constrain it to the value `42` by saying either:

```verse
X = 42

```

or:

```verse
42 = X

```

In other words, this snippet of code:

```verse
X:int = 42

```

and this snippet of code:

```verse
X:int; X = 42

```

and even this other snippet of code:

```verse
42 = X; X:int

```

all have the same effect. They declare `X` so that uses of `X` in the program can rely on it being some `int`, but its value will be `42`. This works because of Verse's functional logic features, or what we call *leniency*. Verse assigns values to immutable variables by equation solving. In most cases, it doesn't matter in what order immutable variables are "assigned" values; what matters is that enough equations are specified to give each variable a value unambiguously.

We could also have alternatively defined `X` this way:

```verse
X := 42

```

This defines `X` to be an immutable variable as before, but this time:

- Its value now *and forever* is `42`.

- All uses of `X` can strongly assume that it is exactly `42`.

For most cases of local variables, this is not different than saying `X:int = 42`, and the "forever" aspect of `X := 42` is not interesting. But as soon as `X` is an externally visible variable, saying `X := 42` means committing to a strong future-compatibility API guarantee that `X` is always `42` and not anything else. On the other hand, an externally visible `X:int = 42` merely promises that `X` will always be some integer.

Let's just consider one more way of declaring `X`. We could say:

```verse
X := :int

```

This means: `X` is definitely, now and forever, some value that the `int` function succeeds for, but we don't know its value. This is exactly the same thing as saying:

```verse
X:int

```

In fact, `X:int` desugars to `X := :int` under the hood. So, we could say something like:

```verse
X = 42; X := :int

```

And that would also have the same effect as `X:int = 42`.