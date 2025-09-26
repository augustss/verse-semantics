# Functions


Let's jump right into functions with a simple example.

```verse
F1(X:int):int = X + 1

```

This defines a function that:

- Takes a single argument of type `int`. This function promises to forever work for *at least* `int` arguments, so it might be overloaded to take other types as well (either already today or at anytime in the future).

- Returns a value of type `int`. This function promises to forever return nothing more than `int`s. So, in the future, it might be refined to return a more specific type.

- Returns `X + 1` today. There is no promise that it must forever return `X + 1`; it could in the future do other stuff, so longer as it takes at least `int` and returns at most `int`.

Consider a slightly different function.

```verse
F2(X:int) := X + 1

```

The only difference between `F1` and `F2` is that `F2` uses `:=`. Just as it did for variables, this distinction for functions means that `F2` promises that its right-hand-side stays exactly the same forever. So, this defines a function that:

- Takes a single argument of type `int`, just like `F1`.

- Returns `X + 1` forever. In particular, if this function was public, the Verse compiler would require you to keep the right-hand-side of `F2` identical forever (modulo whitespace).

Functions defined using `:=`, which make the *forever* guarantee about their bodies, are called *closed-world functions*. Functions defined like `F1`, which put a type between the `:` and the `=`, like `:int =`, are called *open-world functions*. More generally, variables defined using `:=` are *closed-world definitions*, while those that give a type are *open-world*.

It's better to use open-world functions and definitions for anything exposed publicly. But closed-world functions have the benefit of being transparent to the checker: the way the checker checks a call to a closed-world function is by executing the call at compile-time. But when checking a call to an open-world function, the checker abstracts and assumes the worst based on the function's signature.

Here's an example of this in action:

```verse
f3(t:type) := t
X:f3(int) = 42

```

This is exactly like defining `X` as `X:int = 42`, since instead of saying `int`, we said `f3(int)`, and `f3` just returns the type you give it. Note that this would not have worked if we had said:

```verse
f3(t:type):type = t
X:f3(int) = 42    # ERROR!

```

Because the checker would have to assume that `f3` could return any type, and there's no way to guarantee that all types would accept the value `42`.