# Paths and Modules


So far, we've considered snippets of Verse code without talking about what top-level Verse code looks like and what it means. Let's step back and talk about that, since it helps to make Verse's type and visibility rules more clear.

Each Verse file provides the body of some module. Modules can be referred to by ***path***, and that path represents a **single global namespace** for the Metaverse. For example, say that I have a file called `Foo.verse` and it is *mounted* at the Verse path `/pizlonator@fn.com/NightDeath/`, and the contents of `Foo.verse` is just:

```verse
Thing<public>:int = 666

```

This means that I have published a global value to the metaverse, and since it is public, I'm intending for the world to see it. In particular, this says:

- Everyone (i.e. `public`) gets to see that `Thing` is a value of type `int`.
- Its actual value right now is `666`.

Later, I could edit `Foo.verse` to be:

```verse
Thing<public>:int = 10

```

And then *publish* that change to the metaverse. This is a valid and its behavior depends on whether the code that uses `Thing` is running as part of the live metaverse (i.e. our long-term vision) or as part of an island (our current reality). In islands, publishing an update that changes an immutable variable's value means that new instances of the island will pick up the updated value. But in the live metaverse, publishing an update that changes `Thing`'s value does nothing since `Thing` already exists and was already set to `666`.

The Metaverse would reject my change to `Foo.verse` if I either removed `Thing` entirely, or gave it a different (but not more specific) type. So, I could not publish:

```verse
Thing<public>:string = "hello"

```

but I could publish:

```verse
Thing<public>:nat = 20

```

Since `nat` (the type representing integers that are natural numbers) is a subtype of `int`.

## Naming Convention

Before proceeding further with more Verse constructs, let's observe how we've used the Verse naming conventions so far. These aren't enforced by the compiler, but strongly encouraged, and generally obeyed by all Epic-written Verse code:

- Type names, like `int`, `string`, and `nat` are all written in `snake_case`.

- Value names, like `X` or `Thing`, all use `CamelCase`.

## Metaverse Backwards Compatibility Guarantees and the Necessary Superpowers

It's a big deal that the Metaverse will let you publish `Thing<public>:int` in a way that comes with some guarantees:

- Users are guaranteed that `Thing` will never stop existing.

- Users are guaranteed that `Thing` will never stop being an `int`.

- The publisher is free to change the actual value of `Thing`.

Verse takes these kinds of guarantees to the limit, for example preventing the publishing of updates to structs, classes, modules, functions, and many other types of code without first checking that those updates obey compatibility guarantees to users of that code. The fact that you can use Verse at checking-time means that you can enforce complex contracts with your callers and callees.

But what if we ever need an escape hatch? Simple example: someone publishes a code module, and we (or some other sensible enforcement body in the Metaverse) realize that the code must be unpublished for legal reasons, or maybe some other Good Reason (TM) that trumps our desire for a backwards-compatible golden path. That will happen, and we'll allow it. Epic, and in the future other parties, will have superpowers:

- to delete `Thing`.

- to change `Thing`'s type in a non-backward compatible way.

- to rewrite any module in the Metaverse in any way that we see fit, so long as there are Good Reasons (TM).

We need to stay on the golden path of backwards compatibility and we need to be a good steward of the metaverse global namespace. Sometimes being a good steward will mean violating Verse's backwards compatibility rules, and eventually Epic won't be the only entity with that power in the Metaverse.