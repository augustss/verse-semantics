# Verse Code Evolution and Compatibility

Verse takes a unique approach to code evolution, designed with the ambitious goal of creating software that could remain functional and valuable for decades or even centuries. This vision stems from Verse's role as the programming language for a persistent, global metaverse where code must coexist, evolve, and maintain compatibility across vast timescales.

At its core, Verse embraces three fundamental principles that shape how code evolves: future-proof design that avoids being rooted in past artifacts of other languages, a metaverse-first approach where code persistence and compatibility are critical, and strong static verification that catches runtime problems at compile time. These principles create a foundation for a language that can grow and adapt while maintaining the stability required for a global, persistent codebase.

## The Nature of Code Publication

When developers publish code to the Verse metaverse, they enter into a social contract with all future users of that code. This contract is more than just a convention—it's enforced by the language itself. Consider what happens when you publish a simple value:

```verse
Thing<public>:int = 666
```

This seemingly straightforward declaration carries profound implications. By marking `Thing` as public, you're making a commitment that extends indefinitely into the future. Users can depend on `Thing` always existing and always being an integer. While you retain the freedom to change its actual value, the existence and type of `Thing` become permanent fixtures in the metaverse's landscape.

This permanence extends beyond simple values to encompass the entire structure of published code. Persistable structs, once published to an island, become immutable schemas that cannot be altered. Closed enums remain closed forever, unable to accept new values after publication. When a class or interface is marked with the `<castable>` attribute, that decision becomes irreversible, as changing it could introduce unsafe casting behaviors that break existing code.

The publication model distinguishes between two contexts: the live metaverse and islands. In the envisioned live metaverse, publishing an update that attempts to change an immutable variable's value has no effect—the variable already exists with its original value. However, in the current island-based implementation, new instances of an island will adopt the updated value, providing a practical migration path while maintaining conceptual consistency.

## The Architecture of Backward Compatibility

Backward compatibility in Verse goes beyond simple syntactic preservation—it encompasses semantic guarantees about how code behaves. The language enforces these guarantees through multiple mechanisms that work together to create a robust compatibility framework.

Function effects exemplify this approach. When a function is published with specific effects like `<reads>`, indicating it may read mutable heap data, this becomes part of its contract. Future versions of the function can have fewer effects—evolving from `<reads>` to `<computes>`—but never more. This restriction ensures that code depending on the function's effect profile continues to work correctly, as the function only becomes more pure, never less.

Type evolution follows similar principles. Types can become more specific over time, such as changing from `int` to `nat`, as this represents a refinement rather than a fundamental change. Structures must maintain all existing fields, though new fields can be added. Classes marked with `<final_super>` commit to their inheritance hierarchy permanently, ensuring that code relying on specific inheritance relationships remains valid.

The enforcement of these rules happens at publication time, not just at compile time. Verse actively prevents developers from publishing updates that would violate compatibility guarantees, turning what might be runtime failures in other systems into publication-time errors that must be resolved before code can be deployed.

## Managing Breaking Changes

Despite the strong emphasis on compatibility, Verse recognizes that some breaking changes are occasionally necessary. The language provides two mechanisms for managing such changes: a deprecation system for gradual migration and special privileges for essential breaking changes.

The deprecation system operates as a multi-phase process that gives developers ample time to adapt. When code patterns become deprecated, they first generate warnings rather than errors. These warnings appear when saving code, alerting developers to practices that won't be supported in future versions. The code continues to compile and run, allowing projects to function while migration plans are developed. Only when developers explicitly upgrade to a new language version do deprecations become errors, and even then, the option to remain on older versions provides an escape hatch.

Version 1 introduced several significant deprecations that illustrate this process. The prohibition of failure in set expressions, which previously allowed with warnings, now requires explicit handling of failable expressions. Mixed separator syntax, which created implicit blocks and confusing scoping rules, must now use consistent separation. The introduction of local qualifiers provides a new tool for disambiguating identifiers while deprecating the use of 'local' as a regular identifier name.

For truly exceptional circumstances, Epic Games and potentially other future authorities retain "superpowers" to make breaking changes outside the normal compatibility framework. These powers include the ability to delete published entities, change types in non-backward-compatible ways, and rewrite modules for legal or safety reasons. These capabilities acknowledge that being good stewards of the metaverse namespace sometimes requires violating the usual compatibility rules, though such actions should remain rare and justified by compelling reasons.

## Design Philosophy for Longevity

Creating code that remains viable across extended timescales requires a different approach to software design. Developers must think beyond immediate functionality to consider how their code will evolve and interact with future systems. This forward-thinking approach influences every aspect of development, from initial design to ongoing maintenance.

Schema planning becomes critical when working with persistable types. Since these cannot be changed after publication, developers must carefully consider not just current requirements but potential future needs. This might mean including optional fields that aren't immediately necessary or choosing open enums over closed ones when future expansion seems likely. The cost of getting these decisions wrong—being locked into inflexible schemas—encourages thorough upfront design.

Effect specification offers an interesting trade-off. While Verse allows and sometimes encourages over-specification of effects, marking a function as having effects it doesn't currently use, this provides flexibility for future implementation changes. A function marked as `<reads>` can later be optimized to `<computes>` without breaking compatibility, but the reverse isn't true. This asymmetry encourages conservative effect declarations that leave room for future modifications.

The choice between open and closed constructs represents another long-term decision. Open enums allow new values to be added after publication, providing extensibility at the cost of preventing exhaustive pattern matching. Closed enums offer the opposite trade-off. Understanding when flexibility or completeness is more valuable requires thinking about how the code will be used not just today, but years into the future.
