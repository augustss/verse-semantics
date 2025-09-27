# Verse Language Documentation

This documentation provides an in-depth look at the Verse programming language, its philosophy, and core concepts.

## Philosophy

Verse is a *functional logic* programming language with three core principles:

- **It's just code** - Complex concepts are expressed as primitive Verse constructs
- **Just one language** - Same constructs for compile-time and run-time
- **Metaverse first** - Designed for a global simulation environment

## Documentation Sections

### Getting Started

- [Language Overview](00_overview.md) - Introduction to Verse philosophy and features

### Core Language Features

- [Built-in Data Types](01_builtins.md) - Integers, floats, rationals, logic, strings, and container types
- [Composite Types](02_composites.md) - Classes, interfaces, structs, and enums
- [Functions](03_functions.md) - Open-world vs closed-world functions, parameters, and return values
- [Operators](04_operators.md) - Arithmetic, comparison, logical, and assignment operators with precedence
- [Expressions](05_expressions.md) - Everything is an expression paradigm
- [Control Flow and Structure](06_control.md) - If/else, loops, code blocks, and comments
- [Modules and Paths](07_modules.md) - Code organization and the global namespace

### Advanced Concepts

- [Failure System](08_failure.md) - First-class failure, failable expressions, and speculative execution
- [Effects](09_effects.md) - Effect families, specifiers, and capability declarations
- [Concurrency](10_concurrency.md) - Structured concurrency with sync, race, rush, branch, and spawn
- [Mutability](11_mutability.md) - Mutable variables, references, and state management

### Type System and Evolution

- [Types and Type System](12_types.md) - Types as functions and type checking
- [Persistable Types](13_persistable.md) - Types that can be saved and loaded
- [Access Specifiers](14_access.md) - Public, private, and protected visibility
- [Code Evolution](15_evolution.md) - Versioning and backward compatibility

### Language Reference

- [Grammar Features](16_grammar.md) - Language grammar and syntax
