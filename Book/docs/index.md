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

### Foundation

- [Expressions](01_expressions.md) - Everything is an expression paradigm
- [Built-in Data Types](02_builtins.md) - Integers, floats, rationals, logic, strings, and container types
- [Operators](03_operators.md) - Arithmetic, comparison, logical, and assignment operators with precedence

### Program Structure

- [Functions](04_functions.md) - Open-world vs closed-world functions, parameters, and return values
- [Control Flow](05_control.md) - If/else, loops, code blocks, and comments
- [Failure System](06_failure.md) - First-class failure, failable expressions, and speculative execution

### Type System

- [Types and Type System](07_types.md) - Types as functions and type checking
- [Composite Types](08_composites.md) - Classes, interfaces, structs, and enums
- [Mutability](09_mutability.md) - Mutable variables, references, and state management

### Advanced Features

- [Effects](10_effects.md) - Effect families, specifiers, and capability declarations
- [Concurrency](11_concurrency.md) - Structured concurrency with sync, race, rush, branch, and spawn
- [Live Variables](12_live_variables.md) - Reactive values that automatically update
- [Modules and Paths](13_modules.md) - Code organization and the global namespace

### Specialized Topics

- [Access Specifiers](14_access.md) - Public, private, and protected visibility
- [Persistable Types](15_persistable.md) - Types that can be saved and loaded
- [Code Evolution](16_evolution.md) - Versioning and backward compatibility

### Language Reference

- [Grammar Features](16_grammar.md) - Language grammar and syntax
- [Concept Index](concept_index.md) - Comprehensive index of all language concepts with links
