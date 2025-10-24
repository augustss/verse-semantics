# 📚 Concept Index

This index provides quick access to key concepts, language features, and important terms in the Verse documentation. Each entry links to the relevant sections where the concept is discussed.

## Type System

### Primitive Types
- **any** - universal supertype: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **void** - empty type: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **logic** - boolean values: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **int** - integers: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **nat** - natural numbers: [Type System](07_types.md)
- **float** - floating-point: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **rational** - exact fractions: [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Type System](07_types.md)
- **char** - UTF-8 character: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **char32** - UTF-32 character: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **string** - text values: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Type System](07_types.md)

### Composite Types
- **array** - ordered collections: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Composite Types](08_composites.md)
- **map** - key-value pairs: [Built-in Types](02_builtins.md), [Composite Types](08_composites.md)
- **tuple** - fixed-size collections: [Built-in Types](02_builtins.md), [Expressions](01_expressions.md), [Type System](07_types.md)
- **option** - nullable values: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Composite Types](08_composites.md)
- **class** - reference types: [Overview](00_overview.md), [Composite Types](08_composites.md), [Type System](07_types.md)
- **struct** - value types: [Composite Types](08_composites.md), [Type System](07_types.md)
- **interface** - contracts: [Composite Types](08_composites.md)
- **enum** - named values: [Overview](00_overview.md), [Composite Types](08_composites.md)

### Type Features
- **subtype** relationships: [Type System](07_types.md)
- **comparable** - equality testing: [Built-in Types](02_builtins.md), [Type System](07_types.md)
- **parametric types** - generics: [Type System](07_types.md)
- **type{}** - type expressions: [Expressions](01_expressions.md), [Type System](07_types.md)
- **where clauses** - type constraints: [Overview](00_overview.md), [Functions](04_functions.md), [Type System](07_types.md)
- **covariance** - type compatibility: [Type System](07_types.md)

## Effects

### Effect Specifiers
- **\<computes\>** - pure computation: [Overview](00_overview.md), [Effects](10_effects.md), [Mutability](09_mutability.md)
- **\<reads\>** - observe state: [Overview](00_overview.md), [Effects](10_effects.md), [Mutability](09_mutability.md)
- **\<writes\>** - modify state: [Effects](10_effects.md), [Mutability](09_mutability.md)
- **\<allocates\>** - create unique values: [Effects](10_effects.md)
- **\<transacts\>** - full heap access: [Overview](00_overview.md), [Composite Types](08_composites.md), [Effects](10_effects.md)
- **\<decides\>** - can fail: [Overview](00_overview.md), [Functions](04_functions.md), [Failure](06_failure.md), [Effects](10_effects.md)
- **\<suspends\>** - async execution: [Overview](00_overview.md), [Effects](10_effects.md), [Concurrency](11_concurrency.md)
- **\<converges\>** - guaranteed termination: [Effects](10_effects.md)
- **\<diverges\>** - may not terminate: [Effects](10_effects.md)
- **\<predicts\>** - client execution: [Effects](10_effects.md)
- **\<dictates\>** - server-only: [Effects](10_effects.md)

## Control Flow

### Basic Control
- **if/then/else** - conditional execution: [Overview](00_overview.md), [Expressions](01_expressions.md), [Control Flow](05_control.md), [Failure](06_failure.md)
- **case** - pattern matching: [Overview](00_overview.md), [Composite Types](08_composites.md), [Control Flow](05_control.md)
- **for** - iteration: [Overview](00_overview.md), [Control Flow](05_control.md), [Failure](06_failure.md)
- **loop** - infinite loops: [Control Flow](05_control.md), [Failure](06_failure.md), [Concurrency](11_concurrency.md)
- **block** - statement sequences: [Control Flow](05_control.md), [Failure](06_failure.md), [Concurrency](11_concurrency.md)
- **break** - exit loops: [Control Flow](05_control.md)
- **continue** - skip iteration: [Control Flow](05_control.md)
- **defer** - cleanup code: [Control Flow](05_control.md)
- **return** - exit functions: [Functions](04_functions.md)

### Failure System
- **failure** - control through failure: [Overview](00_overview.md), [Failure](06_failure.md), [Effects](10_effects.md)
- **failable expressions** - can fail: [Built-in Types](02_builtins.md), [Functions](04_functions.md), [Failure](06_failure.md)
- **query operator (?)** - test values: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Failure](06_failure.md)
- **speculative execution** - rollback on failure: [Overview](00_overview.md), [Failure](06_failure.md)

## Concurrency

### Structured Concurrency
- **sync** - wait for all: [Overview](00_overview.md), [Concurrency](11_concurrency.md)
- **race** - first to complete: [Overview](00_overview.md), [Concurrency](11_concurrency.md)
- **rush** - first to succeed: [Concurrency](11_concurrency.md)
- **branch** - all that succeed: [Concurrency](11_concurrency.md)

### Unstructured Concurrency
- **spawn** - independent tasks: [Overview](00_overview.md), [Effects](10_effects.md), [Concurrency](11_concurrency.md)
- **task** - concurrent execution: [Concurrency](11_concurrency.md)
- **async expressions** - time-taking operations: [Concurrency](11_concurrency.md)
- **Sleep()** - pause execution: [Concurrency](11_concurrency.md)
- **cancellation** - stopping tasks: [Concurrency](11_concurrency.md)

## Mutability

### Mutation Control
- **var** - mutable variables: [Built-in Types](02_builtins.md), [Effects](10_effects.md), [Mutability](09_mutability.md)
- **set** - assignment: [Overview](00_overview.md), [Functions](04_functions.md), [Mutability](09_mutability.md)
- **immutability** - default behavior: [Overview](00_overview.md), [Effects](10_effects.md), [Mutability](09_mutability.md)
- **deep copying** - struct semantics: [Mutability](09_mutability.md)
- **reference semantics** - class behavior: [Composite Types](08_composites.md), [Mutability](09_mutability.md)
- **value semantics** - struct behavior: [Composite Types](08_composites.md), [Mutability](09_mutability.md)

## Class & Type Specifiers

### Structure Specifiers
- **\<unique\>** - identity equality: [Overview](00_overview.md), [Composite Types](08_composites.md), [Mutability](09_mutability.md), [Access Specifiers](14_access.md)
- **\<abstract\>** - cannot instantiate: [Composite Types](08_composites.md), [Access Specifiers](14_access.md)
- **\<concrete\>** - can instantiate: [Composite Types](08_composites.md), [Access Specifiers](14_access.md)
- **\<final\>** - cannot inherit: [Composite Types](08_composites.md), [Persistable Types](15_persistable.md), [Access Specifiers](14_access.md)
- **\<final_super\>** - terminal inheritance: [Composite Types](08_composites.md), [Access Specifiers](14_access.md)
- **\<final_super_base\>** - inheritance root: [Composite Types](08_composites.md)
- **\<castable\>** - runtime type checking: [Composite Types](08_composites.md), [Access Specifiers](14_access.md), [Code Evolution](16_evolution.md)
- **\<persistable\>** - saveable data: [Overview](00_overview.md), [Composite Types](08_composites.md), [Persistable Types](15_persistable.md)
- **\<constructor\>** - factory methods: [Composite Types](08_composites.md)

### Enum Specifiers
- **\<open\>** - extensible enums: [Composite Types](08_composites.md), [Access Specifiers](14_access.md), [Code Evolution](16_evolution.md)
- **\<closed\>** - fixed enums: [Composite Types](08_composites.md), [Access Specifiers](14_access.md), [Code Evolution](16_evolution.md)

## Access Control

### Visibility Specifiers
- **\<public\>** - universal access: [Overview](00_overview.md), [Composite Types](08_composites.md), [Modules and Paths](13_modules.md), [Access Specifiers](14_access.md)
- **\<private\>** - class/module only: [Composite Types](08_composites.md), [Modules and Paths](13_modules.md), [Access Specifiers](14_access.md)
- **\<protected\>** - subclass access: [Composite Types](08_composites.md), [Modules and Paths](13_modules.md), [Access Specifiers](14_access.md)
- **\<internal\>** - module access: [Modules and Paths](13_modules.md), [Access Specifiers](14_access.md)
- **\<scoped\>** - path-based access: [Access Specifiers](14_access.md)

### Method Specifiers
- **\<override\>** - replace parent method: [Composite Types](08_composites.md), [Access Specifiers](14_access.md)
- **\<native\>** - implemented in C++: [Access Specifiers](14_access.md)

## Operators

### Arithmetic
- **+, -, \*, /, %** - math operations: [Built-in Types](02_builtins.md), [Operators](03_operators.md)
- **+=, -=, \*=, /=** - compound assignment: [Built-in Types](02_builtins.md), [Operators](03_operators.md)

### Comparison
- **<, <=, >, >=** - ordering: [Built-in Types](02_builtins.md), [Operators](03_operators.md)
- **=, <>** - equality/inequality: [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Type System](07_types.md)

### Logical
- **and** - logical AND: [Operators](03_operators.md), [Failure](06_failure.md)
- **or** - logical OR: [Operators](03_operators.md), [Failure](06_failure.md)
- **not** - logical NOT: [Operators](03_operators.md), [Failure](06_failure.md)

### Access
- **.** - member access: [Operators](03_operators.md), [Expressions](01_expressions.md)
- **[]** - indexing: [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Expressions](01_expressions.md)
- **()** - function call: [Operators](03_operators.md), [Expressions](01_expressions.md)
- **{}** - object construction: [Operators](03_operators.md), [Expressions](01_expressions.md)

### Special
- **:=** - initialization: [Operators](03_operators.md), [Expressions](01_expressions.md)
- **..** - range operator: [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Expressions](01_expressions.md)
- **?** - query operator: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Operators](03_operators.md), [Failure](06_failure.md)

## Functions

### Function Features
- **parameters** - function inputs: [Functions](04_functions.md)
- **named arguments** - explicit parameter names: [Functions](04_functions.md)
- **return values** - function outputs: [Functions](04_functions.md)
- **function types** - function signatures: [Functions](04_functions.md), [Type System](07_types.md)
- **overloading** - multiple definitions: [Functions](04_functions.md), [Operators](03_operators.md)
- **lambdas** - anonymous functions: [Overview](00_overview.md), [Functions](04_functions.md), [Expressions](01_expressions.md)
- **higher-order functions** - functions as values: [Overview](00_overview.md), [Functions](04_functions.md)

## Modules & Organization

### Module System
- **module** - code organization: [Modules and Paths](13_modules.md)
- **using** - import statements: [Overview](00_overview.md), [Modules and Paths](13_modules.md)
- **module paths** - hierarchical names: [Modules and Paths](13_modules.md)
- **qualified names** - full paths: [Modules and Paths](13_modules.md)
- **qualified access** - explicit paths: [Modules and Paths](13_modules.md)
- **nested modules** - module hierarchy: [Modules and Paths](13_modules.md)

## Persistence

### Save System
- **weak_map(player, t)** - player data: [Modules and Paths](13_modules.md), [Persistable Types](15_persistable.md)
- **weak_map(session, t)** - session data: [Modules and Paths](13_modules.md)
- **persistable types** - saveable data: [Overview](00_overview.md), [Composite Types](08_composites.md), [Persistable Types](15_persistable.md)
- **module-scoped variables** - persistent storage: [Modules and Paths](13_modules.md), [Persistable Types](15_persistable.md)

## Evolution & Compatibility

### Version Management
- **backward compatibility** - preserving APIs: [Overview](00_overview.md), [Effects](10_effects.md), [Code Evolution](16_evolution.md)
- **versioning** - tracking changes: [Code Evolution](16_evolution.md)
- **deprecation** - phasing out features: [Code Evolution](16_evolution.md)
- **publication** - making code public: [Modules and Paths](13_modules.md), [Access Specifiers](14_access.md), [Code Evolution](16_evolution.md)
- **breaking changes** - incompatible updates: [Code Evolution](16_evolution.md)
- **schema evolution** - data structure changes: [Composite Types](08_composites.md), [Code Evolution](16_evolution.md)

## Built-in Functions

### Math Functions
- **Floor()** - round down: [Built-in Types](02_builtins.md)
- **Ceil()** - round up: [Built-in Types](02_builtins.md)
- **Sqrt()** - square root: [Mutability](09_mutability.md)
- **Min()** - minimum value: [Mutability](09_mutability.md)
- **Max()** - maximum value: [Mutability](09_mutability.md)

### Utility Functions
- **Print()** - output text: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Effects](10_effects.md)
- **ToString()** - convert to string: [Built-in Types](02_builtins.md)
- **Sleep()** - pause execution: [Concurrency](11_concurrency.md)
- **GetSession()** - current session: [Modules and Paths](13_modules.md)

## Special Concepts

### Language Features
- **archetype expression** - prototype patterns: [Composite Types](08_composites.md), [Expressions](01_expressions.md)
- **string interpolation** - embedded expressions: [Built-in Types](02_builtins.md)
- **pattern matching** - structural matching: [Overview](00_overview.md), [Composite Types](08_composites.md), [Control Flow](05_control.md)
- **inheritance** - class hierarchy: [Composite Types](08_composites.md), [Type System](07_types.md), [Access Specifiers](14_access.md)
- **polymorphism** - multiple forms: [Composite Types](08_composites.md), [Type System](07_types.md)
- **transactional semantics** - rollback behavior: [Overview](00_overview.md), [Failure](06_failure.md), [Effects](10_effects.md)
- **option{}** constructor: [Overview](00_overview.md), [Built-in Types](02_builtins.md), [Composite Types](08_composites.md)
- **array{}** constructor: [Overview](00_overview.md), [Built-in Types](02_builtins.md)
- **map{}** constructor: [Built-in Types](02_builtins.md)

---

*Note: This index covers all major concepts in the Verse documentation except for the Grammar reference. Use your browser's search function (Ctrl+F or Cmd+F) to quickly find specific terms.*