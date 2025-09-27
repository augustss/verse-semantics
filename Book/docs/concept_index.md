# Verse Language Concept Index

This index provides quick access to all major concepts in the Verse documentation. Each concept includes links to its primary definition and related discussions.

## A. Language Fundamentals

### Basic Types & Literals
- **int (integer)** - [Definition](01_builtins.md#integers) | [Arithmetic](04_operators.md#arithmetic-operations) | [Type System](12_types.md#basic-types)
- **float** - [Definition](01_builtins.md#floating-point-numbers) | [Operators](04_operators.md#arithmetic-operations) | [Types](12_types.md#basic-types)
- **rational** - [Definition](01_builtins.md#rational-numbers) | [Division](04_operators.md#division-and-rationals)
- **logic (boolean)** - [Definition](01_builtins.md#logic-values) | [Query Operator](04_operators.md#query-operator)
- **string** - [Definition](01_builtins.md#strings) | [Concatenation](04_operators.md#string-operations)
- **char/char32** - [Definition](01_builtins.md#character-types) | [String Encoding](01_builtins.md#strings)
- **void** - [Definition](01_builtins.md#void-type) | [Functions](03_functions.md#return-values) | [Type System](12_types.md#void-type)
- **any** - [Definition](01_builtins.md#any-type) | [Type Hierarchy](12_types.md#type-hierarchies)

### Container Types
- **array** - [Definition](01_builtins.md#arrays) | [Indexing](04_operators.md#indexing-operator) | [Mutability](11_mutability.md#deep-mutation)
- **tuple** - [Definition](01_builtins.md#tuples) | [Expressions](05_expressions.md#tuples-lightweight-aggregation) | [Grammar](16_grammar.md#tuple-system)
- **map** - [Definition](01_builtins.md#maps) | [Indexing](04_operators.md#indexing-operator)
- **weak_map** - [Definition](01_builtins.md#weak-maps) | [Persistence](07_modules.md#session-scoped-variables)
- **option** - [Definition](01_builtins.md#option-types) | [Failure](08_failure.md#option-type-and-failure)

## B. Type System

### Composite Types
- **class** - [Definition](02_composites.md#classes) | [Type System](12_types.md#class-types) | [Access Control](14_access.md#class-access)
- **struct** - [Definition](02_composites.md#structs) | [Mutability](11_mutability.md#struct-mutation) | [Persistable](13_persistable.md#persistable-structs)
- **interface** - [Definition](02_composites.md#interfaces) | [Implementation](02_composites.md#implementing-interfaces)
- **enum** - [Definition](02_composites.md#enums) | [Persistable](13_persistable.md#persistable-enums) | [Access](14_access.md#enum-access)

### Type System Concepts
- **subtyping** - [Definition](12_types.md#subtyping) | [Inheritance](02_composites.md#inheritance)
- **type casting** - [Definition](12_types.md#type-casting-and-conversion) | [Built-ins](01_builtins.md#type-conversions)
- **where clauses** - [Definition](12_types.md#where-clauses) | [Functions](03_functions.md#generic-functions)
- **comparable** - [Definition](12_types.md#comparable-types) | [Built-ins](01_builtins.md#comparable-types)
- **type hierarchies** - [Definition](12_types.md#type-hierarchies) | [Classes](02_composites.md#class-hierarchies)
- **parametric types** - [Where Clauses](12_types.md#where-clauses) | [Functions](03_functions.md#generic-functions)

## C. Functions & Expressions

### Functions
- **open-world functions** - [Definition](03_functions.md#open-world-vs-closed-world) | [Overview](00_overview.md#functional-features)
- **closed-world functions** - [Definition](03_functions.md#open-world-vs-closed-world) | [Overview](00_overview.md#functional-features)
- **parameters** - [Definition](03_functions.md#parameters) | [Grammar](16_grammar.md#function-declarations)
- **named arguments** - [Definition](03_functions.md#named-arguments) | [Grammar](16_grammar.md#function-declarations)
- **return values** - [Definition](03_functions.md#return-values) | [Expressions](05_expressions.md#function-calls)
- **function types** - [Definition](03_functions.md#function-types) | [Type Expressions](16_grammar.md#function-type-signatures)
- **overloading** - [Definition](03_functions.md#function-overloading) | [Methods](02_composites.md#method-overloading)
- **lambda expressions** - [Definition](05_expressions.md#lambda-expressions-functions-as-values) | [Overview](00_overview.md#functional-features) | [Grammar](16_grammar.md#lambda-expressions)

### Expression Types
- **primary expressions** - [Definition](05_expressions.md#primary-expressions-the-building-blocks) | [Grammar](16_grammar.md#primary-expressions)
- **postfix operations** - [Definition](05_expressions.md#postfix-operations-building-complexity) | [Operators](04_operators.md#postfix-operators)
- **object construction** - [Definition](05_expressions.md#object-construction-creating-instances) | [Classes](02_composites.md#object-creation)
- **if expressions** - [Definition](05_expressions.md#conditional-expressions) | [Control Flow](06_control.md#if-statements)
- **for expressions** - [Definition](05_expressions.md#for-expressions-iteration-as-computation) | [Control Flow](06_control.md#for-loops)
- **loop expressions** - [Definition](05_expressions.md#loop-expressions-unbounded-iteration) | [Control Flow](06_control.md#loops)
- **case expressions** - [Definition](05_expressions.md#case-expressions-pattern-based-selection) | [Control Flow](06_control.md#case-statements)
- **binary expressions** - [Definition](05_expressions.md#binary-operations-combining-values) | [Operators](04_operators.md#binary-operators)
- **set expressions** - [Definition](05_expressions.md#set-expressions-mutation-in-a-functional-world) | [Mutability](11_mutability.md#set-expressions)
- **array expressions** - [Definition](05_expressions.md#array-expressions-collections-as-values) | [Grammar](16_grammar.md#array-expressions)
- **type expressions** - [Definition](05_expressions.md#type-expressions-computing-with-types) | [Grammar](16_grammar.md#type-expression-construct)

## D. Operators

### Operator Categories
- **arithmetic operators** - [Definition](04_operators.md#arithmetic-operations) | [Built-ins](01_builtins.md#numeric-operations)
- **comparison operators** - [Definition](04_operators.md#comparison-operations) | [Types](12_types.md#comparable-types)
- **logical operators** - [Definition](04_operators.md#logical-operations) | [Failure](08_failure.md#logical-operators-in-failure)
- **assignment operators** - [Definition](04_operators.md#assignment-and-binding) | [Mutability](11_mutability.md#assignment)
- **compound assignment** - [Definition](04_operators.md#compound-assignment) | [Mutability](11_mutability.md#compound-assignment)
- **operator precedence** - [Definition](04_operators.md#operator-precedence) | [Grammar](16_grammar.md#operator-precedence)
- **query operator (?)** - [Definition](04_operators.md#query-operator) | [Failure](08_failure.md#query-operator) | [Option](01_builtins.md#option-types)
- **range operator (..)** - [Definition](04_operators.md#range-operator) | [Expressions](05_expressions.md#range-expressions)
- **member access (.)** - [Definition](04_operators.md#member-access) | [Expressions](05_expressions.md#member-access)
- **indexing ([])** - [Definition](04_operators.md#indexing-operator) | [Arrays](01_builtins.md#arrays)

## E. Control Flow & Structure

### Control Flow
- **code blocks** - [Definition](06_control.md#code-blocks) | [Expressions](05_expressions.md#compound-and-block-expressions)
- **scoping** - [Definition](06_control.md#scoping-rules) | [Modules](07_modules.md#module-scope)
- **if statement** - [Definition](06_control.md#if-statements) | [Failure](08_failure.md#if-with-failure)
- **case statement** - [Definition](06_control.md#case-statements) | [Enums](02_composites.md#enum-matching)
- **loops** - [Definition](06_control.md#loops-and-iteration) | [Expressions](05_expressions.md#loop-expressions)
- **break/continue** - [Definition](06_control.md#break-and-continue) | [Grammar](16_grammar.md#control-flow-statements)
- **defer expression** - [Definition](06_control.md#defer-expressions) | [Patterns](06_control.md#defer-patterns)
- **profile expression** - [Definition](06_control.md#profile-expressions) | [Performance](06_control.md#profiling)

### Comments & Formatting
- **single-line comments (#)** - [Definition](00_overview.md#single-line-comments) | [Grammar](16_grammar.md#comments)
- **multi-line comments (<# #>)** - [Definition](00_overview.md#multi-line-comments) | [Grammar](16_grammar.md#comments)
- **nested comments** - [Definition](00_overview.md#nested-comments)
- **code formatting** - [Definition](00_overview.md#formatting-conventions)
- **naming conventions** - [Definition](00_overview.md#naming-conventions)
- **syntactic styles** - [Definition](00_overview.md#syntactic-flexibility) | [Grammar](16_grammar.md#braced-vs-indented-bodies)

## F. Advanced Features

### Failure System
- **failable expressions** - [Definition](08_failure.md#failable-expressions) | [Functions](03_functions.md#failable-functions)
- **failure contexts** - [Definition](08_failure.md#failure-contexts) | [Control Flow](06_control.md#failure-handling)
- **decides effect** - [Definition](08_failure.md#decides-effect) | [Effects](09_effects.md#decides-effect)
- **speculative execution** - [Definition](08_failure.md#speculative-execution) | [Overview](00_overview.md#speculative-execution)
- **transactional semantics** - [Definition](08_failure.md#transactional-semantics) | [Mutability](11_mutability.md#transactional-updates)
- **failure propagation** - [Definition](08_failure.md#failure-propagation) | [Concurrency](10_concurrency.md#failure-in-concurrent-contexts)
- **not operator** - [Definition](08_failure.md#not-operator) | [Operators](04_operators.md#logical-operations)
- **or operator** - [Definition](08_failure.md#or-operator-alternatives) | [Operators](04_operators.md#logical-operations)

### Effect System
- **effect families** - [Definition](09_effects.md#effect-families)
- **effect specifiers** - [Definition](09_effects.md#effect-specifiers) | [Functions](03_functions.md#effect-annotations)
- **cardinality effects** - [Definition](09_effects.md#cardinality-effects) | [Failure](08_failure.md#cardinality)
- **computes effect** - [Definition](09_effects.md#computes-effect-pure-computation)
- **reads effect** - [Definition](09_effects.md#reads-effect) | [Mutability](11_mutability.md#read-effects)
- **writes effect** - [Definition](09_effects.md#writes-effect) | [Mutability](11_mutability.md#write-effects)
- **allocates effect** - [Definition](09_effects.md#allocates-effect)
- **transacts effect** - [Definition](09_effects.md#transacts-effect) | [Failure](08_failure.md#transactional-semantics)
- **suspends effect** - [Definition](09_effects.md#suspends-effect) | [Concurrency](10_concurrency.md#suspends-effect)
- **predicts/dictates** - [Definition](09_effects.md#predicts-and-dictates-effects)

### Concurrency
- **async expressions** - [Definition](10_concurrency.md#async-expressions) | [Effects](09_effects.md#suspends-effect)
- **simulation updates** - [Definition](10_concurrency.md#simulation-updates)
- **structured concurrency** - [Definition](10_concurrency.md#structured-concurrency)
- **sync expression** - [Definition](10_concurrency.md#sync-parallel-execution)
- **race expression** - [Definition](10_concurrency.md#race-first-wins-semantics)
- **rush expression** - [Definition](10_concurrency.md#rush-background-execution)
- **branch expression** - [Definition](10_concurrency.md#branch-fire-and-forget)
- **spawn expression** - [Definition](10_concurrency.md#spawn-unstructured-concurrency)
- **task management** - [Definition](10_concurrency.md#task-management)
- **Sleep function** - [Definition](10_concurrency.md#sleep-and-timing)

### Mutability
- **var keyword** - [Definition](11_mutability.md#mutable-variables) | [Expressions](05_expressions.md#set-expressions)
- **set expression** - [Definition](11_mutability.md#set-expressions) | [Operators](04_operators.md#assignment-operators)
- **deep mutation** - [Definition](11_mutability.md#deep-mutation)
- **class mutability** - [Definition](11_mutability.md#class-mutability) | [Classes](02_composites.md#mutable-fields)
- **mutable references** - [Definition](11_mutability.md#mutable-references)
- **pure computations** - [Definition](11_mutability.md#pure-computations) | [Effects](09_effects.md#computes-effect-pure-computation)

## G. Modules & Organization

### Module System
- **modules** - [Definition](07_modules.md#modules) | [Overview](00_overview.md#module-system)
- **module paths** - [Definition](07_modules.md#module-paths)
- **using statement** - [Definition](07_modules.md#using-statement) | [Grammar](16_grammar.md#using-statement)
- **qualified names** - [Definition](07_modules.md#qualified-names)
- **qualified access** - [Definition](07_modules.md#qualified-access-expressions)
- **nested modules** - [Definition](07_modules.md#nested-modules)
- **module-scoped variables** - [Definition](07_modules.md#module-scoped-variables)
- **session-scoped variables** - [Definition](07_modules.md#session-scoped-variables)
- **persistent player data** - [Definition](07_modules.md#persistent-player-data) | [Persistable](13_persistable.md#player-data)

## H. Access Control & Visibility

### Access Specifiers
- **public** - [Definition](14_access.md#public-access) | [Classes](02_composites.md#access-control)
- **protected** - [Definition](14_access.md#protected-access) | [Inheritance](02_composites.md#protected-members)
- **private** - [Definition](14_access.md#private-access) | [Encapsulation](02_composites.md#private-members)
- **internal** - [Definition](14_access.md#internal-access)
- **scoped** - [Definition](14_access.md#scoped-access)
- **dual specifiers** - [Definition](14_access.md#dual-specifiers-for-variables)

### Class/Type Specifiers
- **unique** - [Definition](02_composites.md#unique-specifier)
- **concrete** - [Definition](02_composites.md#concrete-specifier)
- **abstract** - [Definition](02_composites.md#abstract-specifier)
- **castable** - [Definition](02_composites.md#castable-specifier)
- **final** - [Definition](02_composites.md#final-specifier)
- **final_super** - [Definition](02_composites.md#final-super-specifiers)
- **persistable** - [Definition](02_composites.md#persistable-specifier) | [Persistence](13_persistable.md#persistable-classes)
- **open/closed enums** - [Definition](02_composites.md#open-and-closed-enums)

## I. Persistence & Compatibility

### Persistable Types
- **persistable concept** - [Definition](13_persistable.md#persistable-types) | [Modules](07_modules.md#persistent-data)
- **built-in persistable** - [Definition](13_persistable.md#built-in-persistable-types)
- **persistable classes** - [Definition](13_persistable.md#persistable-classes)
- **persistable structs** - [Definition](13_persistable.md#persistable-structs)
- **persistable enums** - [Definition](13_persistable.md#persistable-enums)
- **weak_map with player** - [Definition](13_persistable.md#weak-map-and-player-data)

### Code Evolution
- **publication model** - [Definition](15_evolution.md#publication-model)
- **backward compatibility** - [Definition](15_evolution.md#backward-compatibility)
- **versioning system** - [Definition](15_evolution.md#versioning)
- **deprecation** - [Definition](15_evolution.md#deprecation)
- **breaking changes** - [Definition](15_evolution.md#breaking-changes)
- **schema stability** - [Definition](15_evolution.md#schema-stability)

## J. Language Grammar

### Grammar Elements
- **keywords** - [Definition](16_grammar.md#keywords)
- **operators syntax** - [Definition](16_grammar.md#operators) | [Operators](04_operators.md)
- **literals** - [Definition](16_grammar.md#literals) | [Built-ins](01_builtins.md)
- **declarations** - [Definition](16_grammar.md#declarations)
- **expression grammar** - [Definition](16_grammar.md#expressions) | [Expressions](05_expressions.md)
- **type grammar** - [Definition](16_grammar.md#types) | [Type System](12_types.md)
- **type expressions** - [Definition](16_grammar.md#type-expression-construct)
- **function signatures** - [Definition](16_grammar.md#function-type-signatures)
- **concurrent constructs** - [Definition](16_grammar.md#concurrent-programming-constructs) | [Concurrency](10_concurrency.md)
- **decorator support** - [Definition](16_grammar.md#decorator-support)

## K. Programming Patterns

### Common Patterns
- **validation chain** - [Definition](08_failure.md#validation-chain-pattern)
- **first-success pattern** - [Definition](08_failure.md#first-success-pattern)
- **filtering pattern** - [Definition](08_failure.md#filtering-pattern)
- **transaction pattern** - [Definition](08_failure.md#transaction-pattern)
- **timeout pattern** - [Definition](10_concurrency.md#timeout-pattern)
- **parallel initialization** - [Definition](10_concurrency.md#parallel-initialization)
- **background processing** - [Definition](10_concurrency.md#background-processing)
- **factory pattern** - [Definition](07_modules.md#factory-pattern)
- **service pattern** - [Definition](07_modules.md#service-pattern)
- **builder pattern** - [Definition](12_types.md#builder-pattern-with-where-clauses)

### Design Concepts
- **everything is an expression** - [Definition](00_overview.md#everything-is-an-expression) | [Expressions](05_expressions.md)
- **failure as control flow** - [Definition](00_overview.md#failure-as-control-flow) | [Failure](08_failure.md)
- **effect tracking** - [Definition](00_overview.md#effect-tracking) | [Effects](09_effects.md)
- **structured concurrency** - [Definition](00_overview.md#structured-concurrency) | [Concurrency](10_concurrency.md)
- **speculative execution** - [Definition](00_overview.md#speculative-execution) | [Failure](08_failure.md#speculative-execution)
- **immutable by default** - [Definition](00_overview.md#immutability) | [Mutability](11_mutability.md)
- **functional-logic paradigm** - [Definition](00_overview.md#philosophy) | [Philosophy](00_overview.md#verse-philosophy)
- **metaverse-first design** - [Definition](00_overview.md#metaverse-first) | [Persistence](13_persistable.md)

---

## Quick Reference Guide

### Most Important Concepts for Beginners
1. [Basic Types](01_builtins.md) - Start with int, float, string, logic
2. [Variables and Constants](05_expressions.md#assignment-and-binding) - Using `:=` and `var`
3. [Functions](03_functions.md) - Basic function syntax and calls
4. [If Expressions](05_expressions.md#conditional-expressions) - Conditional logic
5. [For Loops](05_expressions.md#for-expressions-iteration-as-computation) - Iteration
6. [Classes](02_composites.md#classes) - Object-oriented programming
7. [Failure](08_failure.md) - Verse's unique control flow

### Advanced Topics
1. [Effect System](09_effects.md) - Understanding side effects
2. [Where Clauses](12_types.md#where-clauses) - Generic programming
3. [Concurrency](10_concurrency.md) - Parallel and async programming
4. [Persistable Types](13_persistable.md) - Data that survives sessions
5. [Qualified Access](07_modules.md#qualified-access-expressions) - Advanced scoping

### Unique Verse Features
1. [Open-World Functions](03_functions.md#open-world-vs-closed-world) - Multiple implementations
2. [Failure as Control Flow](08_failure.md) - Not exceptions
3. [Everything is an Expression](05_expressions.md) - No statements
4. [Speculative Execution](08_failure.md#speculative-execution) - Automatic rollback
5. [Effect Tracking](09_effects.md) - Compile-time side effect analysis