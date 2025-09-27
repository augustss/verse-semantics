# Verse Language Grammar and Missing Documentation

## Features Found in Grammar but Missing or Under-documented

Based on analysis of the TypeScript parser implementation in `../Tsx`, several language features are present in the grammar but are either missing or insufficiently documented in the current documentation set.

### 1. Using Statements (Imports)

The grammar includes `UsingStatement` nodes for importing modules, but the documentation doesn't cover the import system:

```verse
using { /Fortnite.com/Devices }
using { MyModule.verse }
```

These statements allow importing functionality from other modules and are parsed at the top level of files, but there's no documentation explaining the import syntax, path resolution, or visibility rules for imported symbols.

### 2. Defer Expression

While mentioned briefly in control flow documentation, the `defer` expression deserves more comprehensive coverage. The grammar shows it's a first-class expression that can appear in various contexts, not just as a statement.

### 3. Rush Expression

The grammar includes a `RushExpression` node alongside `RaceExpression`, but `rush` is barely mentioned in the concurrency documentation. Rush appears to be a variant of race with different semantics that needs proper explanation.

### 4. Continue Expression

The `continue` keyword for loop control is mentioned in passing but lacks detailed documentation about its behavior in nested loops, interaction with `for` expression filters, and use within different loop contexts.

### 5. Yield Expression

The grammar suggests support for `yield` expressions, possibly for generator-like functionality or coroutines, but this isn't documented anywhere. This could be related to the concurrency model or iterator protocols.

### 6. Spawn Expression

While `spawn` is parsed as a distinct expression type for launching concurrent tasks, the documentation focuses more on `sync`, `race`, and `branch`. The specific semantics of `spawn` and how it differs from other concurrency primitives needs clarification.

### 7. Tuple Expansion Expression

The grammar includes `TupleExpansionExpression` for unpacking tuples in specific contexts. While tuple unpacking is mentioned for function calls, the full expansion syntax and all contexts where it's valid aren't documented:

```verse
Args := (1, 2, 3)
Function(...Args)  # Expansion syntax
```

### 8. Indented Compound Expression

The parser recognizes `IdentedCompoundExpression` as a distinct syntactic form, suggesting special handling of indentation-based code blocks beyond what's described in the code blocks documentation.

### 9. Qualified Access Expression

The grammar distinguishes between simple member access (`MemberExpression`) and qualified access (`QualifiedAccessExpression`), but the documentation doesn't explain when and why you'd use qualified access:

```verse
(module:)Identifier  # Qualified access
```

### 10. Where Clauses and Type Constraints

The grammar suggests more sophisticated type constraint syntax than what's documented. Where clauses for parametric types might support more complex constraints than shown in examples.

### 11. Module Declaration Syntax

While paths and modules are discussed, the actual syntax for declaring modules inline isn't well documented:

```verse
MyModule := module {
    # Module contents
}
```

### 12. Specifier Combinations

The grammar allows complex combinations of specifiers (public, private, computes, transacts, decides, etc.), but the documentation doesn't explain:
- Which combinations are valid
- Order requirements
- Semantic implications of combinations

### 13. Literal Types Beyond Basic

The grammar's `literalType` field suggests support for additional literal types beyond what's documented (string, integer, float, boolean). This might include character literals, byte literals, or other numeric types.

### 14. Case Expression Pattern Matching

While case expressions are mentioned, the grammar suggests more sophisticated pattern matching capabilities than simple value matching, possibly including destructuring patterns or guards.

### 15. Lambda Expression Full Syntax

The grammar shows lambda expressions as first-class constructs with full parameter lists and type annotations, but lambdas are only briefly mentioned in the functions documentation:

```verse
Lambda := (X:int, Y:int) : int => X + Y
```

### 16. Set Expression Variants

The `SetExpression` in the grammar suggests multiple forms of the set operation beyond simple assignment, possibly including compound assignments that aren't fully documented.

### 17. Range Expression Details

Range expressions are used in examples but their full syntax isn't documented:
- Exclusive vs inclusive ranges
- Step values
- Reverse ranges
- Non-numeric ranges

### 18. Object Constructor Syntax

The grammar shows sophisticated object construction with `ObjectConstructorExpression`, but the documentation doesn't fully explain:
- Named vs positional field initialization
- Partial object construction
- Copying with modifications

## Recommendations

These missing features should be documented with:

1. **Syntax specifications** - Formal grammar rules for each construct
2. **Semantic explanations** - What each feature does and when to use it
3. **Code examples** - Practical demonstrations of usage
4. **Interaction rules** - How features interact with other language elements
5. **Error conditions** - What happens when constructs are misused

The documentation should also include a formal grammar specification or BNF that matches the actual parser implementation to ensure completeness and accuracy.