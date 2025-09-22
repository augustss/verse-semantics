# Test Organization

The tests directory contains all the parseset test files organized into logical categories:

## Directory Structure

### `/basic/` - Basic Expression Tests
Core language features and simple expressions:
- `arith.parseset` - Basic arithmetic operations (+, -, *, /, %)
- `basic.parseset` - Fundamental expression parsing
- `compare.parseset` - Comparison operations (==, !=, <, >, etc.)
- `literals.parseset` - Number, string, boolean literals
- `logical.parseset` - AND, OR, NOT operations
- `members.parseset` - Object member access (obj.field)
- `precedence.parseset` - Order of operations testing
- `range.parseset` - Range expressions (1..10)

### `/control-flow/` - Control Flow Statements
Flow control constructs:
- `block-expr.parseset` - Block expressions { ... }
- `blocks.parseset` - Block statement parsing
- `block-tests.parseset` - Additional block tests
- `bool-if.parseset` - Boolean conditions in if statements
- `break-cont.parseset` - Break and continue statements
- `case-expr.parseset` - Case/switch expressions
- `for-expr.parseset` - For loop expressions
- `for-comp.parseset` - Comprehensive for loop tests
- `for.parseset` - Basic for loop parsing
- `if-expr.parseset` - If/then/else expressions
- `if.parseset` - Basic if statement parsing

### `/data-structures/` - Data Structures and OOP
Classes, objects, and data construction:
- `class-mem.parseset` - Class member parsing
- `class.parseset` - Class definition parsing
- `class-complex.parseset` - Complex class structures
- `func-calls.parseset` - Function call expressions
- `method-indent.parseset` - Indented method body syntax
- `interface.parseset` - Interface, struct, and enum definitions
- `methods.parseset` - Method definition parsing
- `obj-errors.parseset` - Object construction error cases
- `objects.parseset` - Object construction and usage

### `/top-level/` - Top-Level Declarations
Module-level constructs:
- `constants.parseset` - Constant declarations
- `functions.parseset` - Function declarations
- `indent-nest.parseset` - Nested indented structures
- `top-level.parseset` - Top-level declaration parsing
- `variables.parseset` - Variable declarations
- `verse-adv.parseset` - Advanced Verse language features
- `verse-class.parseset` - Verse class definitions
- `verse-mod.parseset` - Verse module system
- `verse-fixes.parseset` - Verse-specific parser fixes
- `verse-using.parseset` - Using/import statements

### `/syntax/` - Syntax and Formatting
Language syntax rules and formatting:
- `comments.parseset` - Comment parsing (line and block)
- `decorators.parseset` - Decorator syntax (@decorator)
- `indent-empty.parseset` - Indentation with empty lines
- `keyword-ids.parseset` - Keywords used as identifiers
- `keyword-restrict.parseset` - Keyword usage restrictions
- `multiline-comm.parseset` - Multi-line comment parsing
- `whitespace.parseset` - Whitespace handling and formatting

### `/edge-cases/` - Edge Cases and Boundary Conditions
Unusual or boundary conditions:
- `boundary.parseset` - Input boundary testing
- `complex.parseset` - Complex expression combinations
- `edges.parseset` - General edge case testing
- `empty-lines.parseset` - Empty line handling
- `multiline.parseset` - Exact multiline parsing
- `nested.parseset` - Deeply nested expressions

### `/error-handling/` - Error Cases and Recovery
Error conditions and parser recovery:
- `errors.parseset` - Expected error conditions
- `recovery.parseset` - Parser error recovery testing

### `/comprehensive/` - Large Test Suites
Comprehensive test collections:
- `unit-extra.parseset` - Additional unit test cases
- `features.parseset` - New feature testing
- `unit-core.parseset` - Core unit test suite

## Usage

Run tests on specific categories:
```bash
# Test basic expressions only
npm test tests/basic/

# Test control flow
npm test tests/control-flow/

# Test all data structures
npm test tests/data-structures/

# Run all tests
npm test tests/
```

## Test File Format

Each `.parseset` file contains test cases in the format:
```
#! Valid expression
# Description of test case
input code here

#! Error TopLevel
# Description of error case
invalid code here
```

- `#! Valid expression` - Expected to parse successfully as expression
- `#! Valid TopLevel` - Expected to parse successfully as top-level declaration
- `#! Error` - Expected to fail parsing
- `# Description` - Human-readable test description