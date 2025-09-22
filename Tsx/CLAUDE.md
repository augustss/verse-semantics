# Verse Expression Parser

A lossless parser for the Verse programming language (Epic Games' UEFN/Fortnite Creative language) that preserves all whitespace and comments for perfect source reconstruction.

## Quick Start

```bash
# Run test suite (TypeScript compilation issues - tests run from pre-built dist/)
node scripts/run-tests.js

# Test basic functionality
node -e "const {parse} = require('./dist/index.js'); console.log('Works:', !!parse('x + y * 2'));"
```

## API

```javascript
const { parse, parseTopLevel, parseProgram, prettyPrint } = require('./dist/index.js');

// Parse expressions and statements
const ast = parse('x + y * 2');
const reconstructed = prettyPrint(ast);  // Lossless reconstruction

// Parse top-level declarations (classes, functions, etc.)
const topLevel = parseTopLevel('MyFunction() := 42');

// Parse complete programs with using statements
const program = parseProgram('using{/Verse.org/Simulation}\nMyClass := class{}');
```

### Core Functions
- `parse(code, verbose?)` - Parse expressions and statements
- `parseTopLevel(code, verbose?)` - Parse classes, modules, functions, and other top-level constructs
- `parseProgram(code, verbose?)` - Parse complete programs including using statements
- `prettyPrint(ast)` - Losslessly reconstruct source from AST
- `toCompactString(ast)` - Generate minified output

## Project Structure

```
src/
├── ast.ts                 # AST types and utilities (2400+ lines)
├── parser-combinators.ts  # Parser combinator framework
├── parser.ts              # Main parser re-exports
├── index.ts               # Public API exports
└── parser/                # Modular parser implementation
    ├── foundation/        # Core parsing utilities (tokens, trivia, helpers)
    ├── literals/          # Number, string, boolean, identifier parsers
    ├── operators/         # Arithmetic, logical, comparison operators
    ├── expressions/       # Expression parsing logic
    ├── statements/        # Statement parsers (if, for, loop, set, var, etc.)
    ├── top-level/         # Top-level declaration parsers
    └── decorators/        # Annotation/decorator parsing

tests/                     # Comprehensive test suite (61 .parseset files)
├── basic/                 # Basic functionality tests
├── control-flow/          # If, for, case, block expressions
├── data-structures/       # Classes, objects, arrays, methods
├── expressions/           # Various expression types
├── top-level/             # Top-level declarations
├── syntax/                # Comments, whitespace, keywords
├── edge-cases/            # Complex and boundary cases
└── comprehensive/         # Integration tests

verse-files-flat/         # 459 real Verse files for validation
dist/                     # Compiled JavaScript output
```

## Current Capabilities

### ✅ Supported Features
- **Expressions**: Basic arithmetic, logical, comparison operators with correct precedence
- **Function calls**: `func(args)`, `func[args]`, method calls `obj.method(args)`
- **Lambdas**: `x => body`, function application
- **Data structures**:
  - Object construction: `Type{field := value}`, `Type: field := value`
  - Array construction: `array{1,2,3}`, `{1,2,3}`
  - Member/index access: `obj.field`, `arr[index]`
- **Control flow**:
  - If expressions: `if(cond) then {body} else {other}`
  - For expressions: `for(x : items) {body}`
  - Case expressions: `case(x) {pattern => result}`
  - Block expressions: `{stmt1; stmt2; result}`
- **Declarations**:
  - Functions: `MyFunc() := body`, `MyFunc<specifier>() := body`
  - Constants: `Const := value`, `Const : type = value`
  - Variables: `var Name : type = value`
  - Classes: `MyClass := class {members}`
  - Interfaces, structs, modules, enums
- **Advanced features**:
  - Annotations/specifiers: `<public>`, `<override>`, `<transacts>`
  - Decorators: `@editable`, `@main`
  - Comments: line `#`, block `<# #>`, nested comments
  - Multiple syntactic styles: braces `{}` vs indentation `:`
  - Assignment expressions: `target := value`
  - Range expressions: `1..10`
  - Break/continue statements
  - Loop/set statements
  - Using statements: `using{/Path/To/Module}`

### 🚧 Partial Support
- **Class inheritance**: Basic syntax works, some edge cases fail
- **Complex indentation**: Multi-level nesting has some issues
- **Error recovery**: Some malformed input cases

### ❌ Known Limitations
- **TypeScript build**: Module resolution issues prevent `npm run build`
- **Advanced type annotations**: Complex logic types
- **Doc comments**: `##` style documentation
- **Native function signatures**: Some advanced native annotations

## Test Results

Current test status from 61 test files with 1359+ individual test cases:
- **Passed**: 1313 tests (96.6%)
- **Failed**: 46 tests (3.4%)
- **Reconstruction Issues**: 62 (ignored in normal mode, use `--strict` to see them)

### Test Categories
- **Basic expressions**: Arithmetic, logical, comparison operators
- **Control flow**: If/else, for loops, case expressions, blocks
- **Data structures**: Classes, objects, arrays, method calls
- **Top-level**: Function/class/module declarations, using statements
- **Syntax**: Comments, whitespace handling, keywords
- **Edge cases**: Complex nesting, boundary conditions

## Development

The parser uses a modular combinator-based approach that preserves all source trivia (whitespace, comments) in the AST for perfect reconstruction. Each token stores its original text including surrounding whitespace.

### Testing
```bash
node scripts/run-tests.js           # Run all tests (show failures only)
node scripts/run-tests.js --verbose # Show all test results
node scripts/run-tests.js --strict  # Include reconstruction failures
```

### Parser Architecture
- **Modular design**: Separate modules for different language constructs
- **Parser combinators**: Composable parsing logic with proper error handling
- **Trivia preservation**: Every AST node includes whitespace and comments
- **Multiple syntax styles**: Supports both brace-delimited and indentation-based syntax
- **Lossless reconstruction**: `prettyPrint()` produces identical source code

### File Organization
- Foundation utilities handle core parsing primitives
- Separate modules for literals, operators, expressions, statements
- Top-level parsers handle program structure and declarations
- Comprehensive test suite validates all functionality

---
*Last updated: September 2024*