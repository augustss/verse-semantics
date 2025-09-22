# Verse Expression Parser

A lossless parser for the Verse programming language (Epic Games' UEFN/Fortnite Creative language) that preserves all whitespace and comments for perfect source reconstruction.

## Quick Start

```bash
# Build the project
npm run build

# Run test suite
node scripts/run-tests.js

# Parse basic expressions (128 test cases)
node dist/index.js
```

## API

```javascript
const { parse, parseTopLevel, prettyPrint } = require('./dist/index.js');

// Parse expressions
const ast = parse('x + y * 2');
const reconstructed = prettyPrint(ast);  // Lossless reconstruction

// Parse top-level declarations
const topLevel = parseTopLevel('MyClass := class: Field:int = 5');
```

### Core Functions
- `parse(code, verbose?)` - Parse expressions and statements
- `parseTopLevel(code, verbose?)` - Parse classes, modules, and other top-level constructs
- `prettyPrint(ast)` - Losslessly reconstruct source from AST
- `toCompactString(ast)` - Generate minified output

## Project Structure

```
src/
├── expression-parser.ts   # Main parser implementation
├── ast.ts                 # AST types and utilities
└── parser-combinators.ts  # Parser combinator framework

tests/                     # Test suite
verse-files-flat/         # 459 real Verse files for validation
dist/                     # Compiled JavaScript output
```

## Current Capabilities

### ✅ Supported Features
- Basic expressions and operators with correct precedence
- Function calls, lambdas, and function application
- Object construction (`array{}`, `map{}`, custom types)
- Class and module declarations
- Method definitions (both `=` and brace syntax)
- Annotations (`<public>`, `<private>`)
- Comments (line `#`, block `<# #>`, nested)
- Array literals (both `array{1,2,3}` and `{1,2,3}`)
- Pattern matching and conditionals
- Assignment expressions in method bodies

### 🚧 Not Yet Supported
- Variable type declarations: `var Health : int = 100`
- Loop statements: `loop:`
- Set statements: `set Health -= 10`
- Logic type annotations
- Doc comments: `##`
- Uninitialized field declarations: `Score:int`
- Native annotations: `<native_callable>`, `<transacts>`, `<decides>`

## Test Results

- **Basic Expressions**: 128/128 (100%)
- **Advanced Features**: 25/26 (96.2%)
- **Real Verse Files**: 107/459 (23.3%)

## Development

The parser uses a combinator-based approach that preserves all source trivia (whitespace, comments) in the AST for perfect reconstruction. Each token stores its original text including surrounding whitespace.

### Build & Test
```bash
npm run build        # Compile TypeScript
npm run typecheck   # Type checking only
npm run lint        # Run linter (if configured)
```

### Parser Architecture
- Parser combinators for composable parsing logic
- Strict operator precedence hierarchy
- Trivia preservation in every AST node
- Separate parsers for expressions vs top-level declarations

---
*Last updated: September 2024*