# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a TypeScript implementation of a lossless parser for the Verse programming language (Epic Games' functional logic language used in Unreal Engine, Fortnite Creative, and UEFN). The parser preserves all source information including comments and whitespace, generates a complete AST with source location tracking, and provides HTML syntax highlighting.

## Development Commands

### Build and Testing
```bash
npm run build        # Compile TypeScript to JavaScript (dist/)
npm run dev          # Run src/index.ts directly with ts-node
npm test             # Run all tests
npm run lint         # Run TypeScript type checking (tsc --noEmit)
npm run clean        # Remove dist/ directory

# Run specific tests
npm test -- parser.test.ts                   # Run specific test file
npm test -- integration.test.ts              # Run integration tests
npm test -- --testNamePattern="logical"      # Run tests matching pattern
npm test -- --coverage                       # Run tests with coverage report
```

### Running the Parser
```bash
# Parse a single file
npx ts-node src/index.ts <file.verse>

# Generate HTML with syntax highlighting
node generate-verse-html.js "tests/Verse/**/*.verse" html-output
```

## Architecture

### Core Parser System

The parser uses a **combinator-based approach** with monadic parsing primitives:

- **src/parser/combinators.ts**: Core parser combinator library implementing fundamental parsing operations (map, flatMap, choice, sequence, etc.)
- **src/parser/parser.ts**: Main Verse language parser with grammar rules. Key exports:
  - `parseVersee(source: string)`: Main entry point returning `ParseResult<L<Exp>>`
  - Contains operator precedence hierarchy (assignment → logical → comparison → arithmetic)
  - Handles both standard operators and Verse-specific constructs (specifiers, modules, etc.)

- **src/parser/trivia-parser.ts**: Handles parsing of whitespace, comments, and other trivia for lossless parsing
- **src/parser/token.ts**: Token definitions and utilities

### AST Structure

All AST nodes use the `L<T>` wrapper type for location tracking:

- **src/ast/expression.ts**: Expression AST nodes (`Exp` type) including:
  - Binary operators (Add, Multiply, And, Or, etc.)
  - Control flow (If, For, While, Case)
  - Literals (Int, Float, String, True, False)
  - Special constructs (Lambda, Array, Tuple, Module)

- **src/ast/pattern.ts**: Pattern matching AST nodes
- **src/ast/identifier.ts**: Identifier and name-related AST nodes
- **src/ast/location.ts**: Source location tracking (`L<T>` type, `Pos` positions)
- **src/ast/trivia.ts**: Trivia AST nodes (comments, whitespace)

### Parser Features

#### Recently Implemented Features
- **Lambda expressions**: `x => x + 1` syntax with proper precedence
- **Logical operators**: `and`/`or` keywords (units support removed to fix conflicts)
- **For-loop ranges**: `for(x:=0..99)` with optional `.body` syntax
- **Array/tuple literals**: Proper distinction between `(x)` (paren) and `(x,y)` (tuple)
- **Case expressions**: Basic `case(expr): result` pattern matching

#### Known Limitations
- Curried function declarations not supported: `F(X:int)(Y:int) := X + Y`
- Complex case patterns with type annotations: `y:int => y + 1`
- Some UI-related Verse constructs (struct fields, @editable attributes)
- Generic types with brackets: `[player]widget_map`

### Lossless Parsing

- **src/lossless-parser.ts**: Lossless parsing implementation that preserves all source information
- **src/printer/pretty-printer.ts**: AST pretty-printing (partially implemented, has type issues)
- **src/error-reporting.ts**: Error reporting and diagnostic utilities

## Test Organization

Tests use Jest with ts-jest for TypeScript support:

- **tests/parser.test.ts**: Core parser functionality tests (200+ tests)
- **tests/integration.test.ts**: Integration tests for all new features (31 tests)
- **tests/logical-operators.test.ts**: Logical operator specific tests
- **tests/golden.test.ts**: Golden reference tests comparing against expected outputs
- **tests/specifier.test.ts**: Tests for Verse specifier parsing

Test coverage: ~99.6% (242/243 tests passing)

## Working with Verse Files

The `tests/Verse/` directory contains real Verse code samples from Epic Games:
- Successfully parses ~80% of real Verse files
- Common failure: "Empty expression list" for complex UI/module files
- Test files included from: SolarisTestbed, Samples, various game templates

### HTML Generation

Two scripts are available for generating HTML with syntax highlighting:
- `generate-verse-html.js`: Production script with light-mode GitHub-inspired theme
- `simple-html-gen.js`: Simplified version without AST dependencies

## Key Design Decisions

1. **Combinator-Based Parser**: Chosen over traditional lexer/parser split for composability and type safety
2. **Immutable AST**: All AST nodes created through factory functions (createInt, createString, etc.)
3. **Location Tracking**: Every AST node includes precise source location for error reporting
4. **Reserved Words**: Maintained in a Set for efficient keyword checking (includes 'and', 'or' after units removal)