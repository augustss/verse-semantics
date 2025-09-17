# Parse Test Format (.parsetest)

This directory contains `.parsetest` files that provide a simple format for testing the Verse parser with expected success/failure outcomes.

## Format

Each `.parsetest` file contains test cases separated by headers:

- `#! Valid` - Code that should parse successfully
- `#! Error` - Code that should fail to parse

Each test can be multiline and may include a description comment on the line following the header.

### Example

```
#! Valid
# Basic variable declarations
var x : int = 5
var y := 10

#! Error
# Invalid syntax: explicit type with :=
var x : int := 5

#! Valid
# Generic types work correctly
var MyMap : weak_map(int, string) = map{}
```

## Running Tests

```bash
# Run all .parsetest files
npm run test:parse

# Run specific test file
npx ts-node tests/parsetest-runner.ts tests/verse-syntax.parsetest
```

## Test Results

The test runner will:
- ✅ Show passed tests with descriptions
- ❌ Show failed tests with detailed error information
- 📊 Provide summary statistics and pass rates
- 🚨 Exit with error code if any tests fail

## Current Coverage

The `verse-syntax.parsetest` file covers:

### ✅ Working Features
- Variable declarations with correct syntax rules
- Optional types (`?int`, `?string`)
- Generic type parameters (`weak_map(T, U)`, `list(T)`)
- Enum declarations with specifiers
- Multiple class inheritance
- Field declarations with decorators
- Function declarations with specifiers
- Control flow (for loops, if statements)
- Constructor calls and literals
- String interpolation
- Method chaining and property access

### ❌ Expected Failures
- Invalid syntax combinations
- Keyword misuse (e.g., `array[5]`)
- Missing type annotations
- Import statements (not yet integrated)

### Current Results
- **100% pass rate** (29/29 tests)
- Comprehensive coverage of implemented parser features
- Validates syntax rule enforcement

## Adding New Tests

To add new test cases:

1. Open or create a `.parsetest` file
2. Add a header (`#! Valid` or `#! Error`)
3. Optionally add a description comment
4. Write the test code (can be multiline)
5. Run tests to verify

Example:
```
#! Valid
# Test new feature
NewSyntax := example{param}

#! Error
# This should fail
InvalidSyntax := wrong{
```

The test framework automatically discovers and runs all `.parsetest` files in the tests directory.