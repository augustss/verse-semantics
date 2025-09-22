# Scripts Directory

This directory contains various development and testing scripts for the expression parser.

## Structure

### `/debug/`
Contains debugging scripts used during development to test specific parsing scenarios:
- `debug-*.js` - Individual debugging scripts for testing specific features
- These scripts are typically one-off tools for investigating parsing issues

### `/test/`
Contains test utilities and specific test cases:
- `test_*.js` and `test-*.js` - Specific test scripts for individual features
- Used for focused testing during development

### `/tools/`
Contains utility scripts for analysis and demonstration:
- `show-*.js` - Scripts that demonstrate parsing and pretty-printing of specific code examples

### Root Scripts

- `run-tests.js` - Main test runner for parseset format tests
  - Usage: `node scripts/run-tests.js` or `npm test`
  - Runs all .parseset test files in the tests/ directory

## Usage

Run the main test suite:
```bash
npm test
# or
node scripts/run-tests.js
```

Run a specific debug script:
```bash
node scripts/debug/debug-specific-feature.js
```

Run a utility tool:
```bash
node scripts/tools/show-reconstruction.js
```