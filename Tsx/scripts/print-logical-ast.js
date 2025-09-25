#!/usr/bin/env node

/**
 * Script to parse code and print the simplified logical AST
 */

const fs = require('fs');
const path = require('path');
const { parseExpression } = require('../dist/parser');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { simplify, simplifyProgram, printLogicalAST } = require('../dist/logical-ast');

// Parse command line arguments
const args = process.argv.slice(2);
const options = {
  program: args.includes('--program'),
  noColors: args.includes('--no-colors'),
  help: args.includes('--help') || args.includes('-h'),
  file: args.find(arg => !arg.startsWith('--'))
};

// Show help
if (options.help || !options.file) {
  console.log(`
Usage: node scripts/print-logical-ast.js [options] <file-or-code>

Options:
  --program      Parse as a complete program (default: expression)
  --no-colors    Disable colored output
  --help, -h     Show this help message

Examples:
  # Parse expression from file
  node scripts/print-logical-ast.js test.verse

  # Parse as program
  node scripts/print-logical-ast.js --program myfile.verse

  # Parse expression directly
  node scripts/print-logical-ast.js "1 + 2 * 3"

The logical AST:
- Removes all position/offset information
- Removes parentheses (precedence is in tree structure)
- Simplifies compound expressions
- Focuses on semantic meaning
`);
  process.exit(0);
}

// Read input
let source;
if (fs.existsSync(options.file)) {
  // It's a file
  source = fs.readFileSync(options.file, 'utf-8');
  console.log(`\n📄 File: ${options.file}`);
} else {
  // Treat as direct code
  source = options.file;
  console.log(`\n📝 Code: ${source}`);
}

console.log('─'.repeat(60));

try {
  // Parse the source
  let ast;
  let logical;

  if (options.program) {
    ast = parseProgram(source);
    logical = simplifyProgram(ast);
    console.log('Type: Program (Logical AST)\n');
  } else {
    ast = parseExpression(source);
    logical = simplify(ast);
    console.log('Type: Expression (Logical AST)\n');
  }

  // Pretty print the logical AST
  const printed = printLogicalAST(logical, {
    useColors: !options.noColors
  });

  console.log(printed);
  console.log();

} catch (error) {
  console.error('❌ Parse Error:', error.message);
  process.exit(1);
}