#!/usr/bin/env node

/**
 * Script to parse and pretty-print AST from Verse code
 */

const fs = require('fs');
const path = require('path');
const { parseExpression } = require('../dist/parser');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { prettyPrintAST } = require('../dist/utils/ast-printer');

// Parse command line arguments
const args = process.argv.slice(2);
const options = {
  showOffsets: args.includes('--offsets'),
  noColors: args.includes('--no-colors'),
  program: args.includes('--program'),
  help: args.includes('--help') || args.includes('-h'),
  file: args.find(arg => !arg.startsWith('--'))
};

// Show help
if (options.help || !options.file) {
  console.log(`
Usage: node scripts/print-ast.js [options] <file-or-code>

Options:
  --program      Parse as a complete program (default: expression)
  --offsets      Show token offsets
  --no-colors    Disable colored output
  --help, -h     Show this help message

Examples:
  # Parse expression from file
  node scripts/print-ast.js test.verse

  # Parse as program
  node scripts/print-ast.js --program myfile.verse

  # Parse expression directly
  node scripts/print-ast.js "1 + 2 * 3"

  # Show token offsets
  node scripts/print-ast.js --offsets "foo(bar, baz)"
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
  if (options.program) {
    ast = parseProgram(source);
    console.log('Parsed as: Program\n');
  } else {
    ast = parseExpression(source);
    console.log('Parsed as: Expression\n');
  }

  // Pretty print the AST
  const printed = prettyPrintAST(ast, {
    showOffsets: options.showOffsets,
    useColors: !options.noColors
  });

  console.log(printed);
  console.log();

} catch (error) {
  console.error('❌ Parse Error:', error.message);
  process.exit(1);
}