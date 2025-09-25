#!/usr/bin/env node

/**
 * Test script to parse parseset files and convert to logical AST
 */

const fs = require('fs');
const path = require('path');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { simplifyProgram } = require('../dist/logical-ast');
const { printLogicalAST } = require('../dist/logical-ast');

// Parse command line arguments
const args = process.argv.slice(2);
const options = {
  verbose: args.includes('--verbose'),
  file: args.find(arg => !arg.startsWith('--'))
};

if (!options.file) {
  console.error('Usage: node scripts/test-logical-ast.js <parseset-file> [--verbose]');
  process.exit(1);
}

// Read the parseset file
const content = fs.readFileSync(options.file, 'utf-8');
const lines = content.split('\n').filter(line => line.trim());

console.log(`\n📄 Processing: ${options.file}`);
console.log(`   Total lines: ${lines.length}`);
console.log('─'.repeat(60));

let successCount = 0;
let failureCount = 0;
const failures = [];

lines.forEach((line, index) => {
  try {
    // Parse as program
    const ast = parseProgram(line);

    // Convert to logical AST
    const logical = simplifyProgram(ast);

    if (options.verbose) {
      console.log(`\n✅ Line ${index + 1}: SUCCESS`);
      console.log(`   Source: ${line.substring(0, 80)}${line.length > 80 ? '...' : ''}`);
      console.log('   Logical AST:');
      const printed = printLogicalAST(logical, { useColors: true });
      console.log(printed.split('\n').map(l => '   ' + l).join('\n'));
    }

    successCount++;
  } catch (error) {
    failureCount++;
    failures.push({
      line: index + 1,
      source: line,
      error: error.message
    });

    if (options.verbose) {
      console.log(`\n❌ Line ${index + 1}: FAILED`);
      console.log(`   Source: ${line.substring(0, 80)}${line.length > 80 ? '...' : ''}`);
      console.log(`   Error: ${error.message}`);
    }
  }
});

// Print summary
console.log('\n' + '═'.repeat(60));
console.log('Summary:');
console.log(`  ✅ Success: ${successCount} (${(successCount / lines.length * 100).toFixed(1)}%)`);
console.log(`  ❌ Failed:  ${failureCount} (${(failureCount / lines.length * 100).toFixed(1)}%)`);

// Show first few failures
if (failures.length > 0 && !options.verbose) {
  console.log('\nFirst 5 failures:');
  failures.slice(0, 5).forEach(failure => {
    console.log(`  Line ${failure.line}: ${failure.error}`);
    console.log(`    Source: ${failure.source.substring(0, 60)}${failure.source.length > 60 ? '...' : ''}`);
  });

  if (failures.length > 5) {
    console.log(`  ... and ${failures.length - 5} more failures`);
  }
}