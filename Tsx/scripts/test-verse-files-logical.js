#!/usr/bin/env node

/**
 * Test script to parse all Verse files and convert to logical AST
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
  showErrors: args.includes('--errors'),
  showSuccess: args.includes('--success'),
  limit: parseInt(args.find(arg => arg.startsWith('--limit='))?.split('=')[1] || '0'),
  dir: args.find(arg => !arg.startsWith('--')) || 'verse-files-flat'
};

// Get all .verse files
const verseDir = path.resolve(options.dir);
if (!fs.existsSync(verseDir)) {
  console.error(`Directory not found: ${verseDir}`);
  process.exit(1);
}

const files = fs.readdirSync(verseDir)
  .filter(f => f.endsWith('.verse'))
  .sort();

console.log(`\n📁 Processing Verse files in: ${options.dir}`);
console.log(`   Total files: ${files.length}`);
if (options.limit) {
  console.log(`   Limiting to: ${options.limit} files`);
}
console.log('═'.repeat(80));

let successCount = 0;
let failureCount = 0;
const failures = [];
const successes = [];

const filesToProcess = options.limit ? files.slice(0, options.limit) : files;

filesToProcess.forEach((file, index) => {
  const filePath = path.join(verseDir, file);
  const content = fs.readFileSync(filePath, 'utf-8');

  try {
    // Parse as program
    const ast = parseProgram(content);

    // Convert to logical AST
    const logical = simplifyProgram(ast);

    successCount++;
    successes.push({
      file,
      lines: content.split('\n').length,
      declarations: logical.declarations?.length || 0
    });

    if (options.showSuccess) {
      console.log(`✅ ${file}`);
      if (options.verbose) {
        const printed = printLogicalAST(logical, { useColors: true });
        console.log(printed.split('\n').map(l => '   ' + l).join('\n'));
        console.log();
      }
    }
  } catch (error) {
    failureCount++;
    failures.push({
      file,
      error: error.message,
      lines: content.split('\n').length
    });

    if (options.showErrors) {
      console.log(`❌ ${file}`);
      console.log(`   Error: ${error.message}`);
      if (options.verbose && error.stack) {
        console.log(`   Stack: ${error.stack.split('\n')[1]}`);
      }
    }
  }

  // Progress indicator
  if ((index + 1) % 10 === 0 && !options.showSuccess && !options.showErrors) {
    process.stdout.write(`\rProcessed: ${index + 1}/${filesToProcess.length}`);
  }
});

if (!options.showSuccess && !options.showErrors) {
  console.log(); // Clear the progress line
}

// Print summary
console.log('\n' + '═'.repeat(80));
console.log('📊 Summary:');
console.log(`  ✅ Success: ${successCount}/${filesToProcess.length} (${(successCount / filesToProcess.length * 100).toFixed(1)}%)`);
console.log(`  ❌ Failed:  ${failureCount}/${filesToProcess.length} (${(failureCount / filesToProcess.length * 100).toFixed(1)}%)`);

// Show statistics for successes
if (successes.length > 0) {
  const totalLines = successes.reduce((sum, s) => sum + s.lines, 0);
  const totalDecls = successes.reduce((sum, s) => sum + s.declarations, 0);
  console.log(`\n📈 Success Statistics:`);
  console.log(`  Total lines parsed: ${totalLines.toLocaleString()}`);
  console.log(`  Total declarations: ${totalDecls.toLocaleString()}`);
  console.log(`  Average file size: ${Math.round(totalLines / successes.length)} lines`);
  console.log(`  Average declarations per file: ${Math.round(totalDecls / successes.length)}`);
}

// Show failure categories
if (failures.length > 0) {
  console.log(`\n⚠️  Failure Analysis:`);
  const errorCategories = {};
  failures.forEach(f => {
    // Categorize errors
    let category = 'Other';
    if (f.error.includes('Expected')) category = 'Parse Error';
    else if (f.error.includes('Unknown node type')) category = 'Unknown Node Type';
    else if (f.error.includes('Cannot read')) category = 'Runtime Error';
    else if (f.error.includes('Unexpected')) category = 'Unexpected Token';

    errorCategories[category] = (errorCategories[category] || 0) + 1;
  });

  Object.entries(errorCategories)
    .sort(([,a], [,b]) => b - a)
    .forEach(([category, count]) => {
      console.log(`  ${category}: ${count} (${(count / failures.length * 100).toFixed(1)}%)`);
    });

  // Show first few failures
  console.log(`\n🔍 Sample Failures (first 5):`);
  failures.slice(0, 5).forEach(failure => {
    console.log(`  ${failure.file}`);
    console.log(`    Error: ${failure.error.substring(0, 100)}${failure.error.length > 100 ? '...' : ''}`);
  });

  if (failures.length > 5) {
    console.log(`  ... and ${failures.length - 5} more failures`);
  }
}

// List all failed files for easy reference
if (failures.length > 0 && failures.length <= 20) {
  console.log(`\n📝 All Failed Files:`);
  failures.forEach(f => console.log(`  - ${f.file}`));
} else if (failures.length > 20) {
  console.log(`\n📝 Failed Files: (too many to list, use --errors to see all)`);
}