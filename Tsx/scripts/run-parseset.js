#!/usr/bin/env node

/**
 * Run parseset tests and report results
 */

const fs = require('fs');
const path = require('path');
const { parseExpression } = require('../dist/parser');
const { parseProgram } = require('../dist/parser/top-level-parser');

// Parse command line arguments
const args = process.argv.slice(2);
const options = {
  verbose: args.includes('--verbose'),
  showErrors: args.includes('--errors'),
  file: args.find(arg => !arg.startsWith('--'))
};

if (!options.file) {
  console.error('Usage: node scripts/run-parseset.js <parseset-file> [--verbose] [--errors]');
  process.exit(1);
}

// Read the parseset file
const content = fs.readFileSync(options.file, 'utf-8');
const lines = content.split('\n');

console.log(`\n📄 Running parseset: ${options.file}`);
console.log('═'.repeat(60));

let currentTest = null;
let testType = null;
let testNumber = 0;
let passCount = 0;
let failCount = 0;
const failures = [];

for (let i = 0; i < lines.length; i++) {
  const line = lines[i];

  // Skip empty lines and pure comments
  if (!line.trim() || (line.startsWith('#') && !line.startsWith('#!'))) {
    continue;
  }

  // Check for test directive
  if (line.startsWith('#!')) {
    // Process previous test if exists
    if (currentTest !== null) {
      testNumber++;
      const testName = `Test ${testNumber}`;

      try {
        if (testType === 'Expression') {
          parseExpression(currentTest);
        } else if (testType === 'TopLevel') {
          parseProgram(currentTest);
        }
        passCount++;
        if (options.verbose) {
          console.log(`✅ ${testName}: ${currentTest.substring(0, 50)}${currentTest.length > 50 ? '...' : ''}`);
        }
      } catch (error) {
        failCount++;
        failures.push({
          number: testNumber,
          code: currentTest,
          error: error.message,
          type: testType
        });
        if (options.showErrors || options.verbose) {
          console.log(`❌ ${testName}: ${error.message}`);
          if (options.verbose) {
            console.log(`   Code: ${currentTest.substring(0, 60)}${currentTest.length > 60 ? '...' : ''}`);
          }
        }
      }
    }

    // Parse directive
    const directive = line.substring(2).trim();
    if (directive.match(/^Valid\s+Expression/i)) {
      testType = 'Expression';
      currentTest = '';
    } else if (directive.match(/^Valid\s+Program/i)) {
      testType = 'TopLevel';
      currentTest = '';
    } else {
      // Unknown directive, skip
      currentTest = null;
      testType = null;
    }
  } else if (currentTest !== null) {
    // Accumulate test code
    if (currentTest) {
      currentTest += '\n';
    }
    currentTest += line;
  }
}

// Process last test
if (currentTest !== null) {
  testNumber++;
  const testName = `Test ${testNumber}`;

  try {
    if (testType === 'Expression') {
      parseExpression(currentTest);
    } else if (testType === 'TopLevel') {
      parseProgram(currentTest);
    }
    passCount++;
    if (options.verbose) {
      console.log(`✅ ${testName}: ${currentTest.substring(0, 50)}${currentTest.length > 50 ? '...' : ''}`);
    }
  } catch (error) {
    failCount++;
    failures.push({
      number: testNumber,
      code: currentTest,
      error: error.message,
      type: testType
    });
    if (options.showErrors || options.verbose) {
      console.log(`❌ ${testName}: ${error.message}`);
      if (options.verbose) {
        console.log(`   Code: ${currentTest.substring(0, 60)}${currentTest.length > 60 ? '...' : ''}`);
      }
    }
  }
}

// Print summary
console.log('\n' + '═'.repeat(60));
console.log('📊 Results:');
console.log(`   ✅ Passed: ${passCount}/${testNumber} (${(passCount/testNumber*100).toFixed(1)}%)`);
console.log(`   ❌ Failed: ${failCount}/${testNumber} (${(failCount/testNumber*100).toFixed(1)}%)`);

// Show failures if not in verbose mode
if (failures.length > 0 && !options.verbose && !options.showErrors) {
  console.log('\n❌ Failed tests (use --errors or --verbose for details):');
  failures.slice(0, 5).forEach(f => {
    console.log(`   Test ${f.number} (${f.type}): ${f.error}`);
  });
  if (failures.length > 5) {
    console.log(`   ... and ${failures.length - 5} more failures`);
  }
}

// Exit with appropriate code
process.exit(failCount > 0 ? 1 : 0);