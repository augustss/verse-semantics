#!/usr/bin/env node

/**
 * Test runner for parseset format tests
 * Run with: node run-tests.js
 */
const fs = require('fs');
const path = require('path');
const { parse, parseTopLevel } = require('../dist/index');
const { prettyPrint } = require('../dist/ast');
// Color output
const green = (s) => `\x1b[32m${s}\x1b[0m`;
const red = (s) => `\x1b[31m${s}\x1b[0m`;
const yellow = (s) => `\x1b[33m${s}\x1b[0m`;
const blue = (s) => `\x1b[34m${s}\x1b[0m`;

// Parse parseset file format
function parseParsesetFile(content) {
  const lines = content.split('\n');
  const tests = [];
  let currentHeader = '';
  let currentComment = '';
  let currentInput = [];
  let testStartLine = 0;
  let inTest = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.startsWith('#! ')) {
      // Save previous test if exists
      if (inTest && currentInput.length > 0) {
        const [expectedResult, parserType] = parseTestHeader(currentHeader, currentComment);
        tests.push({
          description: currentComment || currentHeader,
          input: currentInput.join('\n'),
          lineNumber: testStartLine,
          parserType,
          expectedResult
        });
      }

      // Start new test
      currentHeader = line.substring(3).trim();
      currentComment = '';
      currentInput = [];
      testStartLine = i + 1;
      inTest = true;

      // Check if next line is a comment (description)
      if (i + 1 < lines.length && lines[i + 1].startsWith('# ')) {
        currentComment = lines[i + 1].substring(2).trim();
        testStartLine = i + 2;
        i++; // Skip the comment line
      }
    } else if (inTest && (!line.startsWith('#') || line.startsWith('#>'))) {
      // Continue collecting input until next test or end
      if (i === lines.length - 1 || (i < lines.length - 1 && lines[i + 1].startsWith('#! '))) {
        if (line.trim() !== '') {
          currentInput.push(line);
        }
        if (i === lines.length - 1) {
          const [expectedResult, parserType] = parseTestHeader(currentHeader, currentComment);
          tests.push({
            description: currentComment || currentHeader,
            input: currentInput.join('\n'),
            lineNumber: testStartLine,
            parserType,
            expectedResult
          });
        }
      } else {
        currentInput.push(line);
      }
    }
  }

  return tests;
}

function parseTestHeader(header, comment) {
  // Parse the header line (e.g., "#! Valid Expression" or "#! Error TopLevel")
  let expectedResult = 'Valid';
  let parserType = 'Expression';

  if (header.includes('Error')) {
    expectedResult = 'Error';
  }
  if (header.includes('TopLevel')) {
    parserType = 'TopLevel';
  }

  // Also check the comment for [TopLevel] or [Error] tags for backward compatibility
  if (comment) {
    if (comment.includes('[Error]')) {
      expectedResult = 'Error';
    }
    if (comment.includes('[TopLevel]')) {
      parserType = 'TopLevel';
    }
  }

  return [expectedResult, parserType];
}

// Run parseset tests
function runParsesetTests(filePath, testDir, showVerbose, showStrict) {
  const content = fs.readFileSync(filePath, 'utf-8');
  const tests = parseParsesetFile(content);
  const relativePath = path.relative(testDir, filePath);

  let passed = 0;
  let failed = 0;
  let reconstructionFailed = 0;
  let failedTests = [];

  // Always show file name if verbose, or only if there are failures (checked later)
  if (showVerbose) {
    console.log(`\n${yellow(relativePath)}:`);
    console.log('-'.repeat(40));
  }

  tests.forEach((test, index) => {
    const inputPreview = test.input.replace(/\n/g, '\\n').substring(0, 40);
    const testName = `Test ${index + 1}: ${inputPreview}${test.input.length > 40 ? '...' : ''}`;

    // Suppress stderr for expected error tests
    let originalStderr;
    if (test.expectedResult === 'Error') {
      originalStderr = process.stderr.write;
      process.stderr.write = () => true;
    }

    try {
      const ast = test.parserType === 'TopLevel'
        ? parseTopLevel(test.input, true)  // Always quiet to avoid spurious output
        : parse(test.input, true);          // Always quiet to avoid spurious output

      const parsed = !!ast;
      const isValid = test.expectedResult === 'Valid';

      if (parsed === isValid) {
        // Also verify reconstruction for valid cases
        if (parsed && ast) {
          try {
            const output = prettyPrint(ast);
            if (output === test.input) {
              if (showVerbose) {
                console.log(`  ${green('✅')} ${testName}`);
              }
              passed++;
            } else {
              // Reconstruction failed - only report if strict mode is enabled
              if (showStrict) {
                failedTests.push(`  ${red('❌')} ${testName} - reconstruction failed`);
                failed++;
              } else {
                // Count as passed for non-strict mode since parsing succeeded
                if (showVerbose) {
                  console.log(`  ${yellow('⚠️')} ${testName} - parsed ok, reconstruction differs`);
                }
                passed++;
                reconstructionFailed++;
              }
            }
          } catch (e) {
            // prettyPrint error - only report if strict mode is enabled
            if (showStrict) {
              failedTests.push(`  ${red('❌')} ${testName} - prettyPrint error`);
              failed++;
            } else {
              // Count as passed for non-strict mode since parsing succeeded
              if (showVerbose) {
                console.log(`  ${yellow('⚠️')} ${testName} - parsed ok, prettyPrint failed`);
              }
              passed++;
              reconstructionFailed++;
            }
          }
        } else {
          if (showVerbose) {
            console.log(`  ${green('✅')} ${testName}`);
          }
          passed++;
        }
      } else {
        failedTests.push(`  ${red('❌')} ${testName}`);
        failedTests.push(`      ${red(`Expected ${test.expectedResult}, got ${parsed ? 'Valid' : 'Error'}`)}`);
        failed++;
      }
    } catch (error) {
      if (test.expectedResult === 'Error') {
        if (showVerbose) {
          console.log(`  ${green('✅')} ${testName}`);
        }
        passed++;
      } else {
        failedTests.push(`  ${red('❌')} ${testName}`);
        failedTests.push(`      ${red('Error: ' + error.message)}`);
        failed++;
      }
    } finally {
      // Restore stderr if it was suppressed
      if (test.expectedResult === 'Error' && originalStderr) {
        process.stderr.write = originalStderr;
      }
    }
  });

  // Only show file header and failures if not verbose and there are failures
  if (!showVerbose && failedTests.length > 0) {
    console.log(`\n${yellow(relativePath)}:`);
    console.log('-'.repeat(40));
    failedTests.forEach(line => console.log(line));
  }

  return { passed, failed, reconstructionFailed };
}

// Parse command-line arguments
const args = process.argv.slice(2);
const testDirArg = args.find(a => a.startsWith('--dir='))?.split('=')[1];
const showHelp = args.includes('--help') || args.includes('-h');
const verbose = args.includes('--verbose') || args.includes('-v');
const strict = args.includes('--strict') || args.includes('-s');

// Show help if requested
if (showHelp) {
  console.log('Usage: node run-tests.js [options]');
  console.log('\nOptions:');
  console.log('  --dir=PATH        Specify directory to search for tests (default: ./tests)');
  console.log('  --verbose, -v     Show all tests (default: only show failures)');
  console.log('  --strict, -s      Show reconstruction failures (default: ignore them)');
  console.log('  --help, -h        Show this help message');
  console.log('\nExamples:');
  console.log('  node run-tests.js                       # Run tests, show only parse failures');
  console.log('  node run-tests.js --strict              # Run tests, show all failures');
  console.log('  node run-tests.js --verbose             # Run tests, show all results');
  console.log('  node run-tests.js --dir=./my-tests      # Run tests in custom directory');
  process.exit(0);
}

// Main execution
let totalPassed = 0;
let totalFailed = 0;
let totalReconstructionFailed = 0;

console.log(blue('='.repeat(70)));
console.log(blue('Parseset Test Runner'));
console.log(blue('='.repeat(70)));

// Function to recursively find all .parseset files
function findParsesetFiles(dir, fileList = []) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      // Recursively search subdirectories
      findParsesetFiles(fullPath, fileList);
    } else if (entry.isFile() && entry.name.endsWith('.parseset')) {
      fileList.push(fullPath);
    }
  }

  return fileList;
}

// Find all parseset files
const testDir = testDirArg || path.join(__dirname, '../tests');
if (!fs.existsSync(testDir)) {
  console.log(yellow(`Tests directory not found: ${testDir}`));
  process.exit(1);
}

const parsesetFiles = findParsesetFiles(testDir);
// Sort files for consistent ordering
parsesetFiles.sort();

if (parsesetFiles.length === 0) {
  console.log(yellow('No parseset files found in tests directory'));
  process.exit(1);
}

// Display total files found
console.log(`Found ${parsesetFiles.length} parseset file(s) in ${testDir}`);

// Run tests for each parseset file
parsesetFiles.forEach(file => {
  const { passed, failed, reconstructionFailed } = runParsesetTests(file, testDir, verbose, strict);
  totalPassed += passed;
  totalFailed += failed;
  totalReconstructionFailed += reconstructionFailed;
});

// Summary
console.log('\n' + blue('='.repeat(70)));
console.log(blue('Test Results Summary'));
console.log(blue('='.repeat(70)));
console.log(`  ${green('Passed:')} ${totalPassed}`);
console.log(`  ${red('Failed:')} ${totalFailed}`);
if (!strict && totalReconstructionFailed > 0) {
  console.log(`  ${yellow('Reconstruction Issues:')} ${totalReconstructionFailed} (ignored, use --strict to see them)`);
}

if (totalFailed === 0) {
  console.log(`\n${green('🎉 All tests passed!')}`);
} else {
  console.log(`\n${red('❌ Some tests failed.')}`);
  process.exit(1);
}