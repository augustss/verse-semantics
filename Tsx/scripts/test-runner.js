#!/usr/bin/env node

/**
 * Test Runner for Verse Parser .parseset Files
 *
 * This script processes .parseset test files that contain Verse language test cases
 * and validates them against the parser. Each test case is marked with special comments
 * indicating whether it should parse successfully or fail:
 *
 * - #! Valid expression     - Should parse as a valid expression
 * - #! Error expression     - Should fail when parsed as an expression
 * - #! Valid Program        - Should parse as a valid top-level declaration
 * - #! Error Program        - Should fail when parsed as a top-level declaration
 *
 * The script provides detailed reporting including pass rates, failure analysis,
 * and error categorization to help identify parsing issues.
 */

const fs = require('fs');
const path = require('path');
const { parseExpression, parseProgram, lex, reconstructProgramFromAST, reconstructFromAST, TokenStream } = require('../dist');

/**
 * Recursively find all .parseset files in a directory
 * @param {string} dir - Directory to search
 * @returns {string[]} - Array of .parseset file paths
 */
function findParsesetFiles(dir) {
  const parsesetFiles = [];
  const entries = fs.readdirSync(dir);

  for (const entry of entries) {
    const fullPath = path.join(dir, entry);
    const stat = fs.statSync(fullPath);

    if (stat.isDirectory()) {
      // Recursively search subdirectories
      parsesetFiles.push(...findParsesetFiles(fullPath));
    } else if (entry.endsWith('.parseset')) {
      parsesetFiles.push(fullPath);
    }
  }

  return parsesetFiles;
}

/**
 * Main test runner function that processes multiple .parseset files or directories
 * @param {string[]} filePaths - Array of file paths or directory paths to process
 * @param {Object} options - Command line options (quiet, verbose)
 */
function runTests(filePaths, options = {}) {
  if (!filePaths || filePaths.length === 0) {
    console.error('Usage: node test-runner.js [options] <parseset-file-or-directory>');
    console.error('Options:');
    console.error('  --quiet, -q           Only show summary, hide individual failures');
    console.error('  --verbose, -v         Show detailed failure analysis');
    console.error('  --reconstruct, -r     Test AST reconstruction for each test');
    console.error('  --help, -h            Show this help message');
    process.exit(1);
  }

  let totalTests = 0;
  let totalPassed = 0;
  let totalReconstructed = 0;
  let totalReconstructionMatches = 0;
  const results = [];

  // Process each file path - can be individual .parseset files or directories
  filePaths.forEach(filePath => {
    if (fs.statSync(filePath).isDirectory()) {
      // If directory, find all .parseset files recursively
      const parsesetFiles = findParsesetFiles(filePath);

      parsesetFiles.forEach(file => {
        const result = runParsesetFile(file, options);
        results.push(result);
        totalTests += result.total;
        totalPassed += result.passed;
        if (options.reconstruct) {
          totalReconstructed += result.reconstructed || 0;
          totalReconstructionMatches += result.reconstructionMatches || 0;
        }
      });
    } else if (filePath.endsWith('.parseset')) {
      // Single parseset file
      const result = runParsesetFile(filePath, options);
      results.push(result);
      totalTests += result.total;
      totalPassed += result.passed;
      if (options.reconstruct) {
        totalReconstructed += result.reconstructed || 0;
        totalReconstructionMatches += result.reconstructionMatches || 0;
      }
    } else {
      console.error(`Error: ${filePath} is not a .parseset file or directory`);
      process.exit(1);
    }
  });

  // Print formatted test results summary
  console.log('\n' + '═'.repeat(80));
  console.log('📊 TEST RESULTS');
  console.log('═'.repeat(80));

  // Display results for each file with pass rate
  results.forEach(result => {
    const fileName = path.basename(result.file);
    const rate = result.total > 0 ? (result.passed / result.total * 100).toFixed(1) : '0.0';
    const status = result.passed === result.total ? '✅' : '❌';
    console.log(`${status} ${fileName.padEnd(28)} ${result.passed.toString().padStart(4)}/${result.total.toString().padEnd(4)} (${rate}%)`);
  });

  console.log('─'.repeat(80));
  // Display overall totals
  const overallRate = totalTests > 0 ? (totalPassed / totalTests * 100).toFixed(1) : '0.0';
  const overallStatus = totalPassed === totalTests ? '✅' : '❌';
  console.log(`${overallStatus} ${'TOTAL'.padEnd(28)} ${totalPassed.toString().padStart(4)}/${totalTests.toString().padEnd(4)} (${overallRate}%)`);

  // Display reconstruction statistics if enabled
  if (options.reconstruct) {
    if (totalReconstructed > 0) {
      const reconstructRate = (totalReconstructionMatches / totalReconstructed * 100).toFixed(1);
      const reconstructStatus = totalReconstructionMatches === totalReconstructed ? '✅' : '⚠️';
      console.log(`${reconstructStatus} ${'RECONSTRUCTION'.padEnd(28)} ${totalReconstructionMatches.toString().padStart(4)}/${totalReconstructed.toString().padEnd(4)} (${reconstructRate}% perfect matches)`);

    } else {
      console.log(`⚠️  ${'RECONSTRUCTION'.padEnd(28)} No valid tests to reconstruct`);
    }
  }

  console.log('═'.repeat(80));

  // Show detailed failure analysis if there are failures and not in quiet mode
  if (totalPassed < totalTests && (options.verbose || !options.quiet)) {
    console.log('\n' + '📋 FAILURE ANALYSIS');
    console.log('═'.repeat(80));

    // Collect all failures from all test files
    const allFailures = results.flatMap(r => r.failures || []);
    const errorCounts = {};
    const errorExamples = {};

    // Group failures by error type and collect examples
    allFailures.forEach(failure => {
      const errorKey = failure.error.split(':')[0].trim();
      if (!errorCounts[errorKey]) {
        errorCounts[errorKey] = 0;
        errorExamples[errorKey] = failure.testCode.replace(/\n/g, '\\n').substring(0, 40) + '...';
      }
      errorCounts[errorKey]++;
    });

    // Show top 10 most common parsing errors with examples
    const sortedErrors = Object.entries(errorCounts)
      .sort(([, a], [, b]) => b - a)
      .slice(0, 10); // Top 10 most common errors

    console.log('Most common parsing errors:');
    sortedErrors.forEach(([error, count], index) => {
      const example = errorExamples[error];
      console.log(`${(index + 1).toString().padStart(2)}. ${error.padEnd(35)} (${count} cases)`);
      console.log(`    Example: ${example}`);
    });

    // Show failure breakdown by test category
    console.log('\nFailure breakdown by category:');
    const failuresByType = {};
    allFailures.forEach(failure => {
      const key = `${failure.expectation} ${failure.testType}`;
      if (!failuresByType[key]) failuresByType[key] = 0;
      failuresByType[key]++;
    });

    Object.entries(failuresByType)
      .sort(([, a], [, b]) => b - a)
      .forEach(([type, count]) => {
        console.log(`  ${type}: ${count} failures`);
      });

    console.log('═'.repeat(80));
  }

  // Exit with error code if any tests failed (0 = success, 1 = failure)
  process.exit(totalPassed === totalTests ? 0 : 1);
}

/**
 * Processes a single .parseset file and runs all tests within it
 * @param {string} filePath - Path to the .parseset file
 * @param {Object} options - Command line options (quiet, verbose)
 * @returns {Object} Test results with passed/total counts and failure details
 */
function runParsesetFile(filePath, options = {}) {
  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');

  let passed = 0;
  let total = 0;
  let reconstructed = 0;
  let reconstructionMatches = 0;
  let expectError = false;        // Whether current test should fail
  let expectTopLevel = false;     // Whether to use top-level parser vs expression parser
  let currentTestLines = [];      // Lines of code for current test case
  let currentLineStart = 0;       // Starting line number for current test
  let currentTestNumber = null;   // Test number from comment (e.g., "12" from "# 12. ...")
  let failures = [];              // Failed test cases
  let successes = [];             // Successful test cases

  // Parse each line of the .parseset file
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    // Skip empty lines and regular comments (but not test markers)
    if (!line || (line.startsWith('#') && !line.startsWith('#!'))) {
      continue;
    }

    // Test markers - these indicate the start of a new test case
    if (line.startsWith('#! Error expression')) {
      // Process previous test if we have accumulated test lines
      if (currentTestLines.length > 0) {
        const result = runSingleTest(currentTestLines.join('\n'), expectError, expectTopLevel, currentLineStart, path.basename(filePath), options, currentTestNumber);
        total++;
        if (result.passed) {
          passed++;
          successes.push(result);
          // Track reconstruction results
          if (options.reconstruct && !expectError) {
            reconstructed++;
            if (result.reconstructionMatch) {
              reconstructionMatches++;
            }
          }
        } else {
          failures.push(result);
        }
      }

      // Start new test: expect expression parsing to fail
      expectError = true;
      expectTopLevel = false;
      currentTestLines = [];
      currentLineStart = i + 1;
      currentTestNumber = null;  // Reset test number for new test

      // Look for test number on next line
      if (i + 1 < lines.length) {
        const nextLine = lines[i + 1].trim();
        const match = nextLine.match(/^#\s+(\d+)\./);  // Match "# 12." pattern
        if (match) {
          currentTestNumber = match[1];
        }
      }
      continue;
    } else if (line.startsWith('#! Valid expression')) {
      // Process previous test if we have accumulated test lines
      if (currentTestLines.length > 0) {
        const result = runSingleTest(currentTestLines.join('\n'), expectError, expectTopLevel, currentLineStart, path.basename(filePath), options, currentTestNumber);
        total++;
        if (result.passed) {
          passed++;
          successes.push(result);
          // Track reconstruction results
          if (options.reconstruct && !expectError) {
            reconstructed++;
            if (result.reconstructionMatch) {
              reconstructionMatches++;
            }
          }
        } else {
          failures.push(result);
        }
      }

      // Start new test: expect expression parsing to succeed
      expectError = false;
      expectTopLevel = false;
      currentTestLines = [];
      currentLineStart = i + 1;
      currentTestNumber = null;  // Reset test number for new test

      // Look for test number on next line
      if (i + 1 < lines.length) {
        const nextLine = lines[i + 1].trim();
        const match = nextLine.match(/^#\s+(\d+)\./);  // Match "# 12." pattern
        if (match) {
          currentTestNumber = match[1];
        }
      }
      continue;
    } else if (line.match(/^#!\s+Error\s+Program/i)) {
      // Process previous test if we have accumulated test lines
      if (currentTestLines.length > 0) {
        const result = runSingleTest(currentTestLines.join('\n'), expectError, expectTopLevel, currentLineStart, path.basename(filePath), options, currentTestNumber);
        total++;
        if (result.passed) {
          passed++;
          successes.push(result);
          // Track reconstruction results
          if (options.reconstruct && !expectError) {
            reconstructed++;
            if (result.reconstructionMatch) {
              reconstructionMatches++;
            }
          }
        } else {
          failures.push(result);
        }
      }

      // Start new test: expect top-level parsing to fail
      expectError = true;
      expectTopLevel = true;
      currentTestLines = [];
      currentLineStart = i + 1;
      currentTestNumber = null;  // Reset test number for new test

      // Look for test number on next line
      if (i + 1 < lines.length) {
        const nextLine = lines[i + 1].trim();
        const match = nextLine.match(/^#\s+(\d+)\./);  // Match "# 12." pattern
        if (match) {
          currentTestNumber = match[1];
        }
      }
      continue;
    } else if (line.match(/^#!\s+Valid\s+Program/i)) {
      // Process previous test if we have accumulated test lines
      if (currentTestLines.length > 0) {
        const result = runSingleTest(currentTestLines.join('\n'), expectError, expectTopLevel, currentLineStart, path.basename(filePath), options, currentTestNumber);
        total++;
        if (result.passed) {
          passed++;
          successes.push(result);
          // Track reconstruction results
          if (options.reconstruct && !expectError) {
            reconstructed++;
            if (result.reconstructionMatch) {
              reconstructionMatches++;
            }
          }
        } else {
          failures.push(result);
        }
      }

      // Start new test: expect top-level parsing to succeed
      expectError = false;
      expectTopLevel = true;
      currentTestLines = [];
      currentLineStart = i + 1;
      currentTestNumber = null;  // Reset test number for new test

      // Look for test number on next line
      if (i + 1 < lines.length) {
        const nextLine = lines[i + 1].trim();
        const match = nextLine.match(/^#\s+(\d+)\./);  // Match "# 12." pattern
        if (match) {
          currentTestNumber = match[1];
        }
      }
      continue;
    }

    // Skip test separator lines (optional formatting)
    if (line.startsWith('---')) {
      continue;
    }

    // This line is part of the current test case
    if (currentTestLines.length > 0 || line.trim()) { // Only start collecting if we have a non-empty line
      currentTestLines.push(lines[i]); // Use original line with indentation preserved
    }
  }

  // Process the final test case if we have accumulated test lines
  if (currentTestLines.length > 0) {
    const result = runSingleTest(currentTestLines.join('\n'), expectError, expectTopLevel, currentLineStart, path.basename(filePath), options, currentTestNumber);
    total++;
    if (result.passed) {
      passed++;
      successes.push(result);
      // Track reconstruction results
      if (options.reconstruct && !expectError) {
        reconstructed++;
        if (result.reconstructionMatch) {
          reconstructionMatches++;
        }
      }
    } else {
      failures.push(result);
    }
  }

  // Return summary of test results for this file
  return {
    file: filePath,
    passed: passed,
    total: total,
    failures: failures,
    successes: successes,
    reconstructed: reconstructed,
    reconstructionMatches: reconstructionMatches
  };
}

/**
 * Runs a single test case by attempting to parse the code
 * @param {string} testCode - The Verse code to parse
 * @param {boolean} expectError - Whether this test should fail parsing
 * @param {boolean} expectTopLevel - Whether to use top-level parser (vs expression parser)
 * @param {number} lineStart - Starting line number in source file for error reporting
 * @param {string} fileName - Name of the .parseset file for error reporting
 * @param {Object} options - Command line options (quiet, verbose)
 * @param {string|null} testNumber - Test number extracted from comment (e.g., "12")
 * @returns {Object} Test result with pass/fail status and error details
 */
function runSingleTest(testCode, expectError, expectTopLevel, lineStart, fileName, options = {}, testNumber = null) {
  const testType = expectTopLevel ? 'TopLevel' : 'Expression';
  const expectation = expectError ? 'Error' : 'Valid';

  try {
    let ast;
    if (expectTopLevel) {
      // Parse as a program
      // For top-level tests, we consider the parse successful if the program parses
      // (even if it only contains using statements and no declarations)
      ast = parseProgram(testCode);
    } else {
      ast = parseExpression(testCode);
    }

    // Parsing succeeded - check if this matches expectations
    if (expectError) {
      // Test expected to fail but parsing succeeded - this is a test failure
      const displayCode = testCode.replace(/\n/g, '\\n');
      const truncatedCode = displayCode.length > 60 ? displayCode.substring(0, 60) + '...' : displayCode;
      if (!options.quiet) {
        const testId = testNumber ? `Test #${testNumber}` : `Line ${lineStart}`;
        console.log(`❌ FAIL [${fileName}:${testId}] Expected ${expectation} ${testType} but parsed successfully:`);
        console.log(`   Code: ${truncatedCode}`);
      }
      return {
        passed: false,
        lineStart,
        testNumber,
        testCode,
        error: `Expected error but parsed successfully`,
        testType,
        expectation
      };
    } else {
      // Test expected to succeed and parsing succeeded
      // If reconstruct option is enabled, also test reconstruction
      let reconstructionMatch = true;
      let reconstructed = '';
      if (options.reconstruct && !expectError) {
        try {
          // Use ultra-fast reconstructor with TokenStream caching
          reconstructed = reconstructFromAST(testCode, ast, {
            includeTrailingTrivia: true
          });
          reconstructionMatch = testCode.trim() === reconstructed.trim();
        } catch (reconstructError) {
          reconstructionMatch = false;
          reconstructed = `[Reconstruction error: ${reconstructError.message}]`;
        }

        if (!reconstructionMatch && !options.quiet) {
          const testId = testNumber ? `Test #${testNumber}` : `Line ${lineStart}`;
          console.log(`⚠️  RECONSTRUCTION MISMATCH [${fileName}:${testId}]`);
          if (testCode.length < 100) {
            console.log(`   Original:      '${testCode.trim().replace(/\n/g, '\\n')}'`);
            console.log(`   Reconstructed: '${reconstructed.trim().replace(/\n/g, '\\n')}'`);
          } else {
            console.log(`   Code length: ${testCode.length} chars`);
            console.log(`   Reconstructed length: ${reconstructed.length} chars`);
          }
        }
      }

      return {
        passed: true,
        lineStart,
        testNumber,
        testCode,
        testType,
        expectation,
        reconstructionMatch,
        reconstructed
      };
    }
  } catch (error) {
    // Parsing failed with an error - check if this matches expectations
    if (!expectError) {
      // Test expected to succeed but parsing failed - this is a test failure
      const displayCode = testCode.replace(/\n/g, '\\n');
      const truncatedCode = displayCode.length > 60 ? displayCode.substring(0, 60) + '...' : displayCode;
      if (!options.quiet) {
        const testId = testNumber ? `Test #${testNumber}` : `Line ${lineStart}`;
        console.log(`❌ FAIL [${fileName}:${testId}] Expected ${expectation} ${testType} but got error:`);
        console.log(`   Code: ${truncatedCode}`);
        console.log(`   Error: ${error.message}`);

        // Show error location if available
        if (error.token && error.token.position) {
          const lines = testCode.split('\n');
          const lineNum = error.token.position.line;
          const colNum = error.token.position.column;

          if (lineNum > 0 && lineNum <= lines.length) {
            console.log(`   Location: Line ${lineNum}, Column ${colNum}`);
            console.log(`   Context: ${lines[lineNum - 1]}`);
            console.log(`            ${' '.repeat(colNum - 1)}^ ${error.token.type}: "${error.token.content}"`);
          }
        } else if (error.position !== undefined && error.position >= 0) {
          // Fallback to character offset if no token info
          const lines = testCode.split('\n');
          let charCount = 0;
          for (let i = 0; i < lines.length; i++) {
            const lineLength = lines[i].length;
            if (charCount + lineLength >= error.position) {
              const posInLine = error.position - charCount;
              console.log(`   Location: Line ${i + 1}, Column ${posInLine + 1}`);
              console.log(`   Context: ${lines[i]}`);
              console.log(`            ${' '.repeat(posInLine)}^`);
              break;
            }
            charCount += lineLength + 1; // +1 for newline
          }
        }
      }
      return {
        passed: false,
        lineStart,
        testNumber,
        testCode,
        error: error.message,
        testType,
        expectation
      };
    } else {
      // Test expected to fail and parsing failed - this is a test pass
      return {
        passed: true,
        lineStart,
        testNumber,
        testCode,
        error: error.message,
        testType,
        expectation
      };
    }
  }
}

// Parse command line arguments and extract options vs file paths
const args = process.argv.slice(2);
const options = {};
const filePaths = [];

for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === '--quiet' || arg === '-q') {
    options.quiet = true;
  } else if (arg === '--verbose' || arg === '-v') {
    options.verbose = true;
  } else if (arg === '--reconstruct' || arg === '-r') {
    options.reconstruct = true;
  } else if (arg === '--help' || arg === '-h') {
    console.log('Usage: node test-runner.js [options] <parseset-file-or-directory>');
    console.log('Options:');
    console.log('  --quiet, -q           Only show summary, hide individual failures');
    console.log('  --verbose, -v         Show detailed failure analysis');
    console.log('  --reconstruct, -r     Test AST reconstruction for each test');
    console.log('  --help, -h            Show this help message');
    process.exit(0);
  } else if (!arg.startsWith('-')) {
    filePaths.push(arg);
  }
}

// Run the tests with parsed arguments
runTests(filePaths, options);