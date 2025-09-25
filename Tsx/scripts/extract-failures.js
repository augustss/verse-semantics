#!/usr/bin/env node
/**
 * Extract failing tests from parseset files and create a focused test file
 */

const fs = require('fs');
const path = require('path');
const { parseExpression, parseProgram, lex, reconstructProgramFromAST, reconstructFromAST, TokenStream } = require('../dist');

/**
 * Recursively find all .parseset files in a directory
 */
function findParsesetFiles(dir) {
  const parsesetFiles = [];
  const entries = fs.readdirSync(dir);

  for (const entry of entries) {
    const fullPath = path.join(dir, entry);
    const stat = fs.statSync(fullPath);

    if (stat.isDirectory()) {
      parsesetFiles.push(...findParsesetFiles(fullPath));
    } else if (entry.endsWith('.parseset')) {
      parsesetFiles.push(fullPath);
    }
  }

  return parsesetFiles;
}

/**
 * Extract failures from a single parseset file
 */
function extractFailures(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');
  const failures = [];

  let currentTest = '';
  let testType = null;
  let testNumber = 0;
  let lineNumber = 0;

  for (const line of lines) {
    lineNumber++;

    if (line.startsWith('#!')) {
      // Process previous test if we have one
      if (currentTest.trim() && testType) {
        testNumber++;
        const testResult = testSingle(currentTest, testType, testNumber, path.basename(filePath));
        if (!testResult.passed) {
          failures.push({
            file: path.basename(filePath),
            testNumber,
            testType,
            code: currentTest,
            error: testResult.error,
            lineNumber: lineNumber - currentTest.split('\n').length
          });
        }
      }

      // Parse new directive
      const directive = line.substring(2).trim();
      if (directive.match(/^Valid\s+Expression/i)) {
        testType = 'Expression';
        currentTest = '';
      } else if (directive.match(/^Valid\s+Program/i)) {
        testType = 'TopLevel';
        currentTest = '';
      } else if (directive.match(/^Error\s+Expression/i)) {
        testType = 'ErrorExpression';
        currentTest = '';
      } else if (directive.match(/^Error\s+Program/i)) {
        testType = 'ErrorProgram';
        currentTest = '';
      } else {
        testType = null;
        currentTest = '';
      }
    } else if (testType !== null && line.trim() && !line.startsWith('#')) {
      if (currentTest) currentTest += '\n';
      currentTest += line;
    }
  }

  // Process final test
  if (currentTest.trim() && testType) {
    testNumber++;
    const testResult = testSingle(currentTest, testType, testNumber, path.basename(filePath));
    if (!testResult.passed) {
      failures.push({
        file: path.basename(filePath),
        testNumber,
        testType,
        code: currentTest,
        error: testResult.error,
        lineNumber: lineNumber - currentTest.split('\n').length
      });
    }
  }

  return failures;
}

/**
 * Test a single code snippet
 */
function testSingle(code, testType, testNumber, fileName) {
  try {
    if (testType === 'Expression' || testType === 'ErrorExpression') {
      parseExpression(code);
    } else if (testType === 'TopLevel' || testType === 'ErrorProgram') {
      parseProgram(code);
    }

    // For error tests, success means failure (expected to throw)
    if (testType === 'ErrorExpression' || testType === 'ErrorProgram') {
      return {
        passed: false,
        error: 'Expected parsing to fail but it succeeded'
      };
    }

    return { passed: true };
  } catch (error) {
    // For error tests, exception means success
    if (testType === 'ErrorExpression' || testType === 'ErrorProgram') {
      return { passed: true };
    }

    return {
      passed: false,
      error: error.message
    };
  }
}

/**
 * Main function
 */
function main() {
  const testsDir = 'tests';
  const parsesetFiles = findParsesetFiles(testsDir);

  console.log(`Found ${parsesetFiles.length} parseset files`);

  let allFailures = [];
  let totalTests = 0;
  let totalFailures = 0;

  for (const file of parsesetFiles) {
    console.log(`Processing ${path.basename(file)}...`);
    const failures = extractFailures(file);
    allFailures = allFailures.concat(failures);
    totalFailures += failures.length;
  }

  console.log(`\nFound ${totalFailures} failing tests across ${parsesetFiles.length} files`);

  // Group failures by type
  const failuresByType = {};
  for (const failure of allFailures) {
    const key = failure.testType || 'Unknown';
    if (!failuresByType[key]) failuresByType[key] = [];
    failuresByType[key].push(failure);
  }

  // Create output content
  let output = '# Failing Tests - Extracted for Analysis\n';
  output += `# Generated: ${new Date().toISOString()}\n`;
  output += `# Total failures: ${totalFailures}\n\n`;

  for (const [type, failures] of Object.entries(failuresByType)) {
    output += `# ===== ${type.toUpperCase()} FAILURES (${failures.length}) =====\n\n`;

    for (let i = 0; i < failures.length; i++) {
      const failure = failures[i];
      const header = type === 'ErrorExpression' ? 'Error Expression' :
                    type === 'ErrorProgram' ? 'Error Program' :
                    type === 'Expression' ? 'Valid Expression' : 'Valid Program';

      output += `#! ${header}\n`;
      output += `# Source: ${failure.file}:${failure.lineNumber} (Test ${failure.testNumber})\n`;
      output += `# Error: ${failure.error}\n`;
      output += failure.code + '\n\n';
    }
  }

  // Write to file
  const outputFile = 'tests/failing-tests.parseset';
  fs.writeFileSync(outputFile, output);
  console.log(`\nCreated ${outputFile} with ${totalFailures} failing tests`);

  // Summary
  console.log('\nFailure breakdown:');
  for (const [type, failures] of Object.entries(failuresByType)) {
    console.log(`  ${type}: ${failures.length}`);
  }
}

if (require.main === module) {
  main();
}