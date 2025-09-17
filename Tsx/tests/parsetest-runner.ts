import * as fs from 'fs';
import * as path from 'path';
import { parseVersee } from '../src/parser/parser';

interface ParseTest {
  expectSuccess: boolean;
  code: string;
  lineNumber: number;
  description?: string;
}

/**
 * Parse a .parsetest file and extract test cases
 */
function parseTestFile(filepath: string): ParseTest[] {
  const content = fs.readFileSync(filepath, 'utf-8');
  const lines = content.split('\n');
  const tests: ParseTest[] = [];

  let currentTest: ParseTest | null = null;
  let codeLines: string[] = [];
  let testStartLine = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.startsWith('#! Valid') || line.startsWith('#! Error')) {
      // Save previous test if exists
      if (currentTest !== null && codeLines.length > 0) {
        currentTest.code = codeLines.join('\n').trim();
        if (currentTest.code) {
          tests.push(currentTest);
        }
      }

      // Start new test
      const expectSuccess = line.startsWith('#! Valid');
      testStartLine = i + 1;
      currentTest = {
        expectSuccess,
        code: '',
        lineNumber: testStartLine,
        description: undefined
      };
      codeLines = [];

      // Check if next line is a comment with description
      if (i + 1 < lines.length && lines[i + 1].startsWith('#') && !lines[i + 1].startsWith('#!')) {
        currentTest.description = lines[i + 1].substring(1).trim();
      }
    } else if (!line.startsWith('#!') && currentTest !== null) {
      // Skip description comments (single # at start)
      if (!(line.startsWith('#') && codeLines.length === 0)) {
        codeLines.push(line);
      }
    }
  }

  // Don't forget the last test
  if (currentTest !== null && codeLines.length > 0) {
    currentTest.code = codeLines.join('\n').trim();
    if (currentTest.code) {
      tests.push(currentTest);
    }
  }

  return tests;
}

/**
 * Run all tests in a .parsetest file
 */
function runTestFile(filepath: string): void {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Running tests from: ${path.basename(filepath)}`);
  console.log('='.repeat(60));

  const tests = parseTestFile(filepath);
  let passed = 0;
  let failed = 0;
  const failures: Array<{test: ParseTest, error?: string}> = [];

  tests.forEach((test, index) => {
    const result = parseVersee(test.code);
    const testPassed = result.success === test.expectSuccess;

    if (testPassed) {
      passed++;
      console.log(`✓ Test ${index + 1} (line ${test.lineNumber}): ${test.expectSuccess ? 'Valid' : 'Error'}`);
      if (test.description) {
        console.log(`  ${test.description}`);
      }
    } else {
      failed++;
      console.log(`✗ Test ${index + 1} (line ${test.lineNumber}): ${test.expectSuccess ? 'Valid' : 'Error'} - FAILED`);
      if (test.description) {
        console.log(`  ${test.description}`);
      }

      failures.push({
        test,
        error: result.success ? undefined : result.error.message
      });
    }
  });

  // Summary
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Results: ${passed} passed, ${failed} failed out of ${tests.length} tests`);
  console.log(`Pass rate: ${((passed / tests.length) * 100).toFixed(1)}%`);

  // Show failures in detail
  if (failures.length > 0) {
    console.log(`\n${'='.repeat(60)}`);
    console.log('Failed Tests:');
    console.log('='.repeat(60));

    failures.forEach(({ test, error }, index) => {
      console.log(`\n${index + 1}. Line ${test.lineNumber} - Expected ${test.expectSuccess ? 'success' : 'error'} but got ${test.expectSuccess ? 'error' : 'success'}`);
      if (test.description) {
        console.log(`   Description: ${test.description}`);
      }
      console.log('   Code:');
      test.code.split('\n').forEach(line => console.log(`     ${line}`));
      if (error) {
        console.log(`   Error: ${error}`);
      }
    });
  }

  // Exit with error code if tests failed
  if (failed > 0) {
    process.exit(1);
  }
}

/**
 * Run all .parsetest files in a directory
 */
function runAllTests(directory: string): void {
  const files = fs.readdirSync(directory).filter(f => f.endsWith('.parsetest'));

  if (files.length === 0) {
    console.log('No .parsetest files found in', directory);
    return;
  }

  let totalFiles = 0;

  files.forEach(file => {
    const filepath = path.join(directory, file);
    totalFiles++;
    try {
      runTestFile(filepath);
    } catch (error) {
      console.error(`Error running test file ${file}:`, error);
    }
  });

  console.log(`\nRan ${totalFiles} test file(s)`);
}

// Main execution
if (require.main === module) {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    // Run all tests in the tests directory
    const testsDir = path.join(__dirname);
    runAllTests(testsDir);
  } else {
    // Run specific test file
    const testFile = args[0];
    if (!fs.existsSync(testFile)) {
      console.error(`Test file not found: ${testFile}`);
      process.exit(1);
    }
    runTestFile(testFile);
  }
}

export { parseTestFile, runTestFile, runAllTests };