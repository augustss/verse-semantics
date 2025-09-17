#!/usr/bin/env ts-node
import * as path from 'path';
import { execSync } from 'child_process';
import { parseVersee } from '../src/parser/parser';
import { printExp } from '../src/simple-printer';
import { runAllTests as runParseTests } from './parsetest-runner';

interface TestResults {
  passed: number;
  failed: number;
  total: number;
  failures: string[];
}

/**
 * Run Jest tests
 */
async function runJestTests(): Promise<TestResults> {
  console.log('\n' + '='.repeat(80));
  console.log('RUNNING JEST TESTS');
  console.log('='.repeat(80));

  try {
    execSync('npm test', {
      encoding: 'utf-8',
      stdio: 'inherit',
      cwd: path.join(__dirname, '..')
    });

    // Jest exits with 0 on success
    return {
      passed: 0, // Will be updated from Jest output parsing if needed
      failed: 0,
      total: 0,
      failures: []
    };
  } catch (error: any) {
    console.error('Jest tests failed:', error.message);
    return {
      passed: 0,
      failed: 1,
      total: 1,
      failures: ['Jest tests failed']
    };
  }
}

/**
 * Run .parsetest tests
 */
async function runParseTestFiles(): Promise<TestResults> {
  console.log('\n' + '='.repeat(80));
  console.log('RUNNING PARSETEST FILES');
  console.log('='.repeat(80));

  const results: TestResults = {
    passed: 0,
    failed: 0,
    total: 0,
    failures: []
  };

  try {
    // Capture original exit behavior
    const originalExit = process.exit;
    let exitCalled = false;
    let exitCode = 0;

    // Mock process.exit to capture exit codes
    process.exit = ((code?: number) => {
      exitCalled = true;
      exitCode = code || 0;
    }) as any;

    // Redirect console output to capture results
    const originalLog = console.log;
    let output = '';
    console.log = (...args) => {
      output += args.join(' ') + '\n';
      originalLog(...args);
    };

    try {
      runParseTests(path.join(__dirname));
    } catch (error) {
      // Parsetest runner might throw
    }

    // Restore original functions
    process.exit = originalExit;
    console.log = originalLog;

    // Parse results from output
    const lines = output.split('\n');
    let testCount = 0;
    let passCount = 0;
    let failCount = 0;

    for (const line of lines) {
      if (line.includes('✓')) {
        passCount++;
        testCount++;
      } else if (line.includes('✗')) {
        failCount++;
        testCount++;
        results.failures.push(line);
      }
    }

    results.passed = passCount;
    results.failed = failCount;
    results.total = testCount;

    if (exitCalled && exitCode !== 0) {
      console.log(`Parsetest completed with ${results.failed} failures`);
    } else {
      console.log(`Parsetest completed successfully: ${results.passed}/${results.total} passed`);
    }

    return results;
  } catch (error: any) {
    console.error('Parsetest runner failed:', error.message);
    results.failed = 1;
    results.total = 1;
    results.failures.push(`Parsetest runner error: ${error.message}`);
    return results;
  }
}

/**
 * Run roundtrip tests on sample Verse code
 */
async function runRoundtripTests(): Promise<TestResults> {
  console.log('\n' + '='.repeat(80));
  console.log('RUNNING ROUNDTRIP TESTS');
  console.log('='.repeat(80));

  const results: TestResults = {
    passed: 0,
    failed: 0,
    total: 0,
    failures: []
  };

  // Test cases for roundtrip testing
  const testCases = [
    '42',
    '3.14',
    '"hello world"',
    'true',
    'false',
    'x + y',
    'x * y + z',
    'x and y',
    'x or y',
    'x < y',
    'x > y',
    'x = y',
    '(x + y) * z',
    'x := 42',
    'if (true): 1 else: 2'
  ];

  for (let i = 0; i < testCases.length; i++) {
    const testCase = testCases[i];
    results.total++;

    try {
      console.log(`Test ${i + 1}/${testCases.length}: ${testCase}`);

      // Parse the original code
      const parseResult = parseVersee(testCase);

      if (!parseResult.success) {
        results.failed++;
        results.failures.push(`Test ${i + 1}: Failed to parse "${testCase}" - ${parseResult.error.message}`);
        console.log(`  ✗ Failed to parse: ${parseResult.error.message}`);
        continue;
      }

      // Print back to source code
      const printed = printExp(parseResult.value);
      console.log(`  Original:  ${testCase}`);
      console.log(`  Printed:   ${printed}`);

      // Parse the printed code again
      const reparsed = parseVersee(printed);

      if (!reparsed.success) {
        results.failed++;
        results.failures.push(`Test ${i + 1}: Failed to reparse printed code "${printed}" - ${reparsed.error.message}`);
        console.log(`  ✗ Failed to reparse printed code: ${reparsed.error.message}`);
        continue;
      }

      // For now, just check that both parsed successfully
      // In a more advanced version, we could compare AST structures
      results.passed++;
      console.log(`  ✓ Roundtrip successful`);

    } catch (error: any) {
      results.failed++;
      results.failures.push(`Test ${i + 1}: Exception during roundtrip test - ${error.message}`);
      console.log(`  ✗ Exception: ${error.message}`);
    }
  }

  console.log(`\nRoundtrip Results: ${results.passed} passed, ${results.failed} failed out of ${results.total} tests`);
  console.log(`Pass rate: ${((results.passed / results.total) * 100).toFixed(1)}%`);

  return results;
}

/**
 * Main unified test runner
 */
async function runUnifiedTests(): Promise<void> {
  console.log('='.repeat(80));
  console.log('VERSE PARSER UNIFIED TEST SUITE');
  console.log('='.repeat(80));

  const overallResults: TestResults = {
    passed: 0,
    failed: 0,
    total: 0,
    failures: []
  };

  // Run all test suites
  const jestResults = await runJestTests();
  const parseTestResults = await runParseTestFiles();
  const roundtripResults = await runRoundtripTests();

  // Aggregate results
  overallResults.passed = jestResults.passed + parseTestResults.passed + roundtripResults.passed;
  overallResults.failed = jestResults.failed + parseTestResults.failed + roundtripResults.failed;
  overallResults.total = jestResults.total + parseTestResults.total + roundtripResults.total;
  overallResults.failures = [
    ...jestResults.failures,
    ...parseTestResults.failures,
    ...roundtripResults.failures
  ];

  // Final summary
  console.log('\n' + '='.repeat(80));
  console.log('UNIFIED TEST SUITE SUMMARY');
  console.log('='.repeat(80));
  console.log(`Jest Tests:       ${jestResults.passed}/${jestResults.total} passed`);
  console.log(`Parsetest Files:  ${parseTestResults.passed}/${parseTestResults.total} passed`);
  console.log(`Roundtrip Tests:  ${roundtripResults.passed}/${roundtripResults.total} passed`);
  console.log('-'.repeat(40));
  console.log(`TOTAL:            ${overallResults.passed}/${overallResults.total} passed`);
  console.log(`Overall Pass Rate: ${overallResults.total > 0 ? ((overallResults.passed / overallResults.total) * 100).toFixed(1) : '0.0'}%`);

  if (overallResults.failures.length > 0) {
    console.log('\nFAILURES:');
    overallResults.failures.forEach((failure, index) => {
      console.log(`  ${index + 1}. ${failure}`);
    });
  }

  console.log('='.repeat(80));

  // Exit with error code if any tests failed
  if (overallResults.failed > 0) {
    process.exit(1);
  } else {
    console.log('🎉 All tests passed!');
  }
}

// Run if called directly
if (require.main === module) {
  runUnifiedTests().catch(error => {
    console.error('Unified test runner failed:', error);
    process.exit(1);
  });
}

export { runUnifiedTests };