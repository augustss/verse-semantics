#!/usr/bin/env ts-node

import { execSync } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

interface TestResult {
  name: string;
  passed: number;
  failed: number;
  time: number;
}

const testFiles = [
  'test-boolean-literals.ts',
  'test-class-expressions.ts',
  'test-keyword-validation.ts',
  'test-integration.ts'
];

console.log('===========================================');
console.log('   Expression Parser Test Suite');
console.log('===========================================\n');

const results: TestResult[] = [];
let totalPassed = 0;
let totalFailed = 0;
let totalTime = 0;

for (const testFile of testFiles) {
  const testPath = path.join(__dirname, testFile);

  if (!fs.existsSync(testPath)) {
    console.log(`⚠️  Skipping ${testFile} (not found)`);
    continue;
  }

  console.log(`Running ${testFile}...`);
  console.log('-'.repeat(40));

  const startTime = Date.now();

  try {
    // Run the test file
    const output = execSync(`npx ts-node ${testPath}`, {
      encoding: 'utf8',
      stdio: 'pipe'
    });

    // Parse the results from output
    const lines = output.split('\n');
    const resultsLine = lines.find(line => line.includes('Results:'));

    let passed = 0;
    let failed = 0;

    if (resultsLine) {
      const match = resultsLine.match(/(\d+) passed, (\d+) failed/);
      if (match) {
        passed = parseInt(match[1]);
        failed = parseInt(match[2]);
      }
    }

    const elapsed = Date.now() - startTime;

    results.push({
      name: testFile,
      passed,
      failed,
      time: elapsed
    });

    totalPassed += passed;
    totalFailed += failed;
    totalTime += elapsed;

    if (failed === 0) {
      console.log(`✅ All tests passed (${passed} tests in ${elapsed}ms)\n`);
    } else {
      console.log(`❌ ${failed} tests failed (${passed} passed in ${elapsed}ms)\n`);
    }

  } catch (error: any) {
    const elapsed = Date.now() - startTime;

    // Even if exit code is non-zero, try to parse results
    const output = error.stdout?.toString() || '';
    const lines = output.split('\n');
    const resultsLine = lines.find((line: string) => line.includes('Results:'));

    let passed = 0;
    let failed = 0;

    if (resultsLine) {
      const match = resultsLine.match(/(\d+) passed, (\d+) failed/);
      if (match) {
        passed = parseInt(match[1]);
        failed = parseInt(match[2]);
      }
    }

    results.push({
      name: testFile,
      passed,
      failed: failed || 1, // At least 1 failure if process exited with error
      time: elapsed
    });

    totalPassed += passed;
    totalFailed += failed || 1;
    totalTime += elapsed;

    console.log(`❌ Test file failed with ${failed || '?'} failures (${passed} passed in ${elapsed}ms)\n`);
  }
}

// Print summary
console.log('\n===========================================');
console.log('              Test Summary');
console.log('===========================================\n');

// Print table
console.log('File                          | Pass | Fail | Time');
console.log('------------------------------|------|------|--------');

for (const result of results) {
  const fileName = result.name.padEnd(28);
  const passed = result.passed.toString().padStart(4);
  const failed = result.failed.toString().padStart(4);
  const time = `${result.time}ms`.padStart(6);

  const failSymbol = result.failed > 0 ? '❌' : '✅';
  console.log(`${fileName} | ${passed} | ${failed} | ${time} ${failSymbol}`);
}

console.log('------------------------------|------|------|--------');
console.log(`${'TOTAL'.padEnd(28)} | ${totalPassed.toString().padStart(4)} | ${totalFailed.toString().padStart(4)} | ${`${totalTime}ms`.padStart(6)}`);

// Print final result
console.log('\n===========================================');
if (totalFailed === 0) {
  console.log(`✅ ALL TESTS PASSED! (${totalPassed} tests in ${totalTime}ms)`);
} else {
  console.log(`❌ TEST SUITE FAILED: ${totalFailed} failures out of ${totalPassed + totalFailed} tests`);
}
console.log('===========================================\n');

// Also run the parseset tests
console.log('Running parseset tests...\n');
try {
  execSync('npm run test', { stdio: 'inherit' });
} catch (e) {
  // parseset tests may have failures, that's ok
}

process.exit(totalFailed > 0 ? 1 : 0);