#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing invalid syntax that should fail...\n');

const invalidTests = [
    {
        name: 'Variable without name',
        code: 'var : int = 42',
        shouldFail: true
    },
    {
        name: 'Variable without type',
        code: 'var x = 42',
        shouldFail: true
    },
    {
        name: 'Empty case with braces',
        code: 'case(x) {}',
        shouldFail: true
    },
    {
        name: 'Empty case with spaces',
        code: 'case(x) { }',
        shouldFail: true
    },
    {
        name: 'Keyword as identifier - if',
        code: 'if := 5',
        shouldFail: true
    },
    {
        name: 'Keyword as object type - case',
        code: 'case{x := 1}',
        shouldFail: true
    }
];

const validTests = [
    {
        name: 'Proper variable declaration',
        code: 'var x : int = 42',
        shouldFail: false
    },
    {
        name: 'Case with single branch',
        code: 'case(x) { 1 => a }',
        shouldFail: false
    }
];

const allTests = [...invalidTests, ...validTests];

for (const test of allTests) {
    console.log(`=== ${test.name} ===`);
    console.log(`Code: ${test.code}`);
    console.log(`Should fail: ${test.shouldFail}`);

    const result = parse(test.code, true);
    const actuallyPassed = result !== null;

    const isCorrect = test.shouldFail ? !actuallyPassed : actuallyPassed;
    const status = isCorrect ? '✅ Correct' : '❌ Wrong';

    console.log(`Result: ${actuallyPassed ? 'Passed' : 'Failed'} - ${status}`);
    console.log();
}