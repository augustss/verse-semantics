#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing function declaration variations...\n');

const tests = [
    'Add(s) = value + s',                    // Basic function
    'Add(s) := value + s',                   // With :=
    'Add(s: string) = value + s',            // With parameter type
    'Add(s: string): string = value + s',    // With return type
];

for (const test of tests) {
    console.log(`Testing: ${test}`);
    const result = parse(test, false);

    if (result) {
        if (result.type === 'Program' && result.declarations.length > 0) {
            console.log(`  ✅ ${result.declarations[0].type}`);
        } else {
            console.log(`  ✅ ${result.type}`);
        }
    } else {
        console.log('  ❌ Failed to parse');
    }
}