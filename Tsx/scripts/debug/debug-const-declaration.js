#!/usr/bin/env node

const { parseTopLevel } = require('./dist/index');

console.log('Testing constant declaration parsing...\n');

// Test simple cases first
const tests = [
    'simple := 42',
    'builder := {}',
    'builder := class {}',
    'builder := class { value: string = "" }'
];

for (const test of tests) {
    console.log(`Testing: ${test}`);
    const result = parseTopLevel(test, false);

    if (result) {
        console.log(`  ✅ Success - Declarations: ${result.declarations.length}`);
        if (result.declarations.length > 0) {
            console.log(`    Type: ${result.declarations[0].type}`);
        }
    } else {
        console.log('  ❌ Failed');
    }
}