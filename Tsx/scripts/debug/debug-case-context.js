#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing case expressions in minimal contexts...\n');

const tests = [
    {
        name: 'Minimal case (should work standalone)',
        code: `case(x): 0 => false`
    },
    {
        name: 'Case with assignment',
        code: `result := case(x): 0 => false`
    },
    {
        name: 'Case in minimal block',
        code: `{ case(x): 0 => false }`
    },
    {
        name: 'Case in if then minimal',
        code: `if(true) then { case(x): 0 => false }`
    }
];

for (const test of tests) {
    console.log(`=== ${test.name} ===`);
    console.log(test.code);
    const result = parse(test.code, true);
    console.log(`Result: ${result ? '✅ Success' : '❌ Failed'}`);
    console.log();
}