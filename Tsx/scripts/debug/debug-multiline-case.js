#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing multiline case expressions...\n');

const tests = [
    {
        name: 'Single line case',
        code: `case(x): 0 => false`
    },
    {
        name: 'Two line case',
        code: `case(x):
  0 => false`
    },
    {
        name: 'Three line case',
        code: `case(x):
  0 => false
  1 => true`
    },
    {
        name: 'Full multiline case',
        code: `case(status):
  0 => false
  1 => true
  _ => not true`
    },
    {
        name: 'Assignment with multiline case',
        code: `result := case(status):
  0 => false
  1 => true
  _ => not true`
    },
    {
        name: 'If with multiline case in else',
        code: `if(true) then { 42 } else {
  case(status):
    0 => false
    1 => true
    _ => not true
}`
    }
];

for (const test of tests) {
    console.log(`=== ${test.name} ===`);
    const result = parse(test.code, true);
    console.log(`Result: ${result ? '✅ Success' : '❌ Failed'}`);
    console.log();
}