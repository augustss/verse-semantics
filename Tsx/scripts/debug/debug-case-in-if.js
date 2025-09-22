#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing case expressions in different contexts...\n');

const tests = [
    {
        name: 'Standalone case',
        code: `case(status):
  0 => false
  1 => true
  _ => not true`
    },
    {
        name: 'Case in assignment',
        code: `result := case(status):
  0 => false
  1 => true
  _ => not true`
    },
    {
        name: 'Case in if then branch',
        code: `if(true) then {
  case(status):
    0 => false
    1 => true
    _ => not true
} else { 0 }`
    },
    {
        name: 'Case in if else branch',
        code: `if(true) then { 42 } else {
  case(status):
    0 => false
    1 => true
    _ => not true
}`
    },
    {
        name: 'Assignment with case in if else branch',
        code: `result := if(true) then { 42 } else {
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