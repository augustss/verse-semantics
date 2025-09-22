#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing progressive complexity...\n');

const tests = [
    {
        name: 'Assignment with simple if',
        code: 'result := if(true) then { 42 } else { 0 }'
    },
    {
        name: 'Assignment with if containing for',
        code: `result := if(true) then {
  for(i : 0..10) { i }
} else { 0 }`
    },
    {
        name: 'Assignment with if containing for with break',
        code: `result := if(true) then {
  for(i : 0..10) { if(i > 5) then { break } }
} else { 0 }`
    },
    {
        name: 'Assignment with if containing case',
        code: `result := if(true) then { 42 } else {
  case(status):
    0 => false
    1 => true
}`
    },
    {
        name: 'Full complex expression (failing)',
        code: `result := if(true and not false) then {
  for(i : 0..10) {
    if(i > 5) then { break } else { continue }
  }
} else {
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