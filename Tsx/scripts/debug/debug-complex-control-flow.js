#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing complex control flow...\n');

const test7 = `result := if(true and not false) then {
  for(i : 0..10) {
    if(i > 5) then { break } else { continue }
  }
} else {
  case(status):
    0 => false
    1 => true
    _ => not true
}`;

console.log('Full test case:');
console.log(test7);
console.log();

// Test the full expression
console.log('Testing full expression:');
const fullResult = parse(test7, false);
console.log(`Result: ${fullResult ? '✅ Success' : '❌ Failed'}`);

// Test components separately
console.log('\nTesting components separately:');

const simpleIf = 'if(true and not false) then { 42 } else { 0 }';
console.log(`Simple if: ${parse(simpleIf, true) ? '✅' : '❌'}`);

const simpleFor = 'for(i : 0..10) { i }';
console.log(`Simple for: ${parse(simpleFor, true) ? '✅' : '❌'}`);

const simpleCase = `case(status):
  0 => false
  1 => true
  _ => not true`;
console.log(`Simple case: ${parse(simpleCase, true) ? '✅' : '❌'}`);

const nestedBreak = 'for(i : 0..10) { if(i > 5) then { break } else { continue } }';
console.log(`Nested break/continue: ${parse(nestedBreak, true) ? '✅' : '❌'}`);