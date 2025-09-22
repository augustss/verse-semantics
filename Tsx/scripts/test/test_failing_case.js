const { parse } = require('./dist/index.js');

console.log('Testing the exact failing case...\n');

const failingCode = `result := if(true and not false) then {
  for(i : 0..10) {
    if(i > 5) then { break } else { continue }
  }
} else {
  case(status):
    0 => false
    1 => true
    _ => not true
}`;

console.log('Code to parse:');
console.log(failingCode);
console.log('\n' + '='.repeat(50) + '\n');

try {
  const result = parse(failingCode, true); // verbose mode
  if (result) {
    console.log('✅ SUCCESS - Parse completed!');
  } else {
    console.log('❌ PARSE FAILED - returned null');
  }
} catch (error) {
  console.log(`❌ ERROR: ${error.message}`);
  console.log(`Stack: ${error.stack}`);
}