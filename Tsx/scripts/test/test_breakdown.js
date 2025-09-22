const { parse } = require('./dist/index.js');

console.log('Breaking down the failing case...\n');

const tests = [
  // Test basic assignment
  'result := 42',

  // Test simple if
  'if(true) then { 42 }',

  // Test if with logical operator
  'if(true and not false) then { 42 }',

  // Test case statement
  `case(status):
    0 => false
    1 => true`,

  // Test case with wildcard
  `case(status):
    0 => false
    1 => true
    _ => not true`,

  // Test for loop
  'for(i : 0..10) { 42 }',

  // Test nested if in for
  'for(i : 0..10) { if(i > 5) then { break } else { continue } }',

  // Test assignment with if-else
  `result := if(true) then { 42 } else { 24 }`,

  // Add case to if-else
  `result := if(true) then { 42 } else {
    case(status):
      0 => false
      1 => true
      _ => not true
  }`,
];

tests.forEach((test, i) => {
  console.log(`${i + 1}. Testing:`);
  console.log(test);
  console.log('');

  try {
    const result = parse(test, false);
    if (result) {
      console.log('✅ SUCCESS');
    } else {
      console.log('❌ PARSE FAILED');
    }
  } catch (error) {
    console.log(`❌ ERROR: ${error.message}`);
  }
  console.log('='.repeat(40) + '\n');
});