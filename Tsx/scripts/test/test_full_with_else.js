const { parse } = require('./dist/index.js');

console.log('Testing full expression with else clause...\n');

const tests = [
  // Just the else clause
  `if(true) then { 42 } else {
    case(status):
      0 => false
      1 => true
      _ => not true
  }`,

  // Assignment with else clause
  `result := if(true) then { 42 } else {
    case(status):
      0 => false
      1 => true
      _ => not true
  }`,

  // With for in then clause
  `result := if(true) then {
    for(i : 0..10) { 42 }
  } else {
    case(status):
      0 => false
      1 => true
      _ => not true
  }`,

  // The full failing case
  `result := if(true and not false) then {
    for(i : 0..10) {
      if(i > 5) then { break } else { continue }
    }
  } else {
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
  console.log('='.repeat(50) + '\n');
});