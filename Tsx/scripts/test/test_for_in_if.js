const { parse } = require('./dist/index.js');

console.log('Testing for loop inside if statement...\n');

const tests = [
  // Simple for in if
  `if(true) then {
    for(i : 0..10) { 42 }
  }`,

  // For with break/continue in if
  `if(true) then {
    for(i : 0..10) {
      if(i > 5) then { break } else { continue }
    }
  }`,

  // Assignment with for in if
  `result := if(true) then {
    for(i : 0..10) { 42 }
  }`,

  // Assignment with for containing break/continue in if
  `result := if(true) then {
    for(i : 0..10) {
      if(i > 5) then { break } else { continue }
    }
  }`,

  // Full version with logical operators
  `result := if(true and not false) then {
    for(i : 0..10) {
      if(i > 5) then { break } else { continue }
    }
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