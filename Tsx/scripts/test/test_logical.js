const { parse } = require('./dist/index.js');

console.log('Testing logical operators...\n');

const tests = [
  'true and false',
  'true or false',
  'not true',
  'true and not false',
  'x and y',
  'a or b or c',
  'not (x and y)'
];

tests.forEach((test, i) => {
  console.log(`${i + 1}. Testing: ${test}`);
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
  console.log('');
});