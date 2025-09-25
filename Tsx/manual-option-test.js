const { parseExpression } = require('./dist');

// Test cases for option expressions
const testCases = [
  'option{ 42 }',
  'option{ "hello" }',
  'option{ x + 1 }',
  'option{ Player.Health }'
];

console.log('Testing option expressions:\n');

for (const testCase of testCases) {
  try {
    const result = parseExpression(testCase);
    console.log(`✅ "${testCase}" -> Parsed successfully`);
    console.log(`   AST type: ${result.type}`);
    if (result.type === 'OptionExpression') {
      console.log(`   Value: ${JSON.stringify(result.value, null, 2)}`);
    }
  } catch (error) {
    console.log(`❌ "${testCase}" -> Error: ${error.message}`);
  }
  console.log();
}