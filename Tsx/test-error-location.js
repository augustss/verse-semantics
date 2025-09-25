const { parseExpression } = require('./dist/index.js');

const testCases = [
  'x => ()',
  '()',
  'sum := a + b\ndifference := x - y',
  'case(x): 1 => doSomething(), _ => ()'
];

testCases.forEach(code => {
  console.log(`\nTesting: "${code.replace(/\n/g, '\\n')}"`);
  try {
    const ast = parseExpression(code);
    console.log('  ✓ Parsed successfully');
  } catch (error) {
    console.log(`  ✗ Error: ${error.message}`);
    if (error.position !== undefined) {
      console.log(`  Position: ${error.position}`);
      if (error.token) {
        console.log(`  Token: ${error.token.type} "${error.token.content}"`);
      }
    } else {
      console.log('  No position information available');
    }
  }
});
