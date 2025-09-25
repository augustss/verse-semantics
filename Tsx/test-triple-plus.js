const { parseExpression } = require('./dist/index.js');

console.log('Testing: +++');
try {
  const ast = parseExpression('+++');
  console.log('Parsed as:', JSON.stringify(ast, null, 2));
} catch (e) {
  console.log('Failed:', e.message);
}
