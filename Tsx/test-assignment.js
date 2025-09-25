const { parseExpression } = require('./dist/index.js');

console.log('Testing: x := 5');
try {
  const ast = parseExpression('x := 5');
  console.log('Parsed as:', JSON.stringify(ast, null, 2).substring(0, 200));
  console.log('Type:', ast.type);
} catch (e) {
  console.log('Failed:', e.message);
}

console.log('\nTesting: return 5');
try {
  const ast = parseExpression('return 5');
  console.log('Parsed as:', JSON.stringify(ast, null, 2).substring(0, 200));
  console.log('Type:', ast.type);
} catch (e) {
  console.log('Failed:', e.message);
}
