const { parseProgram, parseExpression } = require('./dist/index.js');

// First check if +++ can be parsed as expression
console.log('1. Testing parseExpression("+++"):');
try {
  const ast = parseExpression('+++');
  console.log('   Result:', JSON.stringify(ast, null, 2));
} catch (e) {
  console.log('   Error:', e.message);
}

// Now test the full program
console.log('\n2. Testing parseProgram("result : type{+++} = getValue()"):');
try {
  const ast = parseProgram('result : type{+++} = getValue()');
  console.log('   Result: Parsed successfully');
  console.log('   Declarations:', ast.declarations.length);
  if (ast.declarations.length > 0) {
    console.log('   First declaration:', JSON.stringify(ast.declarations[0], null, 2).substring(0, 200));
  }
} catch (e) {
  console.log('   Error:', e.message);
}
