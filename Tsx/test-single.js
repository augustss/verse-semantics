const { parseProgram } = require('./dist/index.js');

const code = 'result : type{} = getValue()';
console.log('Testing:', code);

try {
  const ast = parseProgram(code);
  console.log('Parsed AST:', JSON.stringify(ast, null, 2));
} catch (e) {
  console.log('Error:', e.message);
  console.log('Stack:', e.stack);
}
