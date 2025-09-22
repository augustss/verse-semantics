const { parseIndentedStatements } = require('./dist/parser/statements/shared-indented.js');
const { modularExpr } = require('./dist/parser/expressions/core.js');

const code = 'block:\n  x := 1\n  y := 2\n  x + y';
console.log('Input code:', JSON.stringify(code));
console.log('Positions:');
for (let i = 0; i < code.length; i++) {
  const char = code[i] === '\n' ? '\\n' : code[i];
  console.log('  ', i.toString().padStart(2), ':', char);
}
