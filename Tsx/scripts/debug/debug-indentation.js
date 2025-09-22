const { parseTopLevel, prettyPrint } = require('./dist/index.js');

const testCase = 'IPlayer := interface:\n    GetHealth():int';
console.log(`Testing: ${JSON.stringify(testCase)}`);

console.log('\n--- Parsing ---');
const ast = parseTopLevel(testCase);
if (ast) {
  console.log('✅ Parsing succeeded');
  console.log('Body members:', ast.declarations[0].body.length);
  console.log('Trailing trivia:', JSON.stringify(ast.trailingTrivia));

  if (ast.declarations[0].body.length > 0) {
    console.log('First member:', JSON.stringify(ast.declarations[0].body[0], null, 2));
  }
} else {
  console.log('❌ Parsing failed');
}