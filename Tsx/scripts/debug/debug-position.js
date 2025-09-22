const { parseTopLevel } = require('./dist/index.js');

const testCase = 'IPlayer := interface:\n    GetHealth():int';
console.log(`Testing: ${JSON.stringify(testCase)}`);
console.log(`Input length: ${testCase.length}`);

// Find where different parts are
const colonPos = testCase.indexOf(':');
const getHealthPos = testCase.indexOf('GetHealth');
console.log(`Colon at position: ${colonPos}`);
console.log(`GetHealth at position: ${getHealthPos}`);

const ast = parseTopLevel(testCase);
if (ast && ast.declarations.length > 0) {
  const decl = ast.declarations[0];
  console.log(`Declaration span: ${decl.span.start} - ${decl.span.end}`);
  console.log(`Colon span: ${decl.colon.span.start} - ${decl.colon.span.end}`);
  console.log(`Colon trailing trivia: ${JSON.stringify(decl.colon.trivia.trailing)}`);
  console.log(`Trailing trivia span should be: ${decl.colon.span.end} to ${testCase.length}`);
  console.log(`Actual trailing content: ${JSON.stringify(testCase.slice(decl.colon.span.end))}`);
}