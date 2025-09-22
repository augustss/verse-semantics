const { parseTopLevel, parse } = require('./dist/index.js');

const testCode = 'class { x := 1 }';

console.log('Testing:', testCode);

let result = parse(testCode, true);

if (result) {
    console.log('AST Structure:');
    console.log(JSON.stringify(result, null, 2));
} else {
    console.log('❌ Parsing failed');
}