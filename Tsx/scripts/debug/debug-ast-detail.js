const { parseTopLevel, parse } = require('./dist/index.js');

const testCode = `CaptureControl := class:
    @editable
    Device:enabled_device = enabled_device{}`;

console.log('Testing complex class with decorator...');

let result = parseTopLevel(testCode, true);

if (result) {
    console.log('✅ Parsing successful');
    console.log('\nFull AST:');
    console.log(JSON.stringify(result, null, 2));
} else {
    console.log('❌ Parsing failed');
}