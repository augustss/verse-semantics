const { parse } = require('./dist/index.js');

// Test the member part separately
const memberCode = `@editable
Device:enabled_device = enabled_device{}`;

console.log('Testing just the class member:');
console.log(memberCode);

let result = parse(memberCode, true);

if (result) {
    console.log('✅ Member parsing successful');
    console.log('AST Type:', result.type);
    console.log('AST Structure:');
    console.log(JSON.stringify(result, null, 2));
} else {
    console.log('❌ Member parsing failed');
}