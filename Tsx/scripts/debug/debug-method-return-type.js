#!/usr/bin/env node

const { parseTopLevel, prettyPrint } = require('./dist/index');

console.log('Testing method with return type...\n');

// Test a class with a method that has a return type
const test = `builder := class {
  value: string = ""
  Add(s: string): string = value + s
}`;

console.log('Test case:');
console.log(test);
console.log();

const result = parseTopLevel(test, false);
console.log(`Result: ${result ? '✅ Success' : '❌ Failed'}`);

if (result) {
    console.log(`Declarations: ${result.declarations.length}`);
    if (result.declarations.length > 0) {
        console.log(`First declaration type: ${result.declarations[0].type}`);
        if (result.declarations[0].type === 'ConstDeclaration') {
            const classValue = result.declarations[0].value;
            console.log(`Class value type: ${classValue.type}`);
            if (classValue.type === 'ClassExpression') {
                console.log(`Class members: ${classValue.body.length}`);
            }
        }
    }

    const reconstructed = prettyPrint(result);
    console.log(`Lossless: ${test === reconstructed ? '✅' : '❌'}`);
    if (test !== reconstructed) {
        console.log('Expected:', test);
        console.log('Got:', reconstructed);
    }
}