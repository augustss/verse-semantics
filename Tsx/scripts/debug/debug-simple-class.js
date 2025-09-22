#!/usr/bin/env node

const { parseTopLevel, prettyPrint } = require('./dist/index');

console.log('Testing simple class parsing...\n');

// Test a simple class first
const simpleTest = `builder := class {
  value: string = ""
  Build(): string = value
}`;

console.log('Simple class test:');
console.log(simpleTest);
console.log();

const result = parseTopLevel(simpleTest, false);
console.log(`Result: ${result ? '✅ Success' : '❌ Failed'}`);

if (result) {
    console.log(`Declarations: ${result.declarations.length}`);
    if (result.declarations.length > 0) {
        console.log(`First declaration type: ${result.declarations[0].type}`);
    }

    const reconstructed = prettyPrint(result);
    console.log(`Lossless: ${simpleTest === reconstructed ? '✅' : '❌'}`);
    if (simpleTest !== reconstructed) {
        console.log('Expected:', simpleTest);
        console.log('Got:', reconstructed);
    }
}