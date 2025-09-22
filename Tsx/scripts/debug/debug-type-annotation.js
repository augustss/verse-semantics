#!/usr/bin/env node

const { parse, prettyPrint } = require('./dist/index');

// Test case with type annotation that's failing
const testCase = `x : int = 42`;

console.log('Testing constant with type annotation...\n');
console.log('Input:');
console.log(testCase);
console.log();

const ast = parse(testCase, false);

if (ast) {
    console.log('AST type:', ast.type);
    console.log('✅ Parsed successfully!');

    const reconstructed = prettyPrint(ast);
    console.log('\nReconstructed:');
    console.log(reconstructed);

    console.log('\nLossless:', testCase === reconstructed ? '✅' : '❌');

    if (testCase !== reconstructed) {
        console.log(`Original: "${testCase}"`);
        console.log(`Reconstructed: "${reconstructed}"`);
    }
} else {
    console.log('❌ Failed to parse');
}