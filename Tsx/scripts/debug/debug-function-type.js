#!/usr/bin/env node

const { parse, prettyPrint } = require('./dist/index');

// Test function with return type annotation
const testCase = `f() : int = 42`;

console.log('Testing function with return type annotation...\n');
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
        console.log(`\nOriginal: "${testCase}"`);
        console.log(`Reconstructed: "${reconstructed}"`);
    }
} else {
    console.log('❌ Failed to parse');
}