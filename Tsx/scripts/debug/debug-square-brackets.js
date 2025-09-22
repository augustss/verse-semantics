#!/usr/bin/env node

const { parse, prettyPrint } = require('./dist/index');

// Test square bracket function calls
const testCase = `items := getData[1, 2, 3]`;

console.log('Testing square bracket function call...\n');
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
        console.log('\nChecking if square brackets are preserved...');
        console.log('Has square brackets:', reconstructed.includes('[') && reconstructed.includes(']') ? '✅' : '❌');
    }
} else {
    console.log('❌ Failed to parse');
}