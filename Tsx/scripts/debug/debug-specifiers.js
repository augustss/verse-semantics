#!/usr/bin/env node

const { parse, prettyPrint } = require('./dist/index');

// Test function with specifiers
const testCases = [
    `f <public> () := 42`,
    `f <public> <inline> () := x + y`,
    `f <export> (x) : int = x * 2`
];

for (const testCase of testCases) {
    console.log('Testing:', testCase);

    const ast = parse(testCase, false);

    if (ast) {
        console.log('  AST type:', ast.type);

        const reconstructed = prettyPrint(ast);
        console.log('  Reconstructed:', reconstructed);
        console.log('  Lossless:', testCase === reconstructed ? '✅' : '❌');
    } else {
        console.log('  ❌ Failed to parse');
    }
    console.log();
}