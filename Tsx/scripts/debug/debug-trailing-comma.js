#!/usr/bin/env node

const { parse, prettyPrint } = require('./dist/index');

// Test trailing comma in function parameters
const testCase = `f(x, y,) := x + y`;

console.log('Testing function with trailing comma...\n');
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
        console.log(`\nDifference found:`);
        for (let i = 0; i < Math.max(testCase.length, reconstructed.length); i++) {
            if (testCase[i] !== reconstructed[i]) {
                console.log(`Position ${i}: "${testCase[i]}" vs "${reconstructed[i]}"`);
                break;
            }
        }
    }
} else {
    console.log('❌ Failed to parse');
}