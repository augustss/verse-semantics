#!/usr/bin/env node

const { parse, parseTopLevel, prettyPrint } = require('./dist/index');

// Test a specific failing case from the block-tests
const testCase = `block:
  x := 1
  y := 2
  x + y`;

console.log('Testing block with multiple statements...\n');
console.log('Input:');
console.log(testCase);
console.log();

const ast = parse(testCase, false);

if (ast) {
    console.log('✅ Parsed successfully!');
    const reconstructed = prettyPrint(ast);
    console.log('\nReconstructed:');
    console.log(reconstructed);

    console.log('\nComparison:');
    console.log(`Original length: ${testCase.length}`);
    console.log(`Reconstructed length: ${reconstructed.length}`);
    console.log(`Lossless: ${testCase === reconstructed ? '✅' : '❌'}`);

    if (testCase !== reconstructed) {
        // Find first difference
        for (let i = 0; i < Math.max(testCase.length, reconstructed.length); i++) {
            if (testCase[i] !== reconstructed[i]) {
                console.log(`\nFirst difference at position ${i}:`);
                console.log(`  Original: "${testCase[i]}" (code: ${testCase.charCodeAt(i)})`);
                console.log(`  Reconstructed: "${reconstructed[i]}" (code: ${reconstructed.charCodeAt(i) || 'undefined'})`);
                console.log(`  Context: ...${testCase.substring(Math.max(0, i-10), i)}[HERE]${testCase.substring(i, i+10)}...`);
                break;
            }
        }
    }
} else {
    console.log('❌ Failed to parse');
}