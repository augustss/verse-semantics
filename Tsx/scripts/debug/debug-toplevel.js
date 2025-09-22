#!/usr/bin/env node

const { parseTopLevel, prettyPrint } = require('./dist/index');

console.log('Testing top-level parsing...\n');

const testCases = [
    'myValue := 42',
    'add(x, y) := x + y',
    'using { std }\nmyValue := 42'
];

for (const test of testCases) {
    console.log(`Testing: ${test.replace(/\n/g, '\\n')}`);

    const ast = parseTopLevel(test, false);

    if (ast) {
        console.log(`✅ Parsed successfully`);
        console.log(`AST type: ${ast.type}`);

        const reconstructed = prettyPrint(ast);
        console.log(`Reconstructed: ${reconstructed.replace(/\n/g, '\\n')}`);
        console.log(`Lossless: ${test === reconstructed ? '✅' : '❌'}`);

        if (test !== reconstructed) {
            console.log(`  Expected: "${test}"`);
            console.log(`  Got:      "${reconstructed}"`);
        }
    } else {
        console.log('❌ Failed to parse');
    }
    console.log();
}