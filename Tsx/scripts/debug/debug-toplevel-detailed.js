#!/usr/bin/env node

const { parseTopLevel, prettyPrint } = require('./dist/index');

console.log('Detailed top-level parsing debug...\n');

const test = 'myValue := 42';
console.log(`Testing: ${test}`);

const ast = parseTopLevel(test, false);

if (ast) {
    console.log(`✅ Parsed successfully`);
    console.log(`AST type: ${ast.type}`);
    console.log(`Using statements: ${ast.usingStatements.length}`);
    console.log(`Declarations: ${ast.declarations.length}`);

    if (ast.declarations.length > 0) {
        ast.declarations.forEach((decl, i) => {
            console.log(`  Declaration ${i}: type=${decl.type}`);
        });
    }

    const reconstructed = prettyPrint(ast);
    console.log(`Reconstructed: "${reconstructed}"`);
    console.log(`Lossless: ${test === reconstructed ? '✅' : '❌'}`);
} else {
    console.log('❌ Failed to parse');
}