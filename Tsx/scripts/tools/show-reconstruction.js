#!/usr/bin/env node

const fs = require('fs');
const { parseTopLevel, prettyPrint } = require('./dist/index');

// Pick the Deserted.verse file as it's a good example with modules and classes
const filename = 'verse-files-flat/Samples__Deserted__Plugins__Deserted__Content__Deserted.verse';
const code = fs.readFileSync(filename, 'utf8');

console.log('='.repeat(70));
console.log('ORIGINAL FILE:');
console.log('='.repeat(70));
console.log(code);

const ast = parseTopLevel(code, true);

if (ast) {
    const reconstructed = prettyPrint(ast);

    console.log('\n' + '='.repeat(70));
    console.log('RECONSTRUCTED (from AST):');
    console.log('='.repeat(70));
    console.log(reconstructed);

    console.log('\n' + '='.repeat(70));
    console.log('VERIFICATION:');
    console.log('='.repeat(70));
    const isLossless = code === reconstructed;
    console.log(`✅ Lossless parsing: ${isLossless ? 'YES' : 'NO'}`);
    console.log(`Original length: ${code.length} chars`);
    console.log(`Reconstructed length: ${reconstructed.length} chars`);

    if (isLossless) {
        console.log('🎉 Perfect reconstruction - every character preserved!');
    }
} else {
    console.log('❌ Failed to parse the file');
}