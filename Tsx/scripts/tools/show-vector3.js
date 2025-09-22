#!/usr/bin/env node

const fs = require('fs');
const { parseTopLevel, prettyPrint } = require('./dist/index');

// Use the Vector3 file which has no using statements
const filename = 'verse-files-flat/SolarisTestbed__Plugins__SolarisTests__LanguageServerTests__Project__Math__Vector3.verse';
const code = fs.readFileSync(filename, 'utf8');

console.log('='.repeat(70));
console.log('ORIGINAL FILE (first 500 chars):');
console.log('='.repeat(70));
console.log(code.substring(0, 500) + '...\n');

const ast = parseTopLevel(code, false);

if (ast) {
    const reconstructed = prettyPrint(ast);

    console.log('='.repeat(70));
    console.log('RECONSTRUCTED (first 500 chars):');
    console.log('='.repeat(70));
    console.log(reconstructed.substring(0, 500) + '...\n');

    console.log('='.repeat(70));
    console.log('VERIFICATION:');
    console.log('='.repeat(70));
    const isLossless = code === reconstructed;
    console.log(`✅ Parsed successfully`);
    console.log(`✅ Lossless reconstruction: ${isLossless ? 'YES' : 'NO'}`);
    console.log(`Original length: ${code.length} chars`);
    console.log(`Reconstructed length: ${reconstructed.length} chars`);

    if (isLossless) {
        console.log('🎉 Perfect reconstruction!');
    }
} else {
    console.log('❌ Failed to parse the file');
}