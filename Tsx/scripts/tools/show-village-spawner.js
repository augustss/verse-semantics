#!/usr/bin/env node

const fs = require('fs');
const { parseTopLevel, prettyPrint } = require('./dist/index');

// Use the village player spawner file - a complex real-world Verse file
const filename = 'verse-files-flat/Samples__VKT_Village__Plugins__VKT_Village__Content__village_player_spawner.verse';
const code = fs.readFileSync(filename, 'utf8');

console.log('='.repeat(80));
console.log('PARSING: village_player_spawner.verse');
console.log('='.repeat(80));
console.log(`File size: ${code.length} characters`);
console.log(`Lines: ${code.split('\n').length}`);
console.log();

// Parse the file
const ast = parseTopLevel(code, true);

if (ast) {
    const reconstructed = prettyPrint(ast);

    // Verify lossless reconstruction
    const isLossless = code === reconstructed;

    console.log('✅ Successfully parsed!');
    console.log(`✅ Lossless reconstruction: ${isLossless ? 'YES' : 'NO'}`);
    console.log();

    // Show the full reconstructed file
    console.log('='.repeat(80));
    console.log('RECONSTRUCTED FILE (Full):');
    console.log('='.repeat(80));
    console.log(reconstructed);
    console.log('='.repeat(80));

    if (isLossless) {
        console.log('🎉 Perfect reconstruction - every character, comment, and whitespace preserved!');
    }
} else {
    console.log('❌ Failed to parse the file');
}