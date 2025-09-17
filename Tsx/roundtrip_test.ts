import * as fs from 'fs';
import { parseVersee } from './src/parser/parser';
import { printAST } from './src/printer/pretty-printer';

function roundtripTest(filename: string) {
    console.log(`=== ROUNDTRIP TEST: ${filename} ===\n`);

    // Read original file
    const originalSource = fs.readFileSync(filename, 'utf8');
    console.log('ORIGINAL SOURCE:');
    console.log(originalSource);
    console.log('\n' + '='.repeat(50) + '\n');

    // Parse the file
    const parseResult = parseVersee(originalSource);

    if (!parseResult.success) {
        console.log('❌ PARSING FAILED:');
        console.log(parseResult.error);
        return;
    }

    console.log('✓ PARSING SUCCESSFUL\n');

    // Print AST back to source
    try {
        const reconstructedSource = printAST(parseResult.value);
        console.log('RECONSTRUCTED SOURCE:');
        console.log(reconstructedSource);
        console.log('\n' + '='.repeat(50) + '\n');

        // Compare sources
        const originalLines = originalSource.split('\n');
        const reconstructedLines = reconstructedSource.split('\n');

        console.log('COMPARISON:');
        let differences = 0;
        const maxLines = Math.max(originalLines.length, reconstructedLines.length);

        for (let i = 0; i < maxLines; i++) {
            const origLine = originalLines[i] || '';
            const reconLine = reconstructedLines[i] || '';

            if (origLine !== reconLine) {
                differences++;
                console.log(`Line ${i + 1}:`);
                console.log(`  ORIG: "${origLine}"`);
                console.log(`  RCON: "${reconLine}"`);
                console.log();
            }
        }

        if (differences === 0) {
            console.log('🎉 PERFECT MATCH! Lossless roundtrip successful.');
        } else {
            console.log(`⚠️  Found ${differences} differences between original and reconstructed.`);
        }

        // Check if they parse to the same AST
        const reparsedResult = parseVersee(reconstructedSource);
        if (reparsedResult.success) {
            console.log('✓ Reconstructed source parses successfully');

            // Simple structural comparison
            const originalAST = JSON.stringify(parseResult.value, null, 2);
            const reparsedAST = JSON.stringify(reparsedResult.value, null, 2);

            if (originalAST === reparsedAST) {
                console.log('✓ AST structures are identical');
            } else {
                console.log('⚠️  AST structures differ (may be due to formatting)');
            }
        } else {
            console.log('❌ Reconstructed source fails to parse');
        }

    } catch (error) {
        console.log('❌ PRETTY PRINTING FAILED:');
        console.log(error);
    }
}

// Get filename from command line args
const filename = process.argv[2];
if (!filename) {
    console.log('Usage: npx ts-node roundtrip_test.ts <verse-file>');
    process.exit(1);
}

roundtripTest(filename);