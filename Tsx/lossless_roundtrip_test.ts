import * as fs from 'fs';
import { parseLossless, testRoundTrip } from './src/lossless-parser';

function losslessRoundtripTest(filename: string) {
    console.log(`=== LOSSLESS ROUNDTRIP TEST: ${filename} ===\n`);

    // Read original file
    const originalSource = fs.readFileSync(filename, 'utf8');
    console.log('Original file size:', originalSource.length, 'characters');
    console.log('First 200 chars:', originalSource.substring(0, 200) + '...\n');

    // Use the lossless parser with trivia preservation
    const result = parseLossless(originalSource, { preserveTrivia: true });

    if (!result.success) {
        console.log('❌ LOSSLESS PARSING FAILED:');
        console.log(result.error?.message);
        return;
    }

    console.log('✓ LOSSLESS PARSING SUCCESSFUL\n');

    // Check if round-trip is exact
    if (result.isRoundTripExact) {
        console.log('🎉 PERFECT LOSSLESS ROUNDTRIP!');
        console.log('Original and reconstructed sources are identical.\n');
    } else {
        console.log('⚠️  LOSSLESS ROUNDTRIP HAS DIFFERENCES\n');

        if (result.roundTripText) {
            console.log('Reconstructed size:', result.roundTripText.length, 'characters');
            console.log('First 200 chars:', result.roundTripText.substring(0, 200) + '...\n');

            // Show detailed differences
            const testResult = testRoundTrip(originalSource);
            if (testResult.differences) {
                console.log('Differences found:');
                testResult.differences.forEach(diff => console.log('  •', diff));
                console.log();
            }

            // Compare line by line (first 10 lines)
            const originalLines = originalSource.split('\n');
            const reconstructedLines = result.roundTripText.split('\n');
            const maxLines = Math.min(10, Math.max(originalLines.length, reconstructedLines.length));

            console.log('Line-by-line comparison (first 10 lines):');
            for (let i = 0; i < maxLines; i++) {
                const origLine = originalLines[i] || '';
                const reconLine = reconstructedLines[i] || '';

                if (origLine !== reconLine) {
                    console.log(`Line ${i + 1}:`);
                    console.log(`  ORIG: "${origLine}"`);
                    console.log(`  RCON: "${reconLine}"`);
                    console.log();
                }
            }
        }
    }
}

// Get filename from command line args
const filename = process.argv[2];
if (!filename) {
    console.log('Usage: npx ts-node lossless_roundtrip_test.ts <verse-file>');
    console.log('');
    console.log('This test uses the lossless parser that preserves trivia (whitespace, comments)');
    console.log('for true round-trip parsing capability.');
    process.exit(1);
}

losslessRoundtripTest(filename);