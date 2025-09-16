#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// Use ts-node to run TypeScript directly
require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

async function testVerseFiles() {
    console.log('Testing Verse parser on real files...\n');

    // Get all verse files manually
    const verseFiles = fs.readdirSync('verse-files-flat')
        .filter(file => file.endsWith('.verse'))
        .map(file => path.join('verse-files-flat', file));
    console.log(`Found ${verseFiles.length} Verse files\n`);

    let successCount = 0;
    let failCount = 0;
    const failures = [];

    // Test a larger subset (first 50 files) to get better statistics
    const filesToTest = verseFiles.slice(0, 50);

    for (const filePath of filesToTest) {
        const fileName = path.basename(filePath);
        console.log(`Testing: ${fileName}`);

        try {
            const source = fs.readFileSync(filePath, 'utf8');
            const result = parseVersee(source);

            if (result.success) {
                console.log(`  ✅ SUCCESS - parsed ${result.value.value.kind}`);
                successCount++;
            } else {
                console.log(`  ❌ PARSE FAILED - ${result.error.message}`);
                failCount++;
                failures.push({
                    file: fileName,
                    error: result.error.message,
                    location: result.error.location
                });
            }
        } catch (error) {
            console.log(`  💥 ERROR - ${error.message}`);
            failCount++;
            failures.push({
                file: fileName,
                error: error.message
            });
        }

        console.log('');
    }

    // Summary
    console.log('\n=== SUMMARY ===');
    console.log(`Total files tested: ${filesToTest.length}`);
    console.log(`Successful parses: ${successCount} (${(successCount/filesToTest.length*100).toFixed(1)}%)`);
    console.log(`Failed parses: ${failCount} (${(failCount/filesToTest.length*100).toFixed(1)}%)`);

    if (failures.length > 0) {
        console.log('\n=== FAILURES ===');
        failures.forEach(failure => {
            console.log(`${failure.file}: ${failure.error}`);
            if (failure.location) {
                console.log(`  at line ${failure.location.line}, column ${failure.location.column}`);
            }
        });
    }
}

testVerseFiles().catch(console.error);