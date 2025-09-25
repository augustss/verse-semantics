#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

function renumberTestsInFile(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    const lines = content.split('\n');

    let testNumber = 1;
    let inTest = false;
    let currentTestLines = [];
    const renumberedTests = [];

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];

        if (line.startsWith('#! Valid')) {
            // If we were already in a test, save it first
            if (inTest && currentTestLines.length > 0) {
                renumberedTests.push(currentTestLines.join('\n'));
            }

            // Start new test
            inTest = true;
            currentTestLines = [line];
        } else if (inTest) {
            // Check if this line starts a new test or we hit end of file
            if (line.startsWith('#! ') && !line.startsWith('#! Valid')) {
                // End current test and start tracking new one (though we don't process non-Valid tests)
                if (currentTestLines.length > 0) {
                    renumberedTests.push(currentTestLines.join('\n'));
                }
                inTest = false;
                currentTestLines = [];
            } else {
                // Continue current test
                currentTestLines.push(line);
            }
        }
    }

    // Don't forget the last test
    if (inTest && currentTestLines.length > 0) {
        renumberedTests.push(currentTestLines.join('\n'));
    }

    // Now renumber and reconstruct
    const renumberedContent = renumberedTests.map((test, index) => {
        const testLines = test.split('\n');

        // Find and update the comment line with the test number
        for (let i = 0; i < testLines.length; i++) {
            if (testLines[i].startsWith('# ') && /^\# \d+\./.test(testLines[i])) {
                // Replace the number
                testLines[i] = testLines[i].replace(/^\# \d+\./, `# ${index + 1}.`);
                break;
            } else if (testLines[i].startsWith('# ') && !/^\# \d+\./.test(testLines[i])) {
                // Add number to existing comment
                testLines[i] = `# ${index + 1}. ${testLines[i].substring(2)}`;
                break;
            }
        }

        return testLines.join('\n');
    }).join('\n\n');

    // Write back to file
    fs.writeFileSync(filePath, renumberedContent + '\n');

    return renumberedTests.length;
}

function renumberAllTests() {
    const expressionDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/expression';
    const programDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/program';

    console.log('Renumbering expression tests...');
    const expressionFiles = fs.readdirSync(expressionDir).filter(f => f.endsWith('.parseset'));
    let totalExpressionTests = 0;

    for (const file of expressionFiles.sort()) {
        const filePath = path.join(expressionDir, file);
        const testCount = renumberTestsInFile(filePath);
        console.log(`${file}: ${testCount} tests renumbered`);
        totalExpressionTests += testCount;
    }

    console.log('\nRenumbering program tests...');
    const programFiles = fs.readdirSync(programDir).filter(f => f.endsWith('.parseset'));
    let totalProgramTests = 0;

    for (const file of programFiles.sort()) {
        const filePath = path.join(programDir, file);
        const testCount = renumberTestsInFile(filePath);
        console.log(`${file}: ${testCount} tests renumbered`);
        totalProgramTests += testCount;
    }

    console.log(`\nTotal tests renumbered:`);
    console.log(`  Expression tests: ${totalExpressionTests}`);
    console.log(`  Program tests: ${totalProgramTests}`);
    console.log(`  Grand total: ${totalExpressionTests + totalProgramTests}`);
}

renumberAllTests();