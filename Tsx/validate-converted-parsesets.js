const { parseProgram, parseExpression } = require('./dist/index.js');
const fs = require('fs');

function validateParsesetFile(filepath) {
    console.log(`\n📁 Validating ${filepath}...`);

    const content = fs.readFileSync(filepath, 'utf8');
    const lines = content.split('\n');

    let currentTest = null;
    let currentContent = [];
    let testCount = 0;
    let passedCount = 0;
    let failedTests = [];

    for (const line of lines) {
        if (line.startsWith('#! ')) {
            // Start of new test
            if (currentTest && currentContent.length > 0) {
                // Process previous test
                testCount++;
                const code = currentContent.join('\n').trim();

                try {
                    if (currentTest === 'Valid TopLevel' || currentTest === 'Valid Program') {
                        const result = parseProgram(code);
                        // Consider it successful if no exception is thrown
                        passedCount++;
                    } else if (currentTest === 'Valid Expression') {
                        const result = parseExpression(code);
                        // Consider it successful if no exception is thrown
                        passedCount++;
                    }
                } catch (error) {
                    failedTests.push({
                        testNumber: testCount,
                        testType: currentTest,
                        code: code.substring(0, 50) + (code.length > 50 ? '...' : ''),
                        error: error.message
                    });
                }
            }

            currentTest = line.substring(3);
            currentContent = [];
        } else if (!line.startsWith('#') && line.trim() !== '') {
            // Content line
            currentContent.push(line);
        }
    }

    // Process final test
    if (currentTest && currentContent.length > 0) {
        testCount++;
        const code = currentContent.join('\n').trim();

        try {
            if (currentTest === 'Valid TopLevel' || currentTest === 'Valid Program') {
                const result = parseProgram(code);
                passedCount++;
            } else if (currentTest === 'Valid Expression') {
                const result = parseExpression(code);
                passedCount++;
            }
        } catch (error) {
            failedTests.push({
                testNumber: testCount,
                testType: currentTest,
                code: code.substring(0, 50) + (code.length > 50 ? '...' : ''),
                error: error.message
            });
        }
    }

    const successRate = (passedCount / testCount * 100).toFixed(1);
    console.log(`  📊 Results: ${passedCount}/${testCount} tests passed (${successRate}%)`);

    if (failedTests.length > 0) {
        console.log(`  ❌ Failed tests:`);
        failedTests.forEach(test => {
            console.log(`    ${test.testNumber}. [${test.testType}] ${test.code}`);
            console.log(`       Error: ${test.error}`);
        });
    }

    return {
        filepath,
        testCount,
        passedCount,
        successRate: parseFloat(successRate),
        failedTests
    };
}

function validateAllConvertedParsesets() {
    console.log('🧪 Validating all converted .parseset files...');

    const parsesetFiles = [
        'tests/converted-valid-expression.parseset',
        'tests/converted-valid-toplevel.parseset',
        'tests/converted-valid-program.parseset'
    ];

    const results = [];
    let totalTests = 0;
    let totalPassed = 0;

    for (const file of parsesetFiles) {
        if (fs.existsSync(file)) {
            const result = validateParsesetFile(file);
            results.push(result);
            totalTests += result.testCount;
            totalPassed += result.passedCount;
        } else {
            console.log(`⚠️  File not found: ${file}`);
        }
    }

    console.log('\n🎯 Overall Summary:');
    console.log(`  Total tests: ${totalTests}`);
    console.log(`  Passed: ${totalPassed}`);
    console.log(`  Failed: ${totalTests - totalPassed}`);
    console.log(`  Success rate: ${(totalPassed / totalTests * 100).toFixed(1)}%`);

    // Show breakdown by file type
    console.log('\n📋 Breakdown by test type:');
    for (const result of results) {
        const fileName = result.filepath.split('/').pop().replace('converted-', '').replace('.parseset', '');
        console.log(`  ${fileName}: ${result.passedCount}/${result.testCount} (${result.successRate}%)`);
    }

    return {
        totalTests,
        totalPassed,
        successRate: (totalPassed / totalTests * 100).toFixed(1),
        results
    };
}

// Run validation if this script is called directly
if (require.main === module) {
    validateAllConvertedParsesets();
}

module.exports = { validateParsesetFile, validateAllConvertedParsesets };