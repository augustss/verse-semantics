#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

function splitParsesetFile(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    const lines = content.split('\n');

    let currentTest = null;
    let currentLines = [];
    let testCount = 0;
    const baseName = path.basename(filePath, '.parseset');

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];

        // Check if this is a test header
        if (line.startsWith('#! Valid expression') || line.startsWith('#! Valid program')) {
            // Save previous test if exists
            if (currentTest && currentLines.length > 0) {
                saveTest(currentTest, currentLines.join('\n'), baseName, testCount);
                testCount++;
            }

            // Start new test
            currentTest = line.startsWith('#! Valid expression') ? 'expression' : 'program';
            currentLines = [line];
        } else if (line.startsWith('#! Error')) {
            // Skip error tests for now
            currentTest = null;
            currentLines = [];
        } else if (currentTest) {
            currentLines.push(line);
        }
    }

    // Save final test
    if (currentTest && currentLines.length > 0) {
        saveTest(currentTest, currentLines.join('\n'), baseName, testCount);
        testCount++;
    }

    console.log(`Split ${filePath}: ${testCount} tests`);
}

function saveTest(type, content, baseName, index) {
    // Determine if this should be a program or expression based on content
    const testCode = extractTestCode(content);
    const actualType = shouldBeProgram(testCode) ? 'program' : 'expression';

    const dir = `/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/${actualType}`;
    const fileName = `${baseName}-${String(index + 1).padStart(3, '0')}.parseset`;
    const filePath = path.join(dir, fileName);

    // Clean up the content - remove empty lines at start/end
    const cleanContent = content.trim();

    fs.writeFileSync(filePath, cleanContent + '\n');
}

function extractTestCode(content) {
    const lines = content.split('\n');
    const codeLines = [];

    for (const line of lines) {
        if (!line.startsWith('#!') && !line.startsWith('#') && line.trim() !== '') {
            codeLines.push(line);
        }
    }

    return codeLines.join('\n');
}

function shouldBeProgram(code) {
    // Program-level constructs that indicate this should be in the program directory
    const programPatterns = [
        /^\s*using\s*\{/m,                    // using statements
        /^\s*\w+\s*:=\s*class\s*[\{\(]/m,     // class declarations
        /^\s*\w+\s*:=\s*interface\s*[\{\(]/m, // interface declarations
        /^\s*\w+\s*:=\s*struct\s*[\{\(]/m,    // struct declarations
        /^\s*\w+\s*:=\s*enum\s*[\{\(]/m,      // enum declarations
        /^\s*\w+\s*:=\s*module\s*[\{\(]/m,    // module declarations
        /^\s*@\w+/m,                          // decorator on class/function
        /^\s*<\w+>/m,                         // specifiers on declarations
        /^\s*var\s+\w+\s*<\w+>/m             // variable with specifiers
    ];

    return programPatterns.some(pattern => pattern.test(code));
}

// Get all parseset files in tests/valid (but not subdirectories)
const testsDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid';
const files = fs.readdirSync(testsDir);

for (const file of files) {
    if (file.endsWith('.parseset')) {
        const filePath = path.join(testsDir, file);
        const stat = fs.statSync(filePath);

        if (stat.isFile()) {
            console.log(`Processing ${file}...`);
            splitParsesetFile(filePath);
        }
    }
}

console.log('Done splitting all parseset files!');