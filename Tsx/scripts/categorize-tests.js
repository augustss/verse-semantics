#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

function extractTestCode(content) {
    const lines = content.split('\n');
    const codeLines = [];

    for (const line of lines) {
        if (!line.startsWith('#!') && !line.startsWith('#') && line.trim() !== '') {
            codeLines.push(line.trim());
        }
    }

    return codeLines.join('\n').trim();
}

function categorizeTest(code) {
    // Return array of categories that match (a test might fit multiple categories)
    const categories = [];

    // LITERALS - numbers, strings, booleans, null
    if (/^\s*(\d+(\.\d*)?([eE][+-]?\d+)?|"[^"]*"|'[^']*'|true|false|null)\s*$/.test(code) ||
        /^\s*[\d.]+\s*$/.test(code)) {
        categories.push('LITERALS');
    }

    // IDENTIFIERS - simple variable names
    if (/^\s*[a-zA-Z_]\w*\s*$/.test(code)) {
        categories.push('IDENTIFIERS');
    }

    // ARITHMETIC - +, -, *, /, %, unary operators
    if (/[+\-*/%]/.test(code) && !/[<>=!&|:]/.test(code) && !/\b(and|or|not)\b/.test(code)) {
        categories.push('ARITHMETIC');
    }

    // COMPARISON - >, <, >=, <=, =, !=
    if (/[<>=!]=?/.test(code) && !/:=/.test(code)) {
        categories.push('COMPARISON');
    }

    // LOGICAL - and, or, not
    if (/\b(and|or|not)\b/.test(code)) {
        categories.push('LOGICAL');
    }

    // ASSIGNMENT - :=, var, set
    if (/:=/.test(code) || /^\s*(var|set)\s+/.test(code)) {
        categories.push('ASSIGNMENT');
    }

    // FUNCTIONS - function calls with parentheses
    if (/\w+\s*\([^)]*\)/.test(code) && !/^\s*\w+\s*:=\s*/.test(code)) {
        categories.push('FUNCTIONS');
    }

    // METHODS - member access with dots
    if (/\w+\.\w+/.test(code)) {
        categories.push('METHODS');
    }

    // ARRAYS - array access [], array construction array{}
    if (/\[.*\]/.test(code) || /array\s*\{/.test(code)) {
        categories.push('ARRAYS');
    }

    // OBJECTS - object construction Type{...}
    if (/\w+\s*\{[^}]*\}/.test(code) && !/array\s*\{/.test(code)) {
        categories.push('OBJECTS');
    }

    // LAMBDAS - lambda expressions with =>
    if (/=>\s/.test(code)) {
        categories.push('LAMBDAS');
    }

    // RANGES - .. operator
    if (/\.\./.test(code)) {
        categories.push('RANGES');
    }

    // BLOCKS - braced blocks or indented blocks
    if (/^\s*\{[\s\S]*\}\s*$/.test(code) || /block\s*:/.test(code)) {
        categories.push('BLOCKS');
    }

    // CONDITIONALS - if/then/else
    if (/\bif\s*\(/.test(code) || /\bthen\b/.test(code) || /\belse\b/.test(code)) {
        categories.push('CONDITIONALS');
    }

    // LOOPS - for loops
    if (/\bfor\s*\(/.test(code) || /\bfor\s*\w+\s*:/.test(code)) {
        categories.push('LOOPS');
    }

    // CASES - case/match expressions
    if (/\bcase\s*\(/.test(code) || /=>\s*\w+/.test(code)) {
        categories.push('CASES');
    }

    // PARENTHESES - expressions wrapped in parentheses
    if (/^\s*\([^)]+\)\s*$/.test(code)) {
        categories.push('PARENTHESES');
    }

    // COMMENTS - expressions with comments
    if (/<#.*#>/.test(code) || /#[^!]/.test(code)) {
        categories.push('COMMENTS');
    }

    // Program-level categories

    // CLASSES - class declarations
    if (/\w+\s*:=\s*class\s*[{\(]/.test(code)) {
        categories.push('CLASSES');
    }

    // INTERFACES - interface declarations
    if (/\w+\s*:=\s*interface\s*[{\(]/.test(code)) {
        categories.push('INTERFACES');
    }

    // STRUCTS - struct declarations
    if (/\w+\s*:=\s*struct\s*[{\(]/.test(code)) {
        categories.push('STRUCTS');
    }

    // ENUMS - enum declarations
    if (/\w+\s*:=\s*enum\s*[{\(]/.test(code)) {
        categories.push('ENUMS');
    }

    // MODULES - module declarations
    if (/\w+\s*:=\s*module\s*[{\(]/.test(code)) {
        categories.push('MODULES');
    }

    // USING - using statements
    if (/^\s*using\s*\{/.test(code)) {
        categories.push('USING');
    }

    // DECORATORS - @decorator
    if (/^\s*@\w+/.test(code)) {
        categories.push('DECORATORS');
    }

    // SPECIFIERS - <specifier>
    if (/<\w+>/.test(code)) {
        categories.push('SPECIFIERS');
    }

    // VARIABLES - var declarations
    if (/^\s*var\s+\w+/.test(code)) {
        categories.push('VARIABLES');
    }

    // CONSTANTS - const-like declarations
    if (/^\s*\w+\s*:\s*\w+\s*=/.test(code) && !/:=/.test(code)) {
        categories.push('CONSTANTS');
    }

    // TYPES - type expressions
    if (/:\s*\w+(\[\]|\<.*\>)*\s*=/.test(code)) {
        categories.push('TYPES');
    }

    // If no specific category matches, try to infer from context
    if (categories.length === 0) {
        // Default categorization based on simple patterns
        if (code.includes('(') && code.includes(')')) {
            categories.push('FUNCTIONS');
        } else if (code.includes('.')) {
            categories.push('METHODS');
        } else if (/[+\-*/]/.test(code)) {
            categories.push('ARITHMETIC');
        } else {
            categories.push('IDENTIFIERS'); // fallback
        }
    }

    return categories;
}

function moveTestToCategory(testFile, testContent, category, directory) {
    const categoryFile = path.join(directory, `${category}.parseset`);

    // Append test to category file
    fs.appendFileSync(categoryFile, testContent + '\n\n');

    // Delete original test file
    fs.unlinkSync(testFile);

    console.log(`Moved ${path.basename(testFile)} -> ${category}.parseset`);
}

function categorizeAllTests() {
    const expressionDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/expression';
    const programDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/program';

    // Process expression tests
    console.log('Categorizing expression tests...');
    const expressionFiles = fs.readdirSync(expressionDir).filter(f =>
        f.endsWith('.parseset') && !f.match(/^[A-Z]+\.parseset$/));

    for (const file of expressionFiles) {
        const filePath = path.join(expressionDir, file);
        const content = fs.readFileSync(filePath, 'utf8');
        const code = extractTestCode(content);

        if (code.trim() === '') {
            fs.unlinkSync(filePath);
            console.log(`Deleted empty test: ${file}`);
            continue;
        }

        const categories = categorizeTest(code);

        // Use the first matching category (could be improved to use best match)
        const primaryCategory = categories[0];

        if (primaryCategory) {
            moveTestToCategory(filePath, content.trim(), primaryCategory, expressionDir);
        }
    }

    // Process program tests
    console.log('\nCategorizing program tests...');
    const programFiles = fs.readdirSync(programDir).filter(f =>
        f.endsWith('.parseset') && !f.match(/^[A-Z]+\.parseset$/));

    for (const file of programFiles) {
        const filePath = path.join(programDir, file);
        const content = fs.readFileSync(filePath, 'utf8');
        const code = extractTestCode(content);

        if (code.trim() === '') {
            fs.unlinkSync(filePath);
            console.log(`Deleted empty test: ${file}`);
            continue;
        }

        const categories = categorizeTest(code);

        // Filter for program-level categories
        const programCategories = categories.filter(cat =>
            ['CLASSES', 'INTERFACES', 'STRUCTS', 'ENUMS', 'MODULES', 'USING',
             'DECORATORS', 'SPECIFIERS', 'VARIABLES', 'CONSTANTS', 'TYPES',
             'DECLARATIONS', 'VISIBILITY', 'ANNOTATIONS', 'GENERICS'].includes(cat));

        const primaryCategory = programCategories[0] || 'DECLARATIONS'; // fallback

        moveTestToCategory(filePath, content.trim(), primaryCategory, programDir);
    }

    // Show final counts
    console.log('\nFinal category counts:');

    console.log('\nExpression categories:');
    const expressionCategories = fs.readdirSync(expressionDir).filter(f => f.match(/^[A-Z]+\.parseset$/));
    for (const category of expressionCategories.sort()) {
        const content = fs.readFileSync(path.join(expressionDir, category), 'utf8');
        const testCount = (content.match(/#! Valid/g) || []).length;
        console.log(`  ${category}: ${testCount} tests`);
    }

    console.log('\nProgram categories:');
    const programCategories = fs.readdirSync(programDir).filter(f => f.match(/^[A-Z]+\.parseset$/));
    for (const category of programCategories.sort()) {
        const content = fs.readFileSync(path.join(programDir, category), 'utf8');
        const testCount = (content.match(/#! Valid/g) || []).length;
        console.log(`  ${category}: ${testCount} tests`);
    }
}

categorizeAllTests();