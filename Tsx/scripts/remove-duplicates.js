#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

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

function createHash(content) {
    return crypto.createHash('md5').update(content).digest('hex');
}

function findDuplicates(directory) {
    const files = fs.readdirSync(directory);
    const hashToFile = new Map();
    const duplicates = [];

    console.log(`Checking ${files.length} files in ${directory}...`);

    for (const file of files) {
        if (!file.endsWith('.parseset')) continue;

        const filePath = path.join(directory, file);
        const content = fs.readFileSync(filePath, 'utf8');
        const testCode = extractTestCode(content);

        if (testCode === '') {
            console.log(`Empty test: ${file}`);
            duplicates.push(filePath);
            continue;
        }

        const hash = createHash(testCode);

        if (hashToFile.has(hash)) {
            const originalFile = hashToFile.get(hash);
            console.log(`Duplicate found: ${file} (same as ${path.basename(originalFile)})`);
            console.log(`  Code: ${testCode.substring(0, 60)}${testCode.length > 60 ? '...' : ''}`);
            duplicates.push(filePath);
        } else {
            hashToFile.set(hash, filePath);
        }
    }

    return duplicates;
}

function removeDuplicates() {
    const expressionDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/expression';
    const programDir = '/Users/jv/Library/CloudStorage/Dropbox/j/code/verse-paper/Tsx/tests/valid/program';

    console.log('Finding duplicates in expression tests...');
    const expressionDuplicates = findDuplicates(expressionDir);

    console.log('\nFinding duplicates in program tests...');
    const programDuplicates = findDuplicates(programDir);

    const totalDuplicates = expressionDuplicates.length + programDuplicates.length;
    console.log(`\nTotal duplicates found: ${totalDuplicates}`);

    if (totalDuplicates > 0) {
        console.log('\nRemoving duplicates...');

        for (const duplicate of [...expressionDuplicates, ...programDuplicates]) {
            fs.unlinkSync(duplicate);
            console.log(`Removed: ${path.basename(duplicate)}`);
        }

        console.log(`\nRemoved ${totalDuplicates} duplicate files.`);
    } else {
        console.log('\nNo duplicates found!');
    }

    // Final counts
    const expressionCount = fs.readdirSync(expressionDir).filter(f => f.endsWith('.parseset')).length;
    const programCount = fs.readdirSync(programDir).filter(f => f.endsWith('.parseset')).length;
    console.log(`\nFinal counts:`);
    console.log(`  Expression tests: ${expressionCount}`);
    console.log(`  Program tests: ${programCount}`);
    console.log(`  Total: ${expressionCount + programCount}`);
}

removeDuplicates();