#!/usr/bin/env node

const { parse } = require('./dist/index');

// Test simple square bracket call
const testCase = `f[x, y]`;

console.log('Parsing:', testCase);
const ast = parse(testCase, false);

if (ast) {
    console.log('\nAST Structure:');
    console.log(JSON.stringify(ast, (key, value) => {
        if (key === 'trivia') return '<trivia>';
        if (key === 'span') return '<span>';
        return value;
    }, 2));
}