#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing function parsing directly...\n');

const test = 'add(x, y) := x + y';
console.log(`Testing: ${test}`);

// Test as an expression first to see what type it gets
const exprAst = parse(test, false);

if (exprAst) {
    console.log(`✅ Parsed as expression`);
    console.log(`AST type: ${exprAst.type}`);
    console.log(`AST details:`, JSON.stringify(exprAst, (key, value) => {
        if (key === 'trivia') return '<trivia>';
        if (key === 'span') return '<span>';
        return value;
    }, 2));
} else {
    console.log('❌ Failed to parse as expression');
}