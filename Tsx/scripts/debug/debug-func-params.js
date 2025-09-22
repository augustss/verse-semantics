#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Debugging function parameter parsing...\n');

const test = 'add(x, y) := x + y';
console.log(`Testing: ${test}`);

const ast = parse(test, false);

if (ast && ast.type === 'Program' && ast.declarations.length > 0) {
    const func = ast.declarations[0];
    console.log(`Function: ${func.name.text}`);
    console.log(`Parameters (${func.params.length}):`);

    func.params.forEach((param, i) => {
        console.log(`  Param ${i}: name="${param.name.text}"`);
        console.log(`    comma: ${param.comma ? `"${param.comma.text}"` : 'undefined'}`);
        if (param.comma) {
            console.log(`    comma trivia: leading="${param.comma.trivia.leading}", trailing="${param.comma.trivia.trailing}"`);
        }
    });
} else {
    console.log('❌ Failed to parse or unexpected structure');
}