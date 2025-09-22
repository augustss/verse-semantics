#!/usr/bin/env node

const { parseTopLevel } = require('./dist/index');

console.log('Testing class structure parsing...\n');

const test = `builder := class {
  value: string = ""
  Build(): string = value
}`;

const result = parseTopLevel(test, false);

if (result && result.declarations.length > 0) {
    const classDecl = result.declarations[0];
    console.log('Class declaration details:');
    console.log(JSON.stringify(classDecl, (key, value) => {
        if (key === 'trivia') return '<trivia>';
        if (key === 'span') return '<span>';
        return value;
    }, 2));
}