#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing method parsing...\n');

const methodTest = 'Add(s: string): string = value + s';

console.log(`Testing: ${methodTest}`);
const result = parse(methodTest, false);

if (result) {
    console.log(`✅ Parsed as: ${result.type}`);
    if (result.type === 'Program' && result.declarations.length > 0) {
        console.log(`Declaration type: ${result.declarations[0].type}`);
    }
} else {
    console.log('❌ Failed to parse');
}