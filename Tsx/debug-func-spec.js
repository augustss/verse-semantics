#!/usr/bin/env node

require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

// Test function with specifier
const testFunc = `GetValue<private>():int = 42`;

console.log('Testing function with specifier...');
const result = parseVersee(testFunc);

if (result.success) {
    console.log('SUCCESS!');
    console.log('Parsed:', result.value.value.kind);
} else {
    console.log('FAILED:', result.error);
    console.log('At position:', result.error.position);
    console.log('Context:', testFunc.slice(Math.max(0, result.error.position.offset - 10), result.error.position.offset + 10));
}