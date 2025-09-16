#!/usr/bin/env node

require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

// Test a class with variable declarations
const testVars = `capture_point_event_handler := class:
    CapturePointIndex:int
    var CaptureWeightOfThisPoint:int = 0`;

console.log('Testing class with variable declarations...');
const result = parseVersee(testVars);

if (result.success) {
    console.log('SUCCESS!');
    console.log('AST:', JSON.stringify(result.value, null, 2));
} else {
    console.log('FAILED:', result.error);
    console.log('At position:', result.error.position);
    console.log('Context around error:');
    const start = Math.max(0, result.error.position.offset - 30);
    const end = Math.min(testVars.length, result.error.position.offset + 30);
    console.log(testVars.slice(start, end));
    console.log(' '.repeat(result.error.position.offset - start) + '^');
}