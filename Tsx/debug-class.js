#!/usr/bin/env node

require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

// Test a class with indented property declarations
const testClass = `capture_point_event_handler := class:

    CapturePointIndex:int
    DominationSpawnManagerRef:domination_spawn_manager`;

console.log('Testing class with properties...');
const result = parseVersee(testClass);

if (result.success) {
    console.log('SUCCESS!');
    console.log('AST:', JSON.stringify(result.value, null, 2));
} else {
    console.log('FAILED:', result.error);
    console.log('At position:', result.error.position);
    console.log('Context around error:');
    const start = Math.max(0, result.error.position.offset - 30);
    const end = Math.min(testClass.length, result.error.position.offset + 30);
    console.log(testClass.slice(start, end));
    console.log(' '.repeat(result.error.position.offset - start) + '^');
}