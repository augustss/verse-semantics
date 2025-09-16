#!/usr/bin/env node

require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

// Test a simple module with class declarations
const testModule = `DominationSpawnTags<public> := module:
    Tag_SpawnGroup<public>:=            class(tag){}`;

console.log('Testing simple module...');
const result = parseVersee(testModule);

if (result.success) {
    console.log('SUCCESS!');
    console.log('AST:', JSON.stringify(result.value, null, 2));
} else {
    console.log('FAILED:', result.error);
    console.log('At position:', result.error.position);
    console.log('Context around error:');
    const start = Math.max(0, result.error.position.offset - 20);
    const end = Math.min(testModule.length, result.error.position.offset + 20);
    console.log(testModule.slice(start, end));
    console.log(' '.repeat(result.error.position.offset - start) + '^');
}