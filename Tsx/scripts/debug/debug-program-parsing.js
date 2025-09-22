#!/usr/bin/env node

const { parseTopLevel, parse } = require('./dist/index');

console.log('Testing program vs expression parsing...\n');

const simpleClass = `builder := class {
  value: string = ""
}`;

console.log('=== SIMPLE CLASS ===');
console.log(simpleClass);
console.log();

console.log('parse() result:');
const parseResult = parse(simpleClass, false);
if (parseResult) {
    console.log(`Type: ${parseResult.type}`);
    if (parseResult.type === 'Program') {
        console.log(`Declarations: ${parseResult.declarations.length}`);
    }
} else {
    console.log('Failed');
}

console.log('\nparseTopLevel() result:');
const topLevelResult = parseTopLevel(simpleClass, false);
if (topLevelResult) {
    console.log(`Type: ${topLevelResult.type}`);
    console.log(`Declarations: ${topLevelResult.declarations.length}`);
} else {
    console.log('Failed');
}

console.log('\n=== COMPLEX CLASS ===');
const complexClass = `builder := class {
  value: string = ""
  Add(s: string): string = value + s
}`;

console.log(complexClass);
console.log();

const complexResult = parseTopLevel(complexClass, false);
console.log(`parseTopLevel() result: ${complexResult ? 'Success' : 'Failed'}`);
if (complexResult) {
    console.log(`Declarations: ${complexResult.declarations.length}`);
}