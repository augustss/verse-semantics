#!/usr/bin/env node

const { parseTopLevel } = require('./dist/index');

console.log('Testing class complexity levels...\n');

const tests = [
    {
        name: 'Empty class',
        code: 'builder := class {}'
    },
    {
        name: 'Class with field',
        code: 'builder := class {\n  value: string = ""\n}'
    },
    {
        name: 'Class with simple method',
        code: 'builder := class {\n  value: string = ""\n  Get() = value\n}'
    },
    {
        name: 'Class with typed method',
        code: 'builder := class {\n  value: string = ""\n  Get(): string = value\n}'
    },
    {
        name: 'Class with complex method',
        code: `builder := class {
  value: string = ""
  Add(s: string): class = {
    value := value + s;
    this
  }
}`
    }
];

for (const test of tests) {
    console.log(`=== ${test.name} ===`);
    console.log(test.code);
    console.log();

    const result = parseTopLevel(test.code, false);

    if (result) {
        console.log(`✅ Success - Declarations: ${result.declarations.length}`);
        if (result.declarations.length > 0) {
            console.log(`Type: ${result.declarations[0].type}`);
        }
    } else {
        console.log('❌ Failed');
    }
    console.log();
}