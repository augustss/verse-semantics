#!/usr/bin/env node

const { parse, parseTopLevel } = require('./dist/index');

console.log('Testing nested-classes Test 1...\n');

const test1 = `builder := class {
  value: string = ""

  Add(s: string): class = {
    value := value + s;
    this
  }

  Build(): string = value
}`;

console.log('=== Test 1 ===');
console.log(test1);
console.log();

console.log('parse() result:');
const parseResult = parse(test1, false);  // verbose = false to see errors
console.log(`Success: ${parseResult ? '✅' : '❌'}`);

console.log('\nparse() result with verbose:');
const parseVerbose = parse(test1, true);  // verbose = true to see errors
console.log(`Success: ${parseVerbose ? '✅' : '❌'}`);

console.log('\nparseTopLevel() result:');
const topResult = parseTopLevel(test1, false);
console.log(`Success: ${topResult ? '✅' : '❌'}`);
if (topResult) {
    console.log(`Declarations: ${topResult.declarations.length}`);
}