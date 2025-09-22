#!/usr/bin/env node

const { parse, parseTopLevel, prettyPrint } = require('./dist/index');

console.log('Testing nested class parsing...\n');

const test1 = `builder := class {
  value: string = ""

  Add(s: string): class = {
    value := value + s;
    this
  }

  Build(): string = value
}`;

console.log('Test 1 (should be valid):');
console.log(test1);
console.log();

// Try both parsers
const exprResult = parse(test1, false);
console.log(`parse() result: ${exprResult ? '✅ Success' : '❌ Failed'}`);

const topLevelResult = parseTopLevel(test1, false);
console.log(`parseTopLevel() result: ${topLevelResult ? '✅ Success' : '❌ Failed'}`);

if (topLevelResult) {
    console.log(`Declarations: ${topLevelResult.declarations.length}`);
    if (topLevelResult.declarations.length > 0) {
        console.log(`First declaration type: ${topLevelResult.declarations[0].type}`);
    }

    const reconstructed = prettyPrint(topLevelResult);
    console.log(`Lossless: ${test1 === reconstructed ? '✅' : '❌'}`);
}