const fs = require('fs');
const path = require('path');

// Read the parser file
const parserCode = fs.readFileSync('./dist/parser/parsers/declaration-parser.js', 'utf-8');

// Check if the empty type{} check exists
if (parserCode.includes('Empty type{} expressions are not allowed')) {
  console.log('✓ Empty type{} check exists in compiled code');
} else {
  console.log('✗ Empty type{} check NOT found in compiled code');
}

// Test it
const { parseProgram } = require('./dist/index.js');
try {
  const ast = parseProgram('result : type{} = getValue()');
  console.log('✗ Empty type{} parsed successfully (should have failed)');
} catch (e) {
  console.log('✓ Empty type{} failed with:', e.message);
}
