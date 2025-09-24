const { TokenStream } = require('./dist/lexer');

// Test various whitespace scenarios
const tests = [
  'a  +  b',           // Multiple spaces
  'a\n\nb',            // Empty line
  'a\n  \nb',          // Empty line with spaces
  'if(x):\n  y\n\n  z' // Mixed indentation and empty lines
];

for (const source of tests) {
  console.log('\n=== "' + source.replace(/\n/g, '\\n') + '" ===');
  const tokenStream = TokenStream.fromString(source);
  const tokens = tokenStream.getAllTokens();
  
  console.log('All tokens with positions:');
  tokens.forEach((token, i) => {
    const display = token.content.replace(/\n/g, '\\n').replace(/ /g, '·');
    const type = token.type + '               ';
    console.log('  [' + i + '] ' + type.substring(0, 15) + ' "' + display + '" at pos ' + token.position);
  });
}
