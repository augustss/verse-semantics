const { parseProgram } = require('./dist/parser');
const { TokenStream } = require('./dist/lexer');
const { reconstructFromAST } = require('./dist/pretty-printer/ast-reconstructor');

const tests = [
  ['Array type', 'x : []int'],
  ['Multi-dim array', 'x : [][]string'],
  ['Optional array', 'x : []?int']
];

for (const [name, source] of tests) {
  try {
    const program = parseProgram(source);
    const tokenStream = TokenStream.fromString(source);
    
    if (program && program.declarations.length > 0) {
      const reconstructed = reconstructFromAST(source, program.declarations[0], {
        includeTrailingTrivia: true,
        tokenStream
      });
      
      const isMatch = source === reconstructed;
      console.log((isMatch ? '✓' : '✗') + ' ' + name);
      if (!isMatch) {
        console.log('  Original:      "' + source + '"');
        console.log('  Reconstructed: "' + reconstructed + '"');
      }
    } else {
      console.log('✗ ' + name + ': No declarations parsed');
    }
  } catch (e) {
    console.log('✗ ' + name + ': ' + e.message);
  }
}
