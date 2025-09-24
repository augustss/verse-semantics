const { createParser, createParserState } = require('./dist/parser');
const { TokenStream } = require('./dist/lexer');
const { reconstructFromAST } = require('./dist/pretty-printer/ast-reconstructor');

// Lambda tests from grammar audit
const lambdaTests = [
  ['Single param', 'x => x + 1'],
  ['Multi param', '(x, y) => x + y'],
  ['Multi param parens', '(x, y) => x + y'],  // might be duplicate
  ['No params', '() => 42'],
  ['Complex body', 'x => { y := x + 1; y * 2 }']
];

for (const [name, source] of lambdaTests) {
  try {
    const tokenStream = TokenStream.fromString(source);
    const parser = createParser();
    const state = createParserState(tokenStream);
    const parseResult = parser.parseExpression(state);
    
    if (parseResult) {
      const reconstructed = reconstructFromAST(source, parseResult.node, {
        includeTrailingTrivia: true,
        tokenStream
      });
      
      const isMatch = source === reconstructed;
      console.log((isMatch ? '✓' : '✗') + ' ' + name + ': "' + source + '"');
      if (!isMatch) {
        console.log('  => "' + reconstructed + '"');
      }
    }
  } catch (e) {
    console.log('✗ ' + name + ': Parse error - ' + e.message);
  }
}
