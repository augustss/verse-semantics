const { createParser, createParserState } = require('./dist/parser');
const { TokenStream } = require('./dist/lexer');

const source = `for(x : items):
  process(x)`;

const tokenStream = TokenStream.fromString(source);
const parser = createParser();
const state = createParserState(tokenStream);
const parseResult = parser.parseExpression(state);

// Patch the reconstructor to trace calls
const { ASTReconstructor } = require('./dist/pretty-printer/ast-reconstructor');
const originalAppendToken = ASTReconstructor.prototype.appendToken;
ASTReconstructor.prototype.appendToken = function(offset) {
  console.log(`appendToken(${offset}): "${this.tokens[offset]?.content}"`);
  return originalAppendToken.call(this, offset);
};

const { reconstructFromAST } = require('./dist/pretty-printer/ast-reconstructor');
const reconstructed = reconstructFromAST(source, parseResult.node, {
  includeTrailingTrivia: true,
  tokenStream
});
console.log('\nResult:', JSON.stringify(reconstructed));
