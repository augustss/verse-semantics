const { createParser, createParserState } = require('./dist/parser');
const { TokenStream } = require('./dist/lexer');
const { ASTReconstructor } = require('./dist/pretty-printer/ast-reconstructor');

const source = `if(x) then:
  y
else:
  z`;

const tokenStream = TokenStream.fromString(source);
const parser = createParser();
const state = createParserState(tokenStream);
const parseResult = parser.parseExpression(state);

// Patch to trace
const originalReconstructIf = ASTReconstructor.prototype.reconstructIf;
let callCount = 0;
ASTReconstructor.prototype.reconstructIf = function(node) {
  console.log(`\nreconstructIf call #${++callCount}:`);
  console.log('  ifOffset:', node.ifOffset);
  console.log('  thenOffset:', node.thenOffset);
  console.log('  elseOffset:', node.elseOffset);
  console.log('  thenBranch type:', node.thenBranch?.type);
  console.log('  elseBranch type:', node.elseBranch?.type);
  return originalReconstructIf.call(this, node);
};

const { reconstructFromAST } = require('./dist/pretty-printer/ast-reconstructor');
const reconstructed = reconstructFromAST(source, parseResult.node, {
  includeTrailingTrivia: true,
  tokenStream
});

console.log('\nResult:', JSON.stringify(reconstructed));
console.log('Match:', source === reconstructed);
