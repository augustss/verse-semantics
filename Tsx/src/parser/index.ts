/**
 * Verse Parser Package
 *
 * Provides parsing functionality for the Verse language.
 * Main interface is through the parse() function which takes
 * source code or a TokenStream and returns an AST.
 */

// Re-export types
export * from './ast';

// Re-export from main parser
export {
  Parser,
  ParserState,
  ParseError,
  ParseResult,
  createParser,
  createParserState,
  parseExpression,
  parseExpressionFromTokens,
  parseExpressions,
  parseLiteral,
  isValidExpression
} from './parser';

// Re-export top-level parser
export {
  TopLevelParser,
  UsingStatement,
  Program,
  parseProgram,
  createTopLevelParser
} from './top-level-parser';

// Re-export parser modules for advanced usage
export * from './parsers';