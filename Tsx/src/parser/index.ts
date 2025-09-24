/**
 * Verse Parser Package
 *
 * Provides parsing functionality for the Verse language.
 * Main interface is through the parse() function which takes
 * source code or a TokenStream and returns an AST.
 *
 * Enhanced with source range tracking for perfect reconstruction.
 */

// Re-export types
export * from './ast';

// Re-export from main parser (traditional)
export {
  Parser,
  ParserState,
  ParseError,
  createParser,
  createParserState,
  parseExpression
} from './parser';


// Re-export top-level parser
export {
  UsingStatement,
  Program,
  parseProgram
} from './top-level-parser';

