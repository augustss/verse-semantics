/**
 * Literal and Identifier Parser
 *
 * This module handles parsing of literal values and identifiers.
 *
 * Literals:
 * - Integers: 42, -10, 0
 * - Floats: 3.14, -0.5, 1.0e10
 * - Strings: "hello", 'world'
 * - Booleans: true, false (if supported)
 *
 * Identifiers:
 * - Variable names: myVar, _private
 * - Keywords used as identifiers in certain contexts
 *
 * All literals and identifiers store their token offset
 * for source reconstruction.
 */

import { TokenType } from '../../lexer/token';
import { ParserState, ParseResult, ParseError } from '../parser-state';
import * as AST from '../ast';

/**
 * Parser for literal values and identifiers.
 *
 * This class provides parsing for the most basic expression types.
 * Both literals and identifiers are leaf nodes in the AST with
 * no child expressions.
 *
 * Token offset tracking:
 * - Each literal/identifier stores the offset of its token
 * - This allows retrieving the original token with trivia
 */
export class LiteralParser {
  /**
   * Parse a literal expression (number or string).
   *
   * Grammar:
   *   literal = INTEGER | FLOAT | STRING
   *
   * Examples:
   *   42        -> LiteralExpression { value: 42, literalType: 'integer', tokenOffset: n }
   *   3.14      -> LiteralExpression { value: 3.14, literalType: 'float', tokenOffset: n }
   *   "hello"   -> LiteralExpression { value: "hello", literalType: 'string', tokenOffset: n }
   *
   * Note: String values have quotes removed during parsing.
   *
   * @param state Current parser state
   * @returns Parsed literal expression and new state
   * @throws ParseError if current token is not a valid literal
   */
  parseLiteral(state: ParserState): ParseResult<AST.LiteralExpression> {
    // Skip any leading trivia (whitespace, comments)
    state = state.skipTrivia();
    const tokenOffset = state.currentOffset();
    const token = state.current();

    if (!token) throw new ParseError('Expected literal', state.position);

    let value: any;
    let literalType: 'string' | 'integer' | 'float' | 'boolean';

    // Determine literal type and parse value
    if (token.type === TokenType.INTEGER) {
      // Parse integer literal
      value = parseInt(token.content, 10);
      literalType = 'integer';
    } else if (token.type === TokenType.FLOAT) {
      // Parse floating-point literal
      value = parseFloat(token.content);
      literalType = 'float';
    } else if (token.type === TokenType.STRING) {
      // Parse string literal (content already has quotes removed in lexer)
      value = token.content;
      literalType = 'string';
    } else if (token.type === TokenType.INVALID_STRING) {
      // Report error for invalid string with bad escape sequences
      throw new ParseError(`Invalid escape sequence in string literal`, state.position, token);
    } else {
      throw new ParseError(`Expected literal, got ${token.type}`, state.position, token);
    }

    // Advance past the literal token
    state = state.advance();

    // Create the AST node with token offset
    const node: AST.LiteralExpression = {
      type: 'Literal',
      value,
      literalType,
      tokenOffset
    };

    return { node, state };
  }

  /**
   * Parse an identifier expression.
   *
   * Grammar:
   *   identifier = IDENTIFIER
   *
   * Examples:
   *   myVar     -> IdentifierExpression { name: "myVar", tokenOffset: n }
   *   _private  -> IdentifierExpression { name: "_private", tokenOffset: n }
   *   x123      -> IdentifierExpression { name: "x123", tokenOffset: n }
   *
   * The token offset allows retrieving the original identifier token
   * with its surrounding trivia for formatting preservation.
   *
   * @param state Current parser state
   * @returns Parsed identifier expression and new state
   * @throws ParseError if current token is not an identifier
   */
  parseIdentifier(state: ParserState): ParseResult<AST.IdentifierExpression> {
    // Skip any leading trivia
    state = state.skipTrivia();
    const tokenOffset = state.currentOffset();
    const token = state.current();

    // Validate we have an identifier-like token
    // Accept: IDENTIFIER, TYPE_KEYWORD, RESERVED_WORD as valid identifiers in expression context
    // Note: BLOCK_FORMING_KEYWORD and DATA_STRUCTURE_KEYWORD are handled separately
    if (!token || (token.type !== TokenType.IDENTIFIER &&
      token.type !== TokenType.TYPE_KEYWORD &&
      token.type !== TokenType.RESERVED_WORD)) {
      throw new ParseError('Expected identifier', state.position, token || undefined);
    }

    // Advance past the identifier token
    state = state.advance();

    // Create the AST node with token offset
    const node: AST.IdentifierExpression = {
      type: 'Identifier',
      name: token.content,
      tokenOffset
    };

    return { node, state };
  }
}