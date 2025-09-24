/**
 * Compound Expression Parser
 *
 * This module handles parsing of compound and block expressions:
 *
 * Compound expressions: { expr1; expr2; expr3 }
 * - Brace-delimited sequences
 * - Semicolon or newline separators
 * - Stores offsets for braces and separators
 *
 * Indented compound expressions: keyword:
 * - Block-forming keywords (if, then, else, for, block)
 * - Colon-initiated indented blocks
 * - Uses indentation tracking for block boundaries
 * - Stores keyword, colon, and separator offsets
 */

import { Token, TokenType } from '../../lexer/token';
import { ParserState, ParseResult, ParseError } from '../parser-state';
import * as AST from '../ast';

/**
 * Parser for compound expressions.
 *
 * This class is instantiated with a reference to the main expression parser,
 * allowing it to recursively parse nested expressions within compounds.
 *
 * Key responsibilities:
 * - Parse brace-delimited compounds with proper delimiter handling
 * - Parse indented blocks using indentation context tracking
 * - Record token offsets for all significant syntax elements
 */
export class CompoundParser {
  private parseExpression: (state: ParserState) => ParseResult<AST.Expression>;

  constructor(parseExpression: (state: ParserState) => ParseResult<AST.Expression>) {
    this.parseExpression = parseExpression;
  }

  /**
   * Parse a brace-delimited compound expression.
   *
   * Grammar:
   *   compound = "{" (expression (delimiter expression)*)? "}"
   *   delimiter = ";" | NEWLINE
   *
   * Examples:
   *   { }                   -> CompoundExpression { expressions: [] }
   *   { a; b; c }          -> CompoundExpression { expressions: [a, b, c] }
   *   { x := 1\n y := 2 }  -> CompoundExpression { expressions: [x := 1, y := 2] }
   *
   * Offset tracking:
   * - openBraceOffset: Position of '{'
   * - closeBraceOffset: Position of '}'
   * - separatorOffsets: Positions of ';' or newline delimiters
   *
   * @param state Current parser state
   * @returns Parsed compound expression and new state
   * @throws ParseError if braces or delimiters are malformed
   */
  parseCompoundExpression(state: ParserState): ParseResult<AST.CompoundExpression> {
    state = state.skipTrivia();
    const openBraceOffset = state.currentOffset();
    const openBrace = state.current();

    // Verify opening brace
    if (!openBrace || openBrace.type !== TokenType.OPERATOR || openBrace.content !== '{') {
      throw new ParseError('Expected {', state.position, openBrace || undefined);
    }

    state = state.advance();

    // Don't use indentation context for brace-delimited compounds
    // We'll handle newlines explicitly as separators

    const expressions: AST.Expression[] = [];
    const separatorOffsets: number[] = [];

    // Parse expressions within the compound
    while (!state.isAtEnd()) {
      // Skip whitespace and comments but NOT newlines (we need them as separators)
      while (state.current() &&
             (state.current()!.isTrivia() ||
              state.current()!.isWhitespace() ||
              state.current()!.isComment()) &&
             state.current()!.type !== TokenType.NEWLINE) {
        state = state.advance();
      }

      // Check for closing brace
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}') {
        break;
      }

      // Check for empty expression (semicolon or newline)
      const current = state.current();
      if (current?.type === TokenType.OPERATOR && current.content === ';') {
        // Empty expression - just record the separator
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
        continue;
      } else if (current?.type === TokenType.NEWLINE) {
        // Empty line - record separator and continue
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
        continue;
      }

      // Parse expression
      const exprResult = this.parseExpression(state);
      expressions.push(exprResult.node);
      state = exprResult.state;

      // Skip trailing whitespace/comments on the same line (but not newlines)
      while (state.current() &&
             (state.current()!.isTrivia() ||
              state.current()!.type === TokenType.SPACE ||
              state.current()!.type === TokenType.TAB ||
              state.current()!.isComment())) {
        state = state.advance();
      }

      // Check for delimiter (semicolon, newline) or closing brace
      const delim = state.current();
      if (delim?.type === TokenType.OPERATOR && delim.content === ';') {
        // Semicolon delimiter
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      } else if (delim?.type === TokenType.NEWLINE) {
        // Newline delimiter
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      } else if (delim?.type === TokenType.OPERATOR && delim.content === '}') {
        // Closing brace - ok, no delimiter needed
        continue;
      } else {
        // Check if this looks like the start of another expression at the compound level
        // This handles the case where an indented block (like else:) has completed
        // and we're back at the compound's indentation level
        // In this case, treat it as having an implicit newline separator
        if (delim?.type === TokenType.IDENTIFIER ||
            delim?.type === TokenType.BLOCK_FORMING_KEYWORD ||
            (delim?.type === TokenType.OPERATOR && delim.content === '(')) {
          // Looks like the start of a new expression - treat as implicit newline
          // Don't consume anything, just continue to parse the next expression
          continue;
        }

        // Otherwise it's an error
        throw new ParseError('Expected delimiter or }', state.position, delim || undefined);
      }
    }

    // Verify closing brace
    const closeBraceOffset = state.currentOffset();
    const closeBrace = state.current();
    if (!closeBrace || closeBrace.type !== TokenType.OPERATOR || closeBrace.content !== '}') {
      throw new ParseError('Expected }', state.position, closeBrace || undefined);
    }
    state = state.advance();

    // Create compound expression node with offsets
    const node: AST.CompoundExpression = {
      type: 'CompoundExpression',
      expressions,
      openBraceOffset,
      closeBraceOffset,
      separatorOffsets
    };

    return { node, state };
  }

  /**
   * Parse an indented compound expression.
   *
   * This handles indentation-based syntax after block-forming keywords.
   * Block-forming keywords: if, then, else, for, block
   *
   * Grammar:
   *   indented_compound = BLOCK_FORMING_KEYWORD ":" NEWLINE?
   *                      (INDENT expression (NEWLINE expression)* DEDENT)?
   *
   * Examples:
   *   if:              -> IdentedCompoundExpression { keyword: "if", expressions: [] }
   *     x := 1
   *     y := 2
   *
   *   for:             -> IdentedCompoundExpression { keyword: "for", expressions: [i := 0, process(i)] }
   *     i := 0
   *     process(i)
   *
   * Indentation handling:
   * - Looks ahead after ':' to find next line's indentation
   * - Enters indentation-sensitive parsing mode
   * - Automatically exits when indentation decreases
   *
   * Offset tracking:
   * - keywordOffset: Position of block-forming keyword
   * - colonOffset: Position of ':'
   * - separatorOffsets: Positions of newlines between expressions
   * - baseIndentation: Column number of the indented block
   *
   * @param state Current parser state
   * @returns Parsed indented compound expression and new state
   * @throws ParseError if keyword, colon, or indentation is malformed
   */
  parseIdentedCompound(state: ParserState): ParseResult<AST.IdentedCompoundExpression> {
    // Expect block-forming keyword
    state = state.skipTrivia();
    const keywordOffset = state.currentOffset();
    const keyword = state.current();
    if (!keyword || keyword.type !== TokenType.BLOCK_FORMING_KEYWORD) {
      throw new ParseError('Expected block-forming keyword', state.position, keyword || undefined);
    }

    state = state.advance().skipTrivia();

    // Expect colon after keyword
    const colonOffset = state.currentOffset();
    const colon = state.current();
    if (!colon || colon.type !== TokenType.OPERATOR || colon.content !== ':') {
      throw new ParseError('Expected : after block-forming keyword', state.position, colon || undefined);
    }

    state = state.advance();

    // Look ahead to find the indentation of the next line
    const nextLineIndent = state.getNextLineIndentation();
    const baseIndentation = nextLineIndent || 0;

    const expressions: AST.Expression[] = [];
    const separatorOffsets: number[] = [];

    // If we found indentation, parse the indented block
    if (nextLineIndent !== null) {
      // Enter indentation-sensitive context
      state = state.enterIndentationContext(nextLineIndent);

      // Skip to the first element
      state = state.skipTrivia();
      if (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance().skipTrivia();
      }

      // Parse indented expressions
      while (!state.isAtEnd()) {
        // Check if we're still in the indented block
        const currentToken = state.current();
        if (!currentToken) break;

        // The indentation context will handle stopping at unindented lines
        if (currentToken.position.column < nextLineIndent) {
          break;
        }

        // Parse expression
        const exprResult = this.parseExpression(state);
        expressions.push(exprResult.node);
        state = exprResult.state;

        // Skip any trailing trivia on the line
        state = state.skipTrivia();

        // Check for newline
        if (state.current()?.type === TokenType.NEWLINE) {
          separatorOffsets.push(state.currentOffset());
          state = state.advance().skipTrivia();
        } else {
          // No newline, we're done with the compound
          break;
        }
      }

      // Exit indentation context
      state = state.exitIndentationContext();
    }

    // Create indented compound expression node with offsets
    const node: AST.IdentedCompoundExpression = {
      type: 'IdentedCompoundExpression',
      expressions,
      keywordOffset,
      colonOffset,
      separatorOffsets,
      baseIndentation
    };

    return { node, state };
  }
}