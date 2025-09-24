/**
 * Parser State Management
 *
 * This module provides immutable parser state management for the Verse parser.
 * The ParserState class wraps a TokenStream and maintains:
 * - Current position in the token stream
 * - Indentation sensitivity flag for block parsing
 * - Current indentation level for nested structures
 *
 * Key features:
 * - Immutable state for easy backtracking
 * - Token offset tracking for AST nodes
 * - Indentation context management
 * - Lookahead for indentation detection
 */

import { TokenStream } from '../lexer/tokenstream';
import { Token, TokenType } from '../lexer/token';
import * as AST from './ast';

/**
 * Immutable parser state that tracks position in a token stream.
 *
 * This class provides a functional approach to parsing where each
 * operation returns a new state rather than modifying the existing one.
 *
 * State components:
 * - tokens: The underlying token stream
 * - position: Current offset in the token stream
 * - indentationStack: Stack of indentation levels for nested blocks
 *
 * The indentation stack allows proper handling of nested indented
 * structures like nested for loops, if statements within blocks, etc.
 * An empty stack means indentation-insensitive parsing.
 *
 * All methods return new state instances, preserving immutability.
 */
export class ParserState {
  readonly tokens: TokenStream;
  readonly position: number;
  readonly indentationStack: readonly number[];

  constructor(
    tokens: TokenStream,
    position: number = 0,
    indentationStack: readonly number[] = []
  ) {
    this.tokens = tokens;
    this.position = position;
    this.indentationStack = indentationStack;
  }

  /**
   * Get the current token without advancing.
   * Preserves the original stream position.
   */
  current(): Token | null {
    const originalPos = this.tokens.getPosition();
    this.tokens.setPosition(this.position);
    const token = this.tokens.current();
    this.tokens.setPosition(originalPos);
    return token;
  }

  /**
   * Get the current position/offset in the token stream.
   *
   * This offset is stored in AST nodes instead of token references,
   * allowing the original tokens (with trivia) to be retrieved later
   * for source reconstruction or pretty printing.
   */
  currentOffset(): number {
    return this.position;
  }

  /**
   * Look ahead at future tokens without advancing.
   * @param offset Number of tokens to look ahead (default: 1)
   */
  peek(offset: number = 1): Token | null {
    const originalPos = this.tokens.getPosition();
    this.tokens.setPosition(this.position);
    const token = this.tokens.peek(offset);
    this.tokens.setPosition(originalPos);
    return token;
  }

  /**
   * Create a new parser state advanced by one token.
   * Original state remains unchanged (immutable).
   */
  advance(): ParserState {
    return new ParserState(
      this.tokens,
      this.position + 1,
      this.indentationStack
    );
  }

  /**
   * Enter an indentation-sensitive context.
   *
   * Used when parsing indented blocks after ':' operators.
   * Pushes a new indentation level onto the stack, allowing
   * nested indented structures.
   *
   * @param indentLevel The column number of the indented block
   * @returns New state with indentation level pushed onto stack
   */
  enterIndentationContext(indentLevel: number): ParserState {
    return new ParserState(
      this.tokens,
      this.position,
      [...this.indentationStack, indentLevel]
    );
  }

  /**
   * Exit an indentation-sensitive context.
   *
   * Pops the most recent indentation level from the stack.
   * Returns to the previous indentation context (which could be
   * another indented block or top-level parsing).
   *
   * @returns New state with top indentation level removed
   */
  exitIndentationContext(): ParserState {
    if (this.indentationStack.length === 0) {
      // Already at top level, return unchanged
      return this;
    }

    return new ParserState(
      this.tokens,
      this.position,
      this.indentationStack.slice(0, -1)
    );
  }

  /**
   * Check if we're currently in an indentation-sensitive context.
   * True if the indentation stack is non-empty.
   */
  get indentationSensitive(): boolean {
    return this.indentationStack.length > 0;
  }

  /**
   * Get the current indentation level.
   * Returns the top of the stack or 0 if not in indented context.
   */
  get currentIndentationLevel(): number {
    return this.indentationStack.length > 0
      ? this.indentationStack[this.indentationStack.length - 1]
      : 0;
  }

  /**
   * Check if we've reached the end of the token stream.
   */
  isAtEnd(): boolean {
    const token = this.current();
    return !token || token.isEOF();
  }

  /**
   * Check if current token matches a specific type.
   * Useful for conditional parsing without consuming tokens.
   */
  match(type: TokenType): boolean {
    const token = this.current();
    return token !== null && token.type === type;
  }

  /**
   * Check if current token matches any of the given types.
   * Useful for parsing operators with multiple representations.
   */
  matchAny(...types: TokenType[]): boolean {
    const token = this.current();
    return token !== null && types.includes(token.type);
  }

  /**
   * Check if current token has specific content.
   * Useful for matching specific operators or keywords.
   */
  matchContent(content: string): boolean {
    const token = this.current();
    return token !== null && token.content === content;
  }

  /**
   * Check if a token at the given column is properly indented
   * relative to the current indentation context.
   *
   * @param column The column position to check
   * @returns true if the column is greater than current indentation level
   */
  isProperlyIndented(column: number): boolean {
    return column > this.currentIndentationLevel;
  }

  /**
   * Check if we've dedented from the current block.
   * A dedent occurs when we see a line at or before the current
   * indentation level.
   *
   * @param column The column position to check
   * @returns true if we've dedented out of current block
   */
  hasDedented(column: number): boolean {
    // Only check if we're in an indentation context AND the column is less than or equal
    // Note: We use < for dedent, not <=, because same indentation means still in the block
    return this.indentationStack.length > 0 && column < this.currentIndentationLevel;
  }

  /**
   * Determine how many indentation levels to pop based on column position.
   * Returns the number of levels that should be exited.
   *
   * @param column The column position to check
   * @returns Number of indentation levels to exit
   */
  getDedentLevel(column: number): number {
    let count = 0;
    for (let i = this.indentationStack.length - 1; i >= 0; i--) {
      if (column <= this.indentationStack[i]) {
        count++;
      } else {
        break;
      }
    }
    return count;
  }

  /**
   * Exit multiple indentation levels at once.
   * Used when a dedent crosses multiple indentation boundaries.
   *
   * @param levels Number of levels to exit
   * @returns New state with specified number of levels popped
   */
  exitMultipleIndentationContexts(levels: number): ParserState {
    if (levels <= 0 || this.indentationStack.length === 0) {
      return this;
    }

    const levelsToKeep = Math.max(0, this.indentationStack.length - levels);
    return new ParserState(
      this.tokens,
      this.position,
      this.indentationStack.slice(0, levelsToKeep)
    );
  }

  /**
   * Look ahead after a colon to find the indentation of the next significant line.
   *
   * This method is crucial for indentation-sensitive parsing.
   * When we see a ':' operator, we need to determine the indentation
   * level of the following block without consuming tokens.
   *
   * Algorithm:
   * 1. Skip forward to find the next newline
   * 2. Skip trivia/whitespace after the newline
   * 3. Return the column of the first significant token
   *
   * @returns The column number of the first non-trivia token on the next line,
   *          or null if no indented content is found
   */
  getNextLineIndentation(): number | null {
    const originalPos = this.tokens.getPosition();
    this.tokens.setPosition(this.position);

    let pos = this.position;
    let foundNewline = false;

    // Advance until we find a newline
    while (pos < this.tokens.getAllTokens().length) {
      this.tokens.setPosition(pos);
      const token = this.tokens.current();
      if (!token) break;

      if (token.type === TokenType.NEWLINE) {
        foundNewline = true;
        pos++;
        break;
      }
      pos++;
    }

    if (!foundNewline) {
      this.tokens.setPosition(originalPos);
      return null;
    }

    // Skip past any trivia/whitespace to find the first significant token
    while (pos < this.tokens.getAllTokens().length) {
      this.tokens.setPosition(pos);
      const token = this.tokens.current();
      if (!token) break;

      if (!token.isTrivia() && !token.isWhitespace() && !token.isComment() && token.type !== TokenType.NEWLINE) {
        // Found a significant token
        const indent = token.position.column;
        this.tokens.setPosition(originalPos);
        return indent;
      }
      pos++;
    }

    this.tokens.setPosition(originalPos);
    return null;
  }

  /**
   * Skip over trivia tokens (whitespace, comments, TRIVIA).
   *
   * Behavior varies based on context:
   * - Normal mode: Skips all trivia tokens
   * - Indentation-sensitive mode: Stops at newlines that would
   *   exit the current indented block (based on indentation stack)
   *
   * This ensures proper block boundary detection in indented contexts,
   * including nested indented structures.
   *
   * @returns New state positioned at next meaningful token
   */
  skipTrivia(): ParserState {
    let state: ParserState = this;
    while (!state.isAtEnd()) {
      const token = state.current();
      if (!token) break;

      // Skip trivia, whitespace, comments, and (when not indentation-sensitive) newlines
      const isSkippable = token.isTrivia() || token.isWhitespace() || token.isComment() ||
                          (!state.indentationSensitive && token.type === TokenType.NEWLINE);

      if (!isSkippable) {
        break;
      }

      // In indentation-sensitive context, check if we should stop at a newline
      if (state.indentationSensitive && token.type === TokenType.NEWLINE) {
        // Look ahead to check indentation of next line
        const nextToken = state.peek(1);
        if (nextToken && !nextToken.isTrivia() && !nextToken.isWhitespace() && !nextToken.isComment()) {
          // Check if indentation would break out of ANY context in the stack
          if (state.hasDedented(nextToken.position.column)) {
            break; // Don't skip this newline, we're exiting an indented block
          }
        }
      }

      state = state.advance();
    }
    return state;
  }
}

/**
 * Result of a parse operation.
 * Contains both the parsed AST node and the new parser state.
 */
export interface ParseResult<T extends AST.ASTNode> {
  /** The parsed AST node */
  node: T;
  /** Parser state after parsing this node */
  state: ParserState;
}

/**
 * Parse error with position information.
 * Includes the problematic token when available for better error messages.
 */
export class ParseError extends Error {
  /** Position in the token stream where error occurred */
  readonly position: number;
  /** The token that caused the error (if available) */
  readonly token?: Token;

  constructor(message: string, position: number, token?: Token) {
    super(message);
    this.name = 'ParseError';
    this.position = position;
    this.token = token;
  }
}