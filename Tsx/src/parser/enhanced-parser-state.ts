/**
 * Enhanced ParserState with Source Position Tracking
 *
 * This extends the existing ParserState to track character positions
 * in addition to token offsets, enabling perfect source reconstruction.
 */

import { Token, TokenStream, TokenType } from '../lexer';
import { ParserState } from './parser-state';

/**
 * Source position information
 */
export interface SourcePosition {
  /** Character offset in source string */
  offset: number;
  /** Line number (1-based) */
  line: number;
  /** Column number (1-based) */
  column: number;
}

/**
 * Enhanced parser state that tracks source positions
 */
export class EnhancedParserState implements ParserState {
  private tokens: Token[];
  private tokenOffset: number;
  private source: string;
  private sourcePositions: SourcePosition[];

  constructor(
    tokens: Token[],
    tokenOffset: number,
    source: string,
    sourcePositions: SourcePosition[]
  ) {
    this.tokens = tokens;
    this.tokenOffset = tokenOffset;
    this.source = source;
    this.sourcePositions = sourcePositions;
  }

  /**
   * Create enhanced parser state from a token stream
   */
  static fromTokenStream(stream: TokenStream, source: string): EnhancedParserState {
    const tokens = stream.getAllTokens();
    const sourcePositions = EnhancedParserState.computeSourcePositions(tokens, source);
    return new EnhancedParserState(tokens, 0, source, sourcePositions);
  }

  /**
   * Compute source positions for all tokens
   */
  private static computeSourcePositions(tokens: Token[], source: string): SourcePosition[] {
    const positions: SourcePosition[] = [];
    let offset = 0;
    let line = 1;
    let column = 1;

    for (const token of tokens) {
      // Record position for this token
      positions.push({ offset, line, column });

      // Update position based on token content
      for (const char of token.content) {
        if (char === '\n') {
          line++;
          column = 1;
        } else {
          column++;
        }
        offset++;
      }
    }

    return positions;
  }

  /**
   * Get current source position (character offset)
   */
  currentSourcePosition(): number {
    if (this.tokenOffset >= this.sourcePositions.length) {
      return this.source.length;
    }
    return this.sourcePositions[this.tokenOffset].offset;
  }

  /**
   * Get detailed position information
   */
  currentPositionInfo(): SourcePosition {
    if (this.tokenOffset >= this.sourcePositions.length) {
      // End of file position
      const lines = this.source.split('\n');
      return {
        offset: this.source.length,
        line: lines.length,
        column: lines[lines.length - 1].length + 1
      };
    }
    return this.sourcePositions[this.tokenOffset];
  }

  /**
   * Get source position after current token
   */
  positionAfterCurrent(): number {
    if (this.tokenOffset >= this.tokens.length) {
      return this.source.length;
    }

    const currentPos = this.sourcePositions[this.tokenOffset].offset;
    const currentToken = this.tokens[this.tokenOffset];
    return currentPos + currentToken.content.length;
  }

  // Implement ParserState interface methods
  current(): Token | null {
    if (this.tokenOffset >= this.tokens.length) {
      return null;
    }
    return this.tokens[this.tokenOffset];
  }

  currentOffset(): number {
    return this.tokenOffset;
  }

  advance(): EnhancedParserState {
    return new EnhancedParserState(
      this.tokens,
      this.tokenOffset + 1,
      this.source,
      this.sourcePositions
    );
  }

  skipTrivia(): EnhancedParserState {
    let offset = this.tokenOffset;
    while (offset < this.tokens.length) {
      const token = this.tokens[offset];
      if (token.type === TokenType.SPACE ||
          token.type === TokenType.NEWLINE ||
          token.type === TokenType.COMMENT ||
          token.type === TokenType.TRIVIA) {
        offset++;
      } else {
        break;
      }
    }
    return new EnhancedParserState(
      this.tokens,
      offset,
      this.source,
      this.sourcePositions
    );
  }

  isAtEnd(): boolean {
    return this.tokenOffset >= this.tokens.length ||
           this.tokens[this.tokenOffset].type === TokenType.EOF;
  }

  peek(offset: number = 1): Token | null {
    const targetOffset = this.tokenOffset + offset;
    if (targetOffset >= this.tokens.length) {
      return null;
    }
    return this.tokens[targetOffset];
  }

  get position(): any {
    return this.currentPositionInfo();
  }

  /**
   * Mark a source range for an AST node
   */
  markRange(startState: EnhancedParserState): SourceRange {
    return {
      startOffset: startState.currentSourcePosition(),
      endOffset: this.positionAfterCurrent(),
      startPosition: startState.currentPositionInfo(),
      endPosition: this.currentPositionInfo()
    };
  }

  /**
   * Get the source text for a range
   */
  getRangeText(range: SourceRange): string {
    return this.source.substring(range.startOffset, range.endOffset);
  }
}

/**
 * Extended source range with position information
 */
export interface SourceRange {
  startOffset: number;
  endOffset: number;
  startPosition: SourcePosition;
  endPosition: SourcePosition;
}

/**
 * Helper function to wrap existing parser methods with range tracking
 */
export function withRangeTracking<T extends { type: string }>(
  parseMethod: (state: EnhancedParserState) => { node: T; state: EnhancedParserState } | null
): (state: EnhancedParserState) => { node: T & { sourceRange: SourceRange }; state: EnhancedParserState } | null {
  return (state: EnhancedParserState) => {
    const startState = state;
    const result = parseMethod(state);

    if (!result) {
      return null;
    }

    const sourceRange = result.state.markRange(startState);
    const nodeWithRange = {
      ...result.node,
      sourceRange
    };

    return {
      node: nodeWithRange,
      state: result.state
    };
  };
}

/**
 * Example: Using enhanced parser state
 */
export function exampleUsage(): void {
  const source = `  identifier  +  value  `;
  const stream = TokenStream.fromString(source);
  const state = EnhancedParserState.fromTokenStream(stream, source);

  console.log('Initial position:', state.currentSourcePosition()); // 0
  console.log('Current token:', state.current()?.content); // "  "

  const skipped = state.skipTrivia();
  console.log('After skip trivia:', skipped.currentSourcePosition()); // 2
  console.log('Current token:', skipped.current()?.content); // "identifier"

  const advanced = skipped.advance();
  console.log('After advance:', advanced.currentSourcePosition()); // 12
  console.log('Current token:', advanced.current()?.content); // "  "

  // Mark a range
  const range = advanced.markRange(skipped);
  console.log('Range:', range); // { startOffset: 2, endOffset: 14, ... }
  console.log('Range text:', advanced.getRangeText(range)); // "identifier  "
}