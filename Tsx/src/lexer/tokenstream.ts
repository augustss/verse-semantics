import { Token, TokenType, Position } from './token';
import { Lexer } from './lexer';
import * as fs from 'fs';

/**
 * TokenStream provides navigation and manipulation of a token sequence.
 *
 * Features:
 * - Forward/backward navigation through tokens
 * - Token filtering and searching
 * - Whitespace and comment skipping
 * - TRIVIA token combination for efficient parsing
 * - Pretty printing for debugging
 */
export class TokenStream {
  private tokens: Token[] = [];
  private currentIndex: number = 0;
  private filePath?: string;
  private source?: string;

  /**
   * Creates a new TokenStream.
   * @param tokens - Array of tokens to wrap
   * @param filePath - Optional source file path for debugging
   * @param source - Optional source code for reference
   */
  constructor(tokens: Token[], filePath?: string, source?: string) {
    this.tokens = tokens;
    this.filePath = filePath;
    this.source = source;
  }

  /**
   * Creates a TokenStream from a file.
   * @param filePath - Path to the source file
   * @param tabWidth - Visual width of tab characters (default: 4)
   * @returns TokenStream with the file's tokens
   */
  static fromFile(filePath: string, tabWidth: number = 4): TokenStream {
    const source = fs.readFileSync(filePath, 'utf-8');
    const lexer = new Lexer(source, tabWidth);
    const tokens = lexer.tokenize();
    return new TokenStream(tokens, filePath, source);
  }

  /**
   * Creates a TokenStream from a string.
   * @param source - Source code string
   * @param tabWidth - Visual width of tab characters (default: 4)
   * @returns TokenStream with the source's tokens
   */
  static fromString(source: string, tabWidth: number = 4): TokenStream {
    const lexer = new Lexer(source, tabWidth);
    const tokens = lexer.tokenize();
    return new TokenStream(tokens, undefined, source);
  }

  /**
   * Gets the current token without advancing.
   * @returns Current token or null if at end
   */
  current(): Token | null {
    if (this.currentIndex >= this.tokens.length) return null;
    return this.tokens[this.currentIndex];
  }

  /**
   * Looks ahead in the stream without advancing.
   * @param offset - Number of tokens to look ahead (default: 1)
   * @returns Token at offset or null if out of bounds
   */
  peek(offset: number = 1): Token | null {
    const index = this.currentIndex + offset;
    if (index < 0 || index >= this.tokens.length) return null;
    return this.tokens[index];
  }

  /**
   * Advances to and returns the next token.
   * @returns Current token before advancing, or null if at end
   */
  next(): Token | null {
    if (this.currentIndex >= this.tokens.length) return null;
    const token = this.tokens[this.currentIndex];
    this.currentIndex++;
    return token;
  }

  /**
   * Moves back to the previous token.
   * @returns Previous token or null if at start
   */
  previous(): Token | null {
    if (this.currentIndex <= 0) return null;
    this.currentIndex--;
    return this.tokens[this.currentIndex];
  }

  /**
   * Skips all whitespace and comment tokens.
   * Advances the stream past spaces, tabs, newlines, and comments.
   */
  skipWhitespaceAndComments(): void {
    while (this.current() && (this.current()!.isWhitespace() || this.current()!.isComment())) {
      this.next();
    }
  }

  /**
   * Skips whitespace tokens only (not comments).
   * Advances past spaces, tabs, and newlines.
   */
  skipWhitespace(): void {
    while (this.current() && this.current()!.isWhitespace()) {
      this.next();
    }
  }


  /**
   * Checks if current token matches a type without consuming.
   * @param type - TokenType to match
   * @returns true if current token matches type
   */
  match(type: TokenType): boolean {
    const token = this.current();
    return token !== null && token.type === type;
  }


  /**
   * Gets a copy of all tokens in the stream.
   * @returns Array of all tokens
   */
  getAllTokens(): Token[] {
    return [...this.tokens];
  }


  /**
   * Gets the current index in the token stream.
   * @returns Current index position
   */
  getPosition(): number {
    return this.currentIndex;
  }

  /**
   * Sets the current position in the stream.
   * @param index - New index position (must be valid)
   */
  setPosition(index: number): void {
    if (index >= 0 && index <= this.tokens.length) {
      this.currentIndex = index;
    }
  }

  /**
   * Resets the stream to the beginning.
   */
  reset(): void {
    this.currentIndex = 0;
  }

  /**
   * Checks if the stream is at the end.
   * @returns true if at end or current token is EOF
   */
  isAtEnd(): boolean {
    return this.currentIndex >= this.tokens.length ||
           (this.current() !== null && this.current()!.isEOF());
  }


  /**
   * Filters tokens by type.
   * @param type - TokenType to filter by
   * @returns Array of tokens matching the type
   */
  getTokensByType(type: TokenType): Token[] {
    return this.tokens.filter(t => t.type === type);
  }


  /**
   * Finds the next token of a specific type.
   * @param type - TokenType to search for
   * @returns Next token of type or null if not found
   */
  findNext(type: TokenType): Token | null {
    for (let i = this.currentIndex; i < this.tokens.length; i++) {
      if (this.tokens[i].type === type) {
        return this.tokens[i];
      }
    }
    return null;
  }

  /**
   * Finds the previous token of a specific type.
   * @param type - TokenType to search for
   * @returns Previous token of type or null if not found
   */
  findPrevious(type: TokenType): Token | null {
    for (let i = this.currentIndex - 1; i >= 0; i--) {
      if (this.tokens[i].type === type) {
        return this.tokens[i];
      }
    }
    return null;
  }


  /**
   * Makes TokenStream iterable with for...of loops.
   * @returns Iterator over all tokens
   */
  [Symbol.iterator](): Iterator<Token> {
    let index = 0;
    const tokens = this.tokens;

    return {
      next(): IteratorResult<Token> {
        if (index < tokens.length) {
          return { value: tokens[index++], done: false };
        }
        return { done: true, value: undefined };
      }
    };
  }

  /**
   * Creates a debug string showing current stream state.
   * @returns String with previous, current, and next tokens
   */
  toString(): string {
    const current = this.current();
    const prev = this.currentIndex > 0 ? this.tokens[this.currentIndex - 1] : null;
    const next = this.peek();

    let result = `TokenStream[${this.currentIndex}/${this.tokens.length}]\n`;
    if (prev) result += `  prev: ${prev.toString()}\n`;
    if (current) result += `  curr: ${current.toString()}\n`;
    if (next) result += `  next: ${next.toString()}\n`;

    return result;
  }

  /**
   * Pretty prints a range of tokens for debugging.
   * @param start - Starting index (default: 0)
   * @param count - Number of tokens to print (default: 10)
   */
  prettyPrint(start: number = 0, count: number = 10): void {
    const end = Math.min(start + count, this.tokens.length);

    console.log(`\nTokens [${start}-${end - 1}]:`);
    console.log('─'.repeat(60));

    for (let i = start; i < end; i++) {
      const token = this.tokens[i];
      const marker = i === this.currentIndex ? '→' : ' ';
      const content = token.content
        .replace(/\n/g, '\\n')
        .replace(/\t/g, '\\t')
        .slice(0, 30);

      console.log(
        `${marker} ${i.toString().padStart(3)}: ` +
        `${token.type.padEnd(18)} ` +
        `"${content}${token.content.length > 30 ? '...' : ''}" ` +
        `(${token.position.line}:${token.position.column})`
      );
    }
    console.log('─'.repeat(60));
  }

  /**
   * Reconstructs the original source from tokens.
   * Adds quotes back to string tokens for proper reconstruction.
   * @returns Reconstructed source string
   */
  prettyPrintContents(): string {
    let result = '';

    for (const token of this.tokens) {
      // Skip EOF token
      if (token.isEOF()) {
        break;
      }

      // Add quotes back to strings for proper reconstruction
      if (token.isString()) {
        result += '"' + token.content + '"';
      } else {
        result += token.content;
      }
    }

    return result;
  }

  /**
   * Reconstructs source with optional token filtering.
   * @param options - Filtering options for token types
   * @returns Filtered reconstructed source string
   */
  prettyPrintFilteredContents(options?: {
    skipTrivia?: boolean;
    skipWhitespace?: boolean;
    skipComments?: boolean;
    skipEOF?: boolean;
  }): string {
    const opts = {
      skipTrivia: false,
      skipWhitespace: false,
      skipComments: false,
      skipEOF: true,
      ...options
    };

    let result = '';

    for (const token of this.tokens) {
      // Apply filters
      if (opts.skipEOF && token.isEOF()) continue;
      if (opts.skipTrivia && token.isTrivia()) continue;
      if (opts.skipWhitespace && token.isWhitespace()) continue;
      if (opts.skipComments && token.isComment()) continue;

      // Add quotes back to strings for proper reconstruction
      if (token.isString()) {
        result += '"' + token.content + '"';
      } else {
        result += token.content;
      }
    }

    return result;
  }

  /**
   * Combines consecutive whitespace and comment tokens into TRIVIA tokens.
   *
   * Rules:
   * 1. TRIVIA tokens do NOT include newlines (EOL)
   * 2. Newlines remain as separate NEWLINE tokens
   * 3. TRIVIA combines only spaces, tabs, and comments
   *
   * This optimization reduces token count for faster parsing while
   * preserving line structure for indentation-sensitive parsing.
   */
  combineTrivia(): void {
    const newTokens: Token[] = [];
    let i = 0;

    while (i < this.tokens.length) {
      const token = this.tokens[i];

      // Check if this token starts a trivia sequence (spaces, tabs, or comments - not newlines)
      if (token.isWhitespace() || token.isComment()) {
        // Collect all consecutive trivia tokens (spaces, tabs, comments - but not newlines)
        const triviaTokens: Token[] = [];
        let combinedContent = '';
        let startPosition = token.position;

        while (i < this.tokens.length) {
          const current = this.tokens[i];

          // Stop at newline (keep it separate)
          if (current.isNewline()) {
            break;
          }

          // Stop at non-trivia
          if (!current.isWhitespace() && !current.isComment()) {
            break;
          }

          triviaTokens.push(current);
          combinedContent += current.content;
          i++;
        }

        // Calculate length: all non-comment characters
        let finalLength = 0;
        for (const tok of triviaTokens) {
          if (!tok.isComment()) {
            finalLength += tok.length;
          }
        }

        // Create TRIVIA token
        const lastToken = triviaTokens[triviaTokens.length - 1];
        const endPosition: Position = {
          line: lastToken.endPosition.line,
          column: lastToken.endPosition.column,
          offset: lastToken.endPosition.offset
        };

        const triviaToken = new Token(
          combinedContent,
          TokenType.TRIVIA,
          startPosition,
          endPosition,
          finalLength // indentation/length
        );

        newTokens.push(triviaToken);
      } else {
        // Keep newlines and other tokens as is
        newTokens.push(token);
        i++;
      }
    }

    // Replace the tokens array with the new combined version
    this.tokens = newTokens;

    // Reset current index if it's out of bounds
    if (this.currentIndex >= this.tokens.length) {
      this.currentIndex = Math.max(0, this.tokens.length - 1);
    }
  }
}