/**
 * Verse Lexer Package
 *
 * This package provides lexical analysis for the Verse language.
 * The main interface is through the lex() function which returns
 * a TokenStream for navigating and manipulating tokens.
 */

import { TokenStream } from './tokenstream';
import { Token, TokenType } from './token';
import { Lexer } from './lexer';

// Re-export the essential types
export { Token, TokenType } from './token';
export { TokenStream } from './tokenstream';
// Also export Lexer for backward compatibility
export { Lexer } from './lexer';

/**
 * Options for lexing
 */
export interface LexOptions {
  /** Tab width for calculating visual length (default: 4) */
  tabWidth?: number;
  /** Whether to automatically combine trivia tokens */
  combineTrivia?: boolean;
}

/**
 * Lex a string of Verse source code
 * @param source The source code to lex
 * @param options Optional lexing options
 * @returns A TokenStream for navigating the tokens
 */
export function lex(source: string, options?: LexOptions): TokenStream {
  const opts = {
    tabWidth: 4,
    ...options
  };
  const stream = TokenStream.fromString(source, opts.tabWidth);
  stream.combineTrivia();
  return stream;
}

/**
 * Lex a file of Verse source code
 * @param filePath Path to the file to lex
 * @param options Optional lexing options
 * @returns A TokenStream for navigating the tokens
 */
export function lexFile(filePath: string, options?: LexOptions): TokenStream {
  const opts = {
    tabWidth: 4,
    ...options
  };
  const stream = TokenStream.fromFile(filePath, opts.tabWidth);
  stream.combineTrivia();
  return stream;
}


/**
 * Utility function to pretty print tokens
 * @param source The source code to analyze
 * @param options Options for pretty printing
 */
export function prettyPrintTokens(
  source: string,
  options?: {
    combineTrivia?: boolean;
    skipTrivia?: boolean;
    skipWhitespace?: boolean;
    skipComments?: boolean;
  }
): string {
  const stream = lex(source, { combineTrivia: options?.combineTrivia });
  if (options?.skipTrivia || options?.skipWhitespace || options?.skipComments) {
    return stream.prettyPrintFilteredContents({
      skipTrivia: options.skipTrivia,
      skipWhitespace: options.skipWhitespace,
      skipComments: options.skipComments
    });
  }
  return stream.prettyPrintContents();
}

/**
 * Check if a token is meaningful (not trivia, spaces, tabs, comments, or EOF)
 * Note: Newlines ARE considered meaningful as they can be delimiters
 */
export function isMeaningful(token: Token): boolean {
  return !token.isTrivia() &&
    !token.isWhitespace() &&
    !token.isComment() &&
    !token.isEOF();
}

/**
 * Get only meaningful tokens from source
 */
export function getMeaningfulTokens(source: string): Token[] {
  const stream = lex(source, { combineTrivia: true });
  return stream.getAllTokens().filter(isMeaningful);
}