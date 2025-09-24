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

