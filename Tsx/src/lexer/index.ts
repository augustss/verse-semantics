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
import { ColorFormatter, OutputFormat } from '../pretty-printer/color-formatter';

// Re-export the essential types
export { Token, TokenType } from './token';
export { TokenStream } from './tokenstream';
export { OutputFormat } from '../pretty-printer/color-formatter';

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
 * Options for colored pretty printing
 */
export interface ColorPrintOptions {
  /** Output format: 'terminal', 'html', or 'plain' */
  format?: OutputFormat;
  /** Color theme: 'default' or 'light' */
  theme?: string;
}

/**
 * Pretty print source code with syntax highlighting
 * @param source The source code to format
 * @param options Optional formatting options
 * @returns Formatted string with colors
 */
export function prettyPrintColored(source: string, options?: ColorPrintOptions): string {
  const format = options?.format ?? OutputFormat.Terminal;
  const theme = options?.theme ?? 'default';

  const stream = lex(source);
  const formatter = new ColorFormatter(format, theme);
  const tokens = stream.getAllTokens();

  return formatter.formatTokens(tokens);
}

/**
 * Generate HTML with syntax highlighting
 * @param source The source code to format
 * @param theme Color theme: 'default' or 'light'
 * @param includeCSS Whether to include CSS in output
 * @returns HTML string with syntax-highlighted code
 */
export function toHTML(source: string, theme: string = 'default', includeCSS: boolean = false): string {
  const html = prettyPrintColored(source, {
    format: OutputFormat.HTML,
    theme
  });

  if (includeCSS) {
    const css = ColorFormatter.generateCSS(theme);
    return `<style>${css}</style>\n${html}`;
  }

  return html;
}

