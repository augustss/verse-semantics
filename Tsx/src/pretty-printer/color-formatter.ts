/**
 * Color Formatter for Syntax Highlighting
 *
 * Provides colored output for Verse code in multiple formats:
 * - Terminal: Using ANSI escape codes
 * - HTML: Using span tags with classes/inline styles
 */

import { Token, TokenType } from '../lexer/token';

/**
 * Color scheme for syntax highlighting
 */
export interface ColorScheme {
  // Keywords
  keyword: string;           // if, for, class, etc.
  blockKeyword: string;      // block-forming keywords
  dataStructure: string;     // class, struct, interface
  declaration: string;       // var, const

  // Literals
  string: string;           // String literals
  number: string;           // Integer and float literals
  boolean: string;          // true, false

  // Identifiers
  identifier: string;       // Variable names
  type: string;            // Type names
  function: string;        // Function names
  decorator: string;       // @decorators

  // Operators & Punctuation
  operator: string;        // +, -, :=, etc.
  punctuation: string;     // (, ), {, }, etc.

  // Comments
  comment: string;         // Single and multi-line comments

  // Special
  error: string;          // Invalid tokens
  default: string;        // Default color
}

/**
 * Output format for colored text
 */
export enum OutputFormat {
  Terminal = 'terminal',
  HTML = 'html',
  PlainText = 'plain'
}

/**
 * Terminal color schemes using ANSI codes
 */
export const TERMINAL_THEMES = {
  // Default terminal theme - similar to VS Code Dark+
  default: {
    keyword: '\x1b[35m',        // Magenta
    blockKeyword: '\x1b[35m',   // Magenta
    dataStructure: '\x1b[34m',  // Blue
    declaration: '\x1b[34m',    // Blue

    string: '\x1b[32m',         // Green
    number: '\x1b[36m',         // Cyan
    boolean: '\x1b[33m',        // Yellow

    identifier: '\x1b[37m',     // White
    type: '\x1b[36m',          // Cyan
    function: '\x1b[33m',      // Yellow
    decorator: '\x1b[95m',     // Light Magenta

    operator: '\x1b[37m',      // White
    punctuation: '\x1b[90m',   // Gray

    comment: '\x1b[90m',       // Gray

    error: '\x1b[31m',         // Red
    default: '\x1b[0m'         // Reset
  },

  // Light theme - for light terminals
  light: {
    keyword: '\x1b[35m',        // Magenta
    blockKeyword: '\x1b[35m',   // Magenta
    dataStructure: '\x1b[34m',  // Blue
    declaration: '\x1b[34m',    // Blue

    string: '\x1b[32m',         // Green
    number: '\x1b[31m',         // Red
    boolean: '\x1b[33m',        // Yellow

    identifier: '\x1b[30m',     // Black
    type: '\x1b[36m',          // Cyan
    function: '\x1b[34m',      // Blue
    decorator: '\x1b[95m',     // Light Magenta

    operator: '\x1b[30m',      // Black
    punctuation: '\x1b[90m',   // Gray

    comment: '\x1b[90m',       // Gray

    error: '\x1b[91m',         // Light Red
    default: '\x1b[0m'         // Reset
  }
};

/**
 * HTML color schemes using CSS colors
 */
export const HTML_THEMES = {
  // Default HTML theme - VS Code Dark+ inspired
  default: {
    keyword: '#C586C0',        // Purple
    blockKeyword: '#C586C0',   // Purple
    dataStructure: '#569CD6',  // Blue
    declaration: '#569CD6',    // Blue

    string: '#CE9178',         // Orange
    number: '#B5CEA8',         // Light Green
    boolean: '#569CD6',        // Blue

    identifier: '#D4D4D4',     // Light Gray
    type: '#4EC9B0',          // Teal
    function: '#DCDCAA',      // Yellow
    decorator: '#C586C0',     // Purple

    operator: '#D4D4D4',      // Light Gray
    punctuation: '#808080',   // Gray

    comment: '#6A9955',       // Green

    error: '#F44747',         // Red
    default: '#D4D4D4'        // Light Gray
  },

  // Light HTML theme
  light: {
    keyword: '#0000FF',        // Blue
    blockKeyword: '#0000FF',   // Blue
    dataStructure: '#2B91AF',  // Dark Cyan
    declaration: '#0000FF',    // Blue

    string: '#A31515',         // Dark Red
    number: '#098658',         // Green
    boolean: '#0000FF',        // Blue

    identifier: '#000000',     // Black
    type: '#2B91AF',          // Dark Cyan
    function: '#795E26',      // Brown
    decorator: '#0000FF',     // Blue

    operator: '#000000',      // Black
    punctuation: '#000000',   // Black

    comment: '#008000',       // Green

    error: '#FF0000',         // Red
    default: '#000000'        // Black
  }
};

/**
 * Color formatter class
 */
export class ColorFormatter {
  private scheme: ColorScheme;
  private format: OutputFormat;

  constructor(format: OutputFormat = OutputFormat.Terminal, theme: string = 'default') {
    this.format = format;

    // Select color scheme based on format and theme
    switch (format) {
      case OutputFormat.Terminal:
        this.scheme = TERMINAL_THEMES[theme as keyof typeof TERMINAL_THEMES] || TERMINAL_THEMES.default;
        break;
      case OutputFormat.HTML:
        this.scheme = HTML_THEMES[theme as keyof typeof HTML_THEMES] || HTML_THEMES.default;
        break;
      default:
        // Plain text - no colors
        this.scheme = this.createPlainScheme();
    }
  }

  /**
   * Create a plain color scheme (no colors)
   */
  private createPlainScheme(): ColorScheme {
    return {
      keyword: '',
      blockKeyword: '',
      dataStructure: '',
      declaration: '',
      string: '',
      number: '',
      boolean: '',
      identifier: '',
      type: '',
      function: '',
      decorator: '',
      operator: '',
      punctuation: '',
      comment: '',
      error: '',
      default: ''
    };
  }

  /**
   * Get color for a token type
   */
  private getColorForToken(token: Token): string {
    switch (token.type) {
      // Keywords
      case TokenType.BLOCK_FORMING_KEYWORD:
        if (['if', 'for', 'while', 'loop', 'block', 'case'].includes(token.content)) {
          return this.scheme.keyword;
        }
        return this.scheme.blockKeyword;

      case TokenType.DATA_STRUCTURE_KEYWORD:
        return this.scheme.dataStructure;

      case TokenType.DECL_KEYWORD:
        return this.scheme.declaration;

      case TokenType.RESERVED_WORD:
        if (['true', 'false'].includes(token.content)) {
          return this.scheme.boolean;
        }
        return this.scheme.keyword;

      // Literals
      case TokenType.STRING:
      case TokenType.INVALID_STRING:
        return this.scheme.string;

      case TokenType.INTEGER:
      case TokenType.FLOAT:
        return this.scheme.number;

      // Identifiers
      case TokenType.IDENTIFIER:
        // Check for decorator
        if (token.content.startsWith('@')) {
          return this.scheme.decorator;
        }
        return this.scheme.identifier;

      case TokenType.TYPE_KEYWORD:
        return this.scheme.type;

      // Operators
      case TokenType.OPERATOR:
        if (['(', ')', '[', ']', '{', '}'].includes(token.content)) {
          return this.scheme.punctuation;
        }
        return this.scheme.operator;

      // Comments
      case TokenType.COMMENT:
      case TokenType.MULTILINE_COMMENT:
        return this.scheme.comment;

      // Specifiers
      case TokenType.SPECIFIER:
        return this.scheme.decorator;

      // Errors
      case TokenType.UNKNOWN:
        return this.scheme.error;

      // Whitespace and others
      case TokenType.SPACE:
      case TokenType.TAB:
      case TokenType.NEWLINE:
      case TokenType.TRIVIA:
      case TokenType.EOF:
      default:
        return this.scheme.default;
    }
  }

  /**
   * Format a single token with color
   */
  formatToken(token: Token, content?: string): string {
    const text = content !== undefined ? content : token.content;
    const color = this.getColorForToken(token);

    switch (this.format) {
      case OutputFormat.Terminal:
        if (!color || color === this.scheme.default) {
          return text + '\x1b[0m'; // Always reset after token
        }
        return color + text + '\x1b[0m';

      case OutputFormat.HTML:
        if (!color || color === this.scheme.default) {
          return this.escapeHtml(text);
        }
        const tokenClass = this.getTokenClass(token);
        return `<span class="${tokenClass}" style="color: ${color}">${this.escapeHtml(text)}</span>`;

      default:
        return text;
    }
  }

  /**
   * Get CSS class name for token type
   */
  private getTokenClass(token: Token): string {
    switch (token.type) {
      case TokenType.BLOCK_FORMING_KEYWORD:
      case TokenType.RESERVED_WORD:
        return 'keyword';
      case TokenType.DATA_STRUCTURE_KEYWORD:
        return 'type-keyword';
      case TokenType.DECL_KEYWORD:
        return 'declaration';
      case TokenType.STRING:
      case TokenType.INVALID_STRING:
        return 'string';
      case TokenType.INTEGER:
      case TokenType.FLOAT:
        return 'number';
      case TokenType.IDENTIFIER:
        return token.content.startsWith('@') ? 'decorator' : 'identifier';
      case TokenType.TYPE_KEYWORD:
        return 'type';
      case TokenType.OPERATOR:
        return ['(', ')', '[', ']', '{', '}'].includes(token.content) ? 'punctuation' : 'operator';
      case TokenType.COMMENT:
      case TokenType.MULTILINE_COMMENT:
        return 'comment';
      case TokenType.SPECIFIER:
        return 'specifier';
      case TokenType.UNKNOWN:
        return 'error';
      default:
        return 'token';
    }
  }

  /**
   * Escape HTML special characters
   */
  private escapeHtml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  /**
   * Format an array of tokens
   */
  formatTokens(tokens: Token[]): string {
    const formatted = tokens.map(token => this.formatToken(token));

    if (this.format === OutputFormat.HTML) {
      return this.wrapInHtml(formatted.join(''));
    }

    return formatted.join('');
  }

  /**
   * Wrap content in HTML structure
   */
  private wrapInHtml(content: string): string {
    return `<pre class="verse-code"><code>${content}</code></pre>`;
  }

  /**
   * Generate CSS for HTML output
   */
  static generateCSS(theme: string = 'default'): string {
    const scheme = HTML_THEMES[theme as keyof typeof HTML_THEMES] || HTML_THEMES.default;

    return `
.verse-code {
  background-color: ${theme === 'light' ? '#FFFFFF' : '#1E1E1E'};
  color: ${scheme.default};
  padding: 16px;
  border-radius: 4px;
  overflow-x: auto;
  font-family: 'Courier New', Courier, monospace;
  font-size: 14px;
  line-height: 1.5;
}

.verse-code .keyword { color: ${scheme.keyword}; }
.verse-code .type-keyword { color: ${scheme.dataStructure}; }
.verse-code .declaration { color: ${scheme.declaration}; }
.verse-code .string { color: ${scheme.string}; }
.verse-code .number { color: ${scheme.number}; }
.verse-code .identifier { color: ${scheme.identifier}; }
.verse-code .type { color: ${scheme.type}; }
.verse-code .function { color: ${scheme.function}; }
.verse-code .decorator { color: ${scheme.decorator}; }
.verse-code .operator { color: ${scheme.operator}; }
.verse-code .punctuation { color: ${scheme.punctuation}; }
.verse-code .comment { color: ${scheme.comment}; font-style: italic; }
.verse-code .specifier { color: ${scheme.decorator}; }
.verse-code .error { color: ${scheme.error}; text-decoration: wavy underline; }
`;
  }
}