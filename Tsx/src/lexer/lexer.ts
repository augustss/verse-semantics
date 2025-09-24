import { Token, TokenType, Position } from './token';
import * as fs from 'fs';

/**
 * Lexer for the Verse Programming Language
 *
 * Transforms source code text into a stream of tokens that represent the
 * lexical structure of Verse programs. The lexer handles all Verse-specific
 * syntax patterns and provides detailed token classification.
 *
 * KEY FEATURES:
 *
 * VERSE-SPECIFIC SYNTAX:
 * - Specifiers: <public>, <private>, <scoped(path)>
 * - Combined operators: :=, =>, .., ->, +=, -=, *=, /=
 * - Block comments: <# nested comments supported #>
 * - Decorators: @editable, @replicated
 *
 * TOKEN CLASSIFICATION:
 * - LITERALS: Numbers (42, 3.14), strings ("text"), booleans (true/false)
 * - IDENTIFIERS: Variables (myVar), keywords-as-identifiers (string := "text")
 * - KEYWORDS: Categorized by usage (block-forming, data-structure, declaration, etc.)
 * - OPERATORS: Arithmetic (+, -, *, /), comparison (==, !=, <, >), logical (and, or, not)
 * - SPECIFIERS: Access control and behavioral annotations
 * - TRIVIA: Comments, whitespace, newlines (preserved for formatting)
 *
 * ADVANCED FEATURES:
 * - Context-sensitive tokenization (negative numbers, string escapes)
 * - Nested comment handling with proper depth tracking
 * - Indentation tracking for block-structured syntax
 * - Error recovery with UNKNOWN tokens for invalid sequences
 * - Tab width configuration for consistent indentation handling
 *
 * DESIGN DECISIONS:
 * - Keywords are classified by semantic category rather than lumped together
 * - Position tracking includes both character offset and line/column
 * - Trivia tokens can be combined for efficient parsing
 * - UNKNOWN tokens allow lexer to continue after encountering invalid input
 */
export class Lexer {
  private source: string;
  private position: number = 0;
  private line: number = 1;
  private column: number = 1;
  private tokens: Token[] = [];
  private tabWidth: number;
  private currentLineIndent: number = 0;

  /**
   * Set of allowed specifier keywords in Verse.
   * These appear in angle brackets like <public>, <private>, <scoped(...)>
   */
  private static readonly ALLOWED_SPECIFIERS = new Set([
    'abstract',
    'computes',
    'private',
    'public',
    'protected',
    'final',
    'decides',
    'inline',
    'native',
    'override',
    'suspends',
    'transacts',
    'internal',
    'reads',
    'writes',
    'allocates',
    'transacts',
    'scoped',
    'converges',
    'suspends',
    'castable',
    'concrete',
    'native',
    'unique',
    'final_super',
    'open',
    'closed',
    'native_callable',
    'module_scoped_var_weak_map_key',
    'epic_internal',
  ]);

  /**
   * Keywords that can form indented blocks when followed by ':'
   * Example: if: condition then: body else: alternative
   */
  private static readonly BLOCK_FORMING_KEYWORDS = new Set([
    'if',
    'then',
    'else',
    'for',
    'block',
    'loop',   // loop: for loop expressions
    'array',  // array: for indented array syntax
    'case'    // case: for pattern matching
  ]);

  /**
   * Data structure keywords that are reserved words.
   * These cannot be used as regular identifiers.
   */
  private static readonly DATA_STRUCTURE_KEYWORDS = new Set([
    'module',
    'interface',
    'class',
    'struct',
    'enum'
  ]);

  /**
   * Declaration keywords (var, set, using)
   */
  private static readonly DECL_KEYWORDS = new Set([
    'var',
    'set',
    'using'
  ]);

  /**
   * Type keywords
   */
  private static readonly TYPE_KEYWORDS = new Set([
    'int',
    'float',
    'string',
    'logic',
    'char',
    'any',
    'void'
  ]);

  /**
   * Reserved words that cannot be used as identifiers
   */
  private static readonly RESERVED_WORDS = new Set([
    'do',
    'while',
    'break',
    'continue',
    'return',
    'yield',
    'spawn',
    'sync',
    'race'
  ]);

  /**
   * Creates a new Lexer instance.
   * @param source - The source code to tokenize
   * @param tabWidth - Visual width of tab characters (default: 4)
   */
  constructor(source: string, tabWidth: number = 4) {
    this.source = source;
    this.tabWidth = tabWidth;
    // Set the global tab width for all tokens
    Token.setDefaultTabWidth(tabWidth);
  }

  /**
   * Creates a Lexer from a file path.
   * @param filePath - Path to the source file
   * @param tabWidth - Visual width of tab characters (default: 4)
   * @returns A new Lexer instance with the file contents
   */
  static fromFile(filePath: string, tabWidth: number = 4): Lexer {
    const source = fs.readFileSync(filePath, 'utf-8');
    return new Lexer(source, tabWidth);
  }

  /**
   * Gets the current position in the source code.
   * @returns Position object with line, column, and offset
   */
  private getCurrentPosition(): Position {
    return {
      line: this.line,
      column: this.column,
      offset: this.position
    };
  }

  /**
   * Looks ahead in the source without consuming characters.
   * @param offset - Number of characters to look ahead (default: 0)
   * @returns The character at the offset, or null if at end
   */
  private peek(offset: number = 0): string | null {
    const pos = this.position + offset;
    if (pos >= this.source.length) return null;
    return this.source[pos];
  }

  /**
   * Check if '-' followed by digit could be a negative number based on context.
   * Negative numbers are valid after:
   * - Start of input
   * - Whitespace/newlines
   * - Operators like (, =, :=, +, *, /, %, <, >, <=, >=, and, or, not, ,, ;, {, [
   *
   * But NOT after:
   * - Numbers (should be subtraction)
   * - Identifiers (should be subtraction)
   * - Closing operators like ), }, ]
   */
  private couldBeNegativeNumber(): boolean {
    // If no previous tokens, it's a negative number
    if (this.tokens.length === 0) return true;

    // Find the last non-trivia token
    let lastToken: Token | null = null;
    for (let i = this.tokens.length - 1; i >= 0; i--) {
      const token = this.tokens[i];
      if (!token.isTrivia() && !token.isWhitespace() && !token.isComment()) {
        lastToken = token;
        break;
      }
    }

    // If no meaningful token found, treat as negative number
    if (!lastToken) return true;

    // Check the last meaningful token type
    switch (lastToken.type) {
      // After these tokens, '-' should be subtraction
      case TokenType.INTEGER:
      case TokenType.FLOAT:
      case TokenType.IDENTIFIER:
        return false;

      // After closing brackets/parens, '-' should be subtraction
      case TokenType.OPERATOR:
        if (lastToken.content === ')' || lastToken.content === '}' || lastToken.content === ']') {
          return false;
        }
        // After other operators, '-' can start a negative number
        return true;

      // After these tokens, '-' can start a negative number
      default:
        return true;
    }
  }

  /**
   * Consumes and returns the next character, updating position tracking.
   * @returns The consumed character, or null if at end
   */
  private advance(): string | null {
    if (this.position >= this.source.length) return null;
    const char = this.source[this.position];
    this.position++;
    if (char === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return char;
  }

  /**
   * Consumes characters while a predicate is true.
   * @param predicate - Function to test each character
   * @returns String of all consumed characters
   */
  private skipWhile(predicate: (char: string) => boolean): string {
    let result = '';
    while (this.peek() !== null && predicate(this.peek()!)) {
      result += this.advance();
    }
    return result;
  }

  /**
   * Lexes a string literal (single or double quoted).
   * Handles escape sequences including \n, \t, \r, \\, \", \', \0, \b, \f, \v,
   * as well as hex (\xHH) and unicode (\uHHHH) escapes.
   * @returns A STRING token or null if not at a string
   */
  private lexString(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    const quote = this.peek();
    if (quote !== '"' && quote !== "'") return null;

    this.advance(); // consume opening quote
    let content = '';
    let escaped = false;
    let hasInvalidEscape = false;

    // Valid escape sequences according to GRAMMAR.md
    // <escape-sequence> ::= '\' ( '"' | '\' | 'n' | 'r' | 't' | 'b' | 'f' )
    const validEscapes = new Set(['n', 't', 'r', '\\', '"', 'b', 'f']);

    while (this.peek() !== null) {
      const char = this.peek()!;

      if (escaped) {
        // Validate escape sequence
        if (!validEscapes.has(char)) {
          // Invalid escape sequence - mark as invalid but continue parsing
          hasInvalidEscape = true;
          content += '\\' + this.advance();
        } else {
          content += '\\' + this.advance();
        }
        escaped = false;
      } else if (char === '\\') {
        this.advance(); // consume backslash but don't add it yet
        escaped = true;
      } else if (char === quote) {
        this.advance(); // consume closing quote

        // If we found invalid escapes, return an INVALID_STRING token
        const tokenType = hasInvalidEscape ? TokenType.INVALID_STRING : TokenType.STRING;
        const token = new Token(content, tokenType, startPos, this.getCurrentPosition(), startIndent);
        this.currentLineIndent += token.length;
        return token;
      } else {
        content += this.advance();
      }
    }

    // Unclosed string - this is an error
    const token = new Token(content, TokenType.UNKNOWN, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes numeric literals (integers and floats).
   * Handles negative numbers, decimals (including leading dot like .5),
   * trailing decimals (like 3.), and scientific notation (1.5e10).
   * Correctly distinguishes between decimals and the '..' range operator.
   * @returns An INTEGER or FLOAT token, or null if not at a number
   */
  private lexNumber(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    let content = '';
    let isFloat = false;
    let hasDigitsBeforeDot = false;
    let hasDigitsAfterDot = false;

    // Check for negative number
    if (this.peek() === '-') {
      content += this.advance();
    }

    // Check for leading decimal point (e.g., .5)
    if (this.peek() === '.' && this.peek(1) !== null && /[0-9]/.test(this.peek(1)!)) {
      isFloat = true;
      content += this.advance(); // consume '.'
      while (this.peek() !== null && /[0-9]/.test(this.peek()!)) {
        content += this.advance();
        hasDigitsAfterDot = true;
      }
    } else {
      // Collect digits before decimal
      while (this.peek() !== null && /[0-9]/.test(this.peek()!)) {
        content += this.advance();
        hasDigitsBeforeDot = true;
      }

      // Check for decimal point
      if (this.peek() === '.') {
        // IMPORTANT: Check if this is the ".." range operator first
        if (this.peek(1) === '.') {
          // This is the start of ".." operator, don't consume the dot
          // Return the integer we have so far
        } else if (this.peek(1) === null || !/[0-9]/.test(this.peek(1)!)) {
          // Trailing decimal (e.g., 3.)
          isFloat = true;
          content += this.advance(); // consume '.'
        } else {
          // Has digits after decimal (e.g., 3.14)
          isFloat = true;
          content += this.advance(); // consume '.'
          while (this.peek() !== null && /[0-9]/.test(this.peek()!)) {
            content += this.advance();
            hasDigitsAfterDot = true;
          }
        }
      }
    }

    // Check for scientific notation
    if ((hasDigitsBeforeDot || hasDigitsAfterDot) && this.peek() !== null && /[eE]/.test(this.peek()!)) {
      isFloat = true;
      content += this.advance(); // consume 'e' or 'E'
      if (this.peek() !== null && /[+-]/.test(this.peek()!)) {
        content += this.advance(); // consume sign
      }
      while (this.peek() !== null && /[0-9]/.test(this.peek()!)) {
        content += this.advance();
      }
    }

    if (content.length === 0 || content === '-' || content === '-.') return null;

    const token = new Token(
      content,
      isFloat ? TokenType.FLOAT : TokenType.INTEGER,
      startPos,
      this.getCurrentPosition(),
      startIndent
    );
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes single-line comments starting with #.
   * The comment extends to the end of the line.
   * @returns A COMMENT token or null if not at a comment
   */
  private lexSingleLineComment(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    if (this.peek() !== '#') return null;

    // Check if it's a multiline comment start
    if (this.peek(1) === '<') return null;

    let content = this.advance()!; // consume '#' and include it
    content += this.skipWhile(char => char !== '\n');

    const token = new Token(content, TokenType.COMMENT, startPos, this.getCurrentPosition(), startIndent);
    // Comments have length 0, so don't update indentation
    return token;
  }

  /**
   * Lexes multiline comments delimited by <# and #>.
   * Supports nested comments by tracking nesting level.
   * @returns A MULTILINE_COMMENT token or null if not at a multiline comment
   */
  private lexMultiLineComment(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    if (this.peek() !== '<' || this.peek(1) !== '#') return null;

    let content = '';
    let nestingLevel = 0;

    while (this.peek() !== null) {
      // Check for opening <#
      if (this.peek() === '<' && this.peek(1) === '#') {
        content += this.advance(); // consume '<'
        content += this.advance(); // consume '#'
        nestingLevel++;
      }
      // Check for closing #>
      else if (this.peek() === '#' && this.peek(1) === '>') {
        content += this.advance(); // consume '#'
        content += this.advance(); // consume '>'
        nestingLevel--;
        if (nestingLevel === 0) {
          const token = new Token(content, TokenType.MULTILINE_COMMENT, startPos, this.getCurrentPosition(), startIndent);
          // Comments have length 0, so don't update indentation
          return token;
        }
      }
      else {
        content += this.advance();
      }
    }

    // Unclosed multiline comment (nesting level > 0)
    const token = new Token(content, TokenType.MULTILINE_COMMENT, startPos, this.getCurrentPosition(), startIndent);
    // Comments have length 0, so don't update indentation
    return token;
  }

  /**
   * Lexes consecutive space characters.
   * @returns A SPACE token or null if not at spaces
   */
  private lexSpace(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    if (this.peek() !== ' ') return null;

    const content = this.skipWhile(char => char === ' ');
    const token = new Token(content, TokenType.SPACE, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes consecutive tab characters.
   * @returns A TAB token or null if not at tabs
   */
  private lexTab(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    if (this.peek() !== '\t') return null;

    const content = this.skipWhile(char => char === '\t');
    const token = new Token(content, TokenType.TAB, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes newline characters (\n, \r, or \r\n).
   * Resets indentation tracking for the new line.
   * @returns A NEWLINE token or null if not at a newline
   */
  private lexNewline(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    const char = this.peek();

    if (char === '\n') {
      this.advance();
      const token = new Token('\n', TokenType.NEWLINE, startPos, this.getCurrentPosition(), startIndent);
      this.currentLineIndent = 0; // Reset indentation for new line
      return token;
    } else if (char === '\r') {
      this.advance();
      if (this.peek() === '\n') {
        this.advance();
        const token = new Token('\r\n', TokenType.NEWLINE, startPos, this.getCurrentPosition(), startIndent);
        this.currentLineIndent = 0; // Reset indentation for new line
        return token;
      }
      const token = new Token('\r', TokenType.NEWLINE, startPos, this.getCurrentPosition(), startIndent);
      this.currentLineIndent = 0; // Reset indentation for new line
      return token;
    }

    return null;
  }

  /**
   * Lexes operator tokens including single and multi-character operators.
   * Handles: :=, ==, !=, <=, >=, *=, +=, -=, /=, =>, ->, .., and single-char operators.
   * Special handling for '-' to distinguish minus from negative numbers,
   * and '.' to distinguish from decimal numbers.
   * @returns An OPERATOR token or null if not at an operator
   */
  private lexOperator(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    const char = this.peek();
    const next = this.peek(1);

    if (!char) return null;

    let content = '';

    // Two-character operators (check these first)
    if (char === ':' && next === '=') {
      content = this.advance()! + this.advance()!; // :=
    } else if (char === '=' && next === '=') {
      content = this.advance()! + this.advance()!; // ==
    } else if (char === '!' && next === '=') {
      content = this.advance()! + this.advance()!; // !=
    } else if (char === '<' && next === '=') {
      content = this.advance()! + this.advance()!; // <=
    } else if (char === '>' && next === '=') {
      content = this.advance()! + this.advance()!; // >=
    } else if (char === '*' && next === '=') {
      content = this.advance()! + this.advance()!; // *=
    } else if (char === '+' && next === '=') {
      content = this.advance()! + this.advance()!; // +=
    } else if (char === '-' && next === '=') {
      content = this.advance()! + this.advance()!; // -=
    } else if (char === '/' && next === '=') {
      content = this.advance()! + this.advance()!; // /=
    } else if (char === '=' && next === '>') {
      content = this.advance()! + this.advance()!; // =>
    } else if (char === '-' && next === '>') {
      content = this.advance()! + this.advance()!; // ->
    } else if (char === '.' && next === '.') {
      // Check if this is "..." which is invalid
      if (this.peek(2) === '.') {
        // Consume all three dots as UNKNOWN token
        content = this.advance()! + this.advance()! + this.advance()!; // ...
        const token = new Token(content, TokenType.UNKNOWN, startPos, this.getCurrentPosition(), startIndent);
        this.currentLineIndent += token.length;
        return token;
      }
      content = this.advance()! + this.advance()!; // ..
    }
    // Special case: don't treat '-' as operator if followed by digit (negative number)
    // But only in contexts where negative numbers make sense
    else if (char === '-' && next && /[0-9]/.test(next) && this.couldBeNegativeNumber()) {
      return null; // Let lexNumber handle it
    }
    // Special case: don't treat '.' as operator if it's part of a float
    else if (char === '.' && next && /[0-9]/.test(next)) {
      return null; // Let lexNumber handle .5
    }
    // Single character operators
    else if ('<>=+*/%-;:,{}()[].?!'.includes(char)) {
      content = this.advance()!;
    } else {
      return null;
    }

    const token = new Token(content, TokenType.OPERATOR, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes specifier tokens in angle brackets like <public>, <private>.
   * Special handling for <scoped(...)> which captures content in parentheses.
   * Only recognizes keywords from ALLOWED_SPECIFIERS set.
   * @returns A SPECIFIER token or null if not at a specifier
   */
  private lexSpecifier(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;

    // Check for < followed by identifier and >
    if (this.peek() !== '<') return null;

    // Look ahead to check if this is a specifier pattern
    let lookahead = 1;
    let wordContent = '';
    let scopedContentValue: string | undefined = undefined;

    // Check next character should be a letter or underscore
    const nextChar = this.peek(lookahead);
    if (!nextChar || !/[a-zA-Z_]/.test(nextChar)) {
      return null; // Not a specifier, let operator handle <
    }

    // Collect the word inside < >
    while (this.peek(lookahead) && /[a-zA-Z_]/.test(this.peek(lookahead)!)) {
      wordContent += this.peek(lookahead);
      lookahead++;
    }

    // Special handling for <scoped(...)>
    if (wordContent === 'scoped' && this.peek(lookahead) === '(') {
      lookahead++; // skip '('

      let parenContent = '';
      let parenDepth = 1;

      // Collect everything inside parentheses, handling nested parens
      while (this.peek(lookahead) && parenDepth > 0) {
        const char = this.peek(lookahead)!;
        if (char === '(') {
          parenDepth++;
          parenContent += char;
        } else if (char === ')') {
          parenDepth--;
          if (parenDepth > 0) {
            parenContent += char;
          }
        } else {
          parenContent += char;
        }
        lookahead++;
      }

      // Check if we have closing ) and >
      if (parenDepth === 0 && this.peek(lookahead) === '>') {
        scopedContentValue = parenContent;

        // Now consume the entire specifier
        let content = '';
        content += this.advance()!; // consume '<'
        for (let i = 0; i < wordContent.length; i++) {
          content += this.advance(); // consume 'scoped'
        }
        content += this.advance()!; // consume '('
        for (let i = 0; i < parenContent.length; i++) {
          content += this.advance(); // consume the content
        }
        content += this.advance()!; // consume ')'
        content += this.advance()!; // consume '>'

        const token = new Token(content, TokenType.SPECIFIER, startPos, this.getCurrentPosition(), startIndent, scopedContentValue);
        this.currentLineIndent += token.length;
        return token;
      } else {
        return null; // Invalid scoped specifier
      }
    }

    // Check if next character is > for regular specifiers
    if (this.peek(lookahead) !== '>') {
      return null; // Not a specifier pattern
    }

    // Check if this is an allowed specifier
    if (!Lexer.ALLOWED_SPECIFIERS.has(wordContent)) {
      return null; // Not a valid specifier, let operators handle < and >
    }

    // Now consume the entire specifier
    let content = this.advance()!; // consume '<'
    for (let i = 0; i < wordContent.length; i++) {
      content += this.advance(); // consume the word
    }
    content += this.advance()!; // consume '>'

    const token = new Token(content, TokenType.SPECIFIER, startPos, this.getCurrentPosition(), startIndent, undefined);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Lexes identifiers and keywords.
   * Identifiers can start with @ (like @identifier) or letters/underscore.
   * Recognizes block-forming keywords (if, then, else, for, block, array).
   * @returns An IDENTIFIER or BLOCK_FORMING_KEYWORD token, or null
   */
  private lexIdentifier(): Token | null {
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    const firstChar = this.peek();

    // Check for @identifier syntax
    let content = '';
    if (firstChar === '@') {
      content = this.advance()!; // consume '@'
      // Next character must be letter or underscore
      if (!this.peek() || !/[a-zA-Z_]/.test(this.peek()!)) {
        // Not an identifier, backtrack
        this.position--;
        this.column--;
        return null;
      }
    } else if (!firstChar || !/[a-zA-Z_]/.test(firstChar)) {
      // Regular identifier must start with letter or underscore
      return null;
    }

    // Consume identifier characters (letters, digits, and underscores)
    while (this.peek() !== null && /[a-zA-Z0-9_]/.test(this.peek()!)) {
      content += this.advance();
    }

    // Check if this is a reserved keyword
    let tokenType: TokenType;
    if (Lexer.DATA_STRUCTURE_KEYWORDS.has(content)) {
      tokenType = TokenType.DATA_STRUCTURE_KEYWORD;
    } else if (Lexer.BLOCK_FORMING_KEYWORDS.has(content)) {
      tokenType = TokenType.BLOCK_FORMING_KEYWORD;
    } else if (Lexer.DECL_KEYWORDS.has(content)) {
      tokenType = TokenType.DECL_KEYWORD;
    } else if (Lexer.TYPE_KEYWORDS.has(content)) {
      tokenType = TokenType.TYPE_KEYWORD;
    } else if (Lexer.RESERVED_WORDS.has(content)) {
      tokenType = TokenType.RESERVED_WORD;
    } else {
      tokenType = TokenType.IDENTIFIER;
    }

    const token = new Token(content, tokenType, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += token.length;
    return token;
  }

  /**
   * Attempts to lex the next token from the current position.
   * Tries token types in order: whitespace, comments, strings, specifiers,
   * identifiers, operators, numbers. Returns UNKNOWN for unrecognized chars.
   * @returns The next token or EOF token at end of input
   */
  private lexNext(): Token | null {
    if (this.position >= this.source.length) {
      return new Token('', TokenType.EOF, this.getCurrentPosition(), undefined, this.currentLineIndent);
    }

    // Try each token type
    let token: Token | null;

    // Whitespace
    if ((token = this.lexSpace()) !== null) return token;
    if ((token = this.lexTab()) !== null) return token;
    if ((token = this.lexNewline()) !== null) return token;

    // Comments
    if ((token = this.lexMultiLineComment()) !== null) return token;
    if ((token = this.lexSingleLineComment()) !== null) return token;

    // Strings
    if ((token = this.lexString()) !== null) return token;

    // Specifiers (must come before operators to handle <word> pattern)
    if ((token = this.lexSpecifier()) !== null) return token;

    // Identifiers (must come before numbers to handle names correctly)
    if ((token = this.lexIdentifier()) !== null) return token;

    // Operators (must come before numbers to handle negative numbers correctly)
    if ((token = this.lexOperator()) !== null) return token;

    // Numbers
    if ((token = this.lexNumber()) !== null) return token;

    // Unknown character
    const startPos = this.getCurrentPosition();
    const startIndent = this.currentLineIndent;
    const char = this.advance()!;
    const unknownToken = new Token(char, TokenType.UNKNOWN, startPos, this.getCurrentPosition(), startIndent);
    this.currentLineIndent += unknownToken.length;
    return unknownToken;
  }

  /**
   * Tokenizes the entire source code.
   * @returns Array of all tokens including EOF token
   */
  public tokenize(): Token[] {
    this.tokens = [];
    let token: Token | null;

    while ((token = this.lexNext()) !== null) {
      this.tokens.push(token);
      if (token.isEOF()) break;
    }

    return this.tokens;
  }

  /**
   * Gets the array of tokens after tokenization.
   * @returns Array of tokens
   */
  public getTokens(): Token[] {
    return this.tokens;
  }
}